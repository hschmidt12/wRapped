wrap_it_up <- function(path, year = 2025) {
  
  # ----- load required packages ----- #
  # required packages
  required_packages <- c(
    "grid", "fs", "stringr", "purrr", "ggimage", "ggtext",
    "RCurl", "magick", "tidyverse")
  
  # install any missing packages
  missing_packages <- required_packages[!(required_packages %in% 
                                            installed.packages()[,"Package"])]
  if (length(missing_packages) > 0) {install.packages(missing_packages)}
  
  # load with suppressed start up messages
  suppressPackageStartupMessages({
    lapply(required_packages, library, character.only = TRUE)})
  
  # ----- file search ----- #
  message("🔍 Searching for files...")
  files <- dir_ls(path,
                  recurse = TRUE,
                  type = "file", 
                  regexp = "\\.(R|r|Rmd|rmd|RMD|qmd|QMD)$")
  
  # filter by specified year
  files <- files[file_info(files)$modification_time %>% format("%Y") == year]
  
  # stop if no files found, throw message
  if (length(files) == 0) {
    stop("⚠️️ No files found for the specified year")
  }
  
  # print number of files found
  message(paste("Found ", length(files), " files from ", year, ".", sep = ""))
  
  # ----- file reading ----- #
  message("📄 Reading file contents...")
  contents <- map_chr(files, read_file)
  all_text <- paste(contents, collapse = "\n")
  
  # ----- extract packages ----- #
  pkg_pattern <- "(?<=library\\(|require\\()\\s*([A-Za-z0-9\\.]+)\\s*(?=\\))"
  packages <- str_match_all(all_text, pkg_pattern) |>
    unlist() |>
    as.character() |>
    discard(is.na)
  
  top_packages <- as_tibble(packages) |>
    count(value, sort = TRUE, name = "n") |>
    slice_head(n = 5) |>
    rename(package = value)
  
  # ----- extract functions ----- #
  fun_pattern <- "\\b([A-Za-z0-9_\\.]+)\\s*\\("
  functions <- str_match_all(all_text, fun_pattern) |>
    unlist() |>
    as.character()
  
  # remove super common functions like if, for, while, and function
  functions <- functions[!functions %in% c("if", "for", "while", "function")]
  
  top_functions <- as_tibble(functions) |>
    count(value, sort = TRUE, name = "n") |>
    slice_head(n = 5) |>
    rename(func = value)
  
  # ----- add hex images for packages if available ----- #
  message("🖼 Gathering package hexagon stickers...")
  
  # define default image
  #default_hex <- "https://raw.githubusercontent.com/rstudio/hex-stickers/main/PNG/tidyverse.png"
  default_hex <- "https://www.r-project.org/logo/Rlogo.png"
  
  # add hex url to top_packages
  top_packages$hex_url <- paste0(
    "https://raw.githubusercontent.com/rstudio/hex-stickers/main/PNG/",
    top_packages$package,
    ".png", sep = "")
  
  # check validity of each URL
  top_packages$url_validity <- url.exists(top_packages$hex_url)
  
  # replace invalid URLs with default R image
  top_packages$valid_hex_url <- ifelse(top_packages$url_validity, 
                                       top_packages$hex_url, 
                                       default_hex)
  
  # create custom y axis image + text vector
  axis_label_vec <- sprintf(
    "<img src='%s' width='50'/>",
    top_packages$valid_hex_url)
  
  # wrapped label
  img_path <- system.file("extdata", "wRapped.jpg", package = "myPackage")
  wrapped_label_vec <- sprintf("<img src='%s' width='20'/>", img_path)

  # name vector by package to match labels to levels
  names(axis_label_vec) <- top_packages$package
  
  # ----- create top package plot with hex stickers ----- #
  message("📊 Saving packages plot...")
  
  # plot
  top_packages_plot <- ggplot(top_packages, aes(x = n, y = reorder(package, n))) +
    geom_col() +
    geom_text(aes(label = package),
              hjust = 1.05, fontface = "bold",
              color = "white",
              size = 4) +
    theme_minimal() +
    labs(caption = paste0("\n", "Made with ", wrapped_label_vec, "                                                ", year, " Recap", sep = "")) +
    ggtitle("My Top R Packages\n") +
    scale_y_discrete(labels = axis_label_vec) +
    theme(
      axis.text.y = element_markdown(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.y = element_blank(),
      plot.caption = element_markdown(),
      plot.title = element_text(hjust = 0.3),
      plot.background = element_rect(color = "white", fill = "white"),
      plot.margin = unit(c(1,1,1,1),"cm")) 
  
  # save
  ggsave(top_packages_plot, 
         filename = paste(year, "_packages_wrapped.png", sep = ""),
         dpi = 300,
         width = 5,
         height = 6)
  
  # ----- return results ----- #
  return(list(
    files_scanned = files,
    top_packages = top_packages,
    top_functions = top_functions,
    top_packages_plot = top_packages_plot
  ))
  
}
