#' Wrap your most used R packages in a given year
#'
#' This function scours all R scripts in a named path (.Rmd, .qmd, .R, etc.) 
#' to determine your most-used packages for a specified year. Top packages
#' and top functions are printed and a plot showing your top packages is 
#' generated. 
#' @param path path where your scripts can be found
#' @param year year you want to wrap
#' @return top 5 functions and packages plus plot 
#' @export
wrap_it_up <- function(path, year = 2025) {
  
  # ----- load required packages ----- #
  # required packages
  required_packages <- c(
    "grid", "fs", "stringr", "purrr", "ggimage", "ggtext",
    "RCurl", "magick", "sysfonts", "showtext", "here", "tidyverse")
  
  # install any missing packages
  missing_packages <- required_packages[!(required_packages %in% 
                                            installed.packages()[,"Package"])]
  if (length(missing_packages) > 0) {install.packages(missing_packages)}
  
  # load with suppressed start up messages
  suppressPackageStartupMessages({
    lapply(required_packages, library, character.only = TRUE)})
  
  # add fonts
  sysfonts::font_add_google(name = "Fredoka", family = "fredoka")
  showtext::showtext_auto()
  
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
    "<img src='%s' width='45'/>",
    top_packages$valid_hex_url)
  
  # wrapped label
  wrapped_label_vec <- "<img src='https://raw.githubusercontent.com/hschmidt12/wRapped/main/wRapped.png' width='20'/>"

  # name vector by package to match labels to levels
  names(axis_label_vec) <- top_packages$package
  
  # ----- create top package plot with hex stickers ----- #
  message("📊 Saving packages plot...")
  
  # plot
  top_packages_plot <- ggplot(top_packages, aes(x = n, y = reorder(package, n))) +
    geom_col(fill = "#4281A4") +
    geom_text(aes(label = package),
              hjust = 1.05, 
              color = "white", family = "fredoka",
              size = 15) +
    theme_minimal() +
    labs(title = "My Top R Packages",
         subtitle = paste(year, " Recap", sep = ""),
         caption = paste0("\n", wrapped_label_vec, sep = "")) +
    scale_y_discrete(labels = axis_label_vec) +
    theme(
      axis.text.y = element_markdown(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 20, family = "fredoka"),
      axis.title.x = element_blank(),
      axis.ticks.y = element_blank(),
      plot.caption = element_markdown(),
      plot.title = element_text(hjust = 0.3, size = 60, family = "fredoka"),
      plot.subtitle = element_text(hjust = 0.38, size = 50, family = "fredoka"),
      plot.background = element_rect(color = "#8DCECC", fill = "#8DCECC"),
      plot.margin = unit(c(1,1,1,1),"cm")) 
  
  # save
  ggsave(top_packages_plot, 
         filename = here(paste(year, "_packages_wrapped.png", sep = "")),
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
