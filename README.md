# wRapped <img src="man/figures/logo.png" align="right" height="139" alt="" />

## Install Package

```r
install.packages("wRapped")

install.packages("devtools")
devtools::install_github("hschmidt12/wRapped")
```

## Function

Create your own end-of-year summary of R packages and functions using `wRapped`! You can generate a table of your top 5 packages and functions, as well as a plot of your most used packages, following the example below.

```r
wrap_it_up(path = "/Users/hschmidt12/Experiments/", year = 2025)
```

![Example](https://github.com/hschmidt12/wRapped/blob/main/man/figures/example_2025_wrapped.png)
