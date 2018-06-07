
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkgverse

Create your own universe of packages a la
[tidyverse](https://github.com/tidyverse/tidyverse).

## Installation

You can install the dev version of pkgverse from Github with:

``` r
devtools::install_github("pkgverse")
```

## Example

Create a **webscraperverse**

``` r
## name of pkg-verse
pkg <- "webscraper"

## packages to include
pkgs <- c("xml2", "rvest", "httr", "RSelenium")

## build webscraperverse
pkgverse(pkg, pkgs)
```

Now load your pkg universe:

``` r
## load webscraperverse
library(webscraperverse)
> ── Attaching packages ───────────────────────────────────────── webscraperverse 0.0.1 ──
> ✔ xml2      1.2.0     ✔ httr      1.3.1
> ✔ rvest     0.3.2     ✔ RSelenium 1.7.1
```
