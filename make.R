## sysdata
description <- readlines("sources/DESCRIPTION")
rfuns <- readlines("sources/rfuns.R")
docs <- list(description = description, rfuns = rfuns)
save(docs, file = "~/R/pkgverse/R/sysdata.rda")

## test
pkg <- "webscraper"
pkgs <- c("xml2", "rvest", "httr", "RSelenium")
pkgverse(pkg, pkgs)
