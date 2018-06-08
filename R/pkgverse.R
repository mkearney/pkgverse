
build_rfuns <- function(pkg, pkgs) {
  pkgs <- paste(pkgs, collapse = '", "')
  rfuns <- sub("\\{PKGS\\}", pkgs, docs$rfuns)
  gsub("\\{PKG\\}", pkg, rfuns)
}

build_description <- function(pkg) {
  gsub("\\{PKG\\}", pkg, docs$description)
}

#' pkgverse
#'
#' Create your own pkgverse
#'
#' @param pkg Name of your set of packages. It's recommended that users append
#'   this name with something like 'verse' or otherwise provide some kind of
#'   explicit/obvious indicator that the package name is a stand in for a
#'   selection of packages.
#' @param pkgs Character vector of package names.
#' @return Installs a package of desired name, which, when loaded, will load,
#'   handle, and display conflicts of the packages supplied via \code{pkgs}.
#' @examples
#' \dontrun{
#'
#' ## example: web scraping package universe
#' pkgverse("webscraperverse", c("xml2", "rvest", "httr", "RSelenium"))
#'
#' }
#' @export
pkgverse <- function(pkg, pkgs) {
  rfuns <- build_rfuns(pkg, pkgs)
  ## temp dir to write pkg
  tmp <- tempdir(check = TRUE)
  ## original wd
  owd <- getwd()
  ## return to owd on exit
  on.exit(setwd(owd))
  ## set wd to tmp dir
  setwd(tmp)
  ## create pkg folder (remove if already exists)
  if (dir.exists(pkg)) {
    unlink(pkg, recursive = TRUE)
  }
  dir.create(pkg)
  ## cd pkg dir
  setwd(pkg)
  ## create R directory in pkg path
  dir.create("R")
  ## save rfuns to R dir
  writeLines(rfuns, file.path("R", "verse.R"))
  ## build description file
  description <- build_description(pkg)
  ## write description
  writeLines(description, "DESCRIPTION")
  ## add deps
  lapply(pkgs, usethis::use_package)
  ## build package
  devtools::document()
  ## install package
  devtools::install()
  ## remove temp pkg source files
  unlink(".", recursive = TRUE)
}
