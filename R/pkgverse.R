
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
#' @param pkg Name of your -verse (verse will be appending to whatever you enter here).
#' @param pkgs Character vector of packages.
#' @return Installs a package of desired name (with 'verse' attached to what is supplied)
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
  ## create pkg folder
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
}
