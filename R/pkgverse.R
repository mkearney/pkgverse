
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
#' @md
#' @param pkg Name of your -verse (verse will be appending to whatever you enter here).
#' @param pkgs Character vector of packages.
#' @param keep if not `FALSE` then a string with the path to create the `pkg` dir (name should)
#'        _not_ include the `pkg` name but should exist. It will be `path.expand()`ed
#'        and tested for presence.
#' @param use if not `NULL` then a character vector of `usethis` "use" functions (the bit
#'        after the first underscore) that make sense for a package.
#' @return Installs a package of desired name (with 'verse' attached to what is supplied)
#' @export
#' @examples \dontrun{
#' c("curl", "jsonlite", "httr", "xml2", "rvest", "purrr", "dplyr",
#'   "stringi", "gdns", "urltools", "iptools", "seleniumPipes", "webdriver",
#'   "HARtools", "xslt", "V8", "webreadr", "openssl", "splashr") -> tidyweb
#'
#' pkgverse(
#'   "tidyweb", tidyweb, "~/packages",
#'   use=c("readme_rmd", "rstudio", "testthat", "mit_license", "git")
#' )
#' }
pkgverse <- function(pkg, pkgs, keep=FALSE, use=NULL) {

  ## original wd
  owd <- getwd()
  ## return to owd on exit
  on.exit(setwd(owd), add=TRUE)

  keep <- keep[1]
  if (!is.logical(keep)) {

    if (!is.character(keep))
      stop("`keep` should be a logical or character.", call.=FALSE)

    keep <- path.expand(keep)
    if (!dir.exists(keep))
      stop("`keep` must exist.", call.=FALSE)

    if (dir.exists(file.path(keep, pkg)))
      stop("The package you are trying to creatre already exists.", call.=FALSE)

    tmp <- keep

  } else {
    ## temp dir to write pkg
    tmp <- tempdir(check = TRUE)
    on.exit(unlink(tmp, recursive=TRUE), add=TRUE) # cleanup
  }

  rfuns <- build_rfuns(pkg, pkgs)
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


  if (!is.null(use)) { # cld make this a tad safer if usethis is not found

    use <- sprintf("use_%s", use) # the real name
    if (!isNamespaceLoaded("usethis")) attachNamespace("usethis") # so we can find them
    use <- vapply(use, exists, logical(1), where="package:usethis", mode="function") # are they there?
    use <- names(use[use]) # only want the ones that are there ; mebbe warn if not?
    for (use_f in use) {
       f <- get(use_f, pos = "package:usethis", mode = "function")
       # a few have actions on `interactive()` to mute them
       if ((use_f == "use_readme_rmd") | (use_f == "use_readme_md")) {
         f(FALSE)
       } else {
         f()
       }
    }

  }

  ## build package
  devtools::document()
  ## install package
  devtools::install()
}
