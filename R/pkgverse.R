
build_rfuns <- function(pkg, pkgs) {
  pkgs <- paste(pkgs, collapse = '", "')
  rfuns <- sub("\\{PKGS\\}", pkgs, docs$rfuns)
  gsub("\\{PKG\\}", pkg, rfuns)
}

build_description <- function(pkg) {
  gsub("\\{PKG\\}", pkg, docs$description)
}

install_if <- function(pkg) {
  x <- vapply(pkg, install_if_, FUN.VALUE = logical(1))
  data.frame(pkg = names(x), required_install = x,
    row.names = NULL)
}

install_if_ <- function(x) {
  i <- !requireNamespace(x, quietly = TRUE)
  if (i) {
    install.packages(x, quiet = TRUE)
  }
  i
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
#' @param keep If not `FALSE`, then used to indicate location to keep the
#'   `pkg` dir (name), which should  _not_ include the `pkg` name but should
#'   exist. It will be `path.expand()`ed and tested for presence.
#' @param use If not `NULL` (the default), then a character vector of `usethis`
#'   "use" functions (the bit after the first underscore) that make sense for
#'   a package.
#' @param install_if Logical indicating whether to install (from CRAN) any
#'   packages in `pkgs` that are not already installed on the system. Defaults
#'   to `FALSE`.
#' @return Installs a package of desired name, which, when loaded, will load,
#'   handle, and display conflicts of the packages supplied via \code{pkgs}.
#' @examples
#' \dontrun{
#'
#' ## vector of pkgs
#' tidyweb <- c("curl", "jsonlite", "httr", "xml2", "rvest", "purrr", "dplyr",
#'   "stringi", "gdns", "urltools", "iptools", "seleniumPipes", "webdriver",
#'   "HARtools", "xslt", "V8", "webreadr", "openssl", "splashr")
#'
#' ## create tidyweb pkgverse
#' pkgverse("tidyweb", tidyweb,
#'   keep = "~/packages",
#'   use = c("readme_rmd", "rstudio", "testthat", "mit_license", "git")
#' )
#'
#'
#' }
#' @export
pkgverse <- function(pkg, pkgs, keep = FALSE, use = NULL, install_if = FALSE) {

  ## install uninstalled pkgs if desired
  if (install_if) {
    install_if(pkgs)
  }

  ## original wd
  owd <- getwd()
  ## return to owd on exit
  on.exit(setwd(owd))

  ## determine and configure path location and source storage
  keep <- keep[1]
  if (!is.logical(keep)) {

    if (!is.character(keep))
      stop("`keep` should be a logical or character.", call. = FALSE)

    keep <- path.expand(keep)
    if (!dir.exists(keep)) {
      stop("`keep` must exist.", call. = FALSE)
    }

    if (dir.exists(file.path(keep, pkg))) {
      stop("The package you are trying to creatre already exists.",
        call. = FALSE)
    }

    tmp <- keep

  } else {
    ## temp dir to write pkg (check if R version 3.5.0 or greater)
    if (getRversion() < '3.5.0') {
      tmp <- tempdir()
    } else {
      tmp <- tempdir(check = TRUE)
    }
    ## cleanup on exit
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  }

  ## build .R source file
  rfuns <- build_rfuns(pkg, pkgs)

  ## set wd to tmp dir
  setwd(tmp)

  ## rm pkg folder if it already exists
  if (dir.exists(pkg)) {
    unlink(pkg, recursive = TRUE)
  }
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

  ## if use funs provided and usethis is installed
  if (!is.null(use) && requireNamespace("usethis", quietly = TRUE)) {
    ## expand into full use_* names
    use <- sprintf("use_%s", sub("^use_", "", use))

    ## attach usethis to make functions discoverable
    #if (!isNamespaceLoaded("usethis")) {
      attachNamespace("usethis")
    #}

    ## find usethis functions
    use <- vapply(use, exists, FUN.VALUE = logical(1),
      where = "package:usethis", mode = "function") # are they there?

    ## warn if any usethis functions were not found
    if (any(!use)) {
      not_found <- paste(names(use[!use]), collapse = ", ")
      warning("the following usethis functions were not found: ", not_found)
    }

    ## only select found names
    use <- names(use[use])

    ## iterate through usethis functions
    for (use_f in use) {
      f <- get(use_f, pos = "package:usethis", mode = "function")
      ## a few have actions on `interactive()` to mute them
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
  devtools::install(reload = FALSE, quiet = TRUE)
}
