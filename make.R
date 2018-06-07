
usethis::use_build_ignore("sources.*")

x <- search_files("::", "data")
sub("::$", "", unlist(chr::chr_extract(x, "[[:alpha:]\\.]+::"))) %>% table()



description <- readlines("~/R/pkgverse/sources/DESCRIPTION")
#description <- paste(description, collapse = "\n")

rfuns <- readlines("~/R/pkgverse/sources/rfuns.R")
#rfuns <- paste(rfuns, collapse = "\n")

docs <- list(description = description, rfuns = rfuns)
save(docs, file = "~/R/pkgverse/R/sysdata.rda")

usethis::use_git()
usethis::use_git_ignore("sources")

pkg <- "kearney"
pkgs <- c("rtweet", "tfse", "chr", "funique")

pkgverse("mwk", pkgs)
