.onAttach <- function(libname, pkgname) {
  if (stats::runif(1) > .9) {
    packageStartupMessage("Learn more about sjmisc with 'browseVignettes(\"sjmisc\")'.")
  } else if (stats::runif(1) > .9) {
    packageStartupMessage("Install package \"strengejacke\" from GitHub (`devtools::install_github(\"strengejacke/strengejacke\")`) to load all sj-packages at once!")
  }
}
