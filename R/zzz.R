.onAttach <- function(libname, pkgname) {
  msg <- paste("WARNING: This is an experimental branch for internal testing only.\n",
               "Should only be used by the developers.",
               "Use the following to install the current public release:\n",
               "remotes::install_github('sfcheung/semptools')")
  packageStartupMessage(msg)
}
