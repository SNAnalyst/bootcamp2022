

.onLoad <- function(libname, pkgname) {
  op <- options()
  invisible()
}


.onAttach <- function(lib, pkg,...){
  print_message <-  paste("\n",
                          "Welcome to bootcamp version ", utils::packageDescription("bootcamp")$Version,
                          "\n",
                          "Type ?bootcamp to access the package documentation\n\n",
						  "or help(package = 'bootcamp')\n\n",
                          "To suppress this message use:\n",
                          "\tsuppressPackageStartupMessages(library(bootcamp))\n\n",
                          "You can check if you have the latest version with 'check_bootcamp()'\n",
                          sep = "")
  packageStartupMessage(print_message)
}


