#Este es un mensaje de prueba (Jaime Mosquera)
.onAttach <- function(libname, pkgname){
  initmessage <- "<<<<<<<<<<<<<<<<<<<<<   EstimationTools Version 1.2.0  >>>>>>>>>>>>>>>>>>>>>
Feel free to report bugs in https://github.com/Jaimemosg/EstimationTools/issues"
  packageStartupMessage(initmessage)
  invisible()
}
