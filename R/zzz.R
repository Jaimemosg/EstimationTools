#Este es un mensaje de prueba (Jaime Mosquera)
.onAttach <- function(libname, pkgname){
<<<<<<< HEAD
  initmessage <- "><<<<<<<<<<<<<<<<<<<<<  EstimationTools Version 1.2.0  >>>>>>>>>>>>>>>>>>>>><
Feel free to report bugs in https://github.com/Jaimemosg/EstimationTools/issues"
  packageStartupMessage(initmessage)
=======
  initmessage1 <- "<<<<<<<<<<<<<<<<<<<<<   EstimationTools Version"
  initmessage2 <- utils::packageDescription("EstimationTools")$Version
  initmessage3 <- ">>>>>>>>>>>>>>>>>>>>>
  Feel free to report bugs in https://github.com/Jaimemosg/EstimationTools/issues"
  packageStartupMessage(paste0(initmessage1, initmessage2, initmessage3))
>>>>>>> 7323d8b0378f8e8fb77a2143099578aa4587e033
  invisible()
}
