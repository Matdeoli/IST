ist <- function(data, dens_matrix, corer_diameter,transect_area) {
  x <- as.data.frame(data)
  n <- ncol(x)
  nr <- nrow(x)
  qi <- x
  corer_area <- (pi*(corer_diameter/2)^2)/10000 #cm to m
  y <- x/corer_area # standardising the data in density (ind/m^2)
  
  if (dens_matrix == FALSE) {
    for (i in 1:n) {
      if (!is.numeric(y[, i])) {
        stop("Not all variables are numeric")
      } else {
        for (j in 1:nr) {
          qi[j, i] <- ((x[j, i]) * mean(x[, i]))*transect_area
        }
      }
    }
  } else {
    for (i in 1:n) {
      if (!is.numeric(x[, i])) {
        stop("Not all variables are numeric")
      } else {
        for (j in 1:nr) {
          qi[j, i] <- ((x[j, i]) * (mean(x[, i]))*transect_area)
        }
      }
    }
  }
  return(qi)
}
ist(transect_area=80,data = pv[,-1],dens_matrix = FALSE,corer_diameter = 25)


