#' Compute MoI of a rod
#'
#' @param m mass of the object in kilogram.
#' @param l length of object in meter.
#' @param axiss The reference axis on which rotation takes place. Options are `perp. center`,`diameter of surface`.
#'
#' @return I moment of inertia in kg*m^2.
#'
#' @examples
#' Rod(5, 7, "diameter of surface")
#'
#' @export

Rod <- function(m, l, axiss) {
  if (axiss == "perp. center") {
    I <- 1 / 12 * m * (l^2)
  }

  if (axiss == "diameter of surface") {
    I <- 1 / 3 * m * (l^2)
  }

  return(I)
}
