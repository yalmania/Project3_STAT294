#' Compute MoI of a hoop
#'
#' @param m mass of the object in kilogram.
#' @param r radius of object in meter.
#' @param axiss The reference axis on which rotation takes place. Options are `geometrical axis`,`perp. to surface and center`.
#'
#' @return I moment of inertia in kg*m^2.
#'
#' @examples
#' Hoop(7, 2, "geometrical axis")
#'
#' @export

Hoop <- function(m, r, axiss) {
  if (axiss == "geometrical axis") {
    I <- m * (r^2)
  }

  if (axiss == "perp. to surface and center") {
    I <- 0.5 * m * (r^2)
  }

  return(I)
}
