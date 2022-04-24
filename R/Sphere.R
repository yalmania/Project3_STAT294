#' Compute MoI of a sphere
#'
#' @param m mass of the object in kilogram.
#' @param r radius of object in meter.
#' @param type whether sphere is either `solid` or `shell`.
#' @param axiss The reference axis on which rotation takes place. Options are `diameter`,`tangent`.
#'
#' @return I moment of inertia in kg*m^2.
#'
#' @examples
#' Sphere(2, 6, "solid", "tangent")
#'
#' @export

Sphere <- function(m, r, type, axiss) {
  if (type == "solid") {
    if (axiss == "diameter") {
      I <- 2 / 5 * m * (r^2)
    }

    if (axiss == "tangent") {
      I <- 7 / 5 * m * (r^2)
    }
  }


  if (type == "shell") {
    if (axiss == "diameter") {
      I <- 2 / 3 * m * (r^2)
    }

    if (axiss == "tangent") {
      I <- 5 / 3 * m * (r^2)
    }
  }

  return(I)
}
