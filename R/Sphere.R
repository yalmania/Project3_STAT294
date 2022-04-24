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
  if (class(m) != "numeric") {
    stop("You did not supply the m as a numeric.")
  }
  if (class(r) != "numeric") {
    stop("You did not supply the r as a numeric.")
  }


  if (m == 0) {
    stop("m cannot be 0.")
  }
  if (r == 0) {
    stop("r cannot be 0.")
  }


  if (m < 0) {
    warning("The m was less than zero, an absolute value will be taken for it.")
    m <- abs(m)
  }
  if (r < 0) {
    warning("The r was less than zero, an absolute value will be taken for it.")
    r <- abs(r)
  }



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
