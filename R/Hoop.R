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


  if (axiss == "geometrical axis") {
    I <- m * (r^2)
  }

  if (axiss == "perp. to surface and center") {
    I <- 0.5 * m * (r^2)
  }

  return(I)
}
