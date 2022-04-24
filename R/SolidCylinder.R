#' Compute MoI of a solid cylinder
#'
#' @param m mass of the object in kilogram.
#' @param r radius of object in meter.
#' @param l length of object in meter.
#' @param axiss The reference axis on which rotation takes place. Options are `geometrical axis`,`along surface`,`perp. to surface and center`,`diameter of surface`.
#'
#' @return I moment of inertia in kg*m^2.
#'
#' @examples
#' SolidCylinder(5, 3, 7, "geometrical axis")
#'
#' @export

SolidCylinder <- function(m, r, l, axiss) {
  if (class(m) != "numeric") {
    stop("You did not supply the m as a numeric.")
  }
  if (class(r) != "numeric") {
    stop("You did not supply the r as a numeric.")
  }
  if (class(l) != "numeric") {
    stop("You did not supply the l as a numeric.")
  }

  if (m == 0) {
    stop("m cannot be 0.")
  }
  if (r == 0) {
    stop("r cannot be 0.")
  }
  if (l == 0) {
    stop("l cannot be 0.")
  }

  if (m < 0) {
    warning("The m was less than zero, an absolute value will be taken for it.")
    m <- abs(m)
  }
  if (r < 0) {
    warning("The r was less than zero, an absolute value will be taken for it.")
    r <- abs(r)
  }
  if (l < 0) {
    warning("The l was less than zero, an absolute value will be taken for it.")
    l <- abs(l)
  }




  if (axiss == "geometrical axis") {
    I <- 0.5 * m * (r^2)
  }

  if (axiss == "along surface") {
    I <- 3 / 2 * m * (r^2)
  }

  if (axiss == "perp. to surface and center") {
    I <- (0.25 * m * (r^2)) + 1 / 12 * m * (l^2)
  }

  if (axiss == "diameter of surface") {
    I <- (1 / 4 * m * (r^2)) + 1 / 3 * m * (l^2)
  }

  return(I)
}
