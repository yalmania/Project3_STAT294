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
  if (class(m) != "numeric") {
    stop("You did not supply the m as a numeric.")
  }
  if (class(l) != "numeric") {
    stop("You did not supply the l as a numeric.")
  }

  if (m == 0) {
    stop("m cannot be 0.")
  }
  if (l == 0) {
    stop("l cannot be 0.")
  }

  if (m < 0) {
    warning("The m was less than zero, an absolute value will be taken for it.")
    m <- abs(m)
  }
  if (l < 0) {
    warning("The l was less than zero, an absolute value will be taken for it.")
    l <- abs(l)
  }



  if (axiss == "perp. center") {
    I <- 1 / 12 * m * (l^2)
  }

  if (axiss == "diameter of surface") {
    I <- 1 / 3 * m * (l^2)
  }

  return(I)
}
