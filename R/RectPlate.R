#' Compute MoI of a rectangular plate
#'
#' @param m mass of the object in kilogram.
#' @param l1 length of a side of object in meter (perp. to rotation axis).
#' @param l2 length of another side of object in meter.
#' @param axiss The reference axis on which rotation takes place. Options are `perp. center axis`,`along edge`.
#'
#' @return I moment of inertia in kg*m^2.
#'
#' @examples
#' RectPlate(7, 2, 14, "perp. center axis")
#'
#' @export

RectPlate <- function(m, l1, l2, axiss) {
  if (class(m) != "numeric") {
    stop("You did not supply the m as a numeric.")
  }
  if (class(l1) != "numeric") {
    stop("You did not supply the l1 as a numeric.")
  }
  if (class(l2) != "numeric") {
    stop("You did not supply the l2 as a numeric.")
  }

  if (m == 0) {
    stop("m cannot be 0.")
  }
  if (l1 == 0) {
    stop("l1 cannot be 0.")
  }
  if (l2 == 0) {
    stop("l2 cannot be 0.")
  }

  if (m < 0) {
    warning("The m was less than zero, an absolute value will be taken for it.")
    m <- abs(m)
  }
  if (l1 < 0) {
    warning("The l1 was less than zero, an absolute value will be taken for it.")
    l1 <- abs(l1)
  }
  if (l2 < 0) {
    warning("The l2 was less than zero, an absolute value will be taken for it.")
    l2 <- abs(l2)
  }

  if (axiss == "perp. center axis") {
    I <- 1 / 12 * m * (l1^2 + l2^2)
  }

  if (axiss == "along edge") {
    I <- 1 / 3 * m * (l1^2)
  }

  return(I)
}
