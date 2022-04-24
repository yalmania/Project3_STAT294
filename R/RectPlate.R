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
  if (axiss == "perp. center axis") {
    I <- 1 / 12 * m * (l1^2 + l2^2)
  }

  if (axiss == "along edge") {
    I <- 1 / 3 * m * (l1^2)
  }

  return(I)
}
