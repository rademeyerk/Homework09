#' The Pythagorean Theorem
#'
#' This function allows you to compute any side of a right triangle using a famous theorem.
#' @param Side1 is the length of one side.
#' @param Side2 is the length of another side.
#' @param Hyp indicates whether the hypotenuse is included in one of the two provided sides.
#' The computation changes whether this parameter is TRUE or FALSE.
#' @keywords Triangles
#' @export
#' @examples Pythagorean(3,4,T)
#' @examples Pythagorean(3,4,F)

Pythagorean <- function(Side1, Side2, Hyp) {
  ifelse(Hyp == F, sqrt(Side1^2 + Side2^2),
         sqrt(max(Side1, Side2)^2 - min(Side1, Side2)^2))
}

