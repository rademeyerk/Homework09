#' The Trimmed Mean
#'
#' This function allows you to compute the trimmed mean of a numeric vector.
#' @param X is the vector of numeric values.
#' @param s is the number of low values to cut off.
#' @param l is the number of high values to cut off.
#' @keywords Trimmed Mean
#' @export
#' @examples Trimmed_Mean(iris$Sepal.Length, 1, 1)


trimmed_mean <- function(x, s, l){

  n <- length(x)

  if (n < s + l + 1){ stop("Not enough values to trim for trimmed mean!") }

  x_sorted <- sort(x)         # First, sort vector

  x_sorted[1:s]       <- NA   # Mark as NA the first s values and last l values

  x_sorted[(n-l+1):n] <- NA

  mean(x_sorted[!is.na(x_sorted)])  # Take the mean of all non-NA values



}

