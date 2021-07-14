#' @import bayesforecast
#' @import chron
#' @import dplyr
#' @import GGally
#' @import ggfortify
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom lemon grid_arrange_shared_legend
#' @import mvtnorm
#' @import plyr
#' @import RColorBrewer
#' @import stats
#' @import tidyr
#' @import utils
#' @useDynLib hornsharkHMM, .registration = TRUE
#' @importFrom Rcpp sourceCpp

#' Exponentiate diagonal elements
#'
#' @param mat A matrix.
#'
#' @return Matrix with diagonal elements exponentiated.
#' @export
#'
#' @examples
#' diag_exp(diag(2, 2))
diag_exp <- function(mat) {
  diag(mat) <- exp(diag(mat))
  return(mat)
}

#' Get log diagonal and lower triangular entries
#'
#' Applies log to only diagonal elements of matrix.
#'
#' @param mat A matrix.
#'
#' @return Vector of log diagonal elements
#' and lower triangular entries of the matrix,
#' going down column by column.
#' @export
#'
#' @examples
#' diag_log_lower(matrix(c(1:4), nrow=2))
#'
diag_log_lower <- function(mat) {
  diag(mat) <- log(diag(mat))
  vect <- mat[lower.tri(mat, diag = TRUE)]
  return(vect)
}


#' Get nth triangular number
#'
#' @param n A non-negative integer.
#'
#' @return The n-th triangular number. (Where the 0th
#' triangular number is 0.)
#' @export
#'
#' @examples
#' triangular_num(3)
triangular_num <- function(n) {
  nums <- choose(seq(n + 1), 2)
  return(nums[n + 1])
}
