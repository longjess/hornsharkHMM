#' @param mat
#'
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


diag_exp <- function(mat) {
  diag(mat) <- exp(diag(mat))
  return(mat)
}

diag_log_lower <- function(mat) {
  diag(mat) <- log(diag(mat))
  vect <- mat[lower.tri(mat, diag = TRUE)]
  return(vect)
}

mvnorm_densities <- function(x, mod, m) {
  pvect <- numeric(m)
  for (i in 1:m) {
    pvect[i] <- dmvnorm(x, mod$mu[[i]], mod$sigma[[i]])
  }
  return(pvect)
}

mvnorm_hmm_sample_one <- function(state, mod) {
  x <- rmvnorm(1, mean = mod$mu[[state]], sigma = mod$sigma[[state]])
  return(x)
}

triangular_num <- function(n) {
  nums <- choose(seq(n + 1), 2)
  return(nums[n + 1])
}
