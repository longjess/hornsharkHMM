#' Get mean corresponding to a given index in an autoregressive series
#'
#' @param mu Vector of length m, containing means for each
#' state dependent distribution
#' @param phi Matrix of size m x q, containing autoregressive parameters. Each row contains
#' the parameters corresponding to a single state.
#' @param x Vector of observations coming from an autoregressive series
#' @param q Order of the autoregressive model
#' @param i Index of the desired mean
#'
#' @return Vector of length m containing means corresponding to index i
#' for the given autoregressive model
#' @export
#'
#' @examples
get_ar_mean <- function(mu, phi, x, q, i) {
  if (i == 1) {
    mean <- mu
  }
  else if (i <= q) {
    phi <- phi[, 1:(i - 1)]
    x_lag <- x[(i - 1):1]
    if (i == 2) {
      mean <- mu + phi * x_lag
    } else {
      mean <- mu + as.vector(phi %*% x_lag)
    }
  }
  else {
    x_lag <- x[(i - 1):(i - q)]
    mean <- mu + as.vector(phi %*% x_lag)
  }
  return(mean)
}

#' Get all means for autoregressive series x
#'
#' @param m Number of states
#' @inheritParams get_ar_mean
#'
#' @return n x m matrix (where n is the number of observations)
#' containing means for the autoregressive model
#' @export
#'
#' @examples
get_all_ar_means <- function(mu, phi, m, q, x){
  n <- length(x)
  means <- matrix(mu, nrow = n, ncol = m, byrow = TRUE)

  x_lags <- matrix(nrow = n, ncol = q)
  for (i in 1:q){
    x_lag <- lag(x, i)
    x_lag[1:i] <- rep(0, i)
    x_lags[, i] <- x_lag
  }

  for (i in 1:m){
    means[, i] <- means[, i] + x_lags %*% phi[[i]]
  }

  return(means)
}

#' Generate sample from HMM with autoregressive model
#'
#' @param ns Sample size
#' @param mod List of HMM parameters
#'
#' @return Dataframe including index, state, obs
#' @export
#'
#' @examples
ar_hmm_generate_sample <- function(ns, mod) {
  mvect <- 1:mod$m
  state <- numeric(ns)
  state[1] <- sample(mvect, 1, prob = mod$delta)
  for (i in 2:ns) state[i] <- sample(mvect, 1, prob = mod$gamma[state[i - 1], ])
  x <- numeric(ns)
  phi <- matrix(unlist(mod$phi, use.names = FALSE), nrow = mod$q, byrow = TRUE)
  for (i in 1:ns) {
    mean <- get_ar_mean(mod$mu, phi, x, mod$q, i)
    x[i] <- rnorm(1, mean = mean[state[i]], sd = mod$sigma[state[i]])
  }
  return(data_frame(index = c(1:ns), state = state, obs = x))
}

#' Transform normal natural parameters to working parameters
#'
#' mu does not need to be transformed, as there are no constraints.
#'
#' @param m Number of states
#' @param mu Vector of length m, containing means for each
#' state dependent normal distribution
#' @param sigma Vector of length m, containing standard
#' deviations for each state dependent normal distribution
#' @param gamma Transition probabiilty matrix, size m x m
#' @param phi m x q matrix of autoregressive parameters. Each row contains
#' the parameters corresponding to a single state.
#' @param delta Optional, vector of length m containing
#' initial distribution
#' @param stationary Boolean, whether the HMM is stationary or not
#'
#' @return Vector of working parameters
#' @export
#'
#' @examples
ar_hmm_pn2pw <- function(m, mu, sigma, gamma, phi,
                         delta = NULL, stationary = TRUE) {
  tsigma <- log(sigma)
  foo <- log(gamma / diag(gamma))
  tgamma <- as.vector(foo[!diag(m)])
  tphi <- unlist(phi, use.names = FALSE)
  if (stationary) {
    tdelta <- NULL
  }
  else {
    tdelta <- log(delta[-1] / delta[1])
  }
  parvect <- c(mu, tsigma, tgamma, tphi, tdelta)
  return(parvect)
}

#' Transform normal working parameters to natural parameters
#'
#' @param q Order of the autoregressive model
#' @param parvect Vector of working parameters
#' @inheritParams ar_hmm_pn2pw
#'
#' @return List of natural parameters mu, sigma, gamma, phi, delta
#' @export
#'
#' @examples
ar_hmm_pw2pn <- function(m, q, parvect, stationary = TRUE) {
  mu <- parvect[1:m]
  sigma <- exp(parvect[(m + 1):(2 * m)])
  gamma <- diag(m)
  gamma[!gamma] <- exp(parvect[(2 * m + 1):(m + m * m)])
  gamma <- gamma / apply(gamma, 1, sum)

  count <- m + m * m + 1
  phi <- list()
  for (i in 1:m) {
    phi[[i]] <- parvect[count:(count + q - 1)]
    count <- count + q
  }

  if (stationary) {
    delta <- solve(t(diag(m) - gamma + 1), rep(1, m))
  }
  else {
    foo <- c(1, exp(parvect[count:(count + m - 2)]))
    delta <- foo / sum(foo)
  }
  return(list(mu = mu, sigma = sigma, gamma = gamma, phi = phi, delta = delta))
}

#' Get negative log-likelihood from the working parameters
#'
#' @param x Vector of observations
#' @inheritParams ar_hmm_pn2pw
#'
#' @return Negative log-likelihood
#' @export
#'
#' @examples
ar_hmm_mllk <- function(parvect, x, m, q, stationary = TRUE) {
  n <- length(x)
  pn <- ar_hmm_pw2pn(m, q, parvect, stationary = stationary)
  p <- ar_densities(x, pn, m, q, n)
  foo <- matrix(pn$delta, ncol = m)
  lscale <- foralg(n, m, foo, pn$gamma, p)
  mllk <- -lscale
  return(mllk)
}

#' Returns densities for autoregressive model
#'
#' @param mod List of HMM parameters
#' @param n Number of observations
#' @inheritParams ar_hmm_mllk
#'
#' @return n x m matrix of densities for autoregressive model
#' @export
#'
#' @examples
ar_densities <- function(x, mod, m, q, n) {
  p <- matrix(nrow = n, ncol = m)
  means <- get_all_ar_means(mod$mu, mod$phi, m, q, x)
  for (i in 1:n) {
    p[i, ] <- dnorm(x[i], means[i, ], mod$sigma)
  }
  return(p)
}

#' Maximum likelihood estimation of univariate autoregresive parameters
#'
#' @param mu0 Vector of length m, initial values for means
#' @param sigma0 Vector of length m, initial values for standard deviations
#' @param gamma0 Matrix of size m x m, initial values for transition probability matrix
#' @param phi0 Matrix of size m x q, initial values for autoregressive parameters
#' @param delta0 Optional, vector of length m, initial values for
#' initial distribution
#' @param hessian Boolean, whether to return the inverse hessian
#' @inheritParams ar_hmm_mllk
#'
#' @return List of results
#' @export
#'
#' @examples
ar_hmm_mle <- function(x, m, q, mu0, sigma0, gamma0, phi0,
                       delta0 = NULL, stationary = TRUE,
                       hessian = FALSE) {
  parvect0 <- ar_hmm_pn2pw(m, mu0, sigma0, gamma0, phi0, delta0,
                           stationary = stationary
  )
  mod <- nlm(ar_hmm_mllk, parvect0,
             x = x, m = m, q = q,
             stationary = stationary,
             hessian = hessian
  )
  pn <- ar_hmm_pw2pn(m, q, mod$estimate,
                     stationary = stationary
  )
  mllk <- mod$minimum

  np <- length(parvect0)
  aic <- 2 * (mllk + np)
  n <- sum(!is.na(x))
  bic <- 2 * mllk + np * log(n)

  if (hessian) {
    return(list(
      m = m, q = q, mu = pn$mu, sigma = pn$sigma,
      gamma = pn$gamma, delta = pn$delta, phi = pn$phi,
      code = mod$code, mllk = mllk,
      aic = aic, bic = bic, hessian = mod$hessian, np = np
    ))
  }
  else {
    return(list(
      m = m, q = q, mu = pn$mu, sigma = pn$sigma,
      gamma = pn$gamma, delta = pn$delta, phi = pn$phi,
      code = mod$code, mllk = mllk, aic = aic, bic = bic
    ))
  }
}

#' Global decoding of states
#'
#' @param x Vector of observations
#' @param mod List of HMM parameters
#'
#' @return Dataframe of decoded states and index
#' @export
#'
#' @examples
ar_hmm_viterbi <- function(x, mod) {
  n <- length(x)
  xi <- matrix(0, n, mod$m)
  foo <- mod$delta * dnorm(x[1], mod$mu, mod$sigma)
  xi[1, ] <- foo / sum(foo)

  means <- get_all_ar_means(mod$mu, mod$phi, mod$m, mod$q, x)
  for (t in 2:n) {
    p <- dnorm(x[t], mean = means[t, ], sd = mod$sigma)
    foo <- apply(xi[t - 1, ] * mod$gamma, 2, max) * p
    xi[t, ] <- foo / sum(foo)
  }

  iv <- numeric(n)
  iv[n] <- which.max(xi[n, ])
  for (t in (n - 1):1) {
    iv[t] <- which.max(mod$gamma[, iv[t + 1]] * xi[t, ])
  }
  return(data_frame(index = 1:n, state = iv))
}

#' Get forward probabilities
#'
#' @inheritParams ar_hmm_viterbi
#'
#' @return Matrix of forward probabilities
#' @export
#'
#' @examples
ar_hmm_lforward <- function(x, mod) {
  n <- length(x)
  lalpha <- matrix(NA, mod$m, n)
  foo <- mod$delta * dnorm(x[1], mod$mu, mod$sigma)
  sumfoo <- sum(foo)
  lscale <- log(sumfoo)
  foo <- foo / sumfoo
  lalpha[, 1] <- lscale + log(foo)

  means <- get_all_ar_means(mod$mu, mod$phi, mod$m, mod$q, x)
  for (i in 2:n) {
    p <- dnorm(x[i], mean = means[i, ], sd = mod$sigma)
    foo <- foo %*% mod$gamma * p
    sumfoo <- sum(foo)
    lscale <- lscale + log(sumfoo)
    foo <- foo / sumfoo
    lalpha[, i] <- log(foo) + lscale
  }

  return(lalpha)
}

#' Get backward probabilities
#'
#' @inheritParams ar_hmm_viterbi
#'
#' @return Matrix of backward probabilities
#' @export
#'
#' @examples
ar_hmm_lbackward <- function(x, mod) {
  n <- length(x)
  m <- mod$m
  lbeta <- matrix(NA, m, n)
  lbeta[, n] <- rep(0, m)
  foo <- rep(1 / m, m)
  lscale <- log(m)

  means <- get_all_ar_means(mod$mu, mod$phi, mod$m, mod$q, x)
  for (i in (n - 1):1) {
    p <- dnorm(x[i + 1], mean = means[i + 1, ], sd = mod$sigma)
    foo <- mod$gamma %*% (p * foo)
    lbeta[, i] <- log(foo) + lscale
    sumfoo <- sum(foo)
    foo <- foo / sumfoo
    lscale <- lscale + log(sumfoo)
  }

  return(lbeta)
}

#' Generate pseudo residuals
#'
#' @inheritParams ar_hmm_viterbi
#' @param type Type of pseudo-residual, either "ordinary" or "forecast"
#' @param stationary Boolean, whether the HMM is stationary or not
#'
#' @return Dataframe of pseudo-residuals, observations, index
#' @export
#'
#' @examples
ar_hmm_pseudo_residuals <- function(x, mod, type, stationary) {
  if (stationary) {
    delta <- solve(t(diag(mod$m) - mod$gamma + 1), rep(1, mod$m))
  }
  else {
    delta <- mod$delta
  }
  if (type == "ordinary") {
    n <- length(x)
    la <- ar_hmm_lforward(x, mod)
    lb <- ar_hmm_lbackward(x, mod)
    lafact <- apply(la, 2, max)
    lbfact <- apply(lb, 2, max)

    means <- get_all_ar_means(mod$mu, mod$phi, mod$m, mod$q, x)
    p <- matrix(NA, n, mod$m)
    for (i in 1:n) {
      p[i, ] <- pnorm(x[i], mean = means[i, ], sd = mod$sigma)
    }

    npsr <- rep(NA, n)
    npsr[1] <- qnorm(delta %*% p[1, ])
    for (i in 2:n) {
      a <- exp(la[, i - 1] - lafact[i])
      b <- exp(lb[, i] - lbfact[i])
      foo <- (a %*% mod$gamma) * b
      foo <- foo / sum(foo)
      npsr[i] <- qnorm(foo %*% p[i, ])
    }

    return(data_frame(npsr, x, index = c(1:n)))
  }
  else if (type == "forecast") {
    n <- length(x)
    la <- ar_hmm_lforward(x, mod)

    means <- get_all_ar_means(mod$mu, mod$phi, mod$m, mod$q, x)
    p <- matrix(NA, n, mod$m)
    for (i in 1:n) {
      p[i, ] <- pnorm(x[i], mean = means[i, ], sd = mod$sigma)
    }

    npsr <- rep(NA, n)
    npsr[1] <- qnorm(delta %*% p[1, ])
    for (i in 2:n) {
      la_max <- max(la[, i - 1])
      a <- exp(la[, i - 1] - la_max)
      npsr[i] <- qnorm(t(a) %*% (mod$gamma / sum(a)) %*% p[i, ])
    }

    return(data_frame(npsr, x, index = c(1:n)))
  }
}

#' Get inverse of hessian matrix
#'
#' Transform hessian associated with working parameters
#' outputted by nlm.
#' If not stationary, exclude values associated with delta parameter
#' from the hessian matrix.
#'
#' @param mod List of maximum likelihood estimation results
#' @param stationary Boolean, whether the HMM is stationary or not
#'
#' @return Inverse hessian matrix
#' @export
#'
#' @examples
ar_inv_hessian <- function(mod, stationary = TRUE){
  if (!stationary) {
    np2 <- mod$np - mod$m + 1
    h <- mod$hessian[1:np2, 1:np2]
  }
  else {
    np2 <- mod$np
    h <- mod$hessian
  }
  h <- solve(h)
  jacobian <- norm_jacobian(mod, np2)
  h <- t(jacobian) %*% h %*% jacobian
  return(h)
}

#' Get Jacobian matrix
#'
#' @param mod List of maximum likelihood estimation results
#' @param n Total number of working parameters (excluding delta)
#'
#' @return Jacobian matrix, size n x n
#' @export
#'
#' @examples
ar_jacobian <- function(mod, n) {
  m <- mod$m
  q <- mod$q
  jacobian <- matrix(0, nrow = n, ncol = n)
  jacobian[1:m, 1:m] <- diag(m)
  jacobian[(m + 1):(2 * m), (m + 1):(2 * m)] <- diag(mod$sigma)
  count <- 0
  for (i in 1:m) {
    for (j in 1:m) {
      if (j != i) {
        count <- count + 1
        foo <- -mod$gamma[i, j] * mod$gamma[i, ]
        foo[j] <- mod$gamma[i, j] * (1 - mod$gamma[i, j])
        foo <- foo[-i]
        jacobian[
          2 * m + count,
          (2 * m + (i - 1) * (m - 1) + 1):(2 * m + i * (m - 1))
          ] <- foo
      }
    }
  }
  count <- 2 * m + count + 1
  phi <- unlist(mod$phi, use.names = FALSE)
  jacobian[count:n, count:n] <- diag(phi)
  return(jacobian)
}

#' Get bootstrapped estimates of parameters
#'
#' @param mod List of maximum likelihood estimation results
#' @param n Number of bootstrap samples
#' @param len Number of observations
#' @param stationary Boolean, whether the HMM is stationary or not
#'
#' @return List of estimates
#' @export
#'
#' @examples
ar_bootstrap_estimates <- function(mod, n, len, stationary) {
  m <- mod$m
  q <- mod$q
  mu_estimate <- numeric(n * m)
  sigma_estimate <- numeric(n * m)
  gamma_estimate <- numeric(n * m * m)
  delta_estimate <- numeric(n * m)
  phi_estimate <- numeric(n * m * q)
  for (i in 1:n) {
    sample <- ar_hmm_generate_sample(len, mod)
    mod2 <- ar_hmm_mle(sample$obs, m, q, mod$mu, mod$sigma, mod$gamma, mod$phi,
                       mod$delta,
                       stationary = stationary, hessian = FALSE
    )
    mu_estimate[((i - 1) * m + 1):(i * m)] <- mod2$mu
    sigma_estimate[((i - 1) * m + 1):(i * m)] <- mod2$sigma
    gamma_estimate[((i - 1) * m * m + 1):(i * m * m)] <- mod2$gamma
    phi_estimate[((i - 1) * m * q + 1):(i * m * q)] <-
      unlist(mod2$phi, use.names = FALSE)
    delta_estimate[((i - 1) * m + 1):(i * m)] <- mod2$delta
  }
  return(list(
    mu = mu_estimate,
    sigma = sigma_estimate,
    gamma = gamma_estimate,
    phi = phi_estimate,
    delta = delta_estimate
  ))
}

#' Confidence intervals for estimated parameters by bootstrapping
#'
#' @param mod Maximum likelihood estimates of parameters
#' @param bootstrap Bootstrapped estimates for parameters
#' @param alpha Confidence level
#'
#' @return List of lower and upper bounds for confidence intervals
#' for each parameter
#' @export
#'
#' @examples
ar_bootstrap_ci <- function(mod, bootstrap, alpha) {
  m <- mod$m
  mu_lower <- rep(NA, m)
  mu_upper <- rep(NA, m)
  sigma_lower <- rep(NA, m)
  sigma_upper <- rep(NA, m)
  gamma_lower <- rep(NA, m * m)
  gamma_upper <- rep(NA, m * m)
  phi_lower <- rep(NA, m * q)
  phi_upper <- rep(NA, m * q)
  delta_lower <- rep(NA, m)
  delta_upper <- rep(NA, m)
  bootstrap1 <- data_frame(
    mu = bootstrap$mu,
    sigma = bootstrap$sigma,
    delta = bootstrap$delta
  )
  bootstrap2 <- data_frame(gamma = bootstrap$gamma)
  bootstrap3 <- data_frame(phi = bootstrap$phi)
  for (i in 1:m) {
    if (i == m) {
      foo <- bootstrap1 %>% filter((row_number() %% m) == 0)
    }
    else {
      foo <- bootstrap1 %>% filter((row_number() %% m) == i)
    }
    mu_lower[i] <- 2 * mod$mu[i] -
      quantile(foo$mu, 1 - (alpha / 2), names = FALSE)
    mu_upper[i] <- 2 * mod$mu[i] -
      quantile(foo$mu, alpha / 2, names = FALSE)
    sigma_lower[i] <- 2 * mod$sigma[i] -
      quantile(foo$sigma, 1 - (alpha / 2), names = FALSE)
    sigma_upper[i] <- 2 * mod$sigma[i] -
      quantile(foo$sigma, alpha / 2, names = FALSE)
    delta_lower[i] <- 2 * mod$delta[i] -
      quantile(foo$delta, 1 - (alpha / 2), names = FALSE)
    delta_upper[i] <- 2 * mod$delta[i] -
      quantile(foo$delta, alpha / 2, names = FALSE)
  }
  phi <- unlist(mod$phi, use.names = FALSE)
  for (i in 1:(m * m)) {
    if (i == (m * m)) {
      foo <- bootstrap2 %>% filter((row_number() %% (m * m)) == 0)
    }
    else {
      foo <- bootstrap2 %>% filter((row_number() %% (m * m)) == i)
    }

    gamma_lower[i] <- 2 * mod$gamma[i] -
      quantile(foo$gamma, 1 - (alpha / 2), names = FALSE)
    gamma_upper[i] <- 2 * mod$gamma[i] -
      quantile(foo$gamma, alpha / 2, names = FALSE)
  }
  for (i in 1:(m * q)) {
    if (i == (m * q)) {
      foo <- bootstrap3 %>% filter((row_number() %% (m * q)) == 0)
    }
    else {
      foo <- bootstrap3 %>% filter((row_number() %% (m * q)) == i)
    }

    phi_lower[i] <- 2 * phi[i] -
      quantile(foo$phi, 1 - (alpha / 2), names = FALSE)
    phi_upper[i] <- 2 * phi[i] -
      quantile(foo$phi, alpha / 2, names = FALSE)
  }
  return(list(
    mu_lower = mu_lower, mu_upper = mu_upper,
    sigma_lower = sigma_lower, sigma_upper = sigma_upper,
    gamma_lower = gamma_lower, gamma_upper = gamma_upper,
    phi_lower = phi_lower, phi_upper = phi_upper,
    delta_lower = delta_lower, delta_upper = delta_upper
  ))
}
