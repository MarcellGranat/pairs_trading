simulate_bivariate <- function(t = 250, p = .75, innov_sd = 1) {
  # Simulated bivariate cointegrated system
  y2 <- cumsum(rnorm(t, 0, innov_sd))
  y1 <- y2 + arima.sim(list(ar= p), innov = rnorm(t, 0, innov_sd), n = t)
  tibble(x = y1, y = y2)
}

simulate_trivariate_1ci <- function(t = 250, p = .75, innov_sd = 1) {
  # Simulated trivariate cointegrated system with 1 cointegrating vector
  y2 <- cumsum(rnorm(t, 0, innov_sd))
  y3 <- cumsum(rnorm(t, 0, innov_sd))
  y1 <- 1*y2 + 1*y3 + arima.sim(list(ar= p), innov = rnorm(t, 0, innov_sd), n = t)
  tibble(x = y1, y = y2, z = y3)
}

simulate_trivariate_2ci <- function(t = 250, p = .75, innov_sd = 1) {
  # Simulated trivariate cointegrated system with 2 cointegrating vectors
  y3 <- cumsum(rnorm(t, 0, innov_sd))
  y1 <- y3 + arima.sim(list(ar= p), innov = rnorm(t, 0, innov_sd), n = t)
  y2 <- y3 + arima.sim(list(ar= p), innov = rnorm(t, 0, innov_sd), n = t)
  tibble(x = y1, y = y2, z = y3)
}

simulate_dgp <- function(n = 2, ci = 1, t = 250, p = .75, innov_sd = 1) {
  if (n == 2) {
    if (ci == 0) {
      out <- tibble(
        x = cumsum(rnorm(t, 0, innov_sd)),
        y = cumsum(rnorm(t, 0, innov_sd))
      )
    }
    if (ci == 1) {
      out <- simulate_bivariate(t = t, p = p, innov_sd = innov_sd)
    }
  }
  
  if (n == 3) {
    if (ci == 0) {
      out <- tibble(
        x = cumsum(rnorm(t, 0, innov_sd)),
        y = cumsum(rnorm(t, 0, innov_sd)),
        z = cumsum(rnorm(t, 0, innov_sd))
      ) 
    }
    if (ci == 1) {
      out <- simulate_trivariate_1ci(t = t, p = p, innov_sd = innov_sd)
    }
    if (ci == 2) {
      out <- simulate_trivariate_2ci(t = t, p = p, innov_sd = innov_sd)
    }
  }
  out
}
