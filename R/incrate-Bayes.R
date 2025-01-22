## Bayesian estimation of incidence rates with gamma conjugate distribution

# incidence rate posterior mean, median, and equal-tailed credible limits
incrate_bayes <- function(m, T, level=0.95, priora=0.5, priorb=0) {
  # default arguments are for the Jeffreys confidence interval
  alpha <- 1 - level
  posta  <- priora + m
  postb <- priorb + T
  if (m == 0) {
    lower <- 0
  } else {
    lower <- qgamma(alpha / 2, shape = posta, rate = postb)
  }
  upper <- qgamma(1 - alpha / 2, shape = posta, rate = postb)
  postmean <- posta / postb
  postmedian <- qgamma(0.5, shape = posta, rate = postb)
  return(c(postmean = postmean, postmedian = postmedian,
           lower = lower, upper = upper,
           priora = priora, priorb = priorb, level = level))
}

# 7 events in 22 units of person-time
incrate_bayes(7, 22)                          # Jeffreys 95% confidence interval
incrate_bayes(7, 22, level = 0.8)             # Jeffreys 80% confidence interval
incrate_bayes(7, 22, priora = 1, priorb = 1)  # uniform prior
