## Small-sample Poisson point and interval estimation

# median unbiased estimate
medrate_pois <- function(m, T) {
  # m = number of events, T = total person-time

  # Poisson lower tail probability
  lower_tail <- function(rate) {
    mu = rate * T
    ppois(m, mu) - dpois(m, mu) / 2
  }

  # median unbiased estimate
  med <- uniroot(function(rate) lower_tail(rate) - 1 / 2, interval = c(0, 1))
  med$root
}
medrate_pois(7, 22)

# exact confidence limits
# The point estimate is the incidence rate m / T, not the median unbiased rate.
library(DescTools)
PoissonCI(7, 22, method = "exact")
PoissonCI(7, 22, method = "exact", conf.level = 0.8)

# mid-p confidence limits
midp_pois <- function(m, T, level=0.95) {
  # m = number of events, T = total person-time
  # The default confidence level (1 - type I error probability) is 0.95.
  
  # Poisson mid-p lower tail probability
  lower_tail <- function(rate) {
    mu = rate * T
    ppois(m, mu) - dpois(m, mu) / 2
  }

  # lower confidence limit
  alpha <- 1 - level
  lower <- uniroot(function(rate) lower_tail(rate) - (1 - alpha / 2),
                   interval = c(0, 100), extendInt = "yes")
  # upper confidence limit
  upper <- uniroot(function(rate) lower_tail(rate) - alpha / 2,
                   interval = c(0, 100), extendInt = "yes")

  # names for confidence limits
  lower_perc <- paste(round(alpha / 2 * 100, 3), "%", sep = "")
  upper_perc <- paste(round((1 - alpha / 2) * 100, 3), "%", sep = "")
  
  # return named vector of confidence limits
  conflimits <- c(lower$root, upper$root)
  names(conflimits) <- c(lower_perc, upper_perc)
  conflimits
}
midp_pois(7, 22)
midp_pois(7, 22, level = 0.8)
