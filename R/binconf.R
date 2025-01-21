## Binomial confidence intervals

# using BinomCI() function from the DescTools package
library(DescTools)
BinomCI(15, 22, method = "wald")            # Wald confidence interval
BinomCI(15, 22, method = "logit")           # logit-transformed Wald CI
BinomCI(15, 22, method = "wilson")          # score CI (default)
BinomCI(15, 22, method = "agresti-coull")   # Agresti-Coull CI
BinomCI(15, 22, method = "lik")             # likelihood ratio CI

# using binconf() function from the Hmisc package
library(Hmisc)
binconf(15, 22, method = "asymptotic")  # Wald CI
binconf(15, 22, method = "wilson")      # score CI (default)

# using prop.test in base R (stats package)
# Wilson confidence interval with continuity correction by default
# The continuity correction is not generally recommended. Like the exact CI,
# it can be too wide and have a coverage probability greater than 1 - \alpha.
prop.test(15, 22)
names(prop.test(15, 22))
prop.test(15, 22, correct = FALSE)      # score CI

# using binom.test (exact confidence interval)
binom.test(15, 22)    # same as binconf with method = "exact"
names(binom.test(15, 22))

# changing the confidence level (1 - alpha) to 80%
# All are score (Wilson) confidence intervals by default.
BinomCI(15, 22, conf.level = 0.8)
binconf(15, 22, alpha = 0.2)
prop.test(15, 22, conf.level = 0.8, correct = FALSE)

# writing a function to get Wald confidence limits
bconf_wald <- function(x, n, level=0.95) {
  # x is number of successes out of n trials
  p_hat <- x / n
  alpha <- 1 - level
  pvar <- p_hat * (1 - p_hat) / n
  p_int <- p_hat + c(-1, 1) * qnorm(1 - alpha / 2) * sqrt(pvar)

  # return named vector (names do not need quotes)
  return(c(point = p_hat, lower = p_int[1], upper = p_int[2]))
}
bconf_wald(15, 22)
bconf_wald(15, 22, level = 0.80)