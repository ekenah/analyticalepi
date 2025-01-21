## Small-sample binomial point and interval estimates

# median unbiased estimate
medp.binom <- function(k, n) {
  # k = number of successes, n = number of trials

  # binomial lower tail probability
  lower.tail <- function(p) pbinom(k, n, p) - dbinom(k, n, p) / 2

  # median unbiased estimate
  med <- uniroot(function(p) lower.tail(p) - 1 / 2, interval = c(0, 1))

  return(med$root)
}
medp.binom(15, 22)


# exact (Clopper-Pearson) confidence intervals
binom.test(15, 22)                              # base R (stats)
names(binom.test(15, 22))
binom.test(15, 22, conf.level = 0.8)

library(Hmisc)
binconf(15, 22, method = "exact")
binconf(15, 22, method = "exact", alpha = 0.2)

library(DescTools)
BinomCI(15, 22, method = "clopper-pearson")     # exact CI
BinomCI(15, 22, method = "midp")                # mid-p exact CI
