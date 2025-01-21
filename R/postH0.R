## Posterior probability of the null hypothesis (H0)

# function to calculate lower bound
lowerb <- function(pi0=0.5, pval=0.05) {
  # args: pi0 = prior probability of H0, pval = p-value
  # return: lower bound on posterior probability of H0
  z <- qnorm(1 - pval / 2)
  1 / (1 + (1 - pi0) / pi0 * exp(0.5 * z^2))
}

# plot of lower bounds for p-value = 0.01, 0.05, and 0.1
x <- seq(0, 1, by = .01)
plot(x, lowerb(x), type = "n", xlim = c(0, 1), ylim = c(0, 1),
     xlab = expression("Prior probability of H"[0]),
     ylab = expression("Minimum posterior probability of H"[0]))
grid()
abline(0, 1, col = "gray")
lines(x, lowerb(x))
lines(x, lowerb(x, pval = .01), lty = "dashed")
lines(x, lowerb(x, pval = .1), lty = "dotted")
legend("topleft", bg = "white", lty = c("dotted", "solid", "dashed"),
       legend = c("p = 0.10", "p = 0.05", "p = 0.01"))
