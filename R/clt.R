## Central limit theorem

# probability mass function for sample mean
dbline <- function(n, p=.5, ...) {
  x <- (seq(-.5, n + .5) / n - p) * sqrt(n / (p * (1 - p)))
  y <- c(0, dbinom(0:n, n, p), 0) * sqrt(p * (1 - p) * n)
  lines(stepfun(x, y), pch = NA, ...)
}

# define grid of plots
par(mfrow = c(2, 2))
x <- seq(-4, 4, by = .01)

# n = 20
plot(x, dnorm(x), type = "n", ylim = c(0, .5),
     main = "n = 20", xlab = "Z score", ylab = "Probability density")
grid()
dbline(20, p = .1, lty = "dashed")
lines(x, dnorm(x), col = "darkgray")

# n = 50
plot(x, dnorm(x), type = "n", ylim = c(0, .5),
     main = "n = 50", xlab = "Z score", ylab = "Probability density")
grid()
dbline(50, p = .1, lty = "dashed")
lines(x, dnorm(x), col = "darkgray")

# n = 100
plot(x, dnorm(x), type = "n", ylim = c(0, .5),
     main = "n = 100", xlab = "Z score", ylab = "Probability density")
grid()
dbline(100, p = .1, lty = "dashed")
lines(x, dnorm(x), col = "darkgray")

# n = 250
plot(x, dnorm(x), type = "n", ylim = c(0, .5),
     main = "n = 250", xlab = "Z score", ylab = "Probability density")
grid()
dbline(250, p = .1, lty = "dashed")
lines(x, dnorm(x), col = "darkgray")