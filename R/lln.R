## Law of large numbers

n <- 1000
x <- seq(n)
plot(x, cumsum(rbinom(n, 1, .5)) / x, type = "n", ylim = c(0, 1),
     xlab = "Number of samples", ylab = "Sample mean")
grid()
lines(x, cumsum(rbinom(n, 1, .5)) / x, lty = "solid")
lines(x, cumsum(rbinom(n, 1, .5)) / x, lty = "dashed")
lines(x, cumsum(rbinom(n, 1, .5)) / x, lty = "dotted")
abline(h = .5)