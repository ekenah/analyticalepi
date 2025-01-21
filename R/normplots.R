## Normal distribution PDF and CDF

# set grid of plots
par(mfrow = c(2, 1), mar = c(2, 5, 2, 2) + 0.1)

# define variables
x <- seq(-3.5, 3.5, by = 0.01)
a <- 0
b <- 2

# plot of PDF
plot(x, dnorm(x), type = "n",
    ylab = expression(paste("PDF ", phi1(z))))
grid()
lines(x, dnorm(x))
polygon(x = c(b, a, seq(a, b, by = 0.01)),
        y = c(0, 0, dnorm(seq(a, b, by = 0.01))),
        lty = "dashed", col = "darkgray")
text(0.4, 0.18, labels = "Area = Pr(0 < Z < 2)", srt = 90)

# plot of CDF
plot(x, pnorm(x), type = "n",
     ylab = expression(paste("CDF ", Phi(z))))
grid()
lines(x, pnorm(x))
segments(c(-4, -4), pnorm(c(a, b)), c(a, b), pnorm(c(a, b)),
         lty = "dashed")
segments(c(a, b), c(-1, -1), c(a, b), pnorm(c(a, b)), lty = "dashed")
arrows(-3, pnorm(a), -3, pnorm(b), code = 3, length = 0.1)
text(-1.7, sum(pnorm(c(a, b))) / 2, labels = "Change = Pr(0 < Z < 2)")