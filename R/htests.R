## Hypothesis tests based on the log likelihood

# binomial log likelihood, score, and information functions
bin_loglik <- function(p, k=60, n=100) {
  k * log(p) + (n - k) * log(1 - p)
}
bin_score <- function(p, k=60, n=100) {
  k / p - (n - k) / (1 - p)
}
bin_information <- function(p, k=60, n=100) {
  k / p^2 + (n - k) / (1 - p)^2
}

# plot showing Wald, score, and likelihood ratio tests
p <- seq(0.4, 0.8, length.out = 200)
plot(p, bin_loglik(p), type = "n",
     xlim = c(0.40, 0.70), ylim = c(-72, -66),
     main = "Tests of the null hypothesis p = 0.5",
     xlab = "p", ylab = "ln L(p)")
grid()
lines(p, bin_loglik(p))
abline(v = c(0.5, 0.6), lty = "dotted")
abline(h = c(bin_loglik(0.5), bin_loglik(0.6)), lty = "dashed")
abline(a = bin_loglik(0.5) - bin_score(0.5) * 0.5, b = bin_score(0.5),
       col = "darkgray")
text(c(0.5, 0.6), c(-67.05, -67),
     labels = c(expression(p[0]), expression(hat(p))))
text(0.55, -70.7, labels = "Wald test")
arrows(0.5, -70.5, 0.6, code = 3, length = 0.1)
arrows(0.475, bin_loglik(0.5), y1 = bin_loglik(0.6),
       code = 3, length = 0.1)
text(0.45, -68.3, labels = "LRT")

# The slope is the tangent of the angle to the x-axis.
# We also must account for the different scales on the x- and y-axes.
# 0.3 / 6 is xdist / ydist (see xlim and ylim above)
score_angle <- atan(bin_score(0.5) * 0.3 / 6)
angles <- seq(0, score_angle, by = 0.01)
score_x <- 0.5 + 0.04 * cos(angles)
score_y <- bin_loglik(0.5) + 0.04 * (6 / 0.3) * sin(angles)
lines(score_x, score_y)
text(0.56, -68.8, "Score test")
arrows(score_x[2], score_y[2], score_x[1], score_y[1], length = 0.1)
arrows(rev(score_x)[2], rev(score_y)[2], rev(score_x)[1], rev(score_y)[1],
       length = 0.1)