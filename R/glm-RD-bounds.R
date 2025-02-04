## Bounds on beta0 and beta1 in a binomial GLM for the risk difference
# This uses the identity link. We assume a single binary covariate coded 0/1.

# plot
plot(c(0, 0, 1, 1), c(0, 1, 0, -1), type = "n",
     xlim = c(-0.75, 1.75), ylim = c(-1.25, 1.25),
     xlab = expression(paste("Intercept ", (beta[0]))),
     ylab = expression(paste("Slope ", (beta[1]))))
grid()
polygon(c(0, 0, 1, 1), c(0, 1, 0, -1), border = "darkgray", col = "gray")
grid()
abline(h = 0)  # x-axis (y = 0)
abline(v = 0)  # y-axis (x = 0)

# labels
text(-0.1, 0.5, expression(beta[0] == 0), srt = 90)
text(1.1, -0.5, expression(beta[0] == 1), srt = 90)
text(0.4, -0.5, expression(beta[0] + beta[1] == 0), srt = -45)
text(0.6, 0.5, expression(beta[0] + beta[1] == 1), srt = -45)
