## Bounds on beta0 and beta1 in a binomial GLM for the risk ratio
# This uses the log link, and we assume a single binary covariate coded 0/1.

# plot
plot(c(0, 0, -2, -2), c(0, -2, -2, 2), type = "n",
     xlim = c(-1.5, 1), ylim = c(-1.5, 1),
     xlab = expression(paste("Intercept ", (beta[0]))),
     ylab = expression(paste("Slope ", (beta[1]))))
grid()
polygon(c(0, 0, -2, -2), c(0, -2, -2, 2),
        border = "darkgray", col = "gray")
grid()
abline(h = 0)  # x-axis (y = 0)
abline(v = 0)  # y-axis (x = 0)

# labels
text(0.1, -0.75, expression(beta[0] == 0), srt = 90)
text(-0.5, 0.6, expression(beta[0] + beta[1] == 0), srt = -45)
arrows(-1.2, 0.2, -1.5, 0.2, code = 2, length = 0.1)
text(-0.95, 0.2, expression(beta[0] %->% -infinity))
arrows(-1.3, -1.3, -1.5, -1.5, code = 2, length = 0.1)
text(-1.1, -1.2, expression(beta[0] + beta[1] %->% -infinity))
arrows(-0.2, -1.2, -0.2, -1.5, code = 2, length = 0.1)
text(-0.2, -1.1, expression(beta[1] %->% -infinity))
