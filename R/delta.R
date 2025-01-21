## Approximation used by the delta method

p <- seq(0.02, 0.98, by = 0.01)
logit <- function(p) log(p) - log(1 - p)

# plot
plot(p, logit(p), type = "n",
     xlab = "p", ylab = "logit(p)")
grid()
lines(p, logit(p))
points(0.6, logit(0.6))
abline(logit(0.6) - 2.5, 1 / 0.24, lty = "dashed")
text(0.6, -1,
     labels = expression(paste("logit(p) - ", logit(0.6) %~~% logit,
                               "'(0.6) (p - 0.6)")))