## Optimal proportion exposed in a cohort study

# plot of optimal phi as a function of the Bernoulli variance ratio
logvratio <- seq(-3, 3, by = 0.01)
phi <- function(v) 1 / (1 + sqrt(v))
plot(logvratio, phi(exp(logvratio)), type = "l", xaxt = "n", ylim = c(0, 1),
     xlab = "Bernoulli variance ratio (log scale)",
     ylab = expression(paste("Optimal proportion exposed or cases (", phi, "*)")))
axis(1, at = log(c(1 / c(16, 8, 4, 2), 1, c(2, 4, 8, 16))),
     labels = c("1/16", "1/8", "1/4", "1/2", 1, 2, 4, 8, 16))
grid()
abline(h = 0.5, col = "darkgray")