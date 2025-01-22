## Bayesian estimation of incidence rates

x <- seq(0, 1, by = 0.002)
m <- 4
T <- 25

# plot of prior and posterior distributions
plot(x, dgamma(x, shape = 0.5 + m, rate = 0.1 + T), type = "n",
     xlab = expression(paste("Incidence rate (", lambda, ")")),
     ylab = "Probability density")
grid()
lines(x, dgamma(x, shape = 0.5, rate = 0.1), lty = "dashed")
lines(x, dgamma(x, shape = 0.5 + m, rate = 0.1 + T))
legend("topright", lty = c("dashed", "solid"),
       legend = c("gamma(0.5, 0.1) prior", "gamma (4.5, 25.1) posterior"))