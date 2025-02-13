## Rate ratios and odds ratios for the Weibull distribution

# log-logistic odds function
odds_weib <- function(t, shape=1, rate=1) {
  p <- 1 - exp(-(rate * t)^shape)
  p / (1 - p)
}

# save old graphics parameters to restore them
mfrow_old <- par("mfrow")
mar_old <- par("mar")

# set two rows and one column and adjust margins between plots
par(mar = c(4, 4 ,1, 1), mfrow = c(2, 1))

# odds with shape = 2 and rates = 1 and 2
t <- seq(0.01, 0.6, by = 0.01)
plot(t, odds_weib(t, shape = 2, rate = 2), type = "l", lty = "dashed",
     xlab = "", ylab = "Odds")
lines(t, odds_weib(t, shape = 2, rate = 1))
grid()
text(0.1, 2.75, labels = "both shape = 2")
text(0.4, 0.4, labels = "rate = 1")
text(0.4, 1.3, labels = "rate = 2")

# odds ratio
plot(t, odds_weib(t, 2, 2) / odds_weib(t, 2, 1), type = "l", ylim = c(0, 8),
     xlab = "Time", ylab = "Odds ratio")
grid()
text(0.1, 7, labels = "rate ratio = 2")

# restore old graphics parameters
par(mar = mar_old, mfrow = mfrow_old)
