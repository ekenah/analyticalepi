## Rate ratios and hazard ratios for the log-logistic distribution

# log-logistic hazard function
hllog <- function(t, shape=1, rate=1) {
  shape * rate^shape * t^(shape - 1) / (1 + (rate * t)^shape)
}

# save old graphics parameters to restore them
mfrow_old <- par("mfrow")
mar_old <- par("mar")

# set two rows and one column and adjust margins between plots
par(mar = c(4, 4 ,1, 1), mfrow = c(2, 1))

# hazards with shape = 2 and rates = 2 and 1
t <- seq(0.01, 3, by = 0.01)
plot(t, hllog(t, shape = 2, rate = 2), type = "l", lty = "dashed",
     xlab = "", ylab = "Hazard")
lines(t, hllog(t, shape = 2, rate = 1))
grid()
text(2.5, 1.75, labels = "both shape = 2")
text(1.0, 0.75, labels = "rate = 1")
text(1.25, 1.75, labels = "rate = 2")

# hazard ratio
plot(t, hllog(t, 2, 2) / hllog(t, 2, 1), type = "l", ylim = c(0, 4),
     xlab = "Time", ylab = "Hazard ratio")
grid()
text(2.5, 3.5, labels = "rate ratio = 2")

# restore old graphics parameters
par(mar = mar_old, mfrow = mfrow_old)
