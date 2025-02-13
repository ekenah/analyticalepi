## Log-logistic hazard functions

# hazard function
hllog <- function(t, shape=1, rate=1) {
  shape * rate^shape * t^(shape - 1) / (1 + (rate * t)^shape)
}

# hazard plots for shape = 2, 1, 1 / 2
t <- seq(0, 4, by = 0.01)
plot(t, hllog(t, 2), type = "l", lty = "dashed",
     xlab = "Time", ylab = "Log-logistic hazard (rate = 1)")
lines(t, hllog(t))
lines(t, hllog(t, 0.5), lty = "dotted")
grid()
text(1.5, 0.5, "shape = 1")
text(3, 0.7, "shape = 2")
text(1.5, 0.1, "shape = 1 / 2")