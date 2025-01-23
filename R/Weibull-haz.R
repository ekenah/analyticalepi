## Weibull hazard functions

# hazard function
hweib <- function(t, shape=1, rate=1) shape * rate^shape * t^(shape - 1)

# hazard plots for shapes 2, 1, and 1 / 2
t <- seq(0, 2, by = 0.01)
plot(t, hweib(t, 2), type = "l", lty = "dashed",
     xlab = "Time", ylab = "Weibull hazard (rate = 1)")
lines(t, hweib(t))
lines(t, hweib(t, 0.5), lty = "dotted")
grid()
text(1.6, 1.2, "shape = 1 (exponential)")
text(1.25, 0.2, "shape = 1 / 2")
text(0.7, 1.9, "shape = 2")
