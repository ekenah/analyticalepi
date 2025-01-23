## Kaplan-Meier H(t) >= Nelson-Aalen H(t)

# plot of estimated cumulative hazard function increments
x <- seq(0, 1, by = 0.01)
plot(x, x, type = "l", ylim = c(0, 1),
     xlab = expression(d[j] / n[j]),
     ylab = expression(paste("Estimated ", Delta, H[j], " = ",
                             H(t[j]) - H(t[j - 1]))))
lines(x, -log(1 - x), lty = "dashed")
grid()
text(0.75, 0.55, expression(paste("NA estimator: ", d[j] / n[j])))
text(0.25, 0.7, expression(paste("KM estimator: -ln(", 1 - d[j] / n[j], ")",
                                 sep = "")))