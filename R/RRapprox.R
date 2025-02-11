## Approximation to the risk ratio in case-control studies

# contour labels
require(plotrix, quietly = TRUE)

# odds, logistic, and cumulative hazard functions
odds <- function(p) p / (1 - p)
invodds <- function(odds) odds / (1 + odds)
cumhaz <- function(p) -log(1 - p)

# Rothman diagram with RR, OR, and HR contours
riskratios <- c(0.2, 0.5, 2, 5)
p0 <- seq(0.0001, 0.11, by = 0.001)
plot(p0, p0, type = "l", col = "darkgray", xlim = c(0, 0.1), ylim = c(0, 0.1),
     xlab = "Risk in the unexposed", ylab = "Risk in the exposed")
grid()
for (riskratio in riskratios) {
  p1 <- riskratio * p0
  
  # risk ratio contours
  lines(p0, riskratio * p0, lty = "solid")

  # odds ratio contours
  lines(p0, invodds(riskratio * odds(p0)), lty = "dashed")
  
  # cumulative hazard ratio contours
  lines(p0, 1 - (1 - p0)^riskratio, lty = "dotted")

  # labels
  boxed.labels(0.1 / (riskratio + 1), 0.1 * riskratio / (riskratio + 1),
               as.character(riskratio),
               xpad = 1.62, ypad = 1.62, border = FALSE)
}
legend("topright", bg = "white", lty = c("solid", "dotted", "dashed"),
       legend = c("Risk ratio", "Hazard ratio", "Odds ratio"))
