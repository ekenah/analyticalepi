## Contour lines for measures of association
require(plotrix, quietly = TRUE)

# odds, logistic, and cumulative hazard functions
odds <- function(p) p / (1 - p)
invodds <- function(odds) odds / (1 + odds)
cumhaz <- function(p) -log(1 - p)

# risk difference contours
riskdiff_contours <- function() {
  plot(x, x, type = "l", main = "Risk difference contours",
       xlab = "", ylab = "Risk in the exposed")
  grid()
  boxed.labels(.5, .5, "0", xpad = 1.5, ypad = 1.5, border = FALSE)
  for (riskdiff in c(-0.5, -0.2, 0, 0.2, 0.5)) {
    y <- x + riskdiff
    yrange <- y >= 0 & y <= 1
    lines(x[yrange], y[yrange], lty = "dashed")
    boxed.labels(0.5 - riskdiff / 2, 0.5 + riskdiff / 2,
                 as.character(riskdiff),
                 xpad = 1.5, ypad = 1.5, border = FALSE)
  }
}

# risk ratio contours
riskratio_contours <- function() {
  plot(x, x, type = "l", main = "Risk ratio contours",
       xlab = "", ylab = "")
  grid()
  boxed.labels(.5, .5, "1", xpad = 1.5, ypad = 1.5, border = FALSE)
  for (riskratio in c(0.2, 0.5, 2, 5)) {
    y <- riskratio * x
    yrange <- y <= 1
    lines(x[yrange], y[yrange], lty = "dashed")
    boxed.labels(1 / (riskratio + 1), riskratio / (riskratio + 1),
                 as.character(riskratio),
                 xpad = 1.5, ypad = 1.5, border = FALSE)
  }
}

# odds ratio contours
oddsratio_contours <- function() {
  plot(x, x, type = "l", main = "Odds ratio contours",
       xlab = "Risk in the unexposed",
       ylab = "Risk in the exposed")
  grid()
  boxed.labels(.5, .5, "1", xpad = 1.5, ypad = 1.5, border = FALSE)
  for (oddsratio in c(0.2, 0.5, 2, 5)) {
    lines(x, invodds(oddsratio * odds(x)), lty = "dashed")
    boxed.labels((sqrt(oddsratio) - 1) / (oddsratio - 1),
                 (oddsratio - sqrt(oddsratio)) / (oddsratio - 1),
                 as.character(oddsratio),
                 xpad = 1.5, ypad = 1.5, border = FALSE)
  }
}

# hazard ratios
hazratio_contours <- function() {
  plot(x, x, type = "l", main = "Hazard ratio contours",
       xlab = "Risk in the unexposed", ylab = "")
  grid()
  boxed.labels(.5, .5, "1", xpad = 1.5, ypad = 1.5, border = FALSE)
  for (hazratio in c(0.2, 0.5, 2, 5)) {
    lines(c(x, 1), c(1 - (1 - x)^hazratio, 1), lty = "dashed")
    labelx <- uniroot(function(x) x - (1 - x)^hazratio, c(0, 1))$root
    labely <- 1 - labelx
    boxed.labels(labelx, labely, as.character(hazratio),
                 xpad = 1.5, ypad = 1.5, border = FALSE)
  }
}

# save old graphical parameters and set new ones
oldmar <- par("mar")
oldmgp <- par("mgp")
par(mfrow = c(2, 2), mar = 0.6 * (c(5, 5, 4, 1) + 0.1), mgp = c(2, 1, 0))

# grid of plots
x <- seq(0.001, 0.999, by = 0.001)
riskdiff_contours()
riskratio_contours()
oddsratio_contours()
hazratio_contours()
# dev.copy2pdf(file = "contours.pdf")

# reset graphical parameters
par(mfrow = c(1, 1), mar = oldmar, mgp = oldmgp)