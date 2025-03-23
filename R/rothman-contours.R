## Contour lines for measures of association

# load R packages
require(plotrix)    # for boxed.labels()

# odds ratio, cumulative hazard ratio, and inverses
odds <- function(p) p / (1 - p)
invodds <- function(odds) odds / (1 + odds)
chaz <- function(p) -log(1 - p)
invchaz <- function(chaz) 1 - exp(-chaz)

# contour lines plot functions
# risk differences
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
# risk ratios
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
# odds ratios
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
# cumulative hazard ratios
chazratio_contours <- function() {
  plot(x, x, type = "l", main = "Cumulative hazard ratio contours",
       xlab = "Risk in the unexposed", ylab = "")
  grid()
  boxed.labels(.5, .5, "1", xpad = 1.5, ypad = 1.5, border = FALSE)
  for (chazratio in c(0.2, 0.5, 2, 5)) {
    lines(c(x, 1), c(invchaz(chazratio * chaz(x)), 1), lty = "dashed")
    labelx <- uniroot(function(x) x - (1 - x)^chazratio, c(0, 1))$root
    labely <- 1 - labelx
    boxed.labels(labelx, labely, as.character(chazratio),
                 xpad = 1.5, ypad = 1.5, border = FALSE)
  }
}

# plot array
oldmar <- par("mar")
oldmgp <- par("mgp")
par(mfrow = c(2, 2), mar = 0.6 * (c(5, 5, 4, 1) + 0.1), mgp = c(2, 1, 0))
x <- seq(0.001, 0.999, by = 0.001)
riskdiff_contours()
riskratio_contours()
oddsratio_contours()
chazratio_contours()

# reset graphical parameters
par(mfrow = c(1, 1), mar = oldmar, mgp = oldmgp)
