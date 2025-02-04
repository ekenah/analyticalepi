## Local changes in the RD, RR, and OR near (0.8, 0.4)

# required packages
require(ggplot2, quietly = TRUE)
require(geomtextpath, quietly = TRUE)

# odds and logistic functions, conversion from degrees to radians
odds <- function(p) p / (1 - p)
invodds <- function(odds) odds / (1 + odds)
degtorad <- function(degrees) pi * (degrees / 180)

# Point A at (0.2, 0.6) with OR = 6, RR = 3, and RD = 0.4
# and point B at (0.6, 0.2) with OR = 1/6, RR = 1/3, and RD = -0.4
x <- seq(0.001, 0.999, by = 0.001)
datB <- data.frame(x = x, yORb = invodds(odds(x) / 6), yRRb = x / 2,
                   yRDb = x - 0.4)

(ggplot(datB, aes(x, yORb))
  + coord_cartesian(xlim = c(0.7, 0.9), ylim = c(0.3, 0.5))
  + xlab("Risk in the unexposed")
  + ylab("Risk in the exposed")
  + annotate("point", x = 0.8, y = 0.4, size = 3)
  + annotate("text", 0.802, 0.398, hjust = 0, vjust = 1, label = "(0.8, 0.4)")
  + geom_textline(label = "OR = 1/6", hjust = 0.615)
  + geom_textline(aes(x, yRDb), label = "RD = -0.4", hjust = 0.78,
                  linetype = "dotted")
  + geom_textline(aes(x, yRRb), label = "RR = 1/2", hjust = 0.84,
                  linetype = "dashed")
  + annotate("text", 0.75, 0.45,
             label = "All toward the null\n OR > 1/6, RR > 1/2, RD > -0.4")
  + annotate("text", 0.85, 0.35,
             label = "All away from the null\n OR < 1/6, RR < 1/2, RD < -0.4")
  + annotate("text", angle = 36,
             x = 0.8 + 0.09 * cos(degtorad(36)),
             y = 0.4 + 0.09 * sin(degtorad(36)),
             label = "OR < 1/6, RR > 1/2, RD < -0.4")
  + annotate("text", angle = 52,
             x = 0.8 + 0.09 * cos(degtorad(52)),
             y = 0.4 + 0.09 * sin(degtorad(52)),
             label = "OR < 1/6, RR > 1/2, RD > -0.4")
  + annotate("text", angle = 36,
             x = 0.8 + 0.09 * cos(degtorad(180 + 36)),
             y = 0.4 + 0.09 * sin(degtorad(180 + 36)),
             label = "OR > 1/6, RR < 1/2, RD > -0.4")
  + annotate("text", angle = 47,
             x = 0.8 + 0.095 * cos(degtorad(180 + 49)),
             y = 0.4 + 0.095 * sin(degtorad(180 + 49)),
             label = "OR > 1/6, RR < 1/2, RD < -0.4")
)
