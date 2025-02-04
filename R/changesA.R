## Local changes in the RD, RR, and OR near (0.2, 0.6)

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
datA <- data.frame(x = x, yORa = invodds(6 * odds(x)),
                   yRRa = 3 * x, yRDa = x + 0.4)

# plot of contours near point A
(ggplot(datA, aes(x, yORa))
  + coord_cartesian(xlim = c(0.1, 0.3), ylim = c(0.5, 0.7))
  + xlab("Risk in the unexposed")
  + ylab("Risk in the exposed")
  + annotate("point", x = 0.2, y = 0.6, size = 3)
  + annotate("text", 0.202, 0.598, hjust = 0, vjust = 1, label = "(0.2, 0.6)")
  + geom_textline(label = "OR = 6", hjust = 0.385)
  + geom_textline(aes(x, yRDa), label = "RD = 0.40", hjust = 0.22,
                  linetype = "dotted")
  + geom_textline(aes(x, yRRa), label = "RR = 3", hjust = 0.208,
                  linetype = "dashed")
  + annotate("text", 0.25, 0.55,
             label = "All toward the null\n OR < 6, RR < 3, RD < 0.4")
  + annotate("text", 0.15, 0.65,
             label = "All away from the null\n OR > 6, RR > 3, RD > 0.4")
  + annotate("text", angle = 49,
             x = 0.2 + 0.09 * cos(degtorad(49)),
             y = 0.6 + 0.09 * sin(degtorad(49)),
             label = "OR < 6, RR < 3, RD > 0.4")
  + annotate("text", angle = 63,
             x = 0.2 + 0.09 * cos(degtorad(63)),
             y = 0.6 + 0.09 * sin(degtorad(63)),
             label = "OR > 6, RR < 3, RD > 0.4")
  + annotate("text", angle = 52,
             x = 0.2 + 0.09 * cos(degtorad(180 + 52)),
             y = 0.6 + 0.09 * sin(degtorad(180 + 52)),
             label = "OR > 6, RR > 3, RD < 0.4")
  + annotate("text", angle = 66,
             x = 0.2 + 0.087 * cos(degtorad(180 + 66)),
             y = 0.6 + 0.087 * sin(degtorad(180 + 66)),
             label = "OR < 6, RR > 3, RD < 0.4")
)
