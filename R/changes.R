## Contours for the RD, RR, and OR at (0.2, 0.6) and (0.8, 0.4)

# required packages
require(ggplot2, quietly = TRUE)
require(geomtextpath, quietly = TRUE)

# odds and logistic functions
odds <- function(p) p / (1 - p)
invodds <- function(odds) odds / (1 + odds)

# Point A at (0.2, 0.6) with OR = 6, RR = 3, and RD = 0.4
# and point B at (0.6, 0.2) with OR = 1/6, RR = 1/3, and RD = -0.4
x <- seq(0.001, 0.999, by = 0.001)
dat <- data.frame(x = x, yORa = invodds(6 * odds(x)),
                  yRRa = 3 * x, yRDa = x + 0.4,
                  yORb = invodds(odds(x) / 6), yRRb = x / 2,
                  yRDb = x - 0.4)

# with points 1A and 1B
(ggplot(dat, aes(x, yORa))
  + theme_bw()
  + scale_x_continuous(limits = c(0, 1), expand = expansion(mult = 0.02),
                       breaks = seq(0, 1, by = 0.2))
  + scale_y_continuous(limits = c(0, 1), expand = expansion(mult = 0.02),
                       breaks = seq(0, 1, by = 0.2))
  + xlab("Risk in the unexposed")
  + ylab("Risk in the exposed")
  # point A
  + annotate("point", x = 0.2, y = 0.6, size = 3)
  + annotate("text", x = 0.2, y = 0.58, hjust = 0, vjust = 1,
             label = "(0.2, 0.6)")
  + geom_textline(label = "OR = 6", hjust = 0.8)
  + geom_textline(aes(x, yRRa), label = "RR = 3", hjust = 0.9,
                  linetype = "dashed")
  + geom_textline(aes(x, yRDa), label = "RD = 0.4", hjust = 0.9,
                  linetype = "dotted")
  + geom_textline(aes(x, x), col = "darkgray", label = "Null line")
  + annotate("rect", xmin = 0.09, xmax = 0.31, ymin = 0.49, ymax = 0.71,
             alpha = 0.2)
  + annotate("text", x = 0.11, y = 0.69, hjust = 0, vjust = 1,
             label = "Fig. 9.5")
  # point B
  + annotate("point", x = 0.8, y = 0.4, size = 3)
  + annotate("text", x = 0.8, y = 0.38, hjust = 0, vjust = 1,
             label = "(0.8, 0.4)")
  + geom_textline(aes(x, yORb), hjust = 0.8, label = "OR = 1/6")
  + geom_textline(aes(x, yRRb), hjust = 0.5, linetype = "dashed",
                  label = "RR = 1/2")
  + geom_textline(aes(x, yRDb), hjust = 0.05, linetype = "dotted",
                  label = "RD = -0.4")
  + annotate("rect", xmin = 0.69, xmax = 0.91, ymin = 0.29, ymax = 0.51,
             alpha = 0.2)
  + annotate("text", x = 0.71, y = 0.49, hjust = 0, vjust = 1,
             label = "Fig. 9.6")
)
