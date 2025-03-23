## Noncollapsibility and curved contour lines (odds ratio)

# load R packages
library(dplyr)      # for case_when()
library(plotrix)    # for boxed.labels()

# odds ratio and inverse
odds <- function(p) p / (1 - p)
invodds <- function(odds) odds / (1 + odds)

# data from Appleton et al. (The American Statistician, 1996)
# Using an ordered factor allows the use of <, =, and > for comparison.
agegroups <- factor(c("18-24", "25-34", "35-44", "45-54", "55-64",
                      "65-74", "75+"),
                    ordered = TRUE)
AFVdat <- data.frame(smoking = rep(c(0, 0, 1, 1), times = length(agegroups)),
                     death = rep(c(0, 1, 0, 1), times = length(agegroups)),
                     age = rep(agegroups, each = 4))
AFVrows <- rep(seq_len(nrow(AFVdat)),
               times = c(61, 1, 53, 2, 152, 5, 121, 3, 114, 7, 95, 14, 66, 12,
                         103, 27, 81, 40, 64, 51, 28, 101, 7, 29, 0, 64, 0, 13))
AFVdat <- AFVdat[AFVrows, ]
AFVdat$binage <- case_when(AFVdat$age < "65-74" ~ "18-64",
                           AFVdat$age >= "65-74" ~ "65+")

# logistic regression (binomial GLM with logit link) for odds ratio
nointx_logistic <- glm(death ~ smoking + binage, data = AFVdat,
                       family = binomial(link = "logit"))

# noncollapsibility of the odds ratio
pred_newdata <- data.frame(binage = rep(c("18-64", "65+"), each = 2),
                           smoking = rep(c(0, 1), times = 2))
predformat <- function(predrisk) {
  data.frame(matrix(predrisk, ncol = 2, byrow = TRUE,
                    dimnames = list(c("18-64", "65+"),
                                    c("nonsmokers", "smokers"))))
}
predrisk_logistic <- predformat(predict(nointx_logistic, type = "response",
                                        newdata = pred_newdata))
# find minimum odds ratio
std_odds <- function(t) {
  xcoord <- with(predrisk_logistic,
                 (1 - t) * nonsmokers[1] + t * nonsmokers[2])
  ycoord <- with(predrisk_logistic, (1 - t) * smokers[1] + t * smokers[2])
  odds(ycoord) / odds(xcoord)
}
minodds <- optimize(std_odds, c(0, 1))
tmin <- minodds$minimum
xmin <- with(predrisk_logistic,
             (1 - tmin) * nonsmokers[1] + tmin * nonsmokers[2])
ymin <- with(predrisk_logistic, (1 - tmin) * smokers[1] + tmin * smokers[2])

# odds ratio noncollapsibility plot
plot(predrisk_logistic$nonsmokers, predrisk_logistic$smokers, pch = 19,
     xlim = c(0, 1), ylim = c(0, 1), asp = 1,
     xlab = "Risk in the unexposed (nonsmokers)",
     ylab = "Risk in the exposed (smokers)")
x <- seq(0.01, 0.99, by = 0.01)
common_OR <- exp(coef(nointx_logistic)["smoking"])
lines(x, invodds(common_OR * odds(x)), lty = "dashed")
segments(0, 0, 1, 1, col = "darkgray")
segments(predrisk_logistic["18-64", "nonsmokers"],
         predrisk_logistic["18-64", "smokers"],
         predrisk_logistic["65+", "nonsmokers"],
         predrisk_logistic["65+", "smokers"])
points(xmin, ymin)
grid()
text(0.1, 0.06, srt = 45, col = "darkgray", labels = "Null line")
text(0.43, 0.59, srt = 44, labels = "Common OR = 1.537")
text(predrisk_logistic$nonsmokers - 0.02,
     predrisk_logistic$smokers + 0.02,
     adj = c(1, 0), labels = rownames(predrisk_logistic))
boxed.labels(xmin + 0.21, ymin - 0.02, labels = "Standardized OR = 1.229",
             xpad = 1, ypad = 1.5, border = FALSE)
text(xmin + 0.02, ymin - 0.04, adj = c(0, 1),
     labels = expression("when Pr"["std"]("18-64") == 0.484))
