## Collapsibility and straight contour lines (risk difference)

# load R packages
library(dplyr)      # for case_when()

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

# binomial GLM with identity link for risk difference
# Model without interaction requires a starting point for numerical stability.
# We get a starting point from a linear regression model.
nointx_lm <- lm(death ~ smoking + binage, data = AFVdat)
nointx_idbinom <- glm(death ~ smoking + binage, data = AFVdat,
                      family = binomial(link = "identity"),
                      start = coef(nointx_lm))

# risk difference collapsibility plot
pred_newdata <- data.frame(binage = rep(c("18-64", "65+"), each = 2),
                           smoking = rep(c(0, 1), times = 2))
predformat <- function(predrisk) {
  data.frame(matrix(predrisk, ncol = 2, byrow = TRUE,
                    dimnames = list(c("18-64", "65+"),
                                    c("nonsmokers", "smokers"))))
}
predrisk_idbinom <- predformat(predict(nointx_idbinom, type = "response",
                                       newdata = pred_newdata))
plot(predrisk_idbinom$nonsmokers, predrisk_idbinom$smokers, pch = 19,
     xlim = c(0, 1), ylim = c(0, 1), asp = 1,
     xlab = "Risk in the unexposed (nonsmokers)",
     ylab = "Risk in the exposed (smokers)")
x <- seq(0, 1, by = 0.01)
y <- x + coef(nointx_idbinom)["smoking"]
yrange <- y >= 0 & y <= 1
lines(x[yrange], y[yrange], lty = "dashed")
segments(0, 0, 1, 1, col = "darkgray")
segments(predrisk_idbinom["18-64", "nonsmokers"],
         predrisk_idbinom["18-64", "smokers"],
         predrisk_idbinom["65+", "nonsmokers"],
         predrisk_idbinom["65+", "smokers"])
grid()
text(0.1, 0.06, srt = 45, col = "darkgray", labels = "Null line")
text(0.45, 0.57, srt = 45, labels = "Common and standardized RD = 0.052")
