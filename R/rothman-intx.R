## Contour lines and interaction

# load R packages
library(dplyr)      # for case_when()
library(lmtest)     # for lrtest()

# odds ratio, cumulative hazard ratio, and inverses
odds <- function(p) p / (1 - p)
invodds <- function(odds) odds / (1 + odds)
chaz <- function(p) -log(1 - p)
invchaz <- function(chaz) 1 - exp(-chaz)

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

# 2x2 tables
twobytwo <- function(covariate = NULL, dat = AFVdat) {
  # generate crude or stratified 2x2 tables
  # covariate should be a string naming a variable in dat
  formla <- "~ smoking + death"
  # update formula if there is a covariate specified
  if (!is.null(covariate)) formla <- paste(formla, "+", covariate)
  tab22 <- xtabs(formla, data = dat, drop.unused.levels = TRUE)
  # format stratified 2x2 tables into a list
  if (length(dim(tab22)) > 2) {
    tab22 <- unlist(apply(tab22, 3, list), recursive = FALSE)
  } else {
    tab22 <- list(tab22)
  }
  # reorder rows and columns and add margins to 2x2 table(s)
  lapply(tab22, function(x) addmargins(x[c(2, 1), ][, c(2, 1)]))
}

# risk calculations
condrisk <- function(covariate = NULL, dat = AFVdat) {
  # function to calculate crude or stratified risks in smokers and nonsmokers
  # covariate should be a string naming a variable in dat
  strat2 <- twobytwo(covariate, dat)
  risk <- function(v) v[, 1] / v[, 3]   # v = (alive, dead)
  risks <- do.call(rbind, lapply(strat2, risk))
  colnames(risks) <- c("smokers", "nonsmokers", "marginal")
  as.data.frame(risks)
}
margrisk <- condrisk()
stratrisk <- condrisk("binage")

# binomial GLMs with identity link for risk difference
# Model without interaction requires a starting point for numerical stability.
# We get a starting point from a linear regression model.
intx_idbinom <- glm(death ~ smoking * binage, data = AFVdat,
                    family = binomial(link = "identity"))
nointx_lm <- lm(death ~ smoking + binage, data = AFVdat)
nointx_idbinom <- glm(death ~ smoking + binage, data = AFVdat,
                      family = binomial(link = "identity"),
                      start = coef(nointx_lm))

# log-binomial regression for risk ratio
# Both models require a starting point due to numerical instability.
# We get these from Poisson GLMs with a log link.
intx_poisson <- glm(death ~ smoking * binage, data = AFVdat,
                    family = poisson())
intx_logbinom <- glm(death ~ smoking * binage, data = AFVdat,
                     family = binomial(link = "log"),
                     start = coef(intx_poisson))
nointx_poisson <- glm(death ~ smoking + binage, data = AFVdat,
                      family = poisson())
nointx_logbinom <- glm(death ~ smoking + binage, data = AFVdat,
                       family = binomial(link = "log"),
                       start = coef(nointx_poisson) - 0.1)

# logistic regression (binomial GLM with logit link) for odds ratio
intx_logistic <- glm(death ~ smoking * binage, data = AFVdat,
                     family = binomial(link = "logit"))
nointx_logistic <- glm(death ~ smoking + binage, data = AFVdat,
                       family = binomial(link = "logit"))

# binomial GLM with complementary log-log link for cumulative hazard ratio
intx_chrbinom <- glm(death ~ smoking * binage, data = AFVdat,
                     family = binomial(link = "cloglog"))
nointx_chrbinom <- glm(death ~ smoking + binage, data = AFVdat,
                       family = binomial(link = "cloglog"))

# function to collect estimates from binomial GLMs
intxest_plot <- function(model, model0) {
  # model has interaction term, model0 has no interaction term
  meas <- cumsum(coef(model)[c("smoking", "smoking:binage65+")])
  meas0 <- coef(model0)["smoking"]
  ci0 <- confint(model0)["smoking", ]
  if (model$family$link %in% c("log", "logit", "cloglog")) {
    meas <- exp(meas)
    meas0 <- exp(meas0)
    ci0 <- exp(ci0)
  }
  c("18-64" = meas[1], "65+" = meas[2], "common" = meas0)
}

# stratum-specific and combined measures of association
intx_meas <- mapply(intxest_plot,
                    model = list(riskdiff = intx_idbinom,
                                 riskratio = intx_logbinom,
                                 oddsratio = intx_logistic,
                                 chazratio = intx_chrbinom),
                    model0 = list(nointx_idbinom,
                                  nointx_logbinom,
                                  nointx_logistic,
                                  nointx_chrbinom))
# exchange rows and columns of intx_meas and rename columns
intx_tbl <- t(intx_meas)
colnames(intx_tbl) <- c("18-64", "65+", "common")

# Rothman diagrams with estimated effect measure modification
# risk difference interaction
riskdiff_intx <- function() {
  plot(stratrisk$nonsmokers, stratrisk$smokers, pch = 19,
       xlim = c(0, 1), ylim = c(0, 1),
       main = "Risk difference estimates",
       xlab = "", ylab = "Risk in the exposed (smokers)")
  y <- intx_tbl["riskdiff", "common"] + x
  yrange <- y >= 0 & y <= 1
  lines(x[yrange], y[yrange])
  y0 <- intx_tbl["riskdiff", "18-64"] + x
  y0range <- y0 >= 0 & y0 <= 1
  lines(x[y0range], y0[y0range], lty = "dashed")
  y1 <- intx_tbl["riskdiff", "65+"] + x
  y1range <- y1 >= 0 & y1 <= 1
  lines(x[y1range], y1[y1range], lty = "dashed")
  grid()
  text(stratrisk$nonsmokers[1] - 0.02, stratrisk$smokers[1] + 0.02,
       adj = c(1, 0), labels = rownames(stratrisk)[1])
  text(stratrisk$nonsmokers[2] + 0.02, stratrisk$smokers[2] - 0.02,
       adj = c(0, 1), labels = rownames(stratrisk)[2])
}
# risk ratio interaction
riskratio_intx <- function() {
  plot(stratrisk$nonsmokers, stratrisk$smokers, pch = 19,
       xlim = c(0, 1), ylim = c(0, 1),
       main = "Risk ratio estimates", xlab = "", ylab = "")
  y <- intx_tbl["riskratio", "common"] * x
  yrange <- y >= 0 & y <= 1
  lines(x[yrange], y[yrange])
  y0 <- intx_tbl["riskratio", "18-64"] * x
  y0range <- y0 >= 0 & y0 <= 1
  lines(x[y0range], y0[y0range], lty = "dashed")
  y1 <- intx_tbl["riskratio", "65+"] * x
  y1range <- y1 >= 0 & y1 <= 1
  lines(x[y1range], y1[y1range], lty = "dashed")
  grid()
  text(stratrisk$nonsmokers[1] - 0.02, stratrisk$smokers[1] + 0.02,
       adj = c(1, 0), labels = rownames(stratrisk)[1])
  text(stratrisk$nonsmokers[2] + 0.02, stratrisk$smokers[2] - 0.02,
       adj = c(0, 1), labels = rownames(stratrisk)[2])
}
# odds ratio interaction
oddsratio_intx <- function() {
  plot(stratrisk$nonsmokers, stratrisk$smokers, pch = 19,
       xlim = c(0, 1), ylim = c(0, 1),
       main = "Odds ratio estimates",
       xlab = "Risk in the unexposed (nonsmokers)",
       ylab = "Risk in the exposed (smokers)")
  grid()
  text(stratrisk$nonsmokers[1] - 0.02, stratrisk$smokers[1] + 0.02,
       adj = c(1, 0), labels = rownames(stratrisk)[1])
  text(stratrisk$nonsmokers[2] + 0.02, stratrisk$smokers[2] - 0.02,
       adj = c(0, 1), labels = rownames(stratrisk)[2])
  lines(x, invodds(intx_tbl["oddsratio", "common"] * odds(x)))
  lines(x, invodds(intx_tbl["oddsratio", "18-64"] * odds(x)), lty = "dashed")
  lines(x, invodds(intx_tbl["oddsratio", "65+"] * odds(x)), lty = "dashed")
}
# cumulative hazard ratio interaction
chazratio_intx <- function() {
  plot(stratrisk$nonsmokers, stratrisk$smokers, pch = 19,
       xlim = c(0, 1), ylim = c(0, 1),
       main = "Cumulative hazard ratio estimates",
       xlab = "Risk in the unexposed (nonsmokers)", ylab = "")
  lines(x, invchaz(intx_tbl["chazratio", "common"] * chaz(x)))
  lines(x, invchaz(intx_tbl["chazratio", "18-64"] * chaz(x)), lty = "dashed")
  lines(x, invchaz(intx_tbl["chazratio", "65+"] * chaz(x)), lty = "dashed")
  # lines(x, 1 - (1 - x)^intx_tbl["chazratio", "common"])
  # lines(x, 1 - (1 - x)^intx_tbl["chazratio", "18-64"], lty = "dashed")
  # lines(x, 1 - (1 - x)^intx_tbl["chazratio", "65+"], lty = "dashed")
  grid()
  text(stratrisk$nonsmokers[1] - 0.02, stratrisk$smokers[1] + 0.02,
       adj = c(1, 0), labels = rownames(stratrisk)[1])
  text(stratrisk$nonsmokers[2] + 0.02, stratrisk$smokers[2] - 0.02,
       adj = c(0, 1), labels = rownames(stratrisk)[2])
}

# plot array
oldmar <- par("mar")
oldmgp <- par("mgp")
par(mfrow = c(2, 2), mar = 0.6 * (c(5, 5, 4, 1) + 0.1), mgp = c(2, 1, 0))
x <- seq(0.001, 0.999, by = 0.001)
riskdiff_intx()
riskratio_intx()
oddsratio_intx()
chazratio_intx()

# reset graphical parameters
par(mfrow = c(1, 1), mar = oldmar, mgp = oldmgp)


# ## Collapsibility
# pred_newdata <- data.frame(binage = rep(c("18-64", "65+"), each = 2),
#                            smoking = rep(c(0, 1), times = 2))
# predformat <- function(predrisk) {
#   data.frame(matrix(predrisk, ncol = 2, byrow = TRUE,
#                     dimnames = list(c("18-64", "65+"),
#                                     c("nonsmokers", "smokers"))))
# }
# predrisk_logistic <- predformat(predict(nointx_logistic, type = "response",
#                                         newdata = pred_newdata))
# predrisk_idbinom <- predformat(predict(nointx_idbinom, type = "response",
#                                        newdata = pred_newdata))
# # find minimum odds ratio
# std_odds <- function(t) {
#   xcoord <- with(predrisk_logistic,
#                  (1 - t) * nonsmokers[1] + t * nonsmokers[2])
#   ycoord <- with(predrisk_logistic, (1 - t) * smokers[1] + t * smokers[2])
#   odds(ycoord) / odds(xcoord)
# }
# minodds <- optimize(std_odds, c(0, 1))
# tmin <- minodds$minimum
# xmin <- with(predrisk_logistic,
#              (1 - tmin) * nonsmokers[1] + tmin * nonsmokers[2])
# ymin <- with(predrisk_logistic, (1 - tmin) * smokers[1] + tmin * smokers[2])
# cat("\n", "Minimum standardized odds ratio is ",
#     minodds$objective, ".\n", sep = "")
# cat("It occurs in a standard population where Pr(65+) =", tmin, "\n")
# cat("and Pr(18-64) = ", 1 - tmin, ".\n\n", sep = "")

# # risk difference collapsibility plot (Figure 6)
# dev.new()
# plot(predrisk_idbinom$nonsmokers, predrisk_idbinom$smokers, pch = 19,
#      xlim = c(0, 1), ylim = c(0, 1), asp = 1,
#      xlab = "Risk in the unexposed (nonsmokers)",
#      ylab = "Risk in the exposed (smokers)")
# y <- x + intx_meas["riskdiff", "common"]
# yrange <- y >= 0 & y <= 1
# lines(x[yrange], y[yrange], lty = "dashed")
# segments(0, 0, 1, 1, col = "darkgray")
# segments(predrisk_idbinom["18-64", "nonsmokers"],
#          predrisk_idbinom["18-64", "smokers"],
#          predrisk_idbinom["65+", "nonsmokers"],
#          predrisk_idbinom["65+", "smokers"])
# grid()
# text(0.1, 0.06, srt = 45, col = "darkgray", labels = "Null line")
# text(0.46, 0.56, srt = 45, labels = "Common and standardized RD = 0.052")
# # dev.copy2pdf(file = "collapsibilityRD.pdf")

# # odds ratio noncollapsibility plot (Figure 7)
# dev.new()
# plot(predrisk_logistic$nonsmokers, predrisk_logistic$smokers, pch = 19,
#      xlim = c(0, 1), ylim = c(0, 1), asp = 1,
#      xlab = "Risk in the unexposed (nonsmokers)",
#      ylab = "Risk in the exposed (smokers)")
# lines(x, invodds(intx_meas["oddsratio", "common"] * odds(x)), lty = "dashed")
# segments(0, 0, 1, 1, col = "darkgray")
# segments(predrisk_logistic["18-64", "nonsmokers"],
#          predrisk_logistic["18-64", "smokers"],
#          predrisk_logistic["65+", "nonsmokers"],
#          predrisk_logistic["65+", "smokers"])
# points(xmin, ymin)
# grid()
# text(0.1, 0.06, srt = 45, col = "darkgray", labels = "Null line")
# text(0.44, 0.58, srt = 42, labels = "Common OR = 1.537")
# text(predrisk_logistic$nonsmokers - 0.02,
#      predrisk_logistic$smokers + 0.02,
#      adj = c(1, 0), labels = rownames(predrisk_logistic))
# boxed.labels(xmin + 0.21, ymin - 0.02, labels = "Standardized OR = 1.229",
#              xpad = 1, ypad = 1.5, border = FALSE)
# text(xmin + 0.02, ymin - 0.04, adj = c(0, 1),
#      labels = expression("when Pr"["std"]("18-64") == 0.484))
# # dev.copy2pdf(file = "collapsibilityOR.pdf")
