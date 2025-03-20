# R code for "Rothman diagrams: the geometry of confounding and
# standardization" by Eben Kenah
# Modified and tested in R 4.4.1 by EK on 2024-07-31.
# Run using 'source("GCIepi-conf.R")' from the R command line.

require(dplyr)      # version 1.1.4
require(lmtest)     # version 0.9-40
require(plotrix)    # version 3.8-4

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


## Crude 2x2 table and hypothesis tests from Appleton (1996)
# function to generate crude or stratified 2x2 tables
twobytwo <- function(covariate = NULL, dat = AFVdat) {
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

# crude 2x2 table (Table 1)
crude22 <- twobytwo()[[1]]
cat("\n", "Crude 2x2 table (Table 1):\n", sep = "")
print(crude22)
# remove margins from crude 2x2 table for hypothesis tests
crude22_nomarg <- crude22[-3, -3]
cat("\n", "Pearson chi-squared test:\n", sep = "")
print(chisq.test(crude22_nomarg, correct = FALSE))
cat("Fisher's exact test and confidence limits for odds ratio:")
print(fisher.test(crude22_nomarg))

## Crude measures of association (Table 2)
# function to extract point and interval estimates from GLM
mest <- function(model, pval = TRUE, ...) {
  meas <- coef(model)["smoking"]
  ci <- confint(model, ...)["smoking", ]
  # get risk/odds ratio point and interval estimates from their logarithms
  if (model$family$link %in% c("log", "logit", "cloglog")) {
    meas <- exp(meas)
    ci <- exp(ci)
  }
  if (pval == FALSE) {
    return(c(meas, ci[1], ci[2]))
  } else {
    # likelihood ratio p-value (to match likelihood ratio confidence limits)
    pval <- lrtest(model, "smoking")[2, "Pr(>Chisq)"]
    c(meas, ci[1], ci[2], pval = pval)
  }
}
# binomial GLMs for odds ratio, risk ratio, and risk difference
crude_logistic <- glm(death ~ smoking, data = AFVdat,
                      family = binomial(link = "logit"))
crude_logbinom <- glm(death ~ smoking, data = AFVdat,
                      family = binomial(link = "log"))
crude_idbinom <- glm(death ~ smoking, data = AFVdat,
                     family = binomial(link = "identity"))
crude_hrbinom <- glm(death ~ smoking, data = AFVdat,
                     family = binomial(link = "cloglog"))
crude_meas <- do.call(rbind, lapply(list(oddsratio = crude_logistic,
                                         riskratio = crude_logbinom,
                                         riskdiff = crude_idbinom,
                                         hazratio = crude_hrbinom),
                                    mest))
cat("\n", "Crude measures of association (Table 2):\n", sep = "")
print(crude_meas)

## 2x2 tables stratified by age (Table 3)
strat2 <- twobytwo("binage")
cat("\n", "2x2 tables stratified by age (Table 3):\n", sep = "")
print(strat2)

## Risk calculations
# function to calculate crude or stratified risks in smokers and nonsmokers
condrisk <- function(covariate = NULL, dat = AFVdat) {
  # covariate should be a string naming a variable in dat
  strat2 <- twobytwo(covariate, dat)
  risk <- function(v) v[, 1] / v[, 3]   # v = (alive, dead)
  risks <- do.call(rbind, lapply(strat2, risk))
  colnames(risks) <- c("smokers", "nonsmokers", "marginal")
  as.data.frame(risks)
}
margrisk <- condrisk()
stratrisk2 <- condrisk("binage")

# standardized risks of death over binary age groups
# all participants
binage_n <- sapply(strat2, function(x) x[3, 3])
binage_p <- binage_n / sum(binage_n)
stdrisk2_all <- data.frame(smokers = sum(stratrisk2$smokers * binage_p),
                           nonsmokers = sum(stratrisk2$nonsmokers * binage_p))
# smokers
binage_nsmk <- sapply(strat2, function(x) x[1, 3])
binage_psmk <- binage_nsmk / sum(binage_nsmk)
stdrisk2_smk <- data.frame(smokers = sum(stratrisk2$smokers * binage_psmk),
                           nonsmokers = sum(stratrisk2$nonsmokers
                                            * binage_psmk))
# nonsmokers
binage_nnon <- sapply(strat2, function(x) x[2, 3])
binage_pnon <- binage_nnon / sum(binage_nnon)
stdrisk2_non <- data.frame(smokers = sum(stratrisk2$smokers * binage_pnon),
                           nonsmokers = sum(stratrisk2$nonsmokers
                                            * binage_pnon))


## Rothman diagram with standardized segment (Figure 1)
# function to plot Rothman diagram with crude and stratum-specific points
xlabel <- "Risk of death among the unexposed (nonsmokers)"
ylabel <- "Risk of death among the exposed (smokers)"
rothdiagram2 <- function(labels = c("18-64 (0%)", "65+ (100%)")) {
  plot(margrisk$nonsmokers, margrisk$smokers, asp = 1,
       xlim = c(0, 1), ylim = c(0, 1), xlab = xlabel, ylab = ylabel)
  segments(0, 0, 1, 1, col = "darkgray")
  points(stratrisk2$nonsmokers, stratrisk2$smokers, pch = 19)
  grid()
  text(margrisk$nonsmokers + 0.01, margrisk$smokers - 0.02, adj = c(0, 1),
       labels = "Crude point")
  text(stratrisk2["18-64", ]$nonsmokers - 0.01,
       stratrisk2["18-64", ]$smokers + 0.02,
       adj = c(1, 0), labels = labels[1])
  text(stratrisk2["65+", ]$nonsmokers - 0.01,
       stratrisk2["65+", ]$smokers + 0.02,
       adj = c(1, 0), labels = labels[2])
  text(0.1, 0.06, srt = 45, col = "darkgray", labels = "Null line")
}
rothdiagram2()
text(margrisk$nonsmokers + 0.01, margrisk$smokers - 0.06, adj = c(0, 1),
     labels = "(nonsmokers 26% 65+, smokers 8% 65+)")
segments(stratrisk2$nonsmokers[1], stratrisk2$smokers[1],
         stratrisk2$nonsmokers[2], stratrisk2$smokers[2], lty = "solid")
text(0.56, 0.62, srt = 42, labels = "Standardized segment")
points(stdrisk2_non$nonsmokers, stdrisk2_non$smokers)
text(stdrisk2_non$nonsmokers - 0.01, stdrisk2_non$smokers + 0.02, adj = c(1, 0),
     labels = "Nonsmokers (26%)")
points(stdrisk2_all$nonsmokers, stdrisk2_all$smokers)
text(stdrisk2_all$nonsmokers - 0.01, stdrisk2_all$smokers + 0.02, adj = c(1, 0),
     labels = "All participants (18%)")
points(stdrisk2_smk$nonsmokers, stdrisk2_smk$smokers)
text(stdrisk2_smk$nonsmokers - 0.01, stdrisk2_smk$smokers + 0.02, adj = c(1, 0),
     labels = "Smokers (8%)")
# dev.copy2pdf(file = "Rothman_std.pdf")
# dev.copy2eps(file = "Rothman_std.eps")


## Rothman diagram with confounding rectangle (Figure 2)
dev.new()
rothdiagram2(labels = c("18-64", "65+"))
segments(stratrisk2["18-64", "nonsmokers"], stratrisk2["18-64", "smokers"],
         stratrisk2["65+", "nonsmokers"], stratrisk2["65+", "smokers"],
         lty = "solid")
text(0.48, 0.55, srt = 42, labels = "Standardized segment")
text(0.5, 0.83, labels = "Confounding rectangle")
polygon(rep(stratrisk2$nonsmokers, each = 2),
        c(stratrisk2$smokers, rev(stratrisk2$smokers)),
        lty = "dashed")
points(stratrisk2["18-64", "nonsmokers"], stratrisk2["65+", "smokers"])
text(stratrisk2["18-64", "nonsmokers"] + 0.01,
     stratrisk2["65+", "smokers"] + c(0.07, 0.03),
     labels = c("All nonsmokers 18-64,", "all smokers 65+"))
points(stratrisk2["65+", "nonsmokers"], stratrisk2["18-64", "smokers"])
text(stratrisk2["65+", "nonsmokers"],
     stratrisk2["18-64", "smokers"] - c(0.03, 0.07),
     labels = c("All nonsmokers 65+,", "all smokers 18-64"))
# dev.copy2pdf(file = "Rothman_conf.pdf")
# dev.copy2eps(file = "Rothman_conf.eps")

# risk differences at corners of confounding rectangle
cat("\n", "Standardized risk difference bounds (two age groups):\n",
    sep = "")
print(stratrisk2["65+", "smokers"] - stratrisk2["65+", "nonsmokers"])
print(stratrisk2["18-64", "smokers"] - stratrisk2["18-64", "nonsmokers"])
# confounded
cat("\n", "Confounded risk difference bounds (two age groups):\n",
    sep = "")
print(stratrisk2["18-64", "smokers"] - stratrisk2["65+", "nonsmokers"])
print(stratrisk2["65+", "smokers"] - stratrisk2["18-64", "nonsmokers"])


# Rothman diagram with standarized hull and confounding rectangle (Figure 3)
strat7 <- twobytwo("age")
stratrisk7 <- condrisk("age")
cat("\n", "2x2 tables stratified by age groups (Figure 3):\n", sep = "")
print(strat7)

# standardized risks of death over all age groups
age_n <- sapply(strat7, function(x) x[3, 3])
age_p <- age_n / sum(age_n)
stdrisk7_all <- data.frame(smokers = sum(stratrisk7$smokers * age_p),
                           nonsmokers = sum(stratrisk7$nonsmokers * age_p))
# smokers
age_nsmk <- sapply(strat7, function(x) x[1, 3])
age_psmk <- age_nsmk / sum(age_nsmk)
stdrisk7_smk <- data.frame(smokers = sum(stratrisk7$smokers * age_psmk),
                           nonsmokers = sum(stratrisk7$nonsmokers * age_psmk))
# nonsmokers
age_nnon <- sapply(strat7, function(x) x[2, 3])
age_pnon <- age_nnon / sum(age_nnon)
stdrisk7_non <- data.frame(smokers = sum(stratrisk7$smokers * age_pnon),
                           nonsmokers = sum(stratrisk7$nonsmokers * age_pnon))

x7min <- min(stratrisk7$nonsmokers)
x7max <- max(stratrisk7$nonsmokers)
y7min <- min(stratrisk7$smokers)
y7max <- max(stratrisk7$smokers)
rothdiagram7 <- function() {
  plot(stratrisk7$nonsmokers, stratrisk7$smokers, pch = 19,
       xlim = c(0, 1), ylim = c(0, 1), asp = 1,
       xlab = xlabel, ylab = ylabel)
  segments(0, 0, 1, 1, col = "darkgray")
  convhull <- chull(stratrisk7$nonsmokers, stratrisk7$smokers)
  with(stratrisk7, polygon(nonsmokers[convhull], smokers[convhull],
                           lty = "solid"))
  polygon(c(x7min, x7min, x7max, x7max), c(y7min, y7max, y7max, y7min),
          lty = "dashed")
  points(margrisk$nonsmokers, margrisk$smokers)
  points(stdrisk7_all$nonsmokers, stdrisk7_all$smokers)
  points(stdrisk7_smk$nonsmokers, stdrisk7_smk$smokers)
  points(stdrisk7_non$nonsmokers, stdrisk7_non$smokers)
  grid()
  text((x7min + x7max) / 2, y7max - 0.03,
       labels = "Confounding rectangle")
  text(0.47, 0.51, srt = 45, label = "Standardized hull")
  # labels for the 18-24 and 55-64 year age groups
  toplabels <- c(1, 5)
  text(stratrisk7$nonsmokers[toplabels] - 0.01,
       stratrisk7$smokers[toplabels] + 0.02,
       adj = c(1, 0), labels = rownames(stratrisk7)[toplabels])
  # labels for the 25-34 and 75+ year age groups
  bottomlabels <- c(2, 7)
  text(stratrisk7$nonsmokers[bottomlabels] + 0.01,
       stratrisk7$smokers[bottomlabels] - 0.02,
       adj = c(0, 1), labels = rownames(stratrisk7)[bottomlabels])
  # label for the 35-44 year age group
  boxed.labels(stratrisk7$nonsmokers[3] - 0.05, stratrisk7$smokers[3] + 0.03,
               labels = rownames(stratrisk7)[3], xpad = 1, border = FALSE)
  # label for the 45-54 year age group
  boxed.labels(stratrisk7$nonsmokers[4] + 0.05, stratrisk7$smokers[4] - 0.03,
               labels = rownames(stratrisk7)[4], xpad = 1, border = FALSE)
  # label for the 65-74 year age group
  boxed.labels(stratrisk7$nonsmokers[6] + 0.06, stratrisk7$smokers[6] - 0.03,
               labels = rownames(stratrisk7)[6], xpad = 1, border = FALSE)
  # crude point label
  text(margrisk$nonsmokers + 0.01, margrisk$smokers - 0.02, adj = c(0, 1),
       labels = "Crude point")
  # labels for standardized points
  boxed.labels(stdrisk7_all$nonsmokers - 0.11, stdrisk7_all$smokers + 0.03,
               labels = "All participants", xpad = 1, border = FALSE)
  boxed.labels(stdrisk7_smk$nonsmokers - 0.07, stdrisk7_smk$smokers + 0.03,
               labels = "Smokers", xpad = 1, border = FALSE)
  boxed.labels(stdrisk7_non$nonsmokers - 0.09, stdrisk7_non$smokers + 0.03,
               labels = "Nonsmokers", xpad = 1, border = FALSE)
}
dev.new()
rothdiagram7()
# dev.copy2pdf(file = "Rothman_conf7.pdf")
# dev.copy2eps(file = "Rothman_conf7.eps")

# risk differences at corners of confounding rectangle
cat("\n", "Standardized risk difference bounds (seven age groups):\n",
    sep = "")
print(stratrisk7["25-34", "smokers"] - stratrisk7["25-34", "nonsmokers"])
print(stratrisk7["55-64", "smokers"] - stratrisk7["55-64", "nonsmokers"])
# confounded
cat("\n", "Confounded risk difference bounds (seven age groups):\n",
    sep = "")
print(stratrisk7["25-34", "smokers"] - stratrisk7["75+", "nonsmokers"])
print(stratrisk7["75+", "smokers"] - stratrisk7["18-24", "nonsmokers"])
