## Rothman diagram for smoking and 20-year mortality

# load R packages
library(dplyr)
library(plotrix)

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
strat7 <- twobytwo("age")

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
stratrisk7 <- condrisk("age")

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

# Rothman diagram with standarized hull and confounding rectangle
x7min <- min(stratrisk7$nonsmokers)
x7max <- max(stratrisk7$nonsmokers)
y7min <- min(stratrisk7$smokers)
y7max <- max(stratrisk7$smokers)
plot(stratrisk7$nonsmokers, stratrisk7$smokers, pch = 19,
     xlim = c(0, 1), ylim = c(0, 1), asp = 1,
     xlab = "Risk of death among the unexposed (nonsmokers)",
     ylab = "Risk of death among the exposed (smokers)")
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
