## Rothman diagram for smoking and 20-year mortality

# load R packages
library(dplyr)   # for case_when()
library(lmtest)  # for lrtest()

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
strat2 <- twobytwo("binage")

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


# Rothman diagram with standardized segment
xlabel <- "Risk of death among the unexposed (nonsmokers)"
ylabel <- "Risk of death among the exposed (smokers)"
rothdiagram2 <- function(labels = c("18-64 (0%)", "65+ (100%)")) {
  # function to plot Rothman diagram with crude and stratum-specific points
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
