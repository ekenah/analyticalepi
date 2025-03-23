## Binomial GLM analysis of smoking and 20-year mortality

# load R packages
library(dplyr)      # for case_when()
library(lmtest)     # for lrtest()

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

# crude 2x2 table and hypothesis tests
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

# crude 2x2 table and hypothesis tests
crude22 <- twobytwo()[[1]]
crude22_nomarg <- crude22[-3, -3]             # remove margins
chisq.test(crude22_nomarg, correct = FALSE)   # Pearson chi-squared test
fisher.test(crude22_nomarg)                   # Fisher's exact test

# crude measures of association
mest <- function(model, pval = TRUE, ...) {
  # extract point and interval estimates from GLM
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
crude_chrbinom <- glm(death ~ smoking, data = AFVdat,
                      family = binomial(link = "cloglog"))
crude_meas <- do.call(rbind, lapply(list(oddsratio = crude_logistic,
                                         riskratio = crude_logbinom,
                                         riskdiff = crude_idbinom,
                                         chazratio = crude_chrbinom),
                                    mest))

## 2x2 tables stratified by age
strat2 <- twobytwo("binage")
