## Binomial GLM analysis of smoking and 20-year mortality with interactions

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
intxest <- function(model, model0) {
  # model has interaction term, model0 has no interaction term
  meas <- cumsum(coef(model)[c("smoking", "smoking:binage65+")])
  pval <- lrtest(model, model0)[2, "Pr(>Chisq)"]
  meas0 <- coef(model0)["smoking"]
  ci0 <- confint(model0)["smoking", ]
  if (model$family$link %in% c("log", "logit", "cloglog")) {
    meas <- exp(meas)
    meas0 <- exp(meas0)
    ci0 <- exp(ci0)
  }
  c("18-64" = meas[1], "65+" = meas[2], "pval" = pval,
    "unstrat" = meas0, ci0[1], ci0[2])
}

# stratum-specific and combined measures of association
intx_meas <- mapply(intxest,
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
colnames(intx_tbl)[1:4] <- c("18-64", "65+", "pval", "common")
