## Cumulative hazard ratio estimation with a GLM

# rats data is in the survival package
# Mantel, Bohidar, and Ciminera (1977). Cancer Research 37: 3863-3868.
library(survival)     # rats data
library(lmtest)       # coefci() for Wald confidence intervals

# numbers of events and total rat-weeks
# The subset() function can be used as an alternative to vector indexing.
a <- sum(rats$status[rats$rx == 1])
T1 <- sum(rats$time[rats$rx == 1])
c <- with(subset(rats, rx == 0), sum(status))
T0 <- with(subset(rats, rx == 0), sum(time))
ir1hat <- a / T1
ir0hat <- c / T0
IRRhat <- ir1hat / ir0hat
lnIRRvar <- 1 / a + 1 / c
lnIRRci <- log(IRRhat) + c(-1, 1) * qnorm(0.975) * sqrt(lnIRRvar)

# point and interval estimate of the IRR
IRRhat
exp(lnIRRci)

# binomial GLM with complementary log-log link
# The cumulative hazard ratio equals the hazard ratio if the HR is constant.
HRglm <- glm(status ~ rx, family = binomial(link = "cloglog"), data = rats)
summary(HRglm)                  # point estimates, p-values, CIs, global tests
names(HRglm)                    # parts of the fitted model
exp(coef(HRglm))                # cumulative hazard (x = 0) and HR
exp(confint(HRglm))             # likelihood ratio CIs (better)
exp(coefci(HRglm))              # log-transformed Wald CI for RD matches above

# estimates of risks, RR, and RD based on log cumulative hazard
invcloglog <- function(v) 1 - exp(-exp(v))  # inverse of complementary log-log
beta0 <- coef(HRglm)["(Intercept)"]
beta1 <- coef(HRglm)["rx"]
# risk in exposed
risk1 <- a / sum(rats$rx == 1)
risk1
invcloglog(beta0 + beta1)
# risk in unexposed
risk0 <- c / sum(rats$rx == 0)
risk0
invcloglog(beta0)
# risk difference
risk1 - risk0
invcloglog(beta0 + beta1) - invcloglog(beta0)
# risk ratio
risk1 / risk0
invcloglog(beta0 + beta1) / invcloglog(beta0)
# odds ratio
odds <- function(p) p / (1 - p)
odds(risk1) / odds(risk0)
odds(invcloglog(beta0 + beta1)) / odds(invcloglog(beta0))
