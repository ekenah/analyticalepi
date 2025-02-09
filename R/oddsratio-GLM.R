## Odds ratio estimation with GLMs

# to get help on GLMs in R (type q to exit)
?glm
?family

# rats data is in the survival package
# Mantel, Bohidar, and Ciminera (1977). Cancer Research 37: 3863-3868.
library(survival)     # rats data
library(sandwich)     # sandwich() for robust variance
library(lmtest)       # coefci() for Wald confidence intervals

# traditional OR point estimate and Wald 95% confidence interval
# Because follow-up of the rats was complete, risk calculations are valid.
r1 <- sum(rats$rx == 1)
a <- with(rats, sum(status[rx == 1]))
b <- r1 - a
r0 <- sum(rats$rx == 0)
c <- with(rats, sum(status[rx == 0]))
d <- r0 - c
odds1 <- a / b
odds0 <- c / d
ORhat <- odds1 / odds0
lnORvar <- 1 / a + 1 / b + 1 / c + 1 / d
lnORci <- log(ORhat) + c(-1, 1) * qnorm(.975) * sqrt(lnORvar)

# point and interval estimates of the OR
ORhat
exp(lnORci)

# binomial GLM with logit link for the OR
# Default binomial link is "logit", so it does not need to be specified.
ORglm <- glm(status ~ rx, family =  binomial(), data = rats)
summary(ORglm)                  # point estimates, p-values, CIs, global tests
names(ORglm)                    # parts of the fitted model
exp(coef(ORglm))                # estimated odds (x = 0) and OR
exp(confint(ORglm))             # likelihood ratio CIs (better)
exp(coefci(ORglm))              # log-transformed Wald CI matches above

# point and interval estimates for risk in the exposed
# The vector c(1, 1) represents 1 * beta0 + 1 * beta1,
# and as.numeric() is used to return a number instead of a matrix.
odds1hat <- exp(sum(coef(ORglm)))
lnodds1var <- as.numeric(c(1, 1) %*% vcov(ORglm) %*% c(1, 1))
odds1hat
odds1hat * exp(c(-1, 1) * qnorm(0.975) * sqrt(lnodds1var))

# estimates of risks, risk difference, and risk ratio based on odds
expit <- function(v) 1 / (1 + exp(-v))    # logistic function
beta0 <- coef(ORglm)["(Intercept)"]
beta1 <- coef(ORglm)["rx"]
# risk in exposed
risk1 <- a / r1
risk1
expit(beta0 + beta1)
# risk in unexposed
risk0 <- c / r0
risk0
expit(beta0)
# risk difference
RDhat <- risk1 - risk0
RDhat
expit(beta0 + beta1) - expit(beta0)
# risk ratio
RRhat <- risk1 / risk0
RRhat
expit(beta0 + beta1) / expit(beta0)
