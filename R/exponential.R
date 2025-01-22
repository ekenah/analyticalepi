## Exponential rate parameter estimation

# generate right-censored exponential distribution
tevent <- rexp(1000, rate = 2)
tcens <- rexp(1000)             # default rate = 1
sdat <- data.frame(texit = pmin(tcens, tevent),
                   event = ifelse(tcens < tevent, 0, 1))

# calculating incidence rate and log-transformed confidence interval
m <- sum(sdat$event)
T <- sum(sdat$texit)
m / T
m / T * exp(c(-1, 1) * qnorm(.975) * sqrt(1 / m))

# fitting intercept-only exponential regression model
# This uses the survreg() function from the survival package.
library(survival)
expfit <- survreg(Surv(texit, event) ~ 1, data = sdat,
                  dist = "exponential")
summary(expfit)
coef(expfit)
confint(expfit)

# log-transformed Wald CI for the exponential rate
# The intercept is ln(scale), which is -ln(rate).
# The rate is exp(-intercept).
exp(-coef(expfit))
exp(-confint(expfit))

# add delayed entry (left truncation) to sdat
sdat2 <- sdat
sdat2$tentry <- rexp(1000, rate = 5)
sdat2 <- subset(sdat2, tentry < texit)

# incidence rate and log-transformed confidence interval
m2 <- sum(sdat2$event)
T2 <- sum(sdat2$texit - sdat2$tentry)
m2 / T2
m2 / T2 * exp(c(-1, 1) * qnorm(.975) * sqrt(1 / m2))

# survreg() does not handle delayed entry, so use flexsurv::flexsurvreg()
library(flexsurv)
expfit2 <- flexsurvreg(Surv(tentry, texit, event) ~ 1, data = sdat2,
                       dist = "exp")
# The summary() function does not work with flexsurvreg objects.
# Type "expfit2" or "expfit2$res" to get point and interval estimates.
# The "se" in expfit2$res is the delta method standard error.
expfit2
expfit2$res       # rate parameter scale
expfit2$res.t     # log rate parameter scale
