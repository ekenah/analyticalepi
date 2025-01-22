## Poisson regression for incidence rates

# generate right-censored exponential distribution
tevent <- rexp(1000, rate = 2)
tcens <- rexp(1000)             # default rate = 1
sdat <- data.frame(texit = pmin(tcens, tevent),
                   event = ifelse(tcens < tevent, 0, 1))

# Poisson regression model
# Use log(time) offset to get incidence rate from Poisson(time * incidence rate)
poisreg <- glm(event ~ offset(log(texit)), data = sdat, family = poisson())
exp(coef(poisreg))
# GLMs use likelihood ratio confidence intervals in R.
exp(confint(poisreg))

# exponential regression for comparison (log-transformed Wald CI)
library(survival)
expreg <- survreg(Surv(texit, event) ~ 1, data = sdat, dist = "exponential")
exp(-coef(expreg))
exp(-confint(expreg))

# add delayed entry to sdat
sdat2 <- sdat
sdat2$tentry <- rexp(1000, rate = 5)
sdat2 <- subset(sdat2, tentry < texit)

# Poisson regression with delayed entry
# The offset is the total person-time contributed by each participant.
poisreg2 <- glm(event ~ offset(log(texit - tentry)), data = sdat2,
                family = poisson())
exp(coef(poisreg2))
exp(confint(poisreg2))

# exponential regression with delayed entry for comparison
library(flexsurv)
expreg2 <- flexsurvreg(Surv(tentry, texit, event) ~ 1, data = sdat2,
                       dist = "exp")
expreg2$res
