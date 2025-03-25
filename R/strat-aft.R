## Stratified accelerated failure time (AFT) models

# load R packages
library(survival)

# unstratified log-logistic AFT model
rat_unstrat <- survreg(Surv(time, status) ~ rx + sex,
                       data = rats, dist = "loglogistic")

# stratified log-logistic AFT model for rats data
# The strata() command only allows for different shape parameters.
# To get a truly stratified model, you need to include sex as a predictor.
rat_strat <- survreg(Surv(time, status) ~ rx + sex + strata(sex),
                     data = rats, dist = "loglogistic")
summary(rat_strat)
exp(-coef(rat_strat))
exp(-confint(rat_strat))

# baseline rates for female rats and male rats
lambda_f <- exp(-coef(rat_strat)["(Intercept)"])
lambda_m <- lambda_f * exp(-coef(rat_strat)["sexm"])

# estimated logistic rate ratio (same for males and females)
rr <- exp(-coef(rat_strat)["rx"])
exp(-confint(rat_strat)["rx", ])

# estimated shape parameters
1 / rat_strat$scale
shape_f <- 1 / rat_strat$scale["f"]
shape_m <- 1 / rat_strat$scale["m"]

# estimated odds ratios (constant in a log-logistic AFT model)
or_f <- rr^shape_f
or_m <- rr^shape_m


# residual confounding example
# simulate censored Weibull with different shapes but constant rate ratio
n <- 10000
fail0f <- rweibull(3 * n, shape = .5, scale = 1)
fail1f <- rweibull(n, shape = .5, scale = 1 / 2)
fail0m <- rweibull(n, shape = 2, scale = 1)
fail1m <- rweibull(3 * n, shape = 2, scale = 1 / 2)

fail <- c(fail0f, fail1f, fail0m, fail1m)
cens <- 2
time <- pmin(fail, cens)
event <- ifelse(cens < fail, 0, 1)
x <- rep(c(0, 0, 0, 1, 0, 1, 1, 1), each = n)
male <- rep(c(0, 1), each = 4 * n)
wdat <- data.frame(time = time, event = event, x = x, male = male)

# Weibull AFT model that does not include sex
crude <- survreg(Surv(time, event) ~ x, data = wdat)
exp(-coef(crude)["x"])
exp(-confint(crude)["x", ])

# unstratified Weibull AFT model that includes sex as a covariate
unstrat <- survreg(Surv(time, event) ~ x + male, data = wdat)
exp(-coef(unstrat)["x"])
exp(-confint(unstrat)["x", ])

# stratified Weibull AFT model that stratified by sex
strat <- survreg(Surv(time, event) ~ x + male + strata(male),
                 data = wdat)
exp(-coef(strat)["x"])
exp(-confint(strat)["x", ])

# Weibull AFT model that allows different shape parameters only
strat2 <- survreg(Surv(time, event) ~ x + strata(male),
                 data = wdat)

# stratified Cox model (stratified Weibull AFT model is special case)
cox <- coxph(Surv(time, event) ~ x + x:male + strata(male), data = wdat)