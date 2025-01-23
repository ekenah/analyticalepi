## Kaplan-Meier estimator
# The Kaplan-Meier estimator is calculated using survival::survfit().
library(survival)
?survfit          # get general help about survfit
?survfit.formula  # get help with the specific version we use below

# right-censored sample from log-logistic dist with rate = 2 and shape = 1
# Uses samples from logistic distribution with location = -log(rate) and
# scale = 1 / shape.
set.seed(42)
llog_f <- exp(rlogis(500, location = -log(2), scale = 1))
llog_c <- exp(rlogis(500, location = -log(2), scale = 2))
t <- pmin(llog_f, llog_c)
d <- ifelse(llog_c < llog_f, 0, 1)
llogdat <- data.frame(time = t, delta = d)

# Kaplan-Meier estimator with complementary log-log confidence intervals
# The KM estimate is the default for survfit(), but the default confidence
# intervals use a log transformation of the survival function.
llog_km <- survfit(Surv(time, delta) ~ 1, data = llogdat,
                   conf.type = "log-log")

# point and interval estimates of the survival function
summary(llog_km, times = 1:15)
names(llog_km)
names(summary(llog_km, times = 1:15))
