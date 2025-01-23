## Nelson-Aalen estimator
# The Nelson-Aalen estimator is calculated using survival::survfit().
# Use the argument "stype = 2" to get the survival function estimated from the
# Nelson-Aalen estimate of the cumulative hazard.
# Use the argument "ctype = 2" to get the Fleming-Harrington correction.

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

# Nelson-Aalen estimator with log-transformed confidence intervals
# The log transformation of H is the log-log transformation of S, so we use
# the argument conf.type = "log-log".
llog_na <- survfit(Surv(time, delta) ~ 1, data = llogdat,
                   conf.type = "log-log", stype = 2, ctype = 2)

# point and interval estimates of the survival function
summary(llog_na, times = 1:15)

# calculate point and interval estimates of the cumulative hazard function
names(summary(llog_na, times = 1:15))
summary(llog_na, times = 1:15)$cumhaz
-log(summary(llog_na, times = 1:15)$surv)
-log(summary(llog_na, times = 1:15)$lower)
-log(summary(llog_na, times = 1:15)$upper)
