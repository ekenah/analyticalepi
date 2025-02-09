## Log-rank test

# The log-rank test is done using survdiff() is in the survival package.
# The rats data is from:
# Mantel, Bohidar, and Ciminera (1977). Cancer Research 37: 3863-3868.
library(survival)               # rats data and survdiff
library(lmtest)                 # coeftest() for Wald test of coefficients

# log-rank test (rho = 0 by default)
# More tumor incidence observed than expected under the null in treated rats.
survdiff(Surv(time, status) ~ rx, data = rats)

# Peto-Prentice test (rho = 1)
# More tumor incidence observed than expected under the null in treated rats.
survdiff(Surv(time, status) ~ rx, data = rats, rho = 1)

# comparison with parametric AFT models
# exponential
aft_exp <- survreg(Surv(time, status) ~ rx, data = rats, dist = "exponential")
coeftest(aft_exp)["rx", "Pr(>|z|)"]
# Weibull
aft_weib <- survreg(Surv(time, status) ~ rx, data = rats, dist = "weibull")
coeftest(aft_weib)["rx", "Pr(>|z|)"]
# log-logistic
aft_llog <- survreg(Surv(time, status) ~ rx, data = rats, dist = "loglogistic")
coeftest(aft_llog)["rx", "Pr(>|z|)"]
