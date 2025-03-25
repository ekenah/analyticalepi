## Stratified Cox model

# load R packages
library(survival)

# unstratified Cox model and Nelson-Aalen estimator
unstrat <- coxph(Surv(time, status) ~ rx + sex, data = rats)
summary(unstrat)
unstrat_na <- survfit(unstrat, stype =  2, ctype = 2, conf.type = "log-log",
                      newdata = data.frame(rx = 0, sex = c("f", "m")))

# stratified Cox model and Nelson-Aalen estimator
strat <- coxph(Surv(time, status) ~ rx + strata(sex), data = rats)
summary(strat)
strat_na <- survfit(strat, stype = 2, ctype = 2, conf.type = "log-log",
                    newdata = data.frame(rx = 0))

# plot of stratified and unstratified cumulative hazard estimates
plot(unstrat_na, fun = "cumhaz", lty = "dashed",
     xlab = "Weeks since treatment", ylab = "Cumulative hazard")
grid()
lines(strat_na, fun = "cumhaz", conf.int = FALSE)
legend("topleft", bg = "white", lty = c("solid", "dashed"),
       legend = c("Stratified model", "Unstratified model"))

# unstratified and stratified log-rank tests
survdiff(Surv(time, status) ~ rx + sex, data = rats)
survdiff(Surv(time, status) ~ rx + strata(sex), data = rats)
