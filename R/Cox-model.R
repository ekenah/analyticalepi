## Cox proportional hazards regression model

# The rats data is from:
# Mantel, Bohidar, and Ciminera (1977). Cancer Research 37: 3863-3868.
library(survival)               # rats data and survreg()

# Cox model with Efron correction for ties (the default)
# With a single binary predictor, the global score test is the log-rank test.
cox_efron <- coxph(Surv(time, status) ~ rx, data = rats)
names(cox_efron)
summary(cox_efron)

# coefficient point estimates, confidence intervals, and covariance matrix
coef(cox_efron)
confint(cox_efron)
vcov(cox_efron)

# hazard ratio and confidence intervals
exp(coef(cox_efron))
exp(confint(cox_efron))

# Breslow correction for ties
table(rats$time[rats$status == 1])  # table of number of tumor onsets
cox_breslow <- coxph(Surv(time, status) ~ rx, data = rats,
                     ties = "breslow")
summary(cox_breslow)
cox_breslow$method

# exact correction for ties
cox_exact <- coxph(Surv(time, status) ~ rx, data = rats,
                   ties = "exact")
summary(cox_exact)
cox_exact$method

# predicted survival in both treatment groups
# Method for estimating survival mirrors method used to correct for ties.
S_pred <- survfit(cox_efron, newdata = data.frame(rx = c(0, 1)),
                   conf.type = "log-log")
S_summary <- summary(S.efron, times = c(20, 40, 60, 80))
S_summary
names(S_summary)
S_summary$newdata     # order of predicted survival probabilities, etc.
S_summary$conf.int    # confidence level
S_summary$lower       # lower 95% confidence limits
S_summary$upper       # upper 95% confidence limits

# predicted cumulative hazard and confidence limits
# The lower bound for H(t) comes from the upper bound for S(t) and vice versa.
S_summary$cumhaz
-log(S_summary$surv)
-log(S_summary$upper)
-log(S_summary$lower)

# plot of predicted cumulative hazards
# Uses data from both groups for each predicted cumulative hazard function.
plot(S_pred, fun = "cumhaz", lty = c("dashed", "solid"),
     xlab = "Weeks since treatment", ylab = "Cumulative hazard")
grid()
legend("topleft", bg = "white", lty = rep(c("solid", "dashed"), each = 2),
       col = rep(c("black", "darkgray"), 2),
       legend = c("Treated rats (Cox model)", "Treated rats (Nelson-Aalen)",
                  "Untreated rats (Cox model)", "Unreated rats (Nelson-Aalen)"))
# add Nelson-Aalen curves
NAest <- survfit(Surv(time, status) ~ rx, data = rats, stype = 2, ctype = 2,
                 conf.type = "log-log")
lines(NAest, fun = "cumhaz", col = "darkgray", lty = c("dashed", "solid"))

# predicted survival in the untreated rats
S1_pred <- survfit(cox_efron, newdata = data.frame(rx = 1),
                   conf.type = "log-log")
summary(S1_pred, times = c(20, 40, 60, 80))

# predicted survival in the treated rats
S0_pred <- survfit(cox_efron, newdata = data.frame(rx = 0),
                   conf.type = "log-log")
summary(S0_pred, times = c(20, 40, 60, 80))

# plot of risk ratio, hazard ratio, and odds ratio over time
# Risks are zero until time[3], which is 3.
# Negative indices remove elements from vectors.
odds <- function(p) p / (1 - p)
t_indices <- -(1:2)
times <- S0_pred$time[t_indices]
R1_pred <- 1 - S1_pred$surv[t_indices]
R0_pred <- 1 - S0_pred$surv[t_indices]
plot(times, R1_pred / R0_pred, type = "l", lty = "dashed",
     ylim = c(1.7, 2.3), xlab = "Weeks since treatment",
     ylab = "Estimated tumor incidence risk ratio")
lines(times, odds(R1_pred) / odds(R0_pred), lty = "dotted")
abline(h = exp(coef(cox_efron)["rx"]))
grid()
legend("bottomleft", lty = c("dotted", "solid", "dashed"), bg = "white",
       legend = c("Odds ratio", "Hazard ratio", "Risk ratio"))
