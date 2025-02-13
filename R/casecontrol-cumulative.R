## Cumulative case-control study

# The rats data is from:
# Mantel, Bohidar, and Ciminera (1977). Cancer Research 37: 3863-3868.
library(survival)               # rats data

# estimate rsk ratio from cohort study with log-binomial regression
# Risk calculations are valid because follow-up is complete over 104 weeks.
cohort_RR <- glm(status ~ rx, data = rats, family = binomial(link = "log"))
exp(coef(cohort_RR))               # estimated risk ratio = 2
exp(confint(cohort_RR))            # 95% confidence interval (1.14, 3.51)

# sample controls from rats who did not have incidence tumors
# We will use 4 controls per case and sample without replacement.
ncase <- sum(rats$status)
rat_id <- 1:nrow(rats)
controls <- sample(rat_id[rats$status == 0], 4 * ncase)

# combine case and control data
casedat <- subset(rats, status == 1)
controldat <- rats[controls, ]
ccrats <- rbind(casedat, controldat)

# logistic regression model for exposure in cases and controls
# The exposure odds ratio comparing cases and controls approximates the RR.
# In the case-controls data, status = 1 for cases and status = 0 for controls.
ccx <- glm(rx ~ status, data = ccrats, family = binomial())
exp(coef(ccx))
exp(confint(ccx))

# logistic regression model for case/control status in exposed and unexposed
# By symmetry of the odds ratio, we can switch exposure and outcome.
# The estimated odds ratio is the same, but the intercepts are different.
ccd <- glm(status ~ rx, data = ccrats, family = binomial())
exp(coef(ccd))
exp(confint(ccd))

# case-control / cohort coefficient standard error ratio
# With 4 controls per case, it is approximately sqrt(1 + 1 / 4) = 1.12.
sqrt(vcov(cohort_RR)[2, 2])     # log risk ratio SE in cohort study
sqrt(vcov(ccd)[2, 2])           # log odds ratio SE in case-control study
sqrt(vcov(ccd)[2, 2] / vcov(cohort_RR)[2, 2])
