## Density case-control study

# The rats data is from:
# Mantel, Bohidar, and Ciminera (1977). Cancer Research 37: 3863-3868.
library(survival)               # rats data and survreg()

# estimate incidence rate ratio from cohort study with exponential AFT model
# The estimated incidence rate ratio is exp(-beta_rx).
cohort_IRR <- survreg(Surv(time, status) ~ rx, data = rats,
                      dist = "exponential")
exp(-coef(cohort_IRR))
exp(-confint(cohort_IRR))

# function to sample controls for each case
control_sample <- function(id, k) {
  # id should be a rat who has an event
  if (rats$status[id] == 0) stop("Rat ", id, " did not have a tumor.")
  t <- rats$time[id]

  # possible controls are riskset minus individuals with events at time t
  rat_id <- 1:nrow(rats)
  controlset <- rat_id[with(rats, (time > t) | (time == t & status == 0))]

  # sample up to k controls
  if (length(controlset) > 1) {
    ksamp <- min(length(controlset), k)
    controls <- sample(controlset, ksamp)
  } else {
    controls <- NULL
  }
  controls
}
control_sample(1, 4)            # no tumor
control_sample(2, 4)            # returns sampled control ids

# generate case-control data for one case
cc_set <- function(id, k) {
  controls <- rats[control_sample(id, k), ]
  controls$status <- 0
  ccdat <- rbind(rats[id, ], controls)

  # use case id to identify case-control set
  ccdat$case <- id

  # return data for case-control set
  ccdat
}
cc_set(1, 4)
cc_set(2, 4)

# generate density case-control data
# We are recruiting k = 4 controls per case in each risk set.
# The same control could be chosen more than once, and
# a control could later become a case.
cases <- rat_id[rats$status == 1]   # rats with incident tumors
ccrats <- do.call(rbind, lapply(cases, cc_set, k = 4))

# estimate the incidence rate ratio with logistic regression
# The exposure odds ratio comparing cases and controls approximates the IRR.
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
sqrt(vcov(cohort_IRR)[2, 2])    # log incidence rate ratio SE in cohort study
sqrt(vcov(ccd)[2, 2])           # log odds ratio SE in case-control study
sqrt(vcov(ccd)[2, 2] / vcov(cohort_IRR)[2, 2])
