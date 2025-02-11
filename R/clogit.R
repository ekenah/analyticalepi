## Nested case-control study and conditional logistic regression

# The rats data is from:
# Mantel, Bohidar, and Ciminera (1977). Cancer Research 37: 3863-3868.
library(survival)               # rats data and clogit() function

# Cox model with rat cohort data
cohort <- coxph(Surv(time, status) ~ rx, data = rats)
exp(coef(cohort))
exp(confint(cohort))

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

# generate case-control data
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

# estimate hazard ratio with conditional logistic regression
# We are recruiting k = 4 controls per case.
cases <- rat_id[rats$status == 1]   # rats with incidence tumors
ccdat <- do.call(rbind, lapply(cases, cc_set, k = 4))
nested_cc <- clogit(status ~ rx + strata(case), data = ccdat)
exp(coef(nested_cc))
exp(confint(nested_cc))

# The ratio of standard errors is close to sqrt(1 + 1 / 4).
sqrt(vcov(nested_cc) / vcov(cohort))
sqrt(1 + 1 / 4)
