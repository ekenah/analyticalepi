## Incidence rate ratio point and interval estimates

# rats data is in the survival package
# Mantel, Bohidar, and Ciminera (1977). Cancer Research 37: 3863-3868.
library(survival)
?rats               # type q to exit
dim(rats)
names(rats)

# numbers of events and total rat-weeks
# The subset() function can be used as an alternative to vector indexing.
a <- sum(rats$status[rats$rx == 1])
T1 <- sum(rats$time[rats$rx == 1])
c <- with(subset(rats, rx == 0), sum(status))
T0 <- with(subset(rats, rx == 0), sum(time))

# estimated incidence rates
# The units are tumor onsets per rat-week.
ir1hat <- a / T1
ir0hat <- c / T0

# estimated incidence rate ratio, log IRR, and variance of ln(IRRhat)
IRRhat <- ir1hat / ir0hat
lnIRRhat <- log(IRRhat)
lnIRRvar <- 1 / a + 1 / c

# Wald hypothesis test that ln(IRR) = 0 in normal and chi-squared versions
lnIRRz <- lnIRRhat / sqrt(lnIRRvar)
lnIRRz
2 * pnorm(-abs(lnIRRz))    # two-tailed test
lnIRRchisq <- lnIRRz^2
lnIRRchisq
1 - pchisq(lnIRRchisq, df = 1)

# 95% confidence interval for log incidence rate ratio
lnIRRci <- lnIRRhat + c(-1, 1) * qnorm(0.975) * sqrt(lnIRRvar)
lnIRRhat
lnIRRci

# point estimate and 95% confidence interval for the incidence rate ratio
IRRhat
exp(lnIRRci)
