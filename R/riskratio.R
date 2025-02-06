## Risk ratio point and interval estimates

# rats data is in the survival package
# Mantel, Bohidar, and Ciminera (1977). Cancer Research 37: 3863-3868.
library(survival)
?rats               # type q to exit
dim(rats)
names(rats)

# numbers of events and row sums
# Because follow-up of the rats was complete, risk calculations are valid.
a <- sum(rats$status[rats$rx == 1])
r1 <- sum(rats$rx == 1)
c <- with(subset(rats, rx == 0), sum(status))
r0 <- sum(rats$rx == 0)

# estimated risks of tumor onset in 104 weeks
p1hat <- a / r1
p0hat <- c / r0

# estimated risk ratio, log risk ratio, and variance of ln(RRhat)
RRhat <- p1hat / p0hat
lnRRhat <- log(RRhat)
lnRRvar <- 1 / a - 1 / r1 + 1 / c - 1 / r0

# Wald hypothesis test that ln(RR) = 0 in normal and chi-squared versions
lnRRz <- lnRRhat / sqrt(lnRRvar)
lnRRz
2 * pnorm(-abs(lnRRz))
lnRRchisq <- lnRRz^2
lnRRchisq
1 - pchisq(lnRRchisq, df = 1)

# point estimate and 95% confidence interval for log risk ratio
lnRRci <- lnRRhat + c(-1, 1) * qnorm(0.975) * sqrt(lnRRvar)
lnRRhat
lnRRci

# point estimate and 95% confidence interval for risk ratio
RRhat
exp(lnRRci)
