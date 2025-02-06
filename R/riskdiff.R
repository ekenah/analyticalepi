## Risk difference point and interval estimates

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

# estimated risk difference and its variance
RDhat <- p1hat - p0hat
RDvar <- p1hat * (1 - p1hat) / r1 + p0hat * (1 - p0hat) / r0

# Wald hypothesis test that RD = 0 in normal and chi-squared versions
RDz <- RDhat / sqrt(RDvar)
RDz
2 * pnorm(-abs(RDz))
RDchisq <- RDz^2
RDchisq
1 - pchisq(RDchisq, df = 1)

# point estimate and 95% confidence interval for the risk difference
RDhat
RDhat + c(-1, 1) * qnorm(0.975) * sqrt(RDvar)
