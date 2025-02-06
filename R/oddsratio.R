## Odds ratio point and interval estimates

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

# estimated odds ratio, log odds ratio, and variance of ln(ORhat)
b <- r1 - a
d <- r0 - c
odds <- function(p) p / (1 - p)
ORhat <- odds(p1hat) / odds(p0hat)
lnORhat <- log(ORhat)
lnORvar <- 1 / a + 1 / b + 1 / c + 1 / d

# Wald hypothesis test that ln(OR) = 0 in normal and chi-squared versions
lnORz <- lnORhat / sqrt(lnORvar)
lnORz
2 * pnorm(-abs(lnORz))    # two-tailed test
lnORchisq <- lnORz^2
lnORchisq
1 - pchisq(lnORchisq, df = 1)

# 95% confidence interval for log odds ratio
lnORci <- lnORhat + c(-1, 1) * qnorm(0.975) * sqrt(lnORvar)
lnORhat
lnORci

# point estimate and 95% confidence interval for the odds ratio
ORhat
exp(lnORci)
