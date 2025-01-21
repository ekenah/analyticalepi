## binomial distribution

# binomial PMF
# The second and third arguments are n ("size") and p ("prob").
dbinom(2, 10, 0.4)
dbinom(0:10, 10, 0.4)
sum(dbinom(0:10, 10, 0.4))

# binomial CDF
pbinom(0:10, 10, 0.4)
cumsum(dbinom(0:10, 10, 0.4))

# binomial quantiles
qbinom(c(0.25, 0.5, 0.75, 1), 10, 0.4)

# random samples
rbinom(20, 10, 0.4)
x <- rbinom(1000, 10, 0.4)
mean(x)
var(x)
