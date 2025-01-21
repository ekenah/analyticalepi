## normal (Gaussian) distribution

# normal PDF
# Second and third arguments are mean and SD (not variance).
# The defaults are mean = 0 and SD = 1.
dnorm(2, 1.2, 5)

# normal CDF (using default mean and variance)
pnorm(1.96)
pnorm(1.96) - pnorm(-1.96)

# normal quantiles
qnorm(0.975)
pnorm(qnorm(0.975))

# random samples (using named arguments)
rnorm(25, mean = 2.3, sd = 3)