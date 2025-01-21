## Point and period prevalence

# generate onset and recovery data for 100 individuals
# Setting the seed ensures that everyone gets the same random numbers,
# but it is strictly optional.
# The function rexp() randomly samples from an exponential distribution.
set.seed(42)
cohort <- data.frame(onset = rexp(100, rate = 0.4))
cohort$duration <- rexp(100, rate = 2)
cohort$recovery <- cohort$onset + cohort$duration

# statistical summaries (mean, quartiles, range)
summary(cohort$onset)
summary(cohort$duration)
summary(cohort$recovery)

# highest and lowest recovery times
# The function sort() sorts the vector from lowest to highest.
# head() returns the first 6 values of a vector; tails() returns the last 6.
min(cohort$onset)
head(sort(cohort$onset))    # lowest 6 values (first 6 in the sorted vector)
tail(sort(cohort$onset))    # highest 6 values (last 6 in the sorted vector)
max(cohort$onset)

# With a long vector, sorting repeatedly can be slow.
# You can also control the number of elements returned by head() or tail().
onset_ordered <- sort(cohort$onset)
head(onset_ordered, n = 10)
tail(onset_ordered, n = 10)

# seeing rows and columns of the data frame
cohort[1:10, c("onset", "duration", "recovery")]
cohort[c(10, 20, 50), c("onset", "recovery")]
cohort[which(cohort$recovery < 1), c("onset", "recovery")]
cohort[, c("onset", "recovery")]    # all rows
cohort[c(2, 3, 5, 7, 11), ]         # all columns

# point prevalence
prev <- function(t) {
  # vector of TRUE/FALSE for prevalent cases at time t
  prevalent <- cohort$onset <= t & cohort$recovery > t
  mean(prevalent)
}

prev(0)
prev(1)
prev(2)
prev(6)

# period prevalence
# The parentheses around the logical tests are just for readability.
pdprev <- function(ta, tb) {
  # prevalent cases at t_a
  prevalent_ta <- (cohort$onset <= ta & cohort$recovery > ta)
  # incident cases in (t_a, t_b]
  incident_ab <- (cohort$onset > ta & cohort$onset <= tb)
  # mean indicator for prevalent at t_a or incident in (t_a, t_b]
  mean(prevalent_ta | incident_ab)
}

pdprev(0, 1)
pdprev(1, 2)
pdprev(0, 6)

# save the data as a CSV file
write.csv(cohort, "cohort.csv", row.names = FALSE)
