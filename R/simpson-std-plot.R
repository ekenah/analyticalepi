# Standardized measures of association in Simpson's paradox (JRSSB, 1951)

# data frame
simpson <-  data.frame(male = rep(c(0, 1), each = 4),
                       treated = rep(c(0, 1, 0, 1), each = 2),
                       dead = rep(c(0, 1), times = 4),
                       count = c(2, 3, 12, 15, 4, 3, 8, 5))

# function to calculate risk
risk <- function(rows) {
  # rows is a vector of TRUE/FALSE that selects rows
  count <- simpson$count[rows]
  dead <- simpson$dead[rows]
  sum(count * dead) / sum(count)
}

# odds and cumulative hazard
odds <- function(p) p / (1 - p)
chaz <- function(p) -log(1 - p)

# conditional risks among females
p1f <- risk(with(simpson, treated == 1 & male == 0))
p0f <- risk(with(simpson, treated == 0 & male == 0))

# conditional risks among males
p1m <- risk(with(simpson, treated == 1 & male == 1))
p0m <- risk(with(simpson, treated == 0 & male == 1))

# standardized risks as a function of proportion female
# Both functions are designed to work if propf is a vector.
p1std <- function(propf) {
  propm <- 1 - propf
  propf * p1f + propm * p1m
}
p0std <- function(propf) {
  propm <- 1 - propf
  propf * p0f + propm * p0m
}

# plot of standardized risk ratio, odds ratio, and cumulative hazard ratio
propf <- seq(0, 1, by = 0.01)
plot(propf, p1std(propf) / p0std(propf), type = "l", ylim = c(0.8, 1),
     xlab = "Proportion female in the standard population",
     ylab = "Standardized measure of association")
lines(propf, odds(p1std(propf)) / odds(p0std(propf)), lty = "dashed")
lines(propf, chaz(p1std(propf)) / chaz(p0std(propf)), lty = "dotted")
grid()
legend("topleft", bg = "white", lty = c("solid", "dotted", "dashed"),
       legend = c("Risk ratio", "Cumulative hazard ratio", "Odds ratio"))
