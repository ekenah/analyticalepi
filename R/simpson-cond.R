# Conditional measures of association in Simpson's paradox (JRSSB, 1951)

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

# conditional risks and measures of association among females
p1f <- risk(with(simpson, treated == 1 & male == 0))
p0f <- risk(with(simpson, treated == 0 & male == 0))
RDf <- p1f - p0f
RRf <- p1f / p0f
ORf <- odds(p1f) / odds(p0f)
CHRf <- chaz(p1f) / chaz(p0f)

# conditional risks and measures of association among females
p1m <- risk(with(simpson, treated == 1 & male == 1))
p0m <- risk(with(simpson, treated == 0 & male == 1))
RDm <- p1m - p0m
RRm <- p1m / p0m
ORm <- odds(p1m) / odds(p0m)
CHRm <- chaz(p1m) / chaz(p0m)
