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

# standardized risks (standard population half female and half male)
pf <- 0.5
pm <- 1 - pf
p1std <- pf * p1f + pm * p1m
p0std <- pf * p0f + pm * p0m

# standardized measures of association
RDstd <- p1std - p0std
RRstd <- p1std / p0std
ORstd <- odds(p1std) / odds(p0std)
CHRstd <- chaz(p1std) / chaz(p0std)
