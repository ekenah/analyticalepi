# Crude measures of association in Simpson's paradox (JRSSB, 1951)

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

# crude risks and measures of association
p1 <- risk(simpson$treated == 1)
p0 <- risk(simpson$treated == 0)
RD <- p1 - p0
RR <- p1 / p0
OR <- odds(p1) / odds(p0)
CHR <- chaz(p1) / chaz(p0)
