## DAG for Simpson's paradox (Simpson, JRSSB, 1951)

# load R packages
library(dagitty)
library(ggdag, warn.conflicts = FALSE)
library(ggplot2)
library(ggraph)                           # for label_rect()

# define the DAG
simpson <- dagitty('dag{
  "Sex" -> "Death"
  "Sex" -> "Selection"
  "Sex" -> "Treatment"
  "Treatment" -> "Death"
}')

# generate tidy DAG data
coordinates(simpson) <- list(
  x = c("Sex" = 0, "Selection" = 0.5, "Treatment" = 1, "Death" = 2),
  y = c("Sex" = 0, "Selection" = -0.5, "Treatment" = 0, "Death" = 0))
simpson_dat <- tidy_dagitty(simpson)

# plot the DAG using ggplot() to allow curved arrows
# Uncomment "+ coord_equal()" to get a better-looking plot in an R window.
# The start_cap and end_cap arguments keep the arrows from covering the text.
# The size for label_rect should be 3.57 * the text font size.
(ggplot(simpson_dat, aes(x = x, y = y, xend = xend, yend = yend))
  + theme_dag_gray_grid()
  # + coord_equal()
  + geom_text(aes(label = name), size = 8, col = "black")
  + geom_dag_edges_arc(aes(start_cap = label_rect(name, fontsize = 8 * 3.57),
                           end_cap = label_rect(to, fontsize = 8 * 3.57)),
                       curvature = c(0.5, 0, 0, 0), inherit = TRUE)
)
