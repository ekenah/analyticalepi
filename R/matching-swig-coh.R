# DAG for cohort study matched on a confounder

# load R packages
library(dagitty)
library(ggdag, warn.conflicts = FALSE)
library(ggplot2)

# define the DAG
cohort <- dagitty('dag{
  C -> D
  C -> S
  C -> X
  x -> D
  x -> S
}')

# generate data for plotting DAG using ggplot()
# Use geom_dag_label() for adjusted variables to get a box around them.
coordinates(cohort) <- list(x = c(C = 0, X = 0.9, x = 1.1, S = 2, D = 3),
                            y = c(C = 0, X = 0, x = 0, S = -0.5, D = 0))
cohort_dat <- control_for(cohort, c("C", "x"), activate_colliders = FALSE)
label(cohort_dat) <- c("C" = "C", "X" = "X", "x" = "x",
                       "D" = "D^x", "S" = "S^x")
cohort_adj <- filter(cohort_dat, adjusted == "adjusted")
cohort_unadj <- filter(cohort_dat, adjusted == "unadjusted")

# plot DAG with ggplot() to allow for boxes and curved arrows
(ggplot(cohort_dat, aes(x = x, y = y, xend = xend, yend = yend))
  + theme_dag_gray_grid()
  + coord_equal()
  + geom_dag_label(data = cohort_adj, parse = TRUE, size = 8, fill = NA,
                   label.padding = unit(0.15, "lines"))
  + geom_dag_text(aes(label = label), data = cohort_unadj, parse = TRUE,
                  color = "black", size = 8)
  + geom_dag_edges_arc(curvature = c(0.25, -0.15, 0, 0, 0))
  + annotate("text", x = 1, y = 0.01, size = 8, label = "|")
)
