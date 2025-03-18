# DAG for cohort study with confounding and selection bias

# load R packages
library(dagitty)
library(ggdag, warn.conflicts = FALSE)
library(ggplot2)

# define the DAG
cohort <- dagitty('dag{
  C -> D
  C -> S
  C -> X
  X -> D
  X -> S
}')

# generate data for plotting DAG using ggplot()
# Use geom_dag_label() for adjusted variables to get a box around them.
coordinates(cohort) <- list(x = c(C = 0, X = 1, S = 1.5, D = 2),
                            y = c(C = 0, X = 0, S = -0.3, D = 0))
cohort_dat <- control_for(cohort, "S", activate_colliders = FALSE)
cohort_adj <- filter(cohort_dat, adjusted == "adjusted")
cohort_unadj <- filter(cohort_dat, adjusted == "unadjusted")

# plot DAG with ggplot() to allow for boxes and curved arrows
(ggplot(cohort_dat, aes(x = x, y = y, xend = xend, yend = yend))
  + theme_dag_gray_grid()
  + coord_equal()
  + ylim(-0.4, 0.3)
  + geom_dag_label(data = cohort_adj,
                   parse = TRUE, size = 8, fill = NA,
                   label.padding = unit(0.15, "lines"))
  + geom_dag_text(data = cohort_unadj, parse = TRUE,
                  color = "black", size = 8)
  + geom_dag_edges_arc(curvature = c(0.2, -0.13, 0, 0, 0))
)
