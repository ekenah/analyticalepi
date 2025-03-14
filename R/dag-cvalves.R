## Open and closed paths on a DAG

# load R packages
library(dagitty)
library(ggdag)
library(ggplot2)

# define DAG
valves <- dagitty('dag{
  A -> C
  B -> C -> D
}')

# generate data for plotting DAG using ggplot()
# Use geom_dag_label() for adjusted variables to get a box around them.
coordinates(valves) <- list(x = c(A = 0, B = 1, C = 2, D = 3),
                            y = c(A = 0, B = 0, C = 0, D = 0))
cvalves_dat <- control_for(valves, "C", activate_colliders = FALSE)
cvalves_adj <- filter(cvalves_dat, adjusted == "adjusted")
cvalves_unadj <- filter(cvalves_dat, adjusted == "unadjusted")

# plot DAG with ggplot() to allow for boxes and curved arrows
# Uncomment coord_equal() to get a better-looking DAG in an R plot window.
(ggplot(cvalves_dat, aes(x = x, y = y, xend = xend, yend = yend))
  + theme_dag_gray_grid()
  # + coord_equal()
  + ylim(-0.1, 0.6)
  + geom_dag_label(data = cvalves_adj, parse = TRUE, size = 8, fill = NA,
                   label.padding = unit(0.15, "lines"))
  + geom_dag_text(data = cvalves_unadj, parse = TRUE,
                  color = "black", size = 8)
  + geom_dag_edges_arc(curvature = c(0.4, 0, 0))
)
