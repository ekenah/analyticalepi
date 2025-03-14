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

# plot DAG using ggplot() directly to allow for curved arrows
# Uncomment coord_equal() to get a better-looking DAG in an R plot window.
coordinates(valves) <- list(x = c(A = 0, B = 1, C = 2, D = 3),
                            y = c(A = 0, B = 0, C = 0, D = 0))
valves_dat <- tidy_dagitty(valves)
(ggplot(valves_dat, aes(x = x, y = y, xend = xend, yend = yend))
  + theme_dag_gray_grid()
  + coord_equal()
  + ylim(-0.1, 0.6)
  + geom_dag_text(color = "black", size = 8, parse = TRUE)
  + geom_dag_edges_arc(curvature = c(0.4, 0, 0))
)
