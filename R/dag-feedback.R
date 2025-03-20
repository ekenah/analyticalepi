# Feedback loop on a DAG

library(dagitty)
library(ggdag)
library(ggplot2)

# define the DAG
loop <- dagitty('dag{
  A1 -> B1 -> A2 -> B2
  A1 -> A2
  B1 -> B2
}')

# generate DAG data for plot
coordinates(loop) <- list(x = c(A1 = 0, B1 = 1, A2 = 2, B2 = 3),
                          y = c(A1 = 0, B1 = 0, A2 = 0, B2 = 0))
loop_dat <- tidy_dagitty(loop)

# plot DAG using ggplot() to allow curved arrows
# Uncomment coord_equal() to get a better-looking DAG in an R plot window.
(ggplot(loop_dat, aes(x = x, y = y, xend = xend, yend = yend))
  + theme_dag_gray_grid()
  # + coord_equal()
  + geom_dag_text(color = "black", size = 8, parse = TRUE,
                  label = c(expression(A[1]), expression(A[2]),
                            expression(B[1]), expression(B[2])))
  + geom_dag_edges_arc(curvature = c(0.5, 0, 0, 0, -0.5))
)
