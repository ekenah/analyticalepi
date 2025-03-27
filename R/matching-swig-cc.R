# DAG for cohort study matched on a confounder

# load R packages
library(dagitty)
library(ggdag)
library(ggplot2)

# define the DAG
casecontrol <- dagitty('dag{
  C -> D
  C -> S
  C -> X
  d -> S
  X -> D
}')

# generate data for plotting DAG using ggplot()
# Use geom_dag_label() for adjusted variables to get a box around them.
coordinates(casecontrol) <- list(x = c(C = 0, X = 0.9, D = 1.9, d = 2.1, S = 3),
                                 y = c(C = 0, X = 0, D = 0, d = 0, S = -0.4))
cc_dat <- control_for(casecontrol, c("C", "d"), activate_colliders = FALSE)
label(cc_dat) <- c("C" = "C", "X" = "X", "x" = "x", "D" = "D",
                   "d" = "d", "S" = "S^d")
cc_adj <- filter(cc_dat, adjusted == "adjusted")
cc_unadj <- filter(cc_dat, adjusted == "unadjusted")

# plot DAG with ggplot() to allow for boxes and curved arrows
(ggplot(cc_dat, aes(x = x, y = y, xend = xend, yend = yend))
  + theme_dag_gray_grid()
  + coord_equal()
  + geom_dag_label(data = cc_adj, parse = TRUE, size = 8, fill = NA,
                   label.padding = unit(0.15, "lines"))
  + geom_dag_text(aes(label = label), data = cc_unadj, parse = TRUE,
                  color = "black", size = 8)
  + geom_dag_edges_arc(curvature = c(0.25, -0.1, 0, 0, 0))
  + annotate("text", x = 2, y = 0.01, size = 8, label = "|")
)
