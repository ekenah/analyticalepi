## Directed acyclic graph for a light switch
library(dagitty)
library(ggdag)
library(ggplot2)

cxd <- dagitty('dag{
  C -> X -> D
}')
coordinates(cxd) <- list(x = c(C = 0, X = 1, D = 2),
                         y = c(C = 0, X = 0, D = 0))
(ggdag_classic(cxd)
  + theme_dag_gray_grid()
)
