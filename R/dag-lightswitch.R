## Directed acyclic graph for a light switch

# daggity can be installed from github.com/jtextor/dagitty
# ggdag and ggplot2 can be installed using install.packages()
library(dagitty)
library(ggdag, warn.conflicts = FALSE)
library(ggplot2)

# define the DAG
light <- dagitty('dag{
  "Light switch" -> "Light on"
}')
coordinates(light) <- list(x = c("Light switch" = 0, "Light on" = 1),
                           y = c("Light switch" = 0, "Light on" = 0))

# plot the DAG
(ggdag_classic(light)
  + theme_dag_gray_grid()
  + xlim(-0.5, 1.5)
  + ylim(-0.5, 0.5)
)
