## Paths, ancestors, and descendants on a DAG

# load packages
# I set "warn.conflicts = FALSE" to suppress an unnecessary error message.
library(dagitty)
library(ggdag, warn.conflicts = FALSE)
library(ggplot2)

# define the DAG
fam <- dagitty('dag{
  A -> B -> C
  A -> C
  B -> D
}')

# plot the DAG
# Uncomment coord_equal() to get a better-looking DAG in an R plot window.
coordinates(fam) <- list(x = c(A = 0, B = 1, C = 2, D = 2),
                         y = c(A = 0, B = 0, C = 0.5, D = -0.5))
(ggdag_classic(fam)
  + theme_dag_gray_grid()
  + xlim(-0.2, 2.2)
  # + coord_equal()
  # node A
  + annotate("text", x = 0, y = -0.1, label = "Parent of B and C")
  + annotate("text", x = 0, y = -0.2, label = "Ancestor of B, C, and D")
  # node B
  + annotate("text", x = 1, y = -0.1, label = "Child of A")
  + annotate("text", x = 1, y = -0.2, label = "Parent of C and D")
  + annotate("text", x = 1, y = -0.3, label = "Ancestor of C and D")
  # node C
  + annotate("text", x = 2, y = 0.35, label = "Child of A and B")
  + annotate("text", x = 2, y = 0.25, label = "Descendant of A and B")
  # node D
  + annotate("text", x = 2, y = -0.25, label = "Descendant of A and B")
  + annotate("text", x = 2, y = -0.35, label = "Child of B")
)
