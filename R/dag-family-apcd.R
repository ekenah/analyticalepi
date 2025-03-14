## Ancestors, parents, children, and descendants on a DAG

# load packages
library(dagitty)

# define the DAG
fam <- dagitty('dag{
  A -> B -> C
  A -> C
  B -> D
}')

# node A
# Proper ancestors and descendants do not include the node itself.
ancestors(fam, "A")
ancestors(fam, "A", proper = TRUE)
parents(fam, "A")
children(fam, "A")
descendants(fam, "A")
descendants(fam, "A", proper = TRUE)

# node B
ancestors(fam, "B", proper = TRUE)
parents(fam, "B")
children(fam, "B")
descendants(fam, "B", proper = TRUE)

# node C
ancestors(fam, "C", proper = TRUE)
parents(fam, "C")
children(fam, "C")
descendants(fam, "C", proper = TRUE)

# node D
ancestors(fam, "D", proper = TRUE)
parents(fam, "D")
children(fam, "D")
descendants(fam, "D", proper = TRUE)
