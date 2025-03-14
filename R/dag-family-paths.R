## Paths and causal (directed) paths on a DAG

# load packages
library(dagitty)

# define the DAG
fam <- dagitty('dag{
  A -> B -> C
  A -> C
  B -> D
}')

# paths from B to D
# Setting "directed = TRUE" returns causal paths.
paths(fam, "B", "D")
paths(fam, "B", "D", directed = TRUE)
paths(fam, "D", "B")
paths(fam, "D", "B", directed = TRUE)

# paths from A to C
paths(fam, "A", "C")
paths(fam, "A", "C", directed = TRUE)

# paths from B to C
paths(fam, "B", "C")
paths(fam, "B", "C", directed = TRUE)

# paths from C to D
paths(fam, "C", "D")
paths(fam, "C", "D", directed = TRUE)
