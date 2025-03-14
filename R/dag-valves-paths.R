## Open and closed paths on a DAG

# load R packages
library(dagitty)
library(ggdag)

# define DAG
valves <- dagitty('dag{
  A -> C
  B -> C -> D
}')

# path from A to B is blocked by the collider at C
paths(valves, "A", "B")
dconnected(valves, "A", "B")
dseparated(valves, "A", "B")
isCollider(valves, "A", "C", "B")

# path from B to D is open because C is not a collider on it
paths(valves, "B", "D")
dconnected(valves, "B", "D")
dseparated(valves, "B", "D")
isCollider(valves, "B", "C", "D")

# conditional independence relations implied by the DAG
impliedConditionalIndependencies(valves)
