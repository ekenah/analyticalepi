## Conditionally open and closed paths on a DAG

# load R packages
library(dagitty)
library(ggdag)

# define DAG
valves <- dagitty('dag{
  A -> C
  B -> C -> D
}')

# path from A to B opened by conditioning on the collider at C
# The third argument Z is a variable or vector of variables to condition on.
isCollider(valves, "A", "C", "B")
paths(valves, "A", "B", Z = "C")
dconnected(valves, "A", "B", "C")
dseparated(valves, "A", "B", "C")

# path from A to B opened by conditioning on D (descendant of collider C)
isCollider(valves, "A", "C", "D")
paths(valves, "A", "B", "D")
dconnected(valves, "A", "B", "D")
dseparated(valves, "A", "B", "D")

# path from B to D closed by conditioning on non-collider at C
isCollider(valves, "B", "D", "C")
paths(valves, "B", "D", "C")
dconnected(valves, "B", "D", "C")
dseparated(valves, "B", "D", "C")

# conditional independence relations implied by the DAG
impliedConditionalIndependencies(valves)
