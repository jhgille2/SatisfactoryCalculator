library(DiagrammeR)
grViz("
digraph a_nice_graph {

# node definitions with substituted label text
node [fontname = Helvetica]
a [label = '@@1']
b [label = '@@2']
c [label = '@@3']
d [label = '@@4']


# edge definitions with the node IDs
a -> b [color = Red]
b -> c [color = Blue]
b -> d [color = Blue]
}

[1]: 'Iron ore'
[2]: 'Iron ingot'
[3]: 'Iron rod'
[4]: 'Iron plate'

")
