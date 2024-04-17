##======================================================##
##                                                      ##
##    Katya Ognyanova, katya@ognyanova.net              ##
##    www.kateto.net/netscix2016                        ##
##                                                      ##
##======================================================##

# CONTENTS
#
# 1. Networks in igraph
# 2. Reading network data from files
# 3. Turning networks into igraph objects

# Install the package "igraph" if you on't have its latest version (1.0.1) 
# The package (www.igraph.org) is maintained by Gabor Csardi and Tamas Nepusz.

# ================ 1. Networks in igraph ================

library(igraph) # Load the igraph package

#  ------->> Create networks --------

g1 <- graph( edges=c(1,2, 2,3, 3,1), n=3, directed=F ) 
# an undirected graph with 3 edges
# The numbers are interpreted as vertex IDs, so the edges are 1-->2, 2-->3, 3-->1

plot(g1) # A simple plot of the network - we'll talk more about plots later
class(g1)
g1

g2 <- graph( edges=c(1,2, 2,3, 3,1), n=10 ) # now with 10 vertices, and directed by default
plot(g2)   
g2

g3 <- graph( c("John", "Jim", "Jim", "Jill", "Jill", "John")) # named vertices
# When the edge list has vertex names, the number of nodes is not needed
plot(g3)
g3

g4 <- graph( c("John", "Jim", "Jim", "Jack", "Jim", "Jack", "John", "John"), 
             isolates=c("Jesse", "Janis", "Jennifer", "Justin") )  
# In named graphs we can specify isolates by providing a list of their names.

plot(g4, edge.arrow.size=.5, vertex.color="gold", vertex.size=15, 
     vertex.frame.color="gray", vertex.label.color="black", 
     vertex.label.cex=1.5, vertex.label.dist=2, edge.curved=0.2) 

# Small graphs can also be generated with a description of this kind:
# '-' for undirected tie, "+-' or "-+" for directed ties pointing left & right, 
# "++" for a symmetric tie, and ":" for sets of vertices

plot(graph_from_literal(a---b, b---c)) # the number of dashes doesn't matter
plot(graph_from_literal(a--+b, b+--c))
plot(graph_from_literal(a+-+b, b+-+c)) 
plot(graph_from_literal(a:b:c---c:d:e))

gl <- graph_from_literal(a-b-c-d-e-f, a-g-h-b, h-e:f:i, j)
plot(gl)



#  ------->> Edge, vertex, and network attributes --------

# Access vertices and edges:
E(g4) # The edges of the object
V(g4) # The vertices of the object


# You can also manipulate the network matrix directly:
g4[]
g4[1,]
g4[3,3] <- 10
g4[5,7] <- 10

# Add attributes to the network, vertices, or edges:
V(g4)$name # automatically generated when we created the network.
V(g4)$gender <- c("male", "male", "male", "male", "female", "female", "male")
E(g4)$type <- "email" # Edge attribute, assign "email" to all edges
E(g4)$weight <- 10    # Edge weight, setting all existing edges to 10

# Examine attributes
edge_attr(g4)
vertex_attr(g4)
graph_attr(g4)

# Another way to set attributes
# (you can similarly use set_edge_attr(), set_vertex_attr(), etc.)
g4 <- set_graph_attr(g4, "name", "Email Network")
g4 <- set_graph_attr(g4, "something", "A thing")

graph_attr_names(g4)
graph_attr(g4, "name")
graph_attr(g4)

g4 <- delete_graph_attr(g4, "something")
graph_attr(g4)

plot(g4, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1.5,
     vertex.color=c( "pink", "skyblue")[1+(V(g4)$gender=="male")] ) 

# g4 has two edges going from Jim to Jack, and a loop from John to himself.
# We can simplify our graph to remove loops & multiple edges between the same nodes.
# Use 'edge.attr.comb' to indicate how edge attributes are to be combined - possible 
# options include "sum", "mean", "prod" (product), min, max, first/last (selects 
# the first/last edge's attribute). Option "ignore" says the attribute should be 
# disregarded and dropped.

g4s <- simplify( g4, remove.multiple = T, remove.loops = F, 
                 edge.attr.comb=list(weight="sum", type="ignore") )
plot(g4s, vertex.label.dist=1.5)
g4s

# Let's take a look at the description of the igraph object.
# Those will typically start with up to four letters:
# 1. D or U, for a directed or undirected graph
# 2. N for a named graph (where nodes have a name attribute)
# 3. W for a weighted graph (where edges have a weight attribute)
# 4. B for a bipartite (two-mode) graph (where nodes have a type attribute)
#
# The two numbers that follow refer to the number of nodes and edges in the graph. 
# The description also lists graph, node & edge attributes, for example:
# (g/c) - graph-level character attribute
# (v/c) - vertex-level character attribute
# (e/n) - edge-level numeric attribute

# ------->> EXERCISE <<------------------------------

# Create a network consisting of 4 nodes organized in a grid.
# For each edge create two attributes and initialize them randomly
# - a boolean attribute "type"
# - a numeric attribute "weight"
# For each node create a "label" attribute with values node_1, node_2, ...
# In addition, for each node create a boolean attribute "type"
# and initialize it randomly
sq_graph <- graph( edges=c(1,2, 2,3, 3,4, 4,1), n=4, directed=F ) 

E(sq_graph)$type <- sample(c(TRUE, FALSE), ecount(sq_graph), replace=TRUE)
E(sq_graph)$weight <- abs(rnorm(ecount(sq_graph), mean = 0, sd = 1))

V(sq_graph)$label <- paste("node", 1:vcount(sq_graph), sep="_")
V(sq_graph)$type <- sample(c(TRUE, FALSE), vcount(sq_graph), replace=TRUE)

edge_attr(sq_graph)
vertex_attr(sq_graph)

plot(sq_graph)
# ------->> Specific graphs and graph models --------

# Empty graph
eg <- make_empty_graph(40)
plot(eg, vertex.size=10, vertex.label=NA)

# Full graph
fg <- make_full_graph(40)
plot(fg, vertex.size=10, vertex.label=NA)

# Star graph 
st <- make_star(40)
plot(st, vertex.size=10, vertex.label=NA) 

# Tree graph
tr <- make_tree(40, children = 3, mode = "undirected")
plot(tr, vertex.size=10, vertex.label=NA) 

# Ring graph
rn <- make_ring(40)
plot(rn, vertex.size=10, vertex.label=NA)

# Erdos-Renyi random graph 
# ('n' is number of nodes, 'm' is the number of edges)
er <- sample_gnm(n=100, m=40) 
plot(er, vertex.size=6, vertex.label=NA)  

# Watts-Strogatz small-world graph
# Creates a lattice with 'dim' dimensions of 'size' nodes each, and rewires edges 
# randomly with probability 'p'. You can allow 'loops' and 'multiple' edges.
# The neighborhood in which edges are connected is 'nei'.
sw <- sample_smallworld(dim=2, size=10, nei=1, p=0.1)
plot(sw, vertex.size=6, vertex.label=NA, layout=layout_in_circle)
 
# Barabasi-Albert preferential attachment model for scale-free graphs
# 'n' is number of nodes, 'power' is the power of attachment (1 is linear)
# 'm' is the number of edges added on each time step 
ba <-  sample_pa(n=100, power=1, m=1,  directed=F)
plot(ba, vertex.size=6, vertex.label=NA)
 
#igraph can also give you some notable historical graphs. For instance:
zach <- graph("Zachary") # the Zachary carate club
plot(zach, vertex.size=10, vertex.label=NA)
 
# Rewiring a graph
# 'each_edge()' is a rewiring method that changes the edge endpoints
# uniformly randomly with a probability 'prob'.
rn.rewired <- rewire(rn, each_edge(prob=0.05))
plot(rn.rewired, vertex.size=10, vertex.label=NA)
 
# Rewire to connect vertices to other vertices at a certain distance. 
rn.neigh = connect.neighborhood(rn, 5)
plot(rn.neigh, vertex.size=8, vertex.label=NA) 

# Combine graphs (disjoint union, assuming separate vertex sets): %du%
plot(rn, vertex.size=10, vertex.label=NA) 
plot(tr, vertex.size=10, vertex.label=NA) 
plot(rn %du% tr, vertex.size=10, vertex.label=NA) 

# ------->> EXERCISE <<------------------------------

# Plot your network using the following rules:
# - the color of the node should depend on its "type" attribute
# - the color of the edge should depend on its "type" attribute
# - the width of the edge should be proportional to the "weight" attribute

vertex.colors <- c("blue", "green")[V(sq_graph)$type == 1]
edge.colors <- c("red", "orange")[E(sq_graph)$type == 1]
edge.weight <- E(sq_graph)$weight * 10

plot(
  sq_graph
)

#vertex.color <- V(sq_graph)$type,
#edge.color <- E(sq_graph)$type,
#edge.weight <- edge.weight

# ================ 2. Reading network data from files ================

rm(list = ls()) # clear the workspace again

# Download the archive with the data files from http://bitly.com/netscix2016 
 
# Set the working directory to the folder containing the workshop files:
setwd("C:/Users/User/studia/pp_aiess/03_R_igraph_intro/igraph-part-1/igraph-part-1/")  

# DATASET: edgelist 

nodes <- read.csv("d1-nodes.csv", header=T, as.is=T)
links <- read.csv("d1-edges.csv", header=T, as.is=T)

# Examine the data:
head(nodes)
head(links)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))

# Collapse multiple links of the same type between the same two nodes
# by summing their weights, using aggregate() by "from", "to", & "type":
# (we don't use "simplify()" here so as not to collapse different link types)
links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]

colnames(links)[4] <- "weight"
rownames(links) <- NULL

# ================ 3. Turning networks into igraph objects ================ 
 
library(igraph)

#  ------->> DATASET 1 -------- 

# Converting the data to an igraph object:
# The graph.data.frame function, which takes two data frames: 'd' and 'vertices'.
# 'd' describes the edges of the network - it should start with two columns 
# containing the source and target node IDs for each network tie.
# 'vertices' should start with a column of node IDs.
# Any additional columns in either data frame are interpreted as attributes.

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 

# Examine the resulting object:
class(net)
net 

# We can look at the nodes, edges, and their attributes:
E(net)
V(net)
E(net)$type
V(net)$media

plot(net, edge.arrow.size=.4,vertex.label=NA)

# Removing loops from the graph:
net <- simplify(net, remove.multiple = F, remove.loops = T) 

# If you need them, you can extract an edge list or a matrix from igraph networks.
as_edgelist(net, names=T)
as_adjacency_matrix(net, attr="weight")

# Or data frames describing nodes and edges:
as_data_frame(net, what="edges")
as_data_frame(net, what="vertices")

plot(net, vertex.color=V(net)$media.type, vertex.label=V(net)$type.label)

# ------->> EXERCISE <<------------------------------

# Use the stormofswords.csv file to read the network of "Game of Thrones"
# characters and their interactions. Plot the network and use the number 
# of interactions between characters as the weight of each edge.
# Experiment with different layouts of the network. Make sure that
# you display the name of each character in the plot.
data <- read.csv("stormofswords.csv")
got_net <- graph_from_data_frame(data, directed = FALSE)
plot(got_net)

E(got_net)$weight <- data$Weight

plot(
  got_net,
  edge.width = E(got_net)$weight / 3,
  vertex.size = 8
)
#  ------->> DATASET 2 --------

# DATASET 2: matrix 

nodes2 <- read.csv("d2-nodes.csv", header=T, as.is=T)
links2 <- read.csv("d2-edges.csv", header=T, row.names=1)

# Examine the data:
head(nodes2)
head(links2)

# links2 is an adjacency matrix for a two-mode network:
links2 <- as.matrix(links2)

net2 <- graph_from_incidence_matrix(links2)

# A built-in vertex attribute 'type' shows which mode vertices belong to.
table(V(net2)$type)

plot(net2,vertex.label=NA)

# To transform a one-mode network matrix into an igraph object,
# use graph_from_adjacency_matrix()

# We can also easily generate bipartite projections for the two-mode network:
# (co-memberships are easy to calculate by multiplying the network matrix by
# its transposed matrix, or using igraph's bipartite.projection function)

net2.bp <- bipartite.projection(net2)

# We can calculate the projections manually as well:
#   as_incidence_matrix(net2)  %*% t(as_incidence_matrix(net2))
# t(as_incidence_matrix(net2)) %*%   as_incidence_matrix(net2)

plot(net2.bp$proj1, vertex.label.color="black", vertex.label.dist=1,
     vertex.label=nodes2$media[!is.na(nodes2$media.type)])

plot(net2.bp$proj2, vertex.label.color="black", vertex.label.dist=1,
     vertex.label=nodes2$media[ is.na(nodes2$media.type)])
