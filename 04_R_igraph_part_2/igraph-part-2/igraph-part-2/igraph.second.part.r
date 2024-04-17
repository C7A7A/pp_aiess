# CONTENTS
# 5. Plotting networks with igraph
# 6. Network and node descriptives

library(igraph)

# create necessary data structures from the first part of the tutorial

setwd("C:/Users/User/studia/pp_aiess/04_R_igraph_part_2/igraph-part-2/igraph-part-2")  

nodes <- read.csv("d1-nodes.csv", header=T, as.is=T)
links <- read.csv("d1-edges.csv", header=T, as.is=T)

links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
links

colnames(links)[4] <- "weight"
rownames(links) <- NULL

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
net <- simplify(net, remove.multiple = F, remove.loops = T) 

# ================ 5. Plotting networks with igraph ================

#  ------->> Plot parameters in igraph --------

# Plotting with igraph: node options (starting with 'vertex.') and edge options
# (starting with 'edge.'). 
?igraph.plotting

# We can set the node & edge options in two ways - one is to specify
# them in the plot() function, as we are doing below.

# Plot with curved edges (edge.curved=.1) and reduce arrow size:
plot(net, edge.arrow.size=.4, edge.curved=.1)

# Set node color to orange and the border color to hex #555555
# Replace the vertex label with the node names stored in "media"
plot(net, edge.arrow.size=.2, edge.curved=0,
     vertex.color="lavender", vertex.frame.color="#555555",
     vertex.label=V(net)$media, vertex.label.color="black",
     vertex.label.cex=.7) 

# The second way to set attributes is to add them to the igraph object.

# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Set node size based on audience size:
V(net)$size <- V(net)$audience.size*0.7

# Set node label color:
V(net)$label.color <- "black"

# Set edge width based on weight:
E(net)$width <- E(net)$weight/6

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"

plot(net) 

# We can also override the attributes explicitly in the plot:
plot(net, edge.color="orange", vertex.color="gray50") 


# We can also add a legend explaining the meaning of the colors we used:
plot(net) 
legend(x=-1.1, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2.5, bty="n", ncol=3)


# Sometimes, especially with semantic networks, we may be interested in 
# plotting only the labels of the nodes:
plot(net, vertex.shape="none", vertex.label=V(net)$media, 
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85")


# Let's color the edges of the graph based on their source node color.
# We'll get the starting node for each edge with "ends()".
edge.start <- ends(net, es=E(net), names=F)[,1]
edge.col <- V(net)$color[edge.start]

plot(net, edge.color=edge.col, edge.curved=.1)


# ------->> EXERCISE <<------------------------------

# Use the stormofswords.csv file to read the network of "Game of Thrones"
# characters and their interactions. Plot the network as follows:
# - remove all edges that have the weight below the average weight
# - set the color of each node to light blue
# - set the color of node's edge to white
# - make the arrows very small
# - set edge color to light gray
# - set label color to black
data <- read.csv("stormofswords.csv")
got_net <- graph_from_data_frame(data, directed = TRUE)
plot(got_net)


V(got_net)$color <- "lightblue"
V(got_net)$frame.color <- "white"
E(got_net)$arrow.size <- 0.5
E(got_net)$color <- "lightgray"
V(got_net)$label.color <- "black"

plot(got_net)

#  ------->> Network Layouts --------

# Network layouts are algorithms that return coordinates for each
# node in a network.

# Let's generate a slightly larger 80-node graph.

net.bg <- sample_pa(80, 1.2) 
V(net.bg)$size <- 8
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- "" 
E(net.bg)$arrow.mode <- 0
plot(net.bg)

# You can set the layout in the plot function:
plot(net.bg, layout=layout_randomly)

# Or calculate the vertex coordinates in advance:
l <- layout_in_circle(net.bg)
l
plot(net.bg, layout=l)

# l is simply a matrix of x,y coordinates (N x 2) for the N nodes in the graph. 
# You can generate your own:
l <- cbind(1:vcount(net.bg), c(1:vcount(net.bg))**5)
plot(net.bg, layout=l)

# This layout is just an example and not very helpful - thankfully
# igraph has a number of built-in layouts, including:

# Randomly placed vertices
l <- layout_randomly(net.bg)
plot(net.bg, layout=l)

# Circle layout
l <- layout_in_circle(net.bg)
plot(net.bg, layout=l)

# 3D sphere layout
l <- layout_on_sphere(net.bg)
plot(net.bg, layout=l)

# The Fruchterman-Reingold force-directed algorithm 
# Nice but slow, most often used in graphs smaller than ~1000 vertices. 
l <- layout_with_fr(net.bg)
plot(net.bg, layout=l)

# You will also notice that the layout is not deterministic - different runs 
# will result in slightly different configurations. Saving the layout in l
# allows us to get the exact same result multiple times.
par(mfrow=c(2,2), mar=c(1,1,1,1))
plot(net.bg, layout=layout_with_fr)
plot(net.bg, layout=layout_with_fr)
plot(net.bg, layout=l)
plot(net.bg, layout=l)
dev.off()

# By default, the coordinates of the plots are rescaled to the [-1,1] interval
# for both x and y. You can change that with the parameter "rescale=FALSE"
# and rescale your plot manually by multiplying the coordinates by a scalar.
# You can use norm_coords to normalize the plot with the boundaries you want.

# Get the layout coordinates:
l <- layout_with_fr(net.bg)
# Normalize them so that they are in the -1, 1 interval:
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

par(mfrow=c(2,2), mar=c(0,0,0,0))
plot(net.bg, rescale=F, layout=l*0.4)
plot(net.bg, rescale=F, layout=l*0.8)
plot(net.bg, rescale=F, layout=l*1.2)
plot(net.bg, rescale=F, layout=l*1.6)
dev.off()

# Another popular force-directed algorithm that produces nice results for
# connected graphs is Kamada Kawai. Like Fruchterman Reingold, it attempts to 
# minimize the energy in a spring system.

l <- layout_with_kk(net.bg)
plot(net.bg, layout=l)

# By default, igraph uses a layout called layout_nicely which selects
# an appropriate layout algorithm based on the properties of the graph. 

# Check out all available layouts in igraph:
?igraph::layout_

layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(5,3), mar=c(1,1,1,1))

for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(net)) 
  plot(net, edge.arrow.mode=0, layout=l, main=layout) }

dev.off()

# ------->> EXERCISE <<------------------------------

# Use the StormOfSwords network to compute a layout of your choosing.
# Manually modify the layout in such a way that the node "Aemon"
# is displayed in the upper-left corner of the plot
got_net.bg <- sample_pa(80, 1.2) 

l <- layout_with_fr(got_net.bg)
plot(got_net.bg, layout=l)

aemon_idx <- which(V(got_net)$name == "Aemon")
l[aemon_idx, ] <- c(-20, 20)
plot(got_net.bg, layout=l)

# ------->> Improving network plots --------

plot(net)

# Notice that this network plot is still not too helpful.
# We can identify the type and size of nodes, but cannot see
# much about the structure since the links we're examining are so dense.
# One way to approach this is to see if we can sparsify the network.

hist(links$weight)
mean(links$weight)
sd(links$weight)

# There are more sophisticated ways to extract the key edges,
# but for the purposes of this excercise we'll only keep ones
# that have weight higher than the mean for the network.

# We can delete edges using delete_edges(net, edges)
cut.off <- mean(links$weight) 
net.sp <- delete_edges(net, E(net)[weight<cut.off])
plot(net.sp, layout=layout_components) 

# Another way to think about this is to plot the two tie types 
# (hyperlik & mention) separately:

E(net)$width <- 2
plot(net, 
     edge.color=c("dark red", "slategrey")[(E(net)$type=="hyperlink")+1],
     vertex.color="gray40", 
     layout=layout_in_circle)

# Another way to delete edges:  
net.m <- net - E(net)[E(net)$type=="hyperlink"]
net.h <- net - E(net)[E(net)$type=="mention"]

# Plot the two links separately:
par(mfrow=c(1,2))

plot(net.h, vertex.color="orange", main="Tie: Hyperlink")
plot(net.m, vertex.color="lightsteelblue2", main="Tie: Mention")

dev.off()

# Make sure the nodes stay in place in both plots:
par(mfrow=c(1,2),mar=c(1,1,4,1))

l <- layout_with_fr(net)
plot(net.h, vertex.color="orange", layout=l, main="Tie: Hyperlink")
plot(net.m, vertex.color="lightsteelblue2", layout=l, main="Tie: Mention")

dev.off()

# ------->> EXERCISE <<------------------------------

# Plot the histogram representing the number of GoT characters'
# interactions (how many characters interact with one other character,
# how many characters interact with two other characters, etc.)

# ------->> Heatmaps as a way to represent networks -------- 

# A quick reminder that there are other ways to represent a network:

# Heatmap of the network matrix:
netm <- get.adjacency(net, attr="weight", sparse=F)
colnames(netm) <- V(net)$media
rownames(netm) <- V(net)$media

palf <- colorRampPalette(c("gold", "dark orange")) 
heatmap(netm[,17:1], 
        Rowv = NA, 
        Colv = NA, 
        col = palf(20), 
        scale="none", 
        margins=c(10,10) )


# ------->> Plotting two-mode networks with igraph --------  

nodes2 <- read.csv("d2-nodes.csv", header=T, as.is=T)
links2 <- read.csv("d2-edges.csv", header=T, row.names=1)
net2 <- graph_from_incidence_matrix(links2)

head(nodes2)
head(links2)

net2
plot(net2)

# This time we will make nodes look different based on their type.
V(net2)$color <- c("steelblue", "orange")[V(net2)$type+1]
V(net2)$shape <- c("square", "circle")[V(net2)$type+1]
V(net2)$label <- ""
V(net2)$label[V(net2)$type==F] <- nodes2$media[V(net2)$type==F] 
V(net2)$label.cex=.6
V(net2)$label.font=2

plot(net2, vertex.label.color="white", vertex.size=(2-V(net2)$type)*8) 

plot(net2, vertex.label=NA, vertex.size=7, layout=layout_as_bipartite) 
 
# Using text as nodes:
par(mar=c(0,0,0,0))
plot(net2, vertex.shape="none", vertex.label=nodes2$media,
     vertex.label.color=V(net2)$color, vertex.label.font=2, 
     vertex.label.cex=.95, edge.color="gray70",  edge.width=2)

dev.off()


# ================ 6. Network and node descriptives ================


# Density
# The proportion of present edges from all possible ties.
edge_density(net, loops=F)
ecount(net)/(vcount(net)*(vcount(net)-1)) #for a directed network

# Reciprocity
# The proportion of reciprocated ties (for a directed network).
reciprocity(net)
dyad_census(net) # Mutual, asymmetric, and null node pairs
2*dyad_census(net)$mut/ecount(net) # Calculating reciprocity

# Transitivity
# global - ratio of triangles (direction disregarded) to connected triples
# local - ratio of triangles to connected triples each vertex is part of
transitivity(net, type="global")  # net is treated as an undirected network
transitivity(as.undirected(net, mode="collapse")) # same as above
transitivity(net, type="local")
triad_census(net) # for directed networks

# Triad types (per Davis & Leinhardt):
# 
# 003  A, B, C, empty triad.
# 012  A->B, C 
# 102  A<->B, C  
# 021D A<-B->C 
# 021U A->B<-C 
# 021C A->B->C
# 111D A<->B<-C
# 111U A<->B->C
# 030T A->B<-C, A->C
# 030C A<-B<-C, A->C.
# 201  A<->B<->C.
# 120D A<-B->C, A<->C.
# 120U A->B<-C, A<->C.
# 120C A->B->C, A<->C.
# 210  A->B<->C, A<->C.
# 300  A<->B<->C, A<->C, completely connected.


# Diameter (longest geodesic distance)
# Note that edge weights are used by default, unless set to NA.
diameter(net, directed=F, weights=NA)
diameter(net, directed=F)
diam <- get_diameter(net, directed=T)
diam

# Note: vertex sequences asked to behave as a vector produce numeric index of nodes
class(diam)
as.vector(diam)

# Color nodes along the diameter:
vcol <- rep("gray40", vcount(net))
vcol[diam] <- "gold"
ecol <- rep("gray80", ecount(net))
ecol[E(net, path=diam)] <- "orange" 
# E(net, path=diam) finds edges along a path, here 'diam'
plot(net, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0)

# Node degrees
# 'degree' has a mode of 'in' for in-degree, 'out' for out-degree,
# and 'all' or 'total' for total degree. 
deg <- degree(net, mode="all")
plot(net, vertex.size=deg*3)

hist(deg, breaks=1:vcount(net)-1, main="Histogram of node degree")

# Degree distribution
deg.dist <- degree_distribution(net, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")


# Centrality & centralization

# Centrality functions (vertex level) and centralization functions (graph level).
# The centralization functions return "res" - vertex centrality, "centralization", 
# and "theoretical_max" - maximum centralization score for a graph of that size.
# The centrality functions can run on a subset of nodes (set with the "vids" parameter)

# Degree (number of ties)
degree(net, mode="in")
centr_degree(net, mode="in", normalized=T)


# Closeness (centrality based on distance to others in the graph)
# Inverse of the node's average geodesic distance to others in the network
closeness(net, mode="all", weights=NA) 
centr_clo(net, mode="all", normalized=T) 

# Eigenvector (centrality proportional to the sum of connection centralities)
# Values of the first eigenvector of the graph adjacency matrix
eigen_centrality(net, directed=T, weights=NA)
centr_eigen(net, directed=T, normalized=T) 

# Betweenness (centrality based on a broker position connecting others)
# (Number of geodesics that pass through the node or the edge)
betweenness(net, directed=T, weights=NA)
edge_betweenness(net, directed=T, weights=NA)
centr_betw(net, directed=T, normalized=T)

# Hubs and authorities

# The hubs and authorities algorithm developed by Jon Kleinberg was initially used 
# to examine web pages. Hubs were expected to contain catalogues with a large number 
# of outgoing links; while authorities would get many incoming links from hubs, 
# presumably because of their high-quality relevant information. 

hs <- hub_score(net, weights=NA)$vector
as <- authority_score(net, weights=NA)$vector

par(mfrow=c(1,2))
 plot(net, vertex.size=hs*50, main="Hubs")
 plot(net, vertex.size=as*30, main="Authorities")
dev.off()

# ------->> EXERCISE <<------------------------------

# Plot the GoT network using betweenness as the metric used to 
# scale the size of each node. In addition, compute and plot the 
# distribution of degrees of the nodes.

