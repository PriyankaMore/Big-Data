# 
#CSCI 6907 Big Data, Project 1, Group 3
#Priyanka More, Siddarth Gandhi 
# June 13, 2016
# igraph
#

# clear memory (should be at the first of the file)
rm(list=ls())
#----------------------- load igraph package and sample dataset
library("igraph")
load("astrocollab.Rdata")
mydata <- upgrade_graph(astrocollab)
V(mydata) #shows the vertices in the graph
E(mydata) #Shows the edges for the vertices
degree(mydata) #the degree of distribution of the vertices

#----------------------- convert the igraph into data frame
mydataframe <- get.data.frame(mydata, what=c("edges", "vertices", "both"))
write.csv(mydataframe,"mydataDegree.csv") #export to Excel file

#----------------------- Minimization of data 
degreemydata <-degree(mydata)

write.csv(degreemydata,"degreemydata.csv") #export to Excel file
MAX_mydataDegree <- max(degreemydata)
MIN_mydataDegree <- min(degreemydata)

#print the max and min
MAX_mydataDegree
MIN_mydataDegree
#find the vertices that have degree less then 250 
bad.vs<-V(mydata)[degreemydata<250] 
#delet those vertices with degrees less than 250
mydataMinimize<-delete.vertices(mydata, bad.vs)
summary(mydataMinimize) #print summary
#plot
plot(mydataMinimize) 

#----------------------- Change the color of Edges and Vertics depending on conditions
E(mydataMinimize)$color <- "blue"#E is the function for edges
E(mydataMinimize)$color[as.logical(mydataMinimize[,5])] <- "red"
V(mydataMinimize)$color <- "green" 
V(mydataMinimize)$color[degree(mydataMinimize) > 7] <- "yellow" 
tkplot(mydataMinimize)

#----------------------- Canonical Permutation: The canonical permutation brings every isomorphic graphs into the same (labeled) graph

mycanpermutation<-canonical_permutation(mydataMinimize)
mypermute<-permute(mydataMinimize, mycanpermutation$labeling)
plot(mypermute)

#----------------------- Diversity:Calculates a measure of diversity for all vertices

diversity(mydataMinimize, weights = NULL, vids = V(mydataMinimize))

#----------------------- Vertex Connectivity:The vertex connectivity of two vertices (source and target) in a directed graph is the minimum number of vertices needed to remove from the graph to eliminate all (directed) paths from source to target

connectivity<-vertex_connectivity(mydataMinimize,source = NULL, target = NULL, checks = TRUE)
connectivity

#----------------------- Independent set 

ivs_size(mydataMinimize)
myindependentset <-ivs(mydataMinimize, min=ivs_size(mydataMinimize))
myindependentset

#----------------------- cluster_fast_greedy
bad.vs2<-bad.vs<-V(mydata)[degreemydata<190] 
mydataMinimize_Cluster<-delete.vertices(mydata, bad.vs2)
summary(mydataMinimize_Cluster)
cl <- cluster_fast_greedy(mydataMinimize_Cluster)
cl #print the clusters
#plot the clusters
plot(cl, mydataMinimize_Cluster)
#plot with edge color
plot(cl, mydataMinimize_Cluster,edge.color = c("black", "red"))

#----------------------- K-core decomposition
kc <- coreness(mydataMinimize_Cluster, mode="all")
tkplot(mydataMinimize_Cluster, vertex.size=kc*.8, vertex.label=kc, vertex.color="green")

#----------------------- Average path length: the mean of the shortest distance between each pair of nodes in the network (in both directions for directed graphs).
mean_distance(mydataMinimize_Cluster, directed=F)

#----------------------- The length of all shortest paths in the graph:
distances(mydataMinimize_Cluster) # with edge weights
distances(mydataMinimize_Cluster, weights=NA) # ignore weights

#----------------------- Cliques
cliques(mydataMinimize) # list of cliques       
sapply(cliques(mydataMinimize), length) # clique sizes
largest_cliques(mydataMinimize) # cliques with max number of nodes

vcol <- rep("grey80", vcount(mydataMinimize))
vcol[unlist(largest_cliques(mydataMinimize))] <- "gold"
tkplot(as.undirected(mydataMinimize), vertex.label=V(mydataMinimize)$name, vertex.color=vcol)

#----------------------- Filtering:
#Filter out the edges having a multiplicity
#shows the entire graph number of multiplicity:
E(mydata)[is.multiple(mydata, eids=E(mydata))]
#shows the simplified graph multiplicity`s number:
E(mydataMinimize)[is.multiple(mydataMinimize, eids=E(mydataMinimize))]

#----------------------- Visulaization:
#Using different layout for plot function:
plot(mydataMinimize, layout=layout.fruchterman.reingold) 
plot(mydataMinimize, layout=layout.kamada.kawai)
plot(mydataMinimize, layout=layout.circle)

#----------------------- shows the main person in the graph
head(sort(betweenness(mydataMinimize), decreasing = TRUE),1)

#------------------Graph Statistics----------------------------
# 1- No of nodes
length(V(mydata))
length(V(mydataMinimize))

# 2- No of edges
length(E(mydata))

# 3- Density (No of edges / possible edges)
graph.density(mydata)

# 4- Number of islands
clusters(mydata)$no

# 5- Global cluster coefficient:
#(close triplets/all triplets)
transitivity(mydata, type="global")

# 6- Edge connectivity, 0 since graph is disconnected
edge.connectivity(mydata)

# 7- Same as graph adhesion
graph.adhesion(mydata)

# 8- Diameter of the graph
diameter(mydataMinimize)

# 9- Reciprocity of the graph
reciprocity(mydata)

# 10- degree of distribution
degree.distribution(mydataMinimize)

# 11- Find the multiple or loop edges in a graph
any_multiple(mydata)
which_multiple(mydata)
count_multiple(mydata)
which_multiple(simplify(mydata))
all(count_multiple(simplify(mydata)) == 1)

# 12- shows how many components
no.clusters(mydata)
no.clusters(mydataMinimize)

#----------------------- Plotting for degree of distribution
plot(degree.distribution(mydata), xlab="node degree")
lines(degree.distribution(mydata))



#
#----------------------- Graph community structure calculated with the infomap algorithm
imc <- infomap.community(mydataMinimize)
imc

#
# Set edge color to gray, and the node color to orange. 
# Replace the vertex label with the node names stored in "media"
plot(mydataMinimize, edge.arrow.size=.2, edge.curved=0,
     vertex.color="orange", vertex.frame.color="#555555",
     vertex.label=V(mydataMinimize)$media, vertex.label.color="black",
     vertex.label.cex=.7) 

# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(mydataMinimize)$color <- colrs[V(mydataMinimize)$media.type]

# Set node size based on audience size:
V(mydataMinimize)$size <- V(mydataMinimize)$audience.size*0.7

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(mydataMinimize)$label.color <- "black"
V(mydataMinimize)$label <- NA

# Set edge width based on weight:
E(mydataMinimize)$width <- E(mydataMinimize)$weight/6

#change arrow size and edge color:
E(mydataMinimize)$arrow.size <- .2
E(mydataMinimize)$edge.color <- "gray80"

E(mydataMinimize)$width <- 1+E(mydataMinimize)$weight/12

#Find the Average path length
mean_distance(mydata, directed=F)

#Find the length of all shortest paths in the graph
distances(mydataMinimize) # with edge weights
distances(mydataMinimize, weights=NA) # ignore weights

#----------------------- Finding the path between specific nodes

news.path <- shortest_paths(mydataMinimize, 
                            from = V(mydataMinimize)["PARADIJS, JV"], 
                            to  = V(mydataMinimize)["PIRO, L"],
                            output = "both") # both path nodes and edges

# Generate edge color variable to plot the path:
ecol <- rep("gray80", ecount(mydataMinimize))
ecol[unlist(news.path$epath)] <- "orange"
# Generate edge width variable to plot the path:
ew <- rep(2, ecount(mydataMinimize))
ew[unlist(news.path$epath)] <- 4
# Generate node color variable to plot the path:
vcol <- rep("gray40", vcount(mydataMinimize))
vcol[unlist(news.path$vpath)] <- "gold"

plot(mydataMinimize, vertex.color=vcol, edge.color=ecol, 
     edge.width=ew, edge.arrow.mode=0)

#----------------------- closeness
mycloseness<- closeness(mydataMinimize, vids = "PARADIJS, JV", 
                        mode = c("out", "in", "all", "total"))
class(mycloseness)
mycloseness
vcount(mydataMinimize)
ecount(mydataMinimize)

#----------------------- betweenness

b1 <- betweenness(mydataMinimize, v = "HURLEY, K")

b1
#----------------------- longest shortest path
diameter(mydataMinimize_Cluster)
#---------------- largest clique
#without setting min nor max value 
mymaxcliques <- max_cliques(mydataMinimize_Cluster, min = NULL, max = NULL)
mymaxcliques
# with min value 
mymaxcliques <- max_cliques(mydataMinimize_Cluster, min = 3, max = NULL)
mymaxcliques
# with min and max 
mymaxcliques <- max_cliques(mydataMinimize_Cluster, min = 3, max = 4)
mymaxcliques

#----------------------- power centrality 
vcount(mydataMinimize_Cluster) #16 nodes

mydatacentrality <- power_centrality(mydataMinimize_Cluster, 
               nodes = V(mydataMinimize_Cluster), exponent = 3)
mydatacentrality
#----------------------- ego 
#find for the whole dataset
myego<- ego(mydata, order = 2, nodes = V(mydata), 
            mode = c("all", "out", "in"))
myego
#find for a particular node
myego<- ego(mydata, order = 2, nodes = "KOUVELIOTOU, C", 
            mode = c("out"))
myego










