### clearing memory
rm(list = ls())

### setting the working directory
setwd("~/GitHub/snae2018")

### loading packages
library(dplyr)
library(igraph)
library(ggplot2)

#########################################################################
#################################communities ################################
#########################################################################

### mockup data set and most of the code from https://stackoverflow.com/questions/36276433/igraph-k-core-with-graph-coreness-underestimating-some-cores
data <- matrix(c("Ns-1","Ns-1","Ns-1","Ns-1","Ns-1","Ns-1","Ns-1","Ns-1","Ns-14","Ns-14","Ns-15","Ns-15","Ns-15","Ns-17","Ns-17","Ns-17","Ns-17","Ns-2","Ns-2","Ns-2","Ns-2","Ns-4","Ns-4","Ns-4","Ns-5","Ns-5","Ns-5","TAMU-7","Ns-14","Ns-15","Ns-17","Ns-2","Ns-4","Ns-5","TAMU-7","TAMU-8","Ns-15","Ns-17","Ns-17","Ns-4","Ns-18","Ns-2","Ns-4","Ns-5","Ns-18","Ns-4","Ns-5","TAMU-7","TAMU-8","Ns-5","TAMU-7","TAMU-8","TAMU-7","TAMU-8","Ns-18","TAMU-8"),nrow=28,ncol=2)

### creating a graph object
graph <- graph.edgelist(data[,1:2],directed=F)

# examining the graph
plot(graph,
     vertex.size = 20,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.color = "white")


# running infomap community detection
infomap<- cluster_infomap(graph, nb.trials = 10,
                          modularity = F)
names(infomap)
head(infomap$membership)

# running modularity optimization community detection
modularity <- cluster_louvain(graph)

## running Girvan and Newman's degree-based community detection
deg_com <- cluster_edge_betweenness(graph)

## Vizualising communites based on infomap algorithm
plot(graph,
     vertex.size = 20,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.color = infomap$membership)

## Vizualising communities based on modularity optimization algorithm
plot(graph,
     vertex.size = 20,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.color = modularity$membership)

## Vizualising communites based on Girvan and Newman's degree-based community detection
plot(graph,
     vertex.size = 20,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.color = deg_com$membership)

### what if you want to store the output from the algoritm above in the graph object before exporting? Use the code bellow
V(graph)$yourvariablename <- deg_com$membership

### example:
plot(graph,
     vertex.size = 20,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.color = V(graph)$yourvariablename)










#########################################################################
###### Analyzing communities in DIPCON dataset for 1970 ##############
#########################################################################

## loading the data
df <- read.csv("data/dipcon.csv") # data and documentation from: http://www.u.arizona.edu/~volgy/data.html

### using dplyr to keep only relations for 1970
df70 <- df %>% # selecting th data set
  filter(dipcon1970 > 0) %>% # keeping only the connections in 1970
  select(abbrev1, abbrev2) # selecting the variables

#Turning into a graph object
g70 <- graph.data.frame(df70, directed = F) # storing the graph as "undirected" for the sake of simplicity

## Even though the graph object is sstored as "undirected", some edges may appear twice (in cases where reciprocal ties are directional)
g70 <- simplify(g70, remove.multiple = T, remove.loops = T) #use this function to remove multiples or self-ties 


### vizualising the network
plot.igraph(g70,
            main="Diplomatic ties in 1970",
            vertex.size = 2,
            vertex.label = NA,
            vertex.label.color = "black",
            vertex.label.cex = 1,
            vertex.color = "White")


### running infomap community detection
infomap.70<- cluster_infomap(g70, nb.trials = 10,
                             modularity = T)

# running modularity optimization community detection
modularity.70 <- cluster_louvain(g70)

head(modularity.70)

#Laver det til en data.frame
modularity.70
V(g70)$community <- cluster_louvain(g70)$membership
df.m <- data.frame(name = V(g70)$name, community = V(g70)$community)


## running Girvan and Newman's degree-based community detection
deg_com.70 <- cluster_edge_betweenness(g70)


### vizualising the network with communities (infomap)
plot.igraph(g70,
            main="Diplomatic ties in 1970",
            vertex.size = 2,
            vertex.label = NA,
            vertex.label.color = "black",
            vertex.label.cex = 1,
            vertex.color = infomap.70$membership)


### vizualising the network with communities (modularity optimisation)
plot.igraph(g70,
            main="Diplomatic ties in 1970",
            vertex.size = 2,
            vertex.label = NA,
            vertex.label.color = "black",
            vertex.label.cex = 1,
            vertex.color = modularity.70$membership)


### vizualising the network with communities (degree-based)
plot.igraph(g70,
            main="Diplomatic ties in 1970",
            vertex.size = 2,
            vertex.label = NA,
            vertex.label.color = "black",
            vertex.label.cex = 1,
            vertex.color = deg_com.70$membership)





#########################################################################
#################################Bonus: Cliques ################################
#########################################################################

### plotting the graph 
plot(graph,
     vertex.size = 20,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.color = "white")


### identifying cliques
cliques <- cliques(graph, min =5) #mininum clique size = 5 nodes

## how many cliques are there?
length(cliques)

### examining clique members
cliques

### clique size
sapply(cliques(graph, min = 5), length) 

###  identifying the largest clique
largest_cliques(graph) 

### adding grey collor to all of the nodes
vcol <- rep("grey80", vcount(graph))

## adding orange colour to the largest clique
vcol[unlist(largest_cliques(graph))] <- "orange"


### vizualising who is in the largest clique (orange) and who is not (grey)
plot(graph,
     vertex.size = 20,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.color = vcol)

# for more info om vizualisation, see this blog http://kateto.net/networks-r-igraph

### to compute n-cliques and n-clans see the stack oveflow thread: https://stackoverflow.com/questions/40088150/find-n-cliques-in-igraph
