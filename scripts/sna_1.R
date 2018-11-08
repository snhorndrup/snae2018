setwd("~/GitHub/snae2018")

#SNA course - Creating our first network
v1 <- c("Søren", "Søren", "Søren", "Søren", "Søren")
v2 <- c("Sille", "Mille", "Moster", "Simon", "Trine")

df <- data.frame(v1, v2)
df

#Installer ipgrah-pakken
##install.packages('igraph')
library(igraph)

#Gem som graf format og visualiser graf
graf <- graph.data.frame(df)
plot.igraph(graf)

E(graf) #information om edges
V(graf) #information om vertices

#turning the graph object into an adjacency matrix
matrix <- as_adjacency_matrix(graf)
matrix

#turning the matrix into a graph object and visualize
g1 <- graph_from_adjacency_matrix(matrix, mode = "undirected")
plot.igraph(g1) #visualization


##Create new network
c1 <- c("Obama", "Bush", "Kennedy", "Me")
c2 <- c("President", "President", "President", "Non-President")

#Formatting as an edgelist (data frame)
df.c <- data.frame(c1, c2)
df.c

#Formatting as a graph object
g2 <- graph.edgelist(as.matrix(df.c), directed = F)


#adding node attribute indicating the type of the node
V(g2)$type <- bipartite.mapping(g2)$type
colors <- V(g2)$type #adding color to show profile type

#Plotting bi-partite affiliation network
plot.igraph(g2, main="Bi-partite network", vertex.color = colors)

### Plotting one-mode affiliation network
plot.igraph(bipartite_projection(g2)$proj1,main="Affilitaton Network")

### Plotting one-mode affiliation-affiation network
plot.igraph(bipartite_projection(g2)$proj2,main="Affilitaton Network")