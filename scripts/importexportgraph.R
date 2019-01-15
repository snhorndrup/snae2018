library(igraph)

#exporting the rts.g graph object as a graphml file 
write.graph(rts.g, file="your_file.graphml", format="graphml") #rts.g is your graph file


### you can import the network file in gdf format from Youtube Data Tools using readgdf() 
### Alternatively, you can covert the gdf file to graphml by going through the following steps:
#1  Read the gdf file in Gephi
#2 "Export" the network as graphml file into your working directory
#3  Load the graphml file in R using the code below

##reading a graphml file in your working directory as a graph object
g_import <- read.graph("your_file_in_the_working_dirctory.graphml", format = "graphml") 

### test run 
plot.igraph(g_import)