### setting the working directory
setwd("YOUR/WORKING/DIRECTORY")

### loading packages
library(dplyr)
library(igraph)

## loading the data
df <- read.csv("DIPCON_3.0_Dyads.csv") # data and documentation from: http://www.u.arizona.edu/~volgy/data.html

###
df1 <- df %>% filter(dipcon2010 > 0) %>% select(abbrev1, abbrev2)

## creating a graph object
g <- graph.data.frame(df1, directed = T)

### creating a data frame where columns represent variables and rows represent countires
df.g <- data.frame(country = V(g)$name,
                   indegree_norm = degree(g, mode = "in", normalized = T), #normalized indegree
                   indegree = degree(g, mode = "in", normalized = F )) # raw indegree

# ranking the countries by raw (non-normalized) indegree in descending order
df.g <- df.g %>% arrange(-indegree)

### examining the first 10 rows (top 10 countries ranked by (non-normalized) indegree)
head(df.g, 10)


### exporting th dataframe as a csv in your working directory
write.csv(df.g, "diplodata.txt")