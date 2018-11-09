setwd("~/GitHub/snae2018")

library(pacman)
p_load(tidyverse, igraph)


dir("data")

df <- read.csv("data/dipcon.csv")
View(df)
##Create network onlt for 2010

df2010 <- df %>% 
  filter(dipcon2010>0) %>% 
  select(abbrev1, abbrev2)

head(df2010)

##Turning this into a graph object
g2010 <- graph.data.frame(df2010, directed = T)

plot.igraph(g2010)

##Computing metrics
df.g <- data.frame(country = V(g2010)$name, 
                   norm_indegree = degree(g2010, mode="in", normalized = T),
                   indegree = degree(g2010, mode="in", normalized = F))

df.g <- df.g %>% 
  arrange(-indegree)

View(df.g)

df.h <- data.frame(country = V(g2010)$name, 
                   norm_outdegree = degree(g2010, mode="out", normalized = T),
                   outdegree = degree(g2010, mode="out", normalized = F))

df.h <- df.h %>%
  arrange(-outdegree)

