rm(list = ls()) #meget rar at have 

setwd("~/GitHub/snae2018") #sætter bare working directory hvor min fil ligger
library(pacman) #pakken pacman lader dig loade flere pakker samtidig
p_load(tidyverse, igraph, broom) ##loader relevante pakker, evt. install.packages() hvis de ikke er der


df <- read.csv("data/dipcon.csv") #indlæser det sotre data sæt

#Laver datasæt for hvert år vi er interesseret i
df1995 <- df %>% 
  filter(dipcon1995 > 0) %>%
  select(abbrev1, abbrev2)

#Laver dem alle til pgrah data frames med directed egdes
gdf1995 <- graph.data.frame(df1995, directed = T)

centralities1995 <- data.frame(country = V(gdf1995)$name,
                   indegree = degree(gdf1995, mode = 'in'),
                   outdegree = degree(gdf1995, mode = 'out'),
                   betweenness = betweenness(gdf1995, directed = F, normalized = T),
                   closeness = closeness(gdf1995, mode = "all", normalized = T))


#Laver funktion, der beregber centrality
centrality <- function(data, year){
  data1 <- data %>% 
    filter(year > 0) %>%
    select(abbrev1, abbrev2)
  graph <- graph.data.frame(data1, directed = F)
  graph <- simplify(graph, remove.multiple = T, remove.loops = T)
  deg1<- centr_degree(graph, normalized = T)
  deg1$centralization
}

#Beregner centrality for alle relevante år
cent1995 <- centrality(df, df$dipcon1995)
cent2000 <- centrality(df, df$dipcon2000)
cent2010 <- centrality(df, df$dipcon2010)
cent1975 <- centrality(df, df$dipcon1975)

#Laver funktion, der beregber grpah density
dense <- function(data, year){
  data1 <- data %>% 
    filter(year > 0) %>%
    select(abbrev1, abbrev2)
  graph <- graph.data.frame(data1, directed = F)
  graph <- simplify(graph, remove.multiple = T, remove.loops = T)
  deg2<- graph.density(graph)
}

#Beregner graph density for alle relevante år
dense1995 <- dense(df, df$dipcon1995)
dense2000 <- dense(df, df$dipcon2000)
dense2010 <- dense(df, df$dipcon2010)

