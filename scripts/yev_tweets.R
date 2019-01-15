library(pacman)
p_load(igrpah, dplyr)


tweets <- read.csv("http://nodeassets.nbcnews.com/russian-twitter-trolls/tweets.csv",
                   stringsAsFactors = F, sep = ",")



#selecting only the retweets
rts <- grep("^rt @[a-z0-9_]{1,15}", tolower(tweets$text), perl=T, value=T)
# extracting handle names for the senders (those who retweet)
rt.sender <- tolower(as.character(tweets$user_key[grep("^rt @[a-z0-9_]{1,15}", tolower(tweets$text), perl=T)]))
# extracting handle names for the recievers (those who are being retweeted)
rt.receiver<- tolower(regmatches(rts, regexpr("@(?U).*:", rts)))
rt.receiver <- (gsub(":", "", rt.receiver)) #removing ":"
rt.receiver <- (gsub("@", "", rt.receiver)) #removing "@"
### Registering empty entries as missing
rt.sender[rt.sender==""] <- "<NA>"
rt.receiver[rt.receiver==""] <- "<NA>"
# a large proportion of this code is from <https://www.r-bloggers.com/generating-graphs-of-retweets-and-messages-on-twitter-using-r-and-gephi/>


#storing reciever and sender handle names in one dataframe and removing duplicates
handle.all <- unique(as.data.frame(c(rt.sender, rt.receiver))) 
#renaming the handle names variable
handle.all <- handle.all %>% rename(handle = "c(rt.sender, rt.receiver)")


# importing handle names from the official list release in congress
trolls_official <-  read.csv("http://golovchenko.github.io/data/trollhandles.txt", stringsAsFactors = F)
# merging the complete list of official troll handle names with the ones in NBC data
tweets <- tweets %>% rename(handle = user_key) #renaming handle name variable
handles <- tweets %>% select(handle) #selecting only the handles from the data
handles <- rbind(trolls_official, handles)
handles.u <- unique(handles) #removing duplicates
handles.u$troll <- "troll" #assigning all of these users a trolls
### matching trolls with the complete set of handle names in the retweet network
nodes <- right_join(handles.u, handle.all)
nodes <- replace(nodes, is.na(nodes), "non-troll") # now we have a variable indicating wether a user is a troll

### Creating a data frame from the sender-receiver objects
rts.df <- data.frame(rt.sender, rt.receiver)
### creating the retweetnetwork based on the sender-receiver df and the node attributes (troll/non-troll)
rts.g <- graph.data.frame(rts.df, directed=T, vertices = nodes)
### removing self-ties
rts.g <-simplify(rts.g, remove.loops = T, remove.multiple = F)

### creating the retweetnetwork based on the sender-receiver df and the node attributes (troll/non-troll)
rts.g <- graph.data.frame(rts.df, directed=T, vertices = nodes)
### removing self-ties
rts.g <-simplify(rts.g, remove.loops = T, remove.multiple = F)

# removing multiple edges between users
g <- simplify(rts.g, remove.multiple = T, remove.loops = T)
# creating a data frame with weighted and unweighted degree centrality for each profile
df <- data.frame(name =V(g)$name,
                 troll= V(g)$troll,indegree=degree(g,mode='in'),
                 indegree_weighted = degree(rts.g, mode ="in"),
                 outdegree=degree(g,mode='out'),
                 outdegree_weighted = degree(rts.g, mode = "out"))
#ranking users by indegree
rank.indegree <- df %>% select(name, troll, indegree,
                               indegree_weighted) %>% arrange(-indegree)

#ranking users b weigted indegree n users * n retweets
rank.indegree.w <- df %>% select(name, troll, indegree,
                                 indegree_weighted) %>% arrange(-indegree_weighted)

install.packages(knitr)
library(knitr)

kable(rank.indegree[1:10,], caption = "Top 10 profiles ranked by indegree")
