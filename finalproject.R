# final project 3 for Michael Han and Pim Silpacharn
#install.packages('sand')
library('sand')
require(igraph)
data(hc)

# part 1: data preprocessing

# create data frame "nodes" that contains nodes.id, nodes.type, and nodes.type2
nodes <- as.data.frame(hc$ID1, c("nodes.id", "nodes.type", "nodes.type2"))
# create adjacency matrix based on hc

# transfer matrix to data frame called links using
graph.adjacency()
get.data.frame()

# based on "nodes" and "links", create igraph object called "net"
net <- graph.data.frame(links, nodes, directed=F) 
net

# part 2: network plot and decoration

# plot network "net" using favorite layout
plot(net) 

# decorate network plot by -
# color vertex according to 4 node types
##### change everything from gender to node.type #########
# Generate colors base on gender:
colrs <- ifelse(V(net)$gender =='F', 'tomato', 'gold')
V(net)$color = colrs
V(net)$size <- 9+(deg)^(5/6)        # resize vertex size according to degree centrality
V(net)$label <- V(net)$short.name   # use NA to remove label. removes labels of vertices??
E(net)$width = 2                    # adjust edge width according to edge weights

# add legend to plot to annotate node types, change female/male to node types
legend(x=-1.5, y=-1.1, c("Female","Male"), pch=21,
       col="white", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

# report decorated network plot
x11()
plot(net, edge.color="orange", vertex.label.cex=.7,
     vertex.label.color="black", vertex.frame.color='white',
     main='Social media network in stat4004')  # change plot title

# part 3: calculate network centralities

# calculate degree, betweenness, closeness and eigenvector centrality measurers for each node
degree()
betweenness()
closeness()
eigen_centrality()

# from lecture 20
deg = degree(net)
between = betweenness(net)
close = closeness(net)
eig = eigen_centrality(net)$vector
all_centrality = data.frame(V(net)$full.name, deg, between, close, eig)
View(all_centrality)

# plot histograms on a 2 by 2 graphic device
x11()
par(mfrow=c(2,2))
hist(deg, breaks=15, main='histogram of the degree centrality')
hist(between, breaks=15, main='histogram of the betweeness centrality')
hist(close, breaks=15, main='histogram of the closeness centrality')
hist(eig,breaks=15,main='histogram of the eigenvector centrality')

# part 4: detect communities
library(igraph)
library(igraphdata)
data(karate) # change to hc
class(karate)

V(karate)$name
V(karate)$label
V(karate)$color

plot(karate, vertex.color=NA)
# plot network by coloring nodes according to community membership
commu = cluster_louvain(karate) 

length(commu)
sizes(commu)
modularity(commu)
memb <- membership(commu)
names(memb[memb == 2])

x11()
plot(karate, vertex.color = memb, layout = layout.auto, vertex.size = 10,
     edge.width = (E(karate)$weight)^(1/5))
title("Communities detected by Louvain Algorithm")

# Color the community using shades. 
plot(karate, vertex.color= memb, 
     mark.groups=list(which(memb==1),which(memb==2), which(memb==3), which(memb==4)),
     mark.border=NA)

x11()
plot(karate, vertex.color = memb, layout = layout.auto, vertex.size = 10,
     edge.width = (E(karate)$weight)^(1/5), 
     mark.groups=list(which(memb==1),which(memb==2), which(memb==3), which(memb==4)),
     mark.border=NA)
title("Communities detected by Louvain Algorithm")

# report plot, modularity value, # of communities detected and size of each community
