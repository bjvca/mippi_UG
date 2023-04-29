# this script is for social network analysis to depict the 
# varieties that maize consumers identified as the most suitable 
# for the consumption-related traits that they prefer.

install.packages("igraph")
library(igraph)

setwd("C:/Users/LNabwire/OneDrive - CGIAR/L.Nabwire/git/Maize_Uganda/mippi_UG/Qualitative_work/Qualitative_consultations/3_Paper")
getwd()

path <- getwd()

dta <- read.csv(paste(path,"data_traits_variety_connection.csv", sep="/"))
names(dta)

dta1 <- data.frame(dta$Consumption_trait, dta$variety) # picked only 2 variables
network <- graph.data.frame(dta1, directed = T)
V(network) # Nodes
E(network) # Edges
V(network)$label <- V(network)$name
V(network)$degree <- degree(network)

# the network map between traits and varieties
set.seed(222)
plot(network,
     vertex.color = "green",
     vertex.size = 8,
     vertex.label.dist = 1.5,
     vertex.label.color = "black",
     edge.arrow.size = 0.2,
     vertex.label.cex = 0.8,
     layout=layout.circle)

# Highlighting degrees and layouts
plot(network,
     vertex.color = rainbow(22),
     vertex.size = V(network)$degree*0.6,
     vertex.label.color = "black",
     vertex.label.dist = 1.5,
     edge.arrow.size = 0.2,
     layout=layout.circle)
