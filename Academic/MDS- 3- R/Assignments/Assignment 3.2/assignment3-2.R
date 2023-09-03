#setting up working directory 
setwd("C:/Users/ramom/Desktop/MDS/Academic/1st Semester/MDS- 3- R/Assignments/Assignment 3.2")
getwd()

#1.Load the attached "termDocMatrix.rdata" file in R/R studio
load("Ram Krishna Pudasaini - termDocMatrix.rdata")

#2.Check the structure of this data, we can use the str() function:
str(termDocMatrix)
#View(termDocMatrix)
dim(termDocMatrix)

#3.Inspect the first 10 rows and first 10 columns of this data using sub-setting
termDocMatrix[1:10,1:10]

#4.Convert the loaded termDocMatrix data to as matrix with appropriate R code
termMatrix <- as.matrix(termDocMatrix)

#5.Change this matrix to a Boolean matrix with same name 
#(Hint: [termDocMatrix >=1] < 1)
#termMatrix <- (termMatrix >= 1)
#print(termMatrix)

termMatrix[termMatrix >= 1] <- 1
print(termMatrix)

#6.Transform this matrix to a term-term adjacency matrix as termMatrix object 
#(Hint: Use %*% and t(termDocMatrix)
#converting to term adjacency matrix using matrix multiplication and 
#transposing the matrix with t() function 
termAdjMatrix <- termMatrix %*% t(termMatrix)
print(termAdjMatrix)

#7.Inspect the first 10 rows and first 10 columns of this matrix using sub-setting
termAdjMatrix[1:10,1:10]

#8.Define a graph object g of termMatrix using graph.adjacency function of 
#"igraph" package with weighted=T and mode="undirected" arguments in the function
library(igraph)
g <- graph.adjacency(termAdjMatrix, weighted = TRUE, mode = "undirected")
plot(g)


#9.Remove loops of g using simplify function
g <- simplify(g, remove.loops = TRUE)
plot(g)


#10.Set vertices labels with names of g
V(g)$label <- V(g)$name
plot(g)

#11.Set degree with degree of g
degree <- degree(g)

#12.Find degree of g for 'all", "in" and "out" nodes and interpret them carefully
(degree_all <- degree(g, mode = "all"))
(degree_in <- degree(g, mode = "in"))
(degree_out <- degree(g, mode = "out"))

#13.Find diameter of g and interpret it carefully
diameter <- diameter(g)
diameter

#14.Find edge density of g and interpret it carefully
(edge_density <- edge_density(g))

#15. Find reciprocity of g and interpret it carefully
(reciprocity <- reciprocity(g))

#16.closeness between terms
(closeness <- closeness(g))

#17.Find between of g and interpret it carefully
(betweenness <- betweenness(g))

#18.Plot histogram of node degree and interpret it carefully
hist(degree, xlab = "Degree", main = "Histogram of Node Degree")

#19. Set Seed 
set.seed(26)

#20.plot g with vertex.size of degree*0.4 and interpret it carefully
plot(g, vertex.size = degree*0.4)

#21.plot g with fruchterman.reingold layout and interpret it carefully
plot(g, layout = layout.fruchterman.reingold, vertex.size = degree(g) * 0.4)

#22.plot g with kamada.kawai layout and interpret it carefully
plot(g, layout = layout.kamada.kawai, vertex.size = degree(g) * 0.4)


#23.plot hub and authority of g and interpret them carefully
hubs <- hub_score(g)
hubVector <- hubs$vector
auths <- authority_score(g)
authVector <- auths$vector
plot(hubVector, type = "l", xlab = "Nodes", ylab = "Hub Scores")
plot(authVector, type = "l", xlab = "Nodes", ylab = "Authority Scores")


#24.Plot community cluster of g and interpret it carefully
communities <- cluster_edge_betweenness(g)
plot(communities, g)

#25.Write a summary of SNA for the twitter data
#Overall, this analysis demonstrates how SNA can be used to gain insights into 
#the structure and properties of complex networks, such as social media interactions.


