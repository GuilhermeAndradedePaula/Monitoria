library(igraph)
#rm(list = ls()) # Remove all the objects we created so far.

g1 <- graph( edges=c(1,2, 2,3, 3, 1), n=3, directed=F ) 

plot(g1) # A simple plot of the network - we'll talk more about plots later
class(g1)
g1
g2 <- graph( edges=c(1,2, 2,3, 3, 1), n=10 )

plot(g2)  
g2
g3 <- graph( c("John", "Jim", "Jim", "Jill", "Jill", "John")) # named vertices

# When the edge list has vertex names, the number of nodes is not needed

plot(g3)
g3
g4 <- graph( c("John", "Jim", "Jim", "Jack", "Jim", "Jack", "John", "John"), 
             
             isolates=c("Jesse", "Janis", "Jennifer", "Justin") )  

# In named graphs we can specify isolates by providing a list of their names.



plot(g4, edge.arrow.size=.5, vertex.color="gold", vertex.size=15, 
     
     vertex.frame.color="gray", vertex.label.color="black", 
     
     vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2) 
plot(graph_from_literal(a---b, b---c)) # the number of dashes doesn't matter
plot(graph_from_literal(a--+b, b+--c))
plot(graph_from_literal(a+-+b, b+-+c)) 
plot(graph_from_literal(a:b:c---c:d:e))
gl <- graph_from_literal(a-b-c-d-e-f, a-g-h-b, h-e:f:i, j)

plot(gl)
E(g4) # The edges of the object
V(g4) # The vertices of the object

g4[]
g4[1,] 
V(g4)$name # automatically generated when we created the network.
V(g4)$gender <- c("male", "male", "male", "male", "female", "female", "male")

E(g4)$type <- "email" # Edge attribute, assign "email" to all edges

E(g4)$weight <- 10    # Edge weight, setting all existing edges to 10
edge_attr(g4)
vertex_attr(g4)
graph_attr(g4)
#ou
g4 <- set_graph_attr(g4, "name", "Email Network")

g4 <- set_graph_attr(g4, "something", "A thing")



graph_attr_names(g4)
graph_attr(g4, "name")
graph_attr(g4)
g4 <- delete_graph_attr(g4, "something")

graph_attr(g4)
plot(g4, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1.5,
     
     vertex.color=c( "pink", "skyblue")[1+(V(g4)$gender=="male")] ) 
