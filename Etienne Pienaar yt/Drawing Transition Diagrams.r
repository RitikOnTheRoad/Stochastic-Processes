rm(list = ls())
# =========================================================
# Goal: Write a function which draws transition diagrams
# for a given t.p.m.

# ========================================================
# Task 1: Call the igraph library and read the documentation
# on the graph.adjacency() and plot.igraph() functions.

library(igraph)
# ========================================================
# Task 2: Use the skeleton below to write a function
# which draws a t.d. for given t.p.m. You may use the 
# graph.adjacency() and plot.igraph() functions. Use this 
# function to draw a t.d. for a MC w t.p.m:

Gamma = rbind(c(1,0,0,0,0,0)/1,
              c(1,2,1,0,0,0)/4,
              c(0,1,2,1,0,1)/5,
              c(0,0,0,1,2,3)/6,
              c(0,0,0,1,0,1)/2,
              c(0,0,0,1,0,3)/4)

draw_TD = function(Gamma)
{
  # Infer dimensions, round Gamma to 2 or 3, name the
  # states of the chain and apply to rows and columns 
  # of Gamma:
  u=dim(Gamma)[1]
  Gamma=round(Gamma,3)
  rownames(Gamma)=paste0("State",1:u)
  colnames(Gamma)=paste0("State",1:u)  
  # Create a matrix of coordinates for the vertices:
  t=seq(1,0,length=u+1)+0.25
  x=1*cos(2*pi*t)[1:u]
  y=1*sin(2*pi*t)[1:u]
  X=cbind(x,y)
  
  # Draw a directed graph with vertices at the above coordinates and 
  # directed edges according to t.p.m.
  net=graph.adjacency(Gamma,mode="directed",weighted=TRUE,diag=TRUE,add.rownames = TRUE)
  
  plot.igraph(net,layout=X,
              vertex.color='grey75',
              vertex.size=50,
              edge.curved=0.3,
              edge.label=E(net)$weight,
              edge.color="Black",
              margin=0.05,
              edge.loop.angle=-pi/4)
  # Return net[] for reference
  
  tkplot(net,layout=X,
         vertex.color='grey75',
         vertex.size=50,
         edge.curved=0.3,
         edge.label=E(net)$weight,
         edge.color="Black",
         margin=0.05,
         edge.loop.angle=-pi/4) ##interactive plot
  
}

draw_TD(Gamma)


