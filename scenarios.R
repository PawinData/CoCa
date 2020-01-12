library(igraph)

CENTERS <- rbind(c(-1,0),c(1,0),c(0,1))
g1 <- graph(c(2,1), n=2)
V(g1)$label <- c("X","Y")
g2 <- graph(c(2,1, 1,3, 2,3), n=3)
V(g2)$label <- c("X","Y","Z")
g3 <- graph(c(1,3, 2,3), n=3)
V(g3)$label <- c("X","Y","Z")

par(mfrow=c(1,3))
plot(g1, layout=CENTERS[1:2,], axes = FALSE, rescale=FALSE, 
     edge.arrow.mode=1, edge.color="black", edge.width=0.1,
     vertex.label=V(g1)$label, vertex.color="bisque", vertex.size=50,
     main = "Purely Causal")
plot(g2, layout=CENTERS, axes = FALSE, rescale=FALSE,
     edge.arrow.mode=1, edge.color="black", edge.width=0.1,
     vertex.label=V(g2)$label, vertex.color="white", vertex.size=50,
     main = "Generic")
plot(g3, layout=CENTERS, axes = FALSE, rescale=FALSE,
     edge.arrow.mode=1, edge.color="black", edge.width=0.1,
     vertex.label=V(g3)$label, vertex.color="darkolivegreen1", vertex.size=50,
     main = "Purely Confounded")