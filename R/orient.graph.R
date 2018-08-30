orient.graph<-function (cgraph, nvars) 
{
    find.undirected.edge <- function(cgraph, nvars) {
        if (nvars < 3) 
            stop("error in find.undirected")
        x1 <- y1 <- 0
        for (i in 1:(nvars - 1)) {
            for (j in (i + 1):nvars) {
                if (cgraph[i, j] == 1 & cgraph[j, i] == 1) {
                  x1 <- i
                  y1 <- j
                  return(data.frame(x1 = x1, y1 = y1))
                }
            }
        }
        data.frame(x1 = x1, y1 = y1)
    }
    old.graph <- cgraph
    is.change1 <- is.change2 <- T
    while (is.change1 | is.change2) {
        new.graph <- orient.phaseII.1(old.graph, nvars)
        if (sum(old.graph != new.graph) == 0) 
            is.change1 <- F
        if (sum(old.graph != new.graph) > 0) 
            is.change1 <- T
        new.edge <- find.undirected.edge(new.graph, nvars)
        if (new.edge$x1 > 0 & new.edge$y1 > 0) {
            new.graph[new.edge$x1, new.edge$y1] <- 2
            old.graph <- new.graph
            is.change2 <- T
        }
        if (new.edge$x1 == 0 & new.edge$y1 == 0) {
            old.graph <- new.graph
            is.change2 <- F
        }
    }
    amat <- new.graph
    amat[new.graph == 1] <- 0
    amat[new.graph == 2] <- 1
    new.graph
}
