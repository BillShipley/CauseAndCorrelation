# function to create a DAG with latents from a MAG (containing directed paths and bidirected paths)
# Bob Douma 16-01-2023


library(ggm)


initial.mag <- makeMG(dg=DAG(X3~X1+X2,X4~X3,X5~X4),bg=UG(~X1*X2+~X3*X5))
plotGraph(initial.mag)


MAG.to.DAG <- function(x){
  index = which(x==100,arr.ind=T)
  n <- nrow(index)/2 # pairs of depedent errors
  new.DAG <- matrix(0,ncol=ncol(x)+n,nrow=nrow(x)+n) # new DAG including latent variables
  new.DAG[1:ncol(x),1:nrow(x)]<-x
  # add names to matrix
  colnames(new.DAG) <- c(colnames(x),paste("L",1:n,sep=""))
  rownames(new.DAG) <- c(colnames(x),paste("L",1:n,sep=""))
  new.DAG[(nrow(x)+1):(nrow(x)+n),] <- 0 # the latents do not have causal parents, so set to zero
  # find pairs of observed variables caused by the same latent
  cat <- apply(index,1,paste,collapse="")
  cat2 <- apply(index[,c(2,1)],1,paste,collapse="")
  match <- match(cat,cat2) # matrix row number that has the second pair of the dependent error
  # replace dependent error by directed path from Latent
  no = 1
  for (i in 1:n){
    new.DAG[ncol(x)+no,index[i,1]] <- 1
    new.DAG[ncol(x)+no,index[match[i],1]] <- 1
    no = no+1
    index <- index[-match[i],]
    if (no > n){stop}
  }
  new.DAG[new.DAG==100] = 0
  return(new.DAG)
  
}

test <- MAG.to.DAG(initial.mag)
plotGraph(test)


initial.mag <- makeMG(dg=DAG(X3~X1+X2,X4~X3,X5~X4),bg=UG(~X1*X2+~X3*X5+~X4*X2))

plotGraph(MAG.to.DAG(initial.mag))
