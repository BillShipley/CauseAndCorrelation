# the functions below factorize a path model presented as DAG or MAG into
# conditionally independent parts that can each be optimized separately. 

# functions are explained in the forthcoming paper of Douma & Shipley - 
# A GENERIC SOLUTION TO TESTING MODEL FIT IN PATH MODELS WITH CORRELATED ERRORS GIVEN NON-NORMALITY, NON-LINEARITY AND HIERARCHICAL DATA
# submitted


# function to find unique district sets
districtSet <- function(x){
  
  if (sum(x==10) > 0){
    print("Stopped because graph contains undirected edges") 
  } else {
    # remove directed paths
    x[x==1] <- 0  
    count <- ncol(x)
    dis <- list()
    # get all districts
   # names <- colnames(x)
    for (i in 1:ncol(x)){
      # which vertices are in the same district set?
      indx <- which(x[,i]==100)
    #  indx <- names[which(x[,i] == 100)]
      dis[[i]] <- c(i,t(indx))
    #  dis[[i]] <- c(names[i],t(indx))
    }
   # browser()
    for (i in 1:ncol(x)){
      for (j in 1:ncol(x)){
     #   browser()
        if (length(intersect(dis[[i]],dis[[j]]))>0){
          dis[[i]] <- unique(c(dis[[i]],dis[[j]]))
          if (i!=j){
            dis[[j]] <- numeric(0)   
          }
          
        }
      }
    }
    dis <- dis[lapply(dis,length)>0]     
    dis <- lapply(dis,function(y) colnames(x)[y])
  }
  return(dis)
}

"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y

# function that return the graph of a corresponding district set, including the external parents
districtGraph <- function(x,mg){
 # browser()
  if (length(x)>1){
    subgraph <- mg[,colnames(mg) %in% x]
    subgraph <- subgraph[-which(apply(subgraph,1,sum) == 0),]
    # select exogenous parents
    pa <- rownames(subgraph) %w/o% x 
    subgraph <- mg[colnames(mg) %in% c(x,pa),colnames(mg) %in% c(x,pa)]
    subgraph[,colnames(subgraph) %in% pa] <- 0
    colnames(subgraph)[colnames(subgraph) %in% pa] <- paste("epa",colnames(subgraph)[colnames(subgraph) %in% pa])
    rownames(subgraph)[rownames(subgraph) %in% pa] <- paste("epa",rownames(subgraph)[rownames(subgraph) %in% pa])
  } else {
    subgraph <- mg[,colnames(mg) %in% x]
    pa <- rownames(mg)[which(subgraph ==1)]
    #pa <- paste("epa",pa,sep="_")
    if (length(pa)==0){
      print(paste("variable",x,"is independent of all other variables"))
    } else {
    subgraph <- mg[colnames(mg) %in% c(x,pa),colnames(mg) %in% c(x,pa)]
    colnames(subgraph)[colnames(subgraph) %in% pa] <- paste("epa",colnames(subgraph)[colnames(subgraph) %in% pa])
    rownames(subgraph)[rownames(subgraph) %in% pa] <- paste("epa",rownames(subgraph)[rownames(subgraph) %in% pa])
    
    }
  }
  return(subgraph)
}



