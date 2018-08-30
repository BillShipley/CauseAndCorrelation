Observed.Equivalent.DAG<-function (full.DAG, latents = NA,return.graph=FALSE) 
{
<<<<<<< HEAD
   library(ggm)
=======
>>>>>>> 9a01f99b865ec3191546809100e83cda28fdf641
# needs the ggm library
# full.DAG is a binary (0/1) matrix produced from DAG() function in ggm
# latents is a character vector giving the names of the latents
# return.graph=TRUE if you want the get the inducing path graph matrix
#
# combn gives all unique combinations of a vector, taken
# 2 at a time, and outputs a matrix with each unique combination in a
# column and the total number of columns equal to the total number of
# unique combinations.
#
#pairs.without.edge outputs, as a matrix, the number of pairs of variables in my.graph
# that don't share an edge, with one column
# per pair and the two rows giving the variables in the pair.
  
  is.directed.path<-function(use.dag,start.var,end.var){
    # start.var=character name of first variable in use.dag
    # end.var=character name of second variable in use.dag
    # if there is a directed path between start.var and end.var, and these two variables
    # are not d-separated in use.dag, then there is a directed path between them
    # returns as TRUE or FALSE
     var.names<-colnames(use.dag)
     start.node<-(1:length(var.names))[colnames(use.dag)==start.var]
     end.node<-(1:length(var.names))[colnames(use.dag)==end.var]
     test1<-length(findPath(amat=use.dag,st=start.node,en=end.node))>0
     test2<-!dSep(amat=use.dag,first=start.var,second=end.var,cond=NULL)
     test1 & test2
  }
# pairs.without.edge function
    pairs.without.edge <- function(my.graph) {
        nvars<-dim(my.graph)[2]
        com <- combn(1:nvars, 2)
        ncombs <- dim(com)[2]
        keep <- rep(T, ncombs)
        for (i in 1:ncombs) {
# if(there is an edge between this pair) then remove from com
            if (my.graph[com[1, i], com[2, i]] != 0 |
                my.graph[com[2, i], com[1, i]]!=0) {
                com[1, i] <- com[2, i] <- 0
                keep[i]<-F
            }
        }
        matrix(com[, keep],ncol=sum(keep))
    }

# find.possible.Q outputs a vector listing all other variables from 1:nvars
# except x and y
    find.possible.Q <- function(nvars, x, y) {
        z <- 1:nvars
        z[x] <- z[y] <- 0
        z[z > 0]
    }

# converts indices of variables to variable names; for dSep
dag.name<-function (amat,n) 
{
rownames(amat)[n]
}
#
# main function
#
 full.vars<-row.names(full.DAG)
 full.vars.index<-1:length(full.vars)
 n.observed<-length(full.vars)-length(latents)
 observed.DAG<-full.DAG
 observed.vars<-full.vars
 observed.vars.index<-full.vars.index
 for(i in 1:length(latents)){
  observed.vars[latents[i]==full.vars]<-NA
  observed.vars.index[latents[i]==full.vars]<-NA
  observed.DAG[latents[i]==full.vars,]<-NA
  observed.DAG[,latents[i]==full.vars]<-NA
 }
 latent.vars.index<-match(latents,full.vars)

 cat("the original DAG is:","\n")
 total.n.vars<-dim(full.DAG)[2]
 for(i in 1:(total.n.vars-1)){
  for(j in (i+1):total.n.vars){
   if(full.DAG[i,j]==1 & full.DAG[j,i]==0)cat(full.vars[i],"->",full.vars[j],"\n")
   if(full.DAG[i,j]==0 & full.DAG[j,i]==1)cat(full.vars[j],"->",full.vars[i],"\n")
  }
 }
 if(sum(is.na(latents))>0){
  return(cat("There are no latents; the DAG doesn't change ","\n"))
 }
 if(sum(is.na(latents))==0){
  cat("latent variable(s): ",latents,"\n")
  n.latents<-length(latents)
  for(i in 1:n.latents){
   ok<-F
   for(j in 1:length(full.vars))if(latents[i]==full.vars[j])ok<-T
   if(!ok)return("ERROR: latent variable name not in the DAG")
  }
 }
 cat("_____________________","\n")
 observed.vars<-observed.vars[!is.na(observed.vars)]
 observed.vars.index<-observed.vars.index[!is.na(observed.vars.index)]

#
# construct initial observed DAG by removing latents and conserving directed
# edges between pairs of observed variables
#
# HERE
 if(n.observed<=0)return(cat("No observed variables","\n"))
 if(n.observed==1)return(cat("Only one observed variable","\n"))
 if(n.observed==2)return(cat("Only two observed variables","\n"))

 observed.DAG<-observed.DAG[observed.vars.index,observed.vars.index]

 if(n.observed<=0){
  return(cat("All variables are latent; there is no equivalent observed DAG","\n"))
 }

 pairs.to.test<-pairs.without.edge(observed.DAG)
 # here
 n.pairs.to.test<-dim(pairs.to.test)[2]

 n.remaining<-length(observed.vars)-2
# if all observed variables share an edge then return...
#HERE IS A CHANGE
#if(n.remaining<=0){
 if(n.pairs.to.test<=0){
  return(cat("Since there are only two observed variables, nothing further will be done","\n"))
 }
 add.edge<-matrix(NA,nrow=2,ncol=n.pairs.to.test)
# for each pair (i) to test, determine dsep in full graph given only the observed
#
 kount<-0
# i cycles over each pair that are not adjacent...
 for(i in 1:n.pairs.to.test){
  is.pair.dsep<-F
# get those other observed variables in graph except this pair...
  possible.Q<-find.possible.Q(n.observed,pairs.to.test[1,i],pairs.to.test[2,i])

# Do do unconditional dseparation...
# i.e. conditional order=0
  first.var<-observed.vars.index[pairs.to.test[1,i]]
  second.var<-observed.vars.index[pairs.to.test[2,i]]
  test<-dSep(amat=full.DAG,first=dag.name(full.DAG,first.var),second=dag.name(full.DAG,second.var),cond=NULL)
# if first.var is dsep from second.var then there is no edge between them;
  if(test){
   is.pair.dsep<-T

    next
  }
# if here then there are potential conditional variables to consider
# so cycle through all possible conditional orders...
  if(sum(is.na(possible.Q)==0)){
   n.possible.Q<-length(possible.Q)

#
#now, determine, using observed.vars.index[possible.Q], if the pair are dsep
# in the full graph
# j gives the conditional order for a given pair
   for(j in 1:n.possible.Q){

# Q has column = different combinations and rows=elements in each combination
    Q<-combn(possible.Q,j)

    if(j==n.possible.Q)Q<-matrix(possible.Q,nrow=j,ncol=1)

    n.Q<-dim(Q)[2]

    first.var<-observed.vars.index[pairs.to.test[1,i]]

#   pairs.to.test[1,i],"dag name=",dag.name(full.DAG,first.var),"\n")
    second.var<-observed.vars.index[pairs.to.test[2,i]]

#    pairs.to.test[2,i],"dag.name=",dag.name(full.DAG,second.var),"\n")
# k cycles through these different combinations
    for(k in 1:n.Q){
     cond.vars<-as.vector(observed.vars.index[Q[,k]])
     test<-dSep(amat=full.DAG,first=dag.name(full.DAG,first.var),second=dag.name(full.DAG,second.var),
      cond=dag.name(full.DAG,cond.vars))
# if first.var dsep from second.var then there is no edge...
     if(test){
      is.pair.dsep<-T
      break
     }
    }
   }
  }
  if(!is.pair.dsep){
   kount<-kount+1
   add.edge[1,kount]<-pairs.to.test[1,i]
   add.edge[2,kount]<-pairs.to.test[2,i]
  }
 }
# convert observed DAG to a partially oriented graph
 cgraph<-matrix(0,n.observed,n.observed,dimnames=list(observed.vars,observed.vars))
 for(i in 1:(n.observed-1)){
  for(j in (i+1):n.observed){
   if(observed.DAG[i,j]==1 & observed.DAG[j,i]==0){
    cgraph[i,j]<-2
    cgraph[j,i]<-1
   }
   if(observed.DAG[j,i]==1 & observed.DAG[i,j]==0){
    cgraph[j,i]<-2
    cgraph[i,j]<-1
   } 
  }
 }
 for(i in 1:kount){
  cgraph[add.edge[1,i],add.edge[2,i]]<-cgraph[add.edge[2,i],add.edge[1,i]]<-1
 } 

#cgraph now holds the partially oriented inducing graph, with X--Y if there is an inducing
# path between observed variables X & Y.
#Now, orient these if there are directed paths from i to j
 for(i in 1:n.observed){
   for(j in 1:n.observed){

     if(cgraph[i,j]==1 & cgraph[j,i]==1){
       if(is.directed.path(use.dag=full.DAG,start.var=observed.vars[i],end.var=observed.vars[j])){
# there is an inducing path from i to j
         cgraph[i,j]<-2
       }
     }
   }
# there might be an inducing path from a latent to both
   for(i in 1:n.observed){
     for(j in 1:n.observed){
       n.latents<-length(latents)
       if(cgraph[i,j]==1 & cgraph[j,i]==1){
         
        for(lk in 1:n.latents){
         if(is.directed.path(use.dag=full.DAG,start.var=latents[lk],end.var=observed.vars[i]) &
            is.directed.path(use.dag=full.DAG,start.var=latents[lk],end.var=observed.vars[j])){
           # there is an inducing path from i to j via latent

          cgraph[j,i]<-cgraph[i,j]<-2
         }
        }
       }
     }
   }
   #topOrder (from ggm) gives the topological order of the variables in the DAG
   causal.order<-topOrder(full.DAG)[-latent.vars.index]
   # there might be an inducing path from a latent to both
   for(i in 1:(n.observed-1)){
     for(j in i:n.observed){
       if(cgraph[i,j]==1 & cgraph[j,i]==1){
         if(causal.order[i]>causal.order[j])cgraph[j,i]<-3
         if(causal.order[i]<causal.order[j])cgraph[i,j]<-3
       }
     }
   }
 }
   cat("Inducing path graph involving only the observed variables:","\n")
   ind.vars<-rep(T,n.observed)
   for(i in 1:(n.observed-1)){
     for(j in (i+1):n.observed){
       if(cgraph[i,j]==2 & cgraph[j,i]==1)cat(observed.vars[i],"->",observed.vars[j],"\n")
       if(cgraph[i,j]==1 & cgraph[j,i]==2)cat(observed.vars[j],"->",observed.vars[i],"\n")
       if(cgraph[i,j]==3 & cgraph[j,i]==1)cat(observed.vars[i],"~>",observed.vars[j],"\n")
       if(cgraph[i,j]==1 & cgraph[j,i]==3)cat(observed.vars[j],"~>",observed.vars[i],"\n")
       if(cgraph[i,j]==2 & cgraph[j,i]==2)cat(observed.vars[i],"<->",observed.vars[j],"\n")
       if(cgraph[i,j]>0)ind.vars[i]<-ind.vars[j]<-FALSE
     }
   }
   cat("___________________________","\n")
   cat("X->Y means X that is a direct cause of Y given these observed variables","\n")
   cat("although there could also be latent common causes between them as well.","\n")
   cat("X<->Y means that X and Y are not causes of each other, but share a common latent cause","\n")
   cat("X~>Y means that X is not a direct cause of Y but is an indirect cause through","\n")
   cat("an inducing path involving latents","\n")
   cat("___________________________","\n")
   cat("If there are protected colliders then you might be able to remove some X<->Y","\n")
   cat("___________________________","\n")
   if(return.graph){
     cat("inducing path matrix","\n")
     cgraph
   }
}
