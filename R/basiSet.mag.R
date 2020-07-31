# Function to accompany Shipley, B. & Douma, J. Douma. 
#Testing piecewise structural equations models in 
#the presence of latent variables, 
#including correlated errors.
#basis Set for MAG
basiSet.mag<-function(cgraph){
  #cgraph has existing edges of 0-->1, 100<-->100 or 10--10
  #mag will have existing edges of 0-->1, 100<-->100 or 10--10
  
  cat("Basis Set for MAG:","\n")
  cat("I(X,Y|Z) means X is m-separated from Y given the set Z in the MAG",
      "\n")
  mag<-cgraph
  nod<-rownames(mag)
  dv<-length(nod)
  ind<-NULL
  test<-NULL
  for(r in 1:dv){
    for(s in r:dv){
      #if there is an element>0 in column s then r & s are adjacent
      #in mag
      if((mag[r,s]!=0) | (mag[s,r]!=0) | r==s)
        next
      else{
        test<-1
        ed<-nod[c(r,s)]
        pa.r<-nod[mag[,r]==1]
        pa.s<-nod[mag[,s]==1]
        msep<-union(pa.r,pa.s)
        msep<-setdiff(msep,ed)
        b<-list(c(ed,msep))
        ind<-c(ind,b)
        cat("I(",ed[1],",",ed[2],"|",msep,")","\n")
      }
    }
  }
  if(is.null(test))cat("No elements in the basis set","\n")
  return(ind)
}
