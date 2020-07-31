\name{basiSet.mag}
\alias{basiSet.mag}

\title{
Function to obtain the union basis set of a Mixed Acyclic Graph (MAG)
}
\description{
This function outputs the union basis set of a Mixed Acyclic Graph (MAG), as defined in Shipley, B. & Douma, J. Testing piecewise structural equations models in the presence of latent variables,  including correlated errors.
It is typically used along with the function DAG.to.MAG, which outputs the MAG.
}
\usage{
basiSet.mag(cgraph)
}

\arguments{
  \item{cgraph}{a matrix holding the Mixed Acyclic Graph, having  edges of 0-->1, 100<-->100 or 10--10
}
}
\details{
This function is typically called after the DAG.to.MAG function.  The DAG.to.MAG function outputs the Mixed Acyclic Graph (MAG) as a matrix, which is then input into basiSet.MAG.  
}
\value{
The union basis set is printed to the screen and is output from this function as a list.
}
\references{
Shipley, B. & Douma, J.  Testing piecewise structural equations models in the presence of latent variables,  including correlated errors.

}
\author{
  Bill Shipley
}
\seealso{
\code{\link{dsep.test}}, \code{\link{DAG.to.MAG}}, \code{\link{DAG}}
}
\examples{
library(ggm)
dag1.ms<-DAG(X~A+L,Y~L,B~Y,order=T)
#This is a call to the main function: DAG.to.MAG
out1 <- DAG.to.MAG(full.DAG=dag1.ms,latents=c("L"),conditioning.latents = NULL)
#This is a call to the function to output the union
#basis set of a MAG
basiSet.mag(out1)
}
