\name{DAG.to.MAG}
\alias{DAG.to.MAG}

\title{
Function to convert a Directed Acyclic Graph (DAG) into a Mixed Acyclic Graph (MAG), obtained via marginalizing or conditioning on latent variables in the DAG.
}
\description{
This function converts a Directed Acyclic Graph (DAG) into a Mixed Acyclic Graph (MAG), obtained via marginalizing or conditioning on latent variables in the DAG, as described in Shipley, B. & Douma, J. Testing piecewise structural equations models in the presence of latent variables,  including correlated errors.
It is typically used along with the function basiSet.mag, which outputs the union basis set of the MAG.
}
\usage{
DAG.to.MAG(full.DAG, latents = NA,conditioning.latents=NULL)
}

\arguments{
  \item{full.DAG}{a named binary matrix encoding the DAG.  This is typically obtained via the DAG function of the ggm library.
}
  \item{latents}{a vector containing the names of the latent variables in the DAG.  The names of these latent variables must match those used in full.DAG.
}
  \item{conditioning.latents}{a vector containing the names of the latent variables in the DAG that are to be conditioned.  The names of these latent variables must match those used in full.DAG and be a subset of the vector "latents".
}
}
\details{
This function inputs a Directed Acyclic Graph (DAG) and outputs the Mixed Acyclic Graph (MAG) that results from marginalizing on those variables (nodes) contained in the vector "latents" and conditioning on the subset of these latents contained in the vector "conditioning.latents"
}
\value{
The DAG and the resulting MAG are printed to the screen and the MAG is output from this function as a matrix.
}
\references{
Shipley, B. & Douma, J.  Testing piecewise structural equations models in the presence of latent variables,  including correlated errors.
}
\author{
  Bill Shipley
}
\seealso{
\code{\link{dsep.test}}, \code{\link{basiSet.mag}}, \code{\link{DAG}}
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
