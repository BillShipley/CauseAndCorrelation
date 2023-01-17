\name{MAG.to.DAG}
\alias{MAG.to.DAG}

\title{
Function to convert a Mixed Acyclic Graph (MAG) into a Directed Acyclic Graph (DAG) by replacing the dependent error by latent variables.
}
\description{
This function converts a Mixed Acyclic Graph (MAG) into a Directed Acyclic Graph (MAG), by adding a latent for each pair of dependent errors in the MAG. More details can be found in Shipley, B. & Douma, J. Testing piecewise structural equations models in the presence of latent variables,  including correlated errors.
It is typically used to obtain a DAG from which the d-sep equivalent MAG can be derived and its basis-set.
}

\usage{
MAG.to.DAG(full.MAG)
}

\arguments{
  \item{full.DAG}{a named binary matrix encoding the MAG.  This is typically obtained via the DAG function of the ggm library.
}
}
\details{
This function inputs a Mixed Acyclic Graph (MAG) and outputs the Directed Acyclic Graph (MAG). 
}
\value{
The resulting MAG is output from this function as a matrix.
}
\references{
Shipley, B. & Douma, J.  Testing piecewise structural equations models in the presence of latent variables,  including correlated errors.
}
\author{
  Bob Douma
}
\seealso{
\code{\link{dsep.test}}, \code{\link{basiSet.mag}}, \code{\link{DAG},\code{\link{DAG.to.MAG}}}
}
\examples{
library(ggm)
initial.mag <- makeMG(dg=DAG(X3~X1+X2,X4~X3,X5~X4),bg=UG(~X1*X2+~X3*X5))
plotGraph(initial.mag) # plotgraph
my.DAG <- MAG.to.DAG(initial.mag) # resulting DAG
my.mag <- DAG.to.MAG(my.dag,latents=c("L1","L2")) 
plotGraph(my.mag) 
basiSet.mag(my.mag) 
}