\encoding{UTF-8}
\name{dsep.test}
\alias{dsep.test}
\title{
dsep.test
}
\description{
This function impliments the dsep test described in Shipley(2000), given a DAG and a covariance matrix of the variables in the DAG.  It is identical to the shipley.test function in the ggm library, except that, as a default, it only outputs the null probability of the data given the DAG.  This function is meant to be called within other functions.  You should use the shipley.test2 function if you want to get complete output.
}
\usage{
dsep.test(amat, S, n, only.null = F)
}
\arguments{
  \item{amat}{a binary matrix holding the directed acyclic graph; usually produced via the DAG function in the ggm library}
  \item{S}{the covariance matrix of the variables in the DAG}
  \item{n}{the number of independent multivariate observations involved in the covariance matrix}
  \item{only.null}{a logical argument.  If TRUE, only the null probability of the data given the DAG is output; otherwise the output mimics the shipley2.test function}
}
\value{
the null probability
}
\references{
Shipley, B. (2000). A new inferential test for path models based on directed acyclic graphs. Structural Equation Modeling, 7(2), 206–218. \href{http://dx.doi.org/10.1207/S15328007SEM0702_4}{DOI: 10.1207/S15328007SEM0702_4}
}
\author{
 Bill Shipley
}
\examples{
library(ggm)
set.seed(123)
dat <- gen.data()
g <- DAG(B ~ A, C ~ B, D ~ B, E ~ C + D)
dsep.test(amat = g, S = cov(dat), n = 100, only.null = TRUE)
dsep.test(amat = g, S = cov(dat), n=100, only.null = FALSE)
}

