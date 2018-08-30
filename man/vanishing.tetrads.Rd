\name{vanishing.tetrads}
\alias{vanishing.tetrads}
\title{
Vanishing Tetrads Algorithm
}
\description{
For each unique set of four variables in the input data, this function tests if these four variables possess "vanishing" tetrads.  A tetrad is a product of two correlations that exactly equals a second product of two correlations, each  involving the same four variables.  Each set of four variables has three possible tetrad equations (tetrad triplets). If a tetrad vanishes, but every variable of the four is dependent on every other, conditional on every possible set of the other variables, then there must be a latent variable generating these correlations.  This is the Tetrad Representation Theorem whose proof is given in Spirtes et al. (1993), page 196-197.
}
\usage{
vanishing.tetrads(dat, sig = 0.05)
}
\arguments{
  \item{dat}{a matrix or data frame containing only numeric values}
  \item{sig}{the asymptotic significance level for testing the equality of the tetrad components}
}
\details{
The test assumes multivariate normality and linearity.  It is also an asymptotic test and so is biased in small samples.  
}
\value{
Each unique tetrad is tested and the outcome given along with the null probability.
}
\references{
Spirtes, P., Glymour, C., Scheines, R. 1993.  Causation, prediction, and search. Lecture Notes in Statistics 81.  Springer-Verlag.

Shipley, B. 2016. Cause and correlation in biology: A user's guide to path analysis, structural equations and causal inference in R. Cambridge University Press.
}
\author{
  Bill Shipley
}
\examples{
set.seed(123)
dat <- gen.data()
vanishing.tetrads(dat[,c(1,3,4,5)])
}

