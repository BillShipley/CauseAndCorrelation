\name{shipley.test2}
\alias{shipley.test2}

\title{
shipley.test2
}
\description{
A more complete version of the ggm library function Shipley.test, which impliments the dsep test.
}
\usage{
shipley.test2(amat, S, n)
}
\arguments{
  \item{amat}{a binary matrix holding the directed acyclic graph; usually produced via the DAG function in the ggm library}
  \item{S}{the covariance matrix of the variables in the DAG}
  \item{n}{the number of independent multivariate observations involved in the covariance matrix}
}
\details{
The test statistic is C = -2 ∑ ln p_j where p_j are the p-values of tests of conditional independence in the basis set computed by basiSet(A) in the ggm library. The p-values are independent uniform variables on (0,1) and the statistic has exactly a chi square distribution on 2k degrees of freedom where k is the number of elements of the basis set. Shipley (2002) calls this test Fisher's C test.
This is simply the shipley.test function in the ggm library, updated to also output the union basis set of the DAG and the associated null probabilities.  The statistical tests are based on Pearson (partial) correlations, and so assume multivariate normality and linearity of responses.  By converting the observations to ranks and inputting the covariance matrix of these ranks, you convert the inferential tests to ones based on Spearman (partial) correlations.
}
\value{
Each d-separation claim: X_||_Y|[{Q}, the associated correlation coefficient, and the null probability.

ctest:	Test statistic C

df:	Degrees of freedom

pvalue:	The P-value of the test, assuming a two-sided alternative
}
\references{
Shipley, B. (2000). A new inferential test for path models based on directed acyclic graphs. Structural Equation Modeling, 7(2), 206–218. \href{http://dx.doi.org/10.1207/S15328007SEM0702_4}{DOI: 10.1207/S15328007SEM0702_4}
}
\author{
  Bill Shipley
}
\seealso{
\code{\link{shipley.test}}, \code{\link{Pcor.prob}}
}
\examples{
library(ggm)
set.seed(123)
dat <- gen.data()
g <- DAG(B ~ A, C ~ B, D ~ B, E ~ C + D)
shipley.test2(amat = g, S = cov(dat), n = 100)
}
