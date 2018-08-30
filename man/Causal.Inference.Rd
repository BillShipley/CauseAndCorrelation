\name{Causal.Inference}
\alias{Causal.Inference}

\title{
Causal Inference function
}
\description{
This function impliments the CI (Causal Inference) Algorithm of Pearl (2009).  It takes the multivariate observations (3 or more variables) as input and determines the partly directed dependency graph that corresponds to the patterns of correlation and partial correlation in the input data.
}
\usage{
Causal.Inference(dat, alpha.reject = 0.05, write.result = T)
}

\arguments{
  \item{dat}{a matrix or data frame containing only numeric variables}
  \item{alpha.reject}{the significance level used in deciding if a d-separation claim exists or not}
  \item{write.result}{a logical argument; if FALSE only the binary output matrix is written.  If TRUE, the partially oriented dependency graph is output}
}
\details{
As currently written, the funtiton assumes multivariate normality of the input data (so all variables must be numeric) and the statistical inferences are obtained via Pearson (partial) correlation coefficients.  Alternatively, if your input data are in the form of ranks (for example, via the ranks() function), then Spearman rank (partial) correlations are used.  This function only obtains the output for the given alpha.reject value and is called by the Exploratory.path.analysis function, which is the function that you should normally use.
}
\value{
Either a binary matrix (if write.result=F), or the partially oriented dependency graph (if write.result=T).  A "0", "1", or "2" in the binary matrix means "no edge", "undirected edge", or "edge into the column variable", respectively.
A -- B means an indirected edge between variables A and B
A --> B means an edge from A into B.
}
\references{
Pearl, J. 2009.  Causality. Models, Reasoning , and Inference (2nd edition). Cambridge University Press. \href{http://www.cambridge.org/catalogue/catalogue.asp?isbn=9780521895606}{ISBN: 9780521895606}

Shipley, B. 2016. Cause and Correlation in Biology: A user's guide to path analysis, structural equations and causal inference in R. Cambridge University Press. \href{http://www.cambridge.org/catalogue/catalogue.asp?isbn=9781107442597}{ISBN: 9781107442597}
}
\author{
  Bill Shipley
}
\seealso{
\code{\link{dsep.test}}, \code{\link{Pcor.prob}}, \code{\link{shipley.test2}}
}
\examples{
set.seed(123)
dat <- gen.data()
Causal.Inference(dat = dat, alpha.reject = 0.05, write = TRUE)
}
