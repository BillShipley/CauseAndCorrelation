\name{Exploratory.path.analysis}
\alias{Exploratory.path.analysis}
\title{
Temporary title
}
\description{
This function impliments the CI (Causal Inference) Algorithm of Pearl (2009).  It takes  multivariate observations (3 or more variables) as input,  determines the partly directed dependency graph that corresponds to the patterns of correlation and partial correlation in the input data, and then attempts to completely orient it to produce a d-separation equivalent DAG.  It then tests this DAG against the data using the chosen significance level.  The algorithm begins with a severe requirement for d-separation (alpha=0.01) and increases this incrimentally, up to the value specified in "upper.bound" and stops when it finds a DAG that does not contradict the data.
}
\usage{
Exploratory.path.analysis(dat, upper.bound = 0.5, significance.level = 0.05) 
}
\arguments{
  \item{dat}{a matrix or data frame containing only numeric variables}
  \item{upper.bound}{the highest alpha significance level used to determin if a d-seperation claim is true or not}
  \item{significance.level}{the significance level used to test the SAG}
}
\value{
A partially oriented graph and an equivalent DAG, along with the null probability
}
\references{
Pearl, J. 2009.  Causality. Models, Reasoning , and Inference (2nd edition). Cambridge University Press.
Shipley, B. 2016. Cause and Correlation in Biology: A user's guide to path analysis, structural equations and causal inference in R. Cambridge University Press.
}
\author{
  Bill Shipley
}
\seealso{
\code{\link{Causal.Inference}}
}
\examples{
set.seed(123)
dat <- gen.data()
Exploratory.path.analysis(dat)
}

