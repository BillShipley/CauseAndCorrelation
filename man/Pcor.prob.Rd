\name{Pcor.prob}
\alias{Pcor.prob}
\title{
A function to calculate the null probability of a Pearson's partial correlation coefficient.
}
\description{
Given a matrix or data frame of multivariate observations (dat), one enters the column numbers (x, y) of the two variables whose partial correlation is sought and the set of column numbers of the other conditioning variables (Q).  The Pearson partial correlation between the variables in columns x and y, conditional on the variables in the columns included in Q, is calculated and the null probability is calculated via a Student's t distribution.  The result therefore assumes multivariate normality and linearity between the variables.
Thus function, as written, is meant to be called by other functions.
}
\usage{
Pcor.prob(dat, x, y, Q)
}
\arguments{
  \item{dat}{a matrix or data frame containing the multivariate observations}
  \item{x}{column number of the first variable in the pair}
  \item{y}{column number of the second variable in the pair}
  \item{Q}{a vector of column numbers of the conditioning variables}
}
\details{
This function calculates the partial correlation by inverting the covariance matrix constructed 
}
\value{
The null probability associated with the Pearson partial correlation
}
\author{
  Bill Shipley
}
\examples{
# Partial correlation between variables in columns 1 and 3, 
# conditional on the variables in columns 2 and 5.

set.seed(123)
dat <- gen.data()
Pcor.prob(dat = dat, x = 1, y = 3, Q = c(2, 5))
}

