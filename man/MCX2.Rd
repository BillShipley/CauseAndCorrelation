\name{MCX2}
\alias{MCX2}
\title{
Monte Carlo Chi-square simulator
}
\description{
The maximum likelihood chi-square statistic that is commonly calculated in structural equations modelling only asymptotcially follows a theoretical chi-squared distribution; with small sample sizes it can deviate enough from the theoretical distribution to make it problematic.This function estimates the empirical probability distribution of the Maximum Likelihood Chi-Square statistic, given a fixed sample size and degrees of freedom, and outputs the estimated null probability given this sample size and degrees of freedom.
}
\usage{
MCX2(model.df, n.obs, model.chi.square, n.sim = 10000) 
}
\arguments{
  \item{model.df}{the number of degrees of freedom of your model}
  \item{n.obs}{the number of independent observations in your data}
  \item{model.chi.square}{the calculated maximum-likelihood chi-square statistic}
  \item{n.sim}{the number of Monte Carlo runs that you want}
}
\details{
Typically, you will have fit your structural equations model using lavaan and using the maximum likelihood estimator, or its robust equivalent.  This will give you the model df and the maximum likelihood chi-square statistic (or its robust equivalent).  To obtain a less biased estimate of the null probability given your sample size, you would enter these values into this function.  The larger the value of n.sim, the more precise the resulting estimate of the null probability, but the longer it will take.  The default value should be fine for most cases.
}
\value{
the estimated null probability using the theoretical chi-square distribution (MLprobability) and the Monte Carlo estimate, correcting for the sample size (MCprobability.  Also shown is a plot of the theoretical and empirical distributions.
}
\author{
  Bill Shipley
}
\examples{
MCX2(model.df=3,n.obs=20,model.chi.square=5.2,n.sim=10000)
}

