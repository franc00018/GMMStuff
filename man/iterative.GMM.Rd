\name{iterative.GMM}
\alias{iterative.GMM}
\title{Iterative GMM method}
\usage{
iterative.GMM(start, conditions.vector, data, ...,
  W = diag(length(conditions.vector)), R = 0, r = 0,
  lagrangian.start = rep(0, length(conditions.vector)), max.iter = 50,
  epsilon = 1e-06)
}
\arguments{
  \item{start}{Starting values for the parameters and
  lagrangian}

  \item{conditions.vector}{Vector of moment conditions}

  \item{data}{Individual data sample}

  \item{...}{Functions of the vector of moment conditions}

  \item{W}{Weighting matrix}

  \item{R}{Linear constraint matrix of coefficients}

  \item{r}{Linear constraint constants}

  \item{max.iter}{Maximum number of iterations}

  \item{epsilon}{Minimum precision level}
}
\value{
A list containing the optimized vector of parameter and
corresponding covariance matrix
}
\description{
Iterative GMM method
}
\author{
François Pelletier
}

