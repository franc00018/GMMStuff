\name{iterative.GMM}
\alias{iterative.GMM}
\title{Iterative GMM method}
\usage{
iterative.GMM(start, conditions.vector, sample, ..., W, R, r, max.iter = 50,
  epsilon = 1e-06)
}
\arguments{
  \item{start}{Starting values for the parameters and
  lagrangian}

  \item{conditions.vector}{Vector of moment conditions}

  \item{sample}{Individual data sample}

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

