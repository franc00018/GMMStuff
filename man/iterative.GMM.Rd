\name{iterative.GMM}
\alias{iterative.GMM}
\title{Iterative GMM method}
\usage{
iterative.GMM(start.value, conditions.vector, data, W, ..., max.iter = 50,
  epsilon = 1e-06)
}
\arguments{
  \item{start.value}{Starting values for the parameters and
  lagrangian}

  \item{conditions.vector}{Vector of moment conditions}

  \item{data}{Individual data sample}

  \item{W}{Weighting matrix}

  \item{...}{Functions of the vector of moment conditions}

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

