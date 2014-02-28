\name{objective.GMM}
\alias{objective.GMM}
\title{Objective function for the GMM method}
\usage{
objective.GMM(param.lagrangian, conditions.vector, num.param, ...,
  W = diag(length(conditions.vector)), R = 0, r = 0)
}
\arguments{
  \item{param}{Vector of parameters to optimize}

  \item{conditions.vector}{Vector of moment conditions}

  \item{...}{Parameters of the vector of moment conditions}

  \item{W}{Weighting matrix}

  \item{R}{Linear constraint matrix of coefficients}

  \item{r}{Linear constraint constants}
}
\value{
A scalar value
}
\description{
Objective function for the GMM method
}
\author{
François Pelletier
}

