\name{optim.GMM}
\alias{optim.GMM}
\title{Optimization with constraints for GMM methos}
\usage{
optim.GMM(start, conditions.vector, num.param, ..., W, R, r)
}
\arguments{
  \item{start}{Starting values for the parameters and
  lagrangian}

  \item{conditions.vector}{Vector of moment conditions}

  \item{number}{of parameters of the distribution}

  \item{...}{Parameters of the vector of moment conditions}

  \item{W}{Weighting matrix}

  \item{R}{Linear constraint matrix of coefficients}

  \item{r}{Linear constraint constants}
}
\value{
une liste contenant le résultat de l'optimisation
}
\description{
Optimization with constraints for GMM methos
}
\author{
François Pelletier
}

