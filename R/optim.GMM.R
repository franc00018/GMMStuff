#' Optimization with constraints for GMM methos
#'
#' @param start Starting values for the parameters and lagrangian
#' @param conditions.vector Vector of moment conditions
#' @param number of parameters of the distribution
#' @param ... Parameters of the vector of moment conditions
#' @param W Weighting matrix
#' @param R Linear constraint matrix of coefficients
#' @param r Linear constraint constants
#' @return une liste contenant le résultat de l'optimisation
#' @author François Pelletier
optim.GMM <- function(start,conditions.vector,num.param,...,W,R,r)
{
	optim(c(start,objective.GMM,conditions.vector,num.param,...,W,R,r))
}