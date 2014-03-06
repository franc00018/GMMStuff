#' Optimization with constraints for GMM methos
#'
#' @param start Starting values for the parameters and lagrangian
#' @param conditions.vector Vector of moment conditions
#' @param sample Individual data sample
#' @param ... Functions of the vector of moment conditions
#' @param W Weighting matrix
#' @param R Linear constraint matrix of coefficients
#' @param r Linear constraint constants
#' @return une liste contenant le résultat de l'optimisation
#' @export optim.GMM
#' @author François Pelletier
optim.GMM <- function(start,conditions.vector,sample,...,W,R,r)
{
	optim(start,objective.GMM,conditions.vector,sample,...,W,R,r)
}