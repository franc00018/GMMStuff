# Optimization for GMM method
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' Optimization for GMM method
#'
#' @param start.value Starting values for the parameters
#' @param conditions.vector Vector of moment conditions
#' @param data Individual data sample
#' @param W Weighting matrix
#' @param ... Functions of the vector of moment conditions
#' @return a list with optimization results
#' @export optim.GMM
#' @author François Pelletier
optim.GMM <- function(start.value,conditions.vector,data,W,...)
{
	optim(start.value,objective.GMM,gr=NULL,conditions.vector,data,W,...)
}