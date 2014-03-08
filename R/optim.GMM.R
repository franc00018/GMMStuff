# Optimization for GMM method
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' Optimization for GMM method
#'
#' @param start Starting values for the parameters
#' @param conditions.vector Vector of moment conditions
#' @param data Individual data sample
#' @param ... Functions of the vector of moment conditions
#' @param W Weighting matrix
#' @return a list with optimization results
#' @export optim.GMM
#' @author François Pelletier
optim.GMM <- function(start,conditions.vector,data,W,...)
{
	optim(start,objective.GMM,conditions.vector,data,W,...)
}