# Objective function for the GMM method
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' Objective function for the GMM method
#' @param param Vector of parameters
#' @param conditions.vector Vector of moment conditions
#' @param data Individual data sample
#' @param ... Functions of the vector of moment conditions
#' @param W Weighting matrix
#' @return A scalar value
#' @export objective.GMM
#' @author François Pelletier
objective.GMM <- function(param,conditions.vector,data,W,...)
{
	as.vector(colMeans(conditions.vector(param,data,...)) %*% ginv(W) %*% colMeans(conditions.vector(param,data,...)))
}