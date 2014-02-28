# Objective function for the GMM method
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' Objective function for the GMM method
#' @param param Vector of parameters to optimize
#' @param conditions.vector Vector of moment conditions
#' @param ... Parameters of the vector of moment conditions
#' @param W Weighting matrix
#' @param R Linear constraint matrix of coefficients
#' @param r Linear constraint constants
#' @return A scalar value
#' 
#' @author François Pelletier
objective.GMM <- function(param.lagrangian,conditions.vector,num.param,...,
		W=diag(length(conditions.vector)),R=0,r=0)
{
	param <- param.lagrangian[1:num.param]
	lagrangian <- param.lagrangian[num.param+1:length(param.lagrangian)]
	colMeans(conditions.vector(param,...)) %*% ginv(W) %*% colMeans(conditions.vector(param,...))+ abs(t(R %*% param - r) %*% lagrangian)
}