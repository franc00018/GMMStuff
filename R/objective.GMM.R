# Objective function for the GMM method
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' Objective function for the GMM method
#' @param conditions.vector Vector of moment conditions
#' @param ... Parameters of the vector of moment conditions
#' @param W Weighting matrix
#' @return A scalar value
#' 
#' @author François Pelletier
obj.gmmGAL.mu <- function(conditions.vector,...,W=diag(length(conditions.vector)))
{
	colMeans(conditions.vector(...)) %*% ginv(W) %*% colMeans(conditions.vector(...))
}