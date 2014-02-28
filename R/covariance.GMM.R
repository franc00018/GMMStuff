# Estimated covariance matrix
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' Estimated covariance matrix
#' @param conditions.vector Vector of moment conditions
#' @param n Sample size
#' @param ... Parameters of the vector of moment conditions
#' @return A square covariance matrix
#' 
#' @author François Pelletier
covariance.GMM <- function(conditions.vector,n,...)
{
	t(conditions.vector(...)) %*% conditions.vector(...) / n
}