# Estimated covariance matrix
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' Estimated covariance matrix
#' @param conditions.vector Vector of moment conditions
#' @param param Vector of estimated parameters
#' @param sample Individual data sample
#' @param ... Functions of the vector of moment conditions
#' @return A square covariance matrix
#' 
#' @author François Pelletier
covariance.GMM <- function(conditions.vector,param,sample...)
{
	t(conditions.vector(param,sample,...)) %*% conditions.vector(param,sample,...) / length(sample)
}