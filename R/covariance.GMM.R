# Estimated covariance matrix
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' Estimated covariance matrix
#' @param conditions.vector Vector of moment conditions
#' @param param Vector of estimated parameters
#' @param data Individual data sample
#' @param ... Functions of the vector of moment conditions
#' @return A square covariance matrix
#' @export covariance.GMM
#' @author François Pelletier
covariance.GMM <- function(conditions.vector,param,data,...)
{
	t(conditions.vector(param,data,...)) %*% conditions.vector(param,data,...) / length(data)
}