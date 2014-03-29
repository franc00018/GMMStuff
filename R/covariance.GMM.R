# Estimated covariance matrix
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' Estimated covariance matrix
#' @param param Vector of estimated parameters
#' @param conditions.vector Vector of moment conditions
#' @param data Individual data sample
#' @param ... Functions of the vector of moment conditions
#' @return A square covariance matrix
#' @export covariance.GMM
#' @author François Pelletier
covariance.GMM <- function(param,conditions.vector,data,...)
{
	t(conditions.vector(param,data,...)) %*% conditions.vector(param,data,...) / length(data)
}