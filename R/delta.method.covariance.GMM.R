# Covariance matrix of the parameters using delta method
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################


#' Covariance matrix of the parameters using delta method
#' @param covariance Covariance matrix of the moment conditions
#' @param gradient Gradient matrix of the moment conditions
#' @param size Sample size
#' @return The covariance matrix of the parameters
#' @export delta.method.covariance.GMM
#' @author François Pelletier
delta.method.covariance.GMM <- function(covariance,gradient,size)
{
	ginv(gradient %*% ginv(covariance) %*% t(gradient))/size
}
