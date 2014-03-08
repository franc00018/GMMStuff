# GMM vector for mean and variance moment conditions
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################


#' GMM vector for mean and variance moment conditions
#' @param param Estimated parameters
#' @param data Data Sample
#' @param meanf Mean function
#' @param variancef Variance function
#' @return A two column matrix of differences
#' @export meanvariance.gmm.vector
#' @author François Pelletier
meanvariance.gmm.vector <- function(param,data,meanf,variancef)
{
	cbind(data-meanf(param),(data-meanf(param))^2 - variancef(param))
}
