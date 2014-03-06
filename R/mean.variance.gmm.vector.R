# GMM vector for mean and variance moment conditions
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################


#' GMM vector for mean and variance moment conditions
#' @param param Estimated parameters
#' @param sample Data Sample
#' @param meanf Mean function
#' @param variancef Variance function
#' @return A two column matrix of differences
#' @export meanvariance.gmm.vector
#' @author François Pelletier
meanvariance.gmm.vector <- function(param,sample,meanf,variancef)
{
	cbind(X-meanf(param),(X-meanf(param))^2 - variancef(param))
}
