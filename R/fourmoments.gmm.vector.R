# GMM vector for 4 first central moments conditions
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' GMM vector for 4 first central moments conditions
#' @param param Estimated parameters
#' @param sample Data Sample
#' @param meanf Mean function
#' @param variancef Variance function
#' @param skewnessf Skewness function
#' @param kurtosisf Kurtosis function
#' @return A four column matrix of differences
#' @export fourmoments.gmm.vector
#' @author François Pelletier
fourmoments.gmm.vector <- function(param,sample,meanf,variancef,skewnessf,kurtosisf)
{
	cbind(X-meanf(param),
			(X-meanf(param))^2 - variancef(param),
			(X-meanf(param))^3/variancef(param)^(3/2) - skewnessf(param),
			(X-meanf(param))^4/variancef(param)^(2) - kurtosisf(param))
}
