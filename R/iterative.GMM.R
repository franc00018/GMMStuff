# Iterative GMM method
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' Iterative GMM method
#' 
#' @param start.value Starting values for the parameters and lagrangian
#' @param conditions.vector Vector of moment conditions
#' @param data Individual data sample
#' @param W Weighting matrix
#' @param ... Functions of the vector of moment conditions
#' @param max.iter Maximum number of iterations
#' @param epsilon Minimum precision level
#' @return A list containing the optimized vector of parameter and corresponding covariance matrix
#' @export iterative.GMM
#' @author François Pelletier
iterative.GMM <- function(start.value,conditions.vector,data,W,...,max.iter=50,epsilon=1E-6)
{
	theta1 <- optim.GMM(start.value,conditions.vector=conditions.vector,data=data,W=W,...)$par
	i <- 1
	repeat
	{
		theta2 <- optim.GMM(theta1,conditions.vector,data,W,...)$par
		S <- covariance.GMM(theta2,conditions.vector,data,...)
		if(sqrt(sum((theta1-theta2)^2))<epsilon)
			return(list(par=theta2,cov=S))
		else if (i>max.iter)
			stop("Iterative GMM does not converge")
		else
		{
			theta1 <- theta2
			i <- i+1
		}
		
	}
}
