# Iterative GMM method
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################

#' Iterative GMM method
#' 
#' @param start Starting values for the parameters and lagrangian
#' @param conditions.vector Vector of moment conditions
#' @param data Individual data sample
#' @param ... Functions of the vector of moment conditions
#' @param W Weighting matrix
#' @param R Linear constraint matrix of coefficients
#' @param r Linear constraint constants
#' @param max.iter Maximum number of iterations
#' @param epsilon Minimum precision level
#' @return A list containing the optimized vector of parameter and corresponding covariance matrix
#' @export iterative.GMM
#' @author François Pelletier
iterative.GMM <- function(start,conditions.vector,data,...,
		W=diag(length(conditions.vector)),R=0,r=0,lagrangian.start=rep(0,length(conditions.vector)),max.iter=50,epsilon=1E-6)
{
	theta1 <- optim.GMM(start,conditions.vector,data,...,W,R,r,lagrangian.start)
	i <- 1
	repeat
	{
		theta2 <- optim.GMM(theta1,conditions.vector,data,...,W,R,r,lagrangian.start)
		S <- covariance.GMM(conditions.vector,param,data,...)
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
