#' Maximum likelihood estimation for the fixed-effect model
#'
#' @param y A vector of the outcome measure estimates (e.g., MD, SMD, log OR, log HR)
#' @param v A vector of the variance estimates of outcome measures
#' @param maxitr The maximum number of iterative compuation of the Newton-Raphson method.
#' @return mu V0 Loglikelihood (list)
#' @export

ML_FE <- function(y,v,maxitr=200){

	N <- length(y)

	V0 <- 0

	LL1 <- function(V){

		ll1 <- 0
		
		for(i in 1:N){

			yi <- y[i]
			vi <- v[i]

			A1 <- 0.5*log(2*pi*(vi+V))
			A2 <- (yi-mu)^2/(2*(vi+V))
		
			ll1 <- ll1 + A1 + A2

		}

		return(ll1)
	
	}

	wi <- (v + V0)^-1
		
	mu <- sum(wi * y)/sum(wi)

	LL <- -LL1(V0)
	
	R1 <- list("mu"=mu,"V0"=V0,"Loglikelihood"=LL)
	
	return(R1)
	
}

