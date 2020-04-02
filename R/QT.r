#' Quantile computation from the bootstrap distribution
#'
#' @param x A vector of the bootstrap samples
#' @param x0 A numeric that one wants to compute what quantile the point corresponds to
#' @return qt (numeric)
#' @export

QT <- function(x,x0){

 x1 <- sort(c(x,x0))
 w1 <- which(x1==as.numeric(x0))
 qt <- 1 - w1/(length(x)+1)
 return(qt)
 
}

