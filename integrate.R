#' integrate light spectra
#' @param data data frame containing values to be integrated
#' @param yval variable to be integrated
#' @param xval variable that contains the limits of integration
#' @param lower lower bound for integration
#' @param upper upper bound for integration
#' @return value of integrand 

integrate <- function(data, yval, xval, lower, upper)
{
  # calculate dx (assuming to be constant step interval)
  dx <- as.numeric(data[2, xval] - data[1, xval])
  # subset within bounds
  yval_sub <- subset(data[,yval], data[,xval] >= lower & data[,xval] <= upper)
  
  return(sum(yval_sub)*dx)
}