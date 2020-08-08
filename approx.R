# libs d
library(dplyr)

#' linear interpolation
#' @param data data frame containing values to be interpolated
#' @param x variable specifying the values along the x-axis
#' @param y dependent variable, specifying the values along the y-axis
#' @param xout the x-value to interpolate to
#' @return interpolated value at xout

interpolation <- function(data, x, y, xout)
{
  # get indices for upper and lower neighbours
  ind <- attr(data, "n_neighbour")
  
  yout <- (data[ind[[1]], y] - data[ind[[2]], y]) / (data[ind[[1]], x] - data[ind[[2]], x]) *
    (xout - data[ind[[1]], x]) + data[ind[[1]], y]
  
  return(yout)
}

n_neighbour <- function(data, x, x_approx)
{
  # add column for diff. calculation
  data$diff <- rep(NA, nrow(data))
  data$diff <- data[[x]] - x_approx
  
  # get indices for upper and lower neighbours
  n_down <- which(data$diff == max(data$diff[data$diff < 0]))
  n_up <- which(data$diff == min(data$diff[data$diff > 0]))
  # add indices as attribute to dataframe
  attr(data, "n_neighbour") <- list(n_down, n_up)
  
  return(data)
}