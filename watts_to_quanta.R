# constants

CONV_FACTOR = 0.00835    # conversion factor

#' function converts watts to micro-mole of quanta per second
#' @param watts values in watts
#' @param lambda wavelength in nm

watts_to_quanta <- function(watts, lambda)
{
  return(CONV_FACTOR*watts*lambda)
}