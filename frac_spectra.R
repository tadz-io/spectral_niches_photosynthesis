#' calculate relative energy available at
#' every depth compared to reference depth
#' @param data a data frame containing values to calculate fraction on and a reference variable
#' @param val val is the variable name to calculate fractions on
#' @param ref_var is the variable name used as reference
#' @param ref_val is the value of ref_var used as reference point
#' @return vector of length data specifying relative energy avaialble at every depht in data

frac_spectra <- function(data, val, ref_var, ref_val)
{
  frac <- data[[val]] / data[[val]][match(ref_val, data[[ref_var]])]
  
  return(frac)
}
