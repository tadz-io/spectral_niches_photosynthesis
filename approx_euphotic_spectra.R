# libs
require(plotly)
require(dplyr)
require(data.table)
library(pracma)

# set working dir (only needed when sourcing this script without chdir = T)
#setwd("/Users/tadzio/Dropbox/PhD_VU/code/euphotic/R/plot_euphotic/")

# relative paths to source files
source('integrate.R')
source('frac_spectra.R')
source('approx.R')
source('watts_to_quanta.R')

# constants
# ---------
LAMBDA_INT <- c(400, 700)            # wavelength integration interval to calculate total energy
#EUPH_FRAC <- 0.01                     # fraction of energy available at euphotic depth
GROUP_VARS <- list("cdom", "depth")   # variables used for grouping of spectra

# ------------------------
#' plot underwater spectra
#'
#' @param csv_path path to csv file
#' @param y_var variable to be integrated + interpolated (string)
#' @param quanta should irradiance values be converted to quanta before interpolation
#' @param EUPH_FRAC fraction of incident PAR at which interpolation should occur; default is 1% level = euphotic depth
#' @return dataframe with interpolated values at depth where PAR is reduced to EUPH_FRAC

approx_euphotic_spectra <- function(csv_path, y_var, quanta=FALSE, EUPH_FRAC=0.01)
{
  # load data
  data <- read.csv(csv_path, header = TRUE)
  #subset data to PAR region
  data <- data %>% filter(lambda>=400 & lambda<=700)
  # get x-axis values
  x_val <- unique(data$lambda)
  
  if(quanta){
    # convert Ed/Eod to quanta
    data <- data %>% mutate(!!y_var := watts_to_quanta(.[[y_var]], lambda))
  }
  
  # create vars for total and relative energy
  data$E_tot <- rep(NA, nrow(data))
  data$E_rel <- rep(NA, nrow(data))
  
  # group data by cdom and depth
  #data <- group_by_(data, .dots = GROUP_VARS)
  data <- data %>% group_by(cdom, depth)
  
  # at every depth integrate spectra and calculate relative energy
  for(i in attr(data, "groups")$.rows)
  {
    data$E_tot[i] <- trapz(pull(data[i,],lambda), pull(data[i,], y_var))
    #data$E_tot[i] <- integrate(data[i,], y_var, "lambda", LAMBDA_INT[1], LAMBDA_INT[2])
  }
  
  data <- ungroup(data)
  #data <- group_by_(data, .dots = GROUP_VARS[1])
  data <- data %>% group_by(cdom)
  
  # init output df
  out_df = data.frame()
  
  for(i in attr(data, "groups")$.rows)
  {
    # calculate relative energy available per depth
    data$E_rel[i] <- frac_spectra(data[i,], "E_tot", "depth", 0)
    # get two neigbours nearest to euphotic depth
    nn_data <- n_neighbour(data[i,], "E_rel", EUPH_FRAC)
    
    # PAR at euphotic depth
    E_PAR_eu <- EUPH_FRAC*nn_data$E_tot[1]
    # do log transform for log. interpolation
    nn_data$E_tot <- -log(nn_data$E_tot)
    nn_data[,y_var] <- -log(nn_data[,y_var])
    # interpolate depth to euphotic depth
    depth_euph <- interpolation(nn_data, "E_tot", "depth", -log(E_PAR_eu))
    # interpolate spectra to euphotic depth
    spectra_euph <- interpolation(nn_data, "depth", y_var, depth_euph)
    # transform spectra back
    spectra_euph <- exp(-spectra_euph)
    
    # convert units of irradiance from watts to micro mole quanta
    #spectra_euph$quanta <- watts_to_quanta(spectra_euph[,y_var], x_val)
    
    # create df
    cdom_conc <- nn_data[attr(nn_data, "n_neighbour")[[1]],]$cdom
    spectra_out <- cbind(lambda=x_val, spectra_euph, cdom_conc, depth_euph)
    
    # bind dfs
    out_df = rbindlist(list(out_df, spectra_out))
    
  }
  
return(out_df)
}