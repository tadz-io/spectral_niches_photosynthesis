library(RColorBrewer)
library(plotly)
library(dplyr)

source('approx_euphotic_spectra.R')

# CONSTANTS ---------
# range x-axis
X_RANGE <- c(400,700)
# interval x-axis
X_INT <- 2
# slope cdom
S_CDOM <- 0.017
# absorption cdom; sp is vector of spectrum in nm
ABS_CDOM <- function(sp){ exp(-S_CDOM*(sp-440))}
#line colors
LINE_COL = c("#377eb8","#ff7f00","#e41a1c")

ANNOT <- list(
  x = 0,
  y = 4,
  text = "",
  textangle = 90,
  font = list(color = "grey20"),
  xref = "x",
  yref = "paper",
  ax = 0,
  ay = 0,
  showarrow = FALSE
)

LINE <- list(
  type = "line",
  line    = list(color = "black",
                 dash = "dot",
                 width = 1.5), 
  opacity = 0.8,
  x0      = 0,
  x1      = 0,
  xref    = "x",
  y0      = 0,
  y1      = 0.48,
  yref    = "paper")

HARMONICS <- list(
  list(
    harmonics = 7,
    lambda = 449,
    absorption = 0.01),
  list(
    harmonics = 6,
    lambda = 514,
    absorption = 0.045),
  list(
    harmonics = 5.1,
    lambda = 550,
    absorption = 0.06),
  list(
    harmonics = 5,
    lambda = 605,
    absorption = 0.28),
  list(
    harmonics = 4.1,
    lambda = 662,
    absorption = 0.43))



# MAIN --------


plot_figure_2 <- function()
{
  # load water IOP
  data_water <- read.csv("./data/water_abs_scat_updated.csv", header = TRUE, sep = ";")
  data_water <- data_water %>% filter(lambda>=400 & lambda<=700)
  # approximate spectra @ euphotic depth for CDOM simulations
  spectrum_eu_scalar <- approx_euphotic_spectra("./data/irradiance.csv", "Eod", quanta = TRUE)
  spectrum_eu_scalar <- group_by(spectrum_eu_scalar, cdom_conc)

  #approximate spectra @ 3% irradiance level
  spectrum_3_scalar <- approx_euphotic_spectra("./data/irradiance.csv", "Eod", quanta = TRUE, EUPH_FRAC = 0.03)
  spectrum_3_scalar <- group_by(spectrum_3_scalar, cdom_conc)
  #approximate spectra @ 10% irradiance level
  spectrum_10_scalar <-  approx_euphotic_spectra("./data/irradiance.csv", "Eod", quanta = TRUE, EUPH_FRAC = 0.1)
  spectrum_10_scalar <- group_by(spectrum_10_scalar, cdom_conc)

  p_total_abs <- plot_ly(
    type = "scatter",
    mode = "lines",
    showlegend = FALSE)
  
  # number of intervals
  n_int <- 50
  # absorption traces to be highlighted
  traces_abs <- c(seq(1,50,10),50)
  # cdom concentration; lower and upper limit (exponential)
  cdom_conc <- c(-6,3)
  # cdom exponential intervals
  cdom_range = exp(seq(cdom_conc[1], cdom_conc[2], (cdom_conc[2]-cdom_conc[1])/(n_int-1)))
  
  # absorption curve water
  k_water <- filter(data_water, lambda >= X_RANGE[1] & lambda <= X_RANGE[2])$absorption
  dx_water <- data_water$lambda[2] - data_water$lambda[1]
  # dx water is not constant 2nm 400-450 and then 5nm
  abs_cdom_dxWater <- ABS_CDOM(data_water$lambda)
  
  # create df with total absorption profiles
  data_cdom <- data.frame(lambda = rep(data_water$lambda, n_int),
                          absorption = as.vector(sapply(cdom_range, function(x){
                            x*abs_cdom_dxWater+k_water})),
                          cdom_conc = as.vector(mapply(rep, x = cdom_range, times = length(abs_cdom_dxWater))))
  # group by cdom concnetration
  data_cdom <- group_by(data_cdom, cdom_conc)
  
  p_total_abs <- plot_ly(data_cdom, x=~lambda, y=~absorption) %>%
    add_lines(line = list(color = toRGB("black", 0.2)))
  
  # highlight spectra
  # p_total_abs <- p_total_abs %>% highlight_traces(traces_abs)
  
  # create plotly object
  p_total_abs <- layout(p_total_abs,
                        xaxis = list(
                          title = "",
                          range = c(401,699),
                          showgrid = FALSE,
                          showline = TRUE),
                        yaxis = list(
                          title = "total absorption (m<sup>-1</sup>)",
                          type = "log",
                          showgrid = FALSE,
                          showline = TRUE,
                          range = c(-2.2,0.4)))
  
  p_spectrum_eu_scalar <- plot_ly(spectrum_eu_scalar, x=~lambda, y=~Eod) %>%
    add_lines(line = list(color = toRGB("black", 0.2)))
  
  p_spectrum_10_scalar <- plot_ly(spectrum_10_scalar, x=~lambda, y=~Eod) %>%
    add_lines(line = list(color = toRGB("black", 0.2)))
  
  p_spectrum_3_scalar <- plot_ly(spectrum_3_scalar, x=~lambda, y=~Eod) %>%
    add_lines(line = list(color = toRGB("black", 0.2)))

  # create plotly object
  p_spectrum_eu_scalar <- p_spectrum_eu_scalar %>%
    layout(
      xaxis = list(
        range = c(X_RANGE[1], X_RANGE[2]),
        title = "wavelength (nm)"),
      yaxis = list(
        title = "scalar irradiance (&mu;mol photons s<sup>-1</sup>m<sup>-2</sup>nm<sup>-1</sup>)",
        showline = TRUE))
  
  p_spectrum_3_scalar <- p_spectrum_3_scalar %>%
    layout(
      xaxis = list(
        range = c(X_RANGE[1], X_RANGE[2]),
        title = ""),
      yaxis = list(
        title = "scalar irradiance (&mu;mol photons s<sup>-1</sup>m<sup>-2</sup>nm<sup>-1</sup>)",
        showline = TRUE))

    p_spectrum_10_scalar <- p_spectrum_10_scalar %>%
    layout(
      xaxis = list(
        range = c(X_RANGE[1], X_RANGE[2]),
        title = ""),
      yaxis = list(
        title = "scalar irradiance (&mu;mol photons s<sup>-1</sup>m<sup>-2</sup>nm<sup>-1</sup>)",
        showline = TRUE))
 
   sp <- subplot(p_total_abs, p_spectrum_10_scalar, p_spectrum_3_scalar, p_spectrum_eu_scalar, 
                 nrows = 4, titleX = TRUE, titleY = TRUE) %>%
     layout(showlegend = FALSE)
  
  return(sp)
}