
#' Oil Spill Spread Function
#' by: Taylor Cook and Kelsey Warren
#' ESM 262
#' Assignment 5
#'
#' To estimate the oil spill area (km^2) and thickness of oil spill spread (mm) after an oil spill in the ocean
#' 
#' @param volume the volume of oil that leaked in the spill (m^3)
#' @param wtemp the water temperature of the ocean (degrees C)
#' @param windspeed the wind speed (m/s)
#' @param viscosity the oil viscosity factor
#' @param ratecoeff the spread rate coefficient
#' 
#' 
#' @return list with the following items
#' \describe{
#' \item{area}{Area of oil spill spread (km^2)}
#' \item{thickness}{Thickness of the oil spill spread (mm)}
#' }
#' 
#' 
#' @examples
#' oil(volume, wtemp, windspeed, viscosity=0.0001, ratecoeff=0.16)
#' @references
#' https://link.springer.com/chapter/10.1007/978-1-4684-9019-0_5


oil = function(volume, wtemp, windspeed, viscosity, ratecoeff) {
  
  # error checking
  # check volume
  volume = ifelse( (volume<0), return("Caution: oil volume cannot be negative"), volume)

  # check wtemp
  wtemp = ifelse( (wtemp<0), return("Caution: ocean temperature is below freezing"), wtemp)
  
  # check windpseed
  windspeed = ifelse( (windpseed<0), return("Caution: windpseed cannot be negative"), windpseed)
  
  # check viscosity
  viscosity = ifelse( (viscosity<0), return("Caution: viscosity cannot be negative"), viscosity) 
  
  # check ratecoeff
  ratecoeff = ifelse( (ratecoeff<0), return("Caution: ratecoeff cannot be negative"), ratecoeff)
  
  
# make some adjustments to oil spread based on ocean temperature and wind speed 
# ocean temperature adjustment
wtemp_adj = 1 + (wtemp-15) * 0.01  # warmer water increases spread rate
# windspeed adjustment
windspeed_adj = 1 + (windspeed * 0.05) # faster wind speed increases spread rate
  
  
# calculate oil spill area (km^2) using Fay's Model for Spill Area
area = ratecoeff * (volume^(2/3)) * wtemp_adj * windspeed_adj

# calculate oil spread thickness (mm)
thickness = (volume / (area * 1e6)) * 10^3 # multiply area*10^6 for km^2 --> m^2; multiply thickness (m) by 10^3 to get mm

# output from function
return(list(area_km2=area, thickness_mm=thickness)) 
}

  

  
  
  
  
  
  
  
  
  
  
  
  
  
