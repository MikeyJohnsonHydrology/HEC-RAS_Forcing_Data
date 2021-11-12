#####################################################################
### calculate manning's n.R
###
### A function to calcualte Manning's Roughness Coefficients.
###
### Note: check the input units
###
### Resources:
### http://www.fsl.orst.edu/geowater/FX3/help/8_Hydraulic_Reference/Manning_s_Equation.htm
###
#####################################################################

Manning_s_Roughness_SI <- function(Q,   # Volumetric Flow Rate (m^3/s)
                                   RH,  # Hydraulic Raidus (m)
                                   s,   # Slope of the channel (m/m)
                                   A){  # Crosssectional Area (m^2)
  
  n <- 1 * RH^(2/3) * sqrt(s) * A / Q

  return(n)
}

Manning_s_Roughness_US <- function(Q,   # Volumetric Flow Rate (ft^2/s)
                                   RH,  # Hydraulic Raidus (m or ft)
                                   s,   # Slope of the channel (unitles)
                                   A){  # Crosssectional Area (m^2 or ft^2)
  
  n <- 1.49 * RH^(2/3) * sqrt(s) * A / Q
  
  return(n)
}

# Code to test the function
#Manning_s_Roughness_Coefficients(Q = 8.4,
#                                 RH = 0.6,
#                                 s = 0.005,
#                                 A = 6.75)
