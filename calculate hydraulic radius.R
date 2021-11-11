#########################################################
### calculate hydraulic radius
###
### A simple function to calculate the hydrology radius
### of a stream channel cross section. This information
### can be supplied from a stream survey or derived
### from Lidar data.  Cross sections data can easily be
### derived in HEC-RAS
###
### Note: - make sure you are using the correct units.
###       - HEC-RAS uses SI (feet) or metric (meters)
#########################################################

calculate_hydraulic_radius <- function(
  station,    # vector if station locations across a stream cross section (meters or feet)
  elevation,  # elevation of each stations (meters of feet)
  stage){     # stage depth of the river as related the the thalweg (meters of feet)

  if(length(station)!=length(elevation)){
    print("Stations and Elevation data are unequal length")
    print(paste0("Stations Count = ",length(station)))
    print(paste0("Elevation Count = ", length(elevation)))
    stop()
  }
  
  base_elevation <- min(elevation)
  water_surface_elevation <- stage + base_elevation
  nstations <- length(station)

  # moving window across the cross sections
  area <- 0
  wetted_primiter <- 0

  for(i in 1:(nstations-1)){

    tmp_stations <- station[i:(i+1)]
    tmp_elevation <- elevation[i:(i+1)]

    # window fully dry
    if(tmp_elevation[1] > water_surface_elevation &
       tmp_elevation[2] > water_surface_elevation){
      wetted_primiter <- wetted_primiter
      area <- area

    # window fully wet
    } else if(tmp_elevation[1] < water_surface_elevation &
              tmp_elevation[2] < water_surface_elevation){
      wetted_primiter <- wetted_primiter +
        sqrt(
          (tmp_stations[2]-tmp_stations[1])^2 +
            (tmp_elevation[2]-tmp_elevation[1])^2)

      area <- area +
        abs(water_surface_elevation - mean(tmp_elevation))*
           abs(tmp_stations[2]-tmp_stations[1])

    # window partially wet 
    } else{
      m <- (tmp_stations[2]-tmp_stations[1]) / (tmp_elevation[2]-tmp_elevation[1])
      tmp_x <- abs(m * stage)
    
      wetted_primiter <- wetted_primiter +
        sqrt(tmp_x^2 +
            (min(tmp_elevation) - water_surface_elevation)^2)
    
      area <- area +
        (water_surface_elevation-min(tmp_elevation)) * tmp_x / 2
    }
    
  } # end of moving window loop (i loop) 

  RH <- area/wetted_primiter
  return(RH)

} # End of function 

# # Code to test the function
# channel.xc.example.data <- read.csv("~/Documents/R Scripts/HEC-RAS Models/channel xc example data.csv")
# 
# plot(channel.xc.example.data$Station,
#      channel.xc.example.data$Elevation)
# 
# abline(h=1256+10, col="blue")
# calculate_hydraulic_radius(station = channel.xc.example.data$Station,
#                            elevation = channel.xc.example.data$Elevation,
#                            stage = 10)
# 
# abline(h=1256+20, col="blue")
# calculate_hydraulic_radius(station = channel.xc.example.data$Station,
#                            elevation = channel.xc.example.data$Elevation,
#                            stage = 20)


