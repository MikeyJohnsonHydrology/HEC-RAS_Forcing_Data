########################################################
### USGS Stream Flow Data Extract.R
###
### A simple script to download USGS stream flow data
###

### Loading Libraries
library(dplyr)         # data manipulation
library(devtools)      # developer tools, simplifying tasks
library(dataRetrieval) # function to source USGS stream flow data

### Sourcing usefull functions
source_url("https://raw.githubusercontent.com/MikeyJohnsonHydrology/Useful_R_Functions/master/date_to_water_year.R")

### Local Gage Information
#USGS 10337500 TRUCKEE R A TAHOE CITY CA
#USGS 10337810 NF SQUAW C A OLYMPIC VALLEY CA

### Site & Date Information
site_id <- '10337500'
startDate <- '1940-10-01'
endDate <- '2021-09-30'
pCode <- '00060'

### Site Information
siteInfo <- readNWISsite(site_id)

### Stage Discharge Relationship
ratingData <- readNWISrating(site_id, "base") %>%
  rename(Gage_hight_ft = INDEP,
         Discharge_cfs = DEP) %>%
  mutate(Gage_hight_m = Gage_hight_ft * 0.3048,
         Discharge_cms = Discharge_cfs * 0.0283)

### Manning's N (Roughness coefficient) Rough Guess
slope = 0.0035



### Flow Data
DailyQ <- readNWISdv(site_id,pCode, startDate, endDate) %>%
  rename(Q_cfs = X_00060_00003) %>%
  mutate(Q_cms = Q_cfs * 0.0283,
         WY = date_to_water_year(Date))

plot(DailyQ$Date ,DailyQ$Q_cfs)

YearlyQmax <- DailyQ %>%
  group_by(WY) %>%
  summarise(Peak_Q_cfs = max(Q_cfs),
            Peak_Q_cms = max(Q_cms))

YearlyQmax <- YearlyQmax %>%
  arrange(Peak_Q_cfs) %>%
  mutate(Rank = 1:nrow(YearlyQmax),
         EP = Rank/(nrow(YearlyQmax)+1)) %>%
  arrange(WY)

plot(YearlyQmax$Peak_Q_cfs, YearlyQmax$EP)

# Using the data to predice exceedence probility 
#library(mgcv)
#EP_Q_GAM_Model <- gam(Peak_Q_cfs ~ s(EP), data = YearlyQmax)
#coef(EP_Q_GAM_Model)
#anova(EP_Q_GAM_Model)
#newEP <- data.frame(EP=c(0.2, 0.5, 1.0))
#newQ_cfs <- predict.gam(EP_Q_GAM_Model , newEP)

Q_n_cfs <-  function(dat,n){

  # Non-existent probability
  if(n<0){
    print("n must be a number greater then 0")
    return(NaN)
    }

  # Interpolation between known values
  if(n<1){
    tmp_low <- dat %>% filter(EP < n)
    tmp_low <- tmp_low %>% filter(EP == max(tmp_low$EP))

    tmp_high <- dat %>% filter(EP > n)
    tmp_high <- tmp_high %>% filter(EP == min(tmp_high$EP))

    m <- (tmp_high$Peak_Q_cfs-tmp_low$Peak_Q_cfs) / (tmp_high$EP-tmp_low$EP)
    b <- tmp_low$Peak_Q_cfs - (m * tmp_low$EP)

    Q_n <-  (n*m) + b
  
    return(Q_n)
  }

  # Interpolation using a lm and the top 90%  
  if(n == 1.0){
    tmp <- dat %>% filter(EP > 0.9)
    
    tmp_lm <- lm(Peak_Q_cfs ~ EP, data = tmp)
    
    tmp_EP <- data.frame(EP=1)
    
    Q_n <- round(as.numeric(predict(tmp_lm, tmp_EP)), digits = 1)
    
    return(Q_n)
  }
}


Q_20_cfs <- Q_n_cfs(YearlyQmax, 0.2)
Q_50_cfs <- Q_n_cfs(YearlyQmax, 0.5)
Q_100_cfs <- Q_n_cfs(YearlyQmax, 1.0)

