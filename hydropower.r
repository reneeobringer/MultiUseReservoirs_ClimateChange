# Purpose: Hydropower Project
# Author: Renee Obringer
# Last Run: 15 September 2025

# ORGANIZATION: 
# This code is organized into sections, the start of each is denoted by multiple #
# The sections can be run independently by loading the rdata files at the beginning of each section
# Each section is described below
#
# LOAD DATA: load the data
# DATA PRE-PROCESSING: spatiotemporal aggregation, unit conversion, etc.
# MODEL DEVELOPMENT: develop the observational models
# CLIMATE CHANGE PROJECTIONS: project the single and moutli-outcome models into the future
# INTERPRETATION: evaluate the model performance (observations) and percent change (projections)
# FIGURES AND TABLES: code for plotting figures and creating tables included in manuscript 

rm(list=ls())
options(scipen = 999)

# libraries
library(tidyverse)
library(readxl)        
library(MultivariateRandomForest)
library(hydroGOF)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(sf)
library(sp)
library(ggrepel)

# set file path
# NOTE: set this path to the folder on your personal machine which contains the downloaded data 
# for example: path <- '/Users/obringer/Downloads/Hydropower'

path <- '/Users/rqo5125/Library/Mobile Documents/com~apple~CloudDocs/Documents/Research' # main file path

# set directories
maindir <- path    # main directory

datadir1 <- paste(path, '/data/reservoirs/AZ/LakePowell', sep = '')  # directory for Lake Powell
datadir2 <- paste(path, '/data/reservoirs/CA/folsom', sep = '')      # directory for Lake Folsom
datadir3 <- paste(path, '/data/reservoirs/CA/oroville', sep = '')    # directory for Lake Oroville
datadir4 <- paste(path, '/data/reservoirs/CA/Shasta', sep = '')      # directory for Lake Shasta
datadir5 <- paste(path, '/data/reservoirs/NV/LakeMead', sep = '')    # directory for Lake Mead
datadir6 <- paste(path, '/data/GCMsimulations/FutureStreams/Shasta_Oroville_Folsom_Mead_Powell', sep = '') # directory for future streamflow values
datadir7 <- paste(path, '/data/GCMsimulations/CMIP5_reservoirs', sep = '')                                 # directory for future weather values

rdatadir <- paste(path, '/2025_26/papers/hydropower/rdatafiles', sep = '')  # directory for storing rdata files
outputdir <- paste(path,'/2025_26/papers/hydropower/output', sep = '')      # directory for storing non-rdata output

############# LOAD DATA #############

# Observed Data

# Lake Powell
setwd(datadir1)

powell_generation <- read.csv('glencanyongeneration.csv')       # MWh
powell_storage <- read.csv('storagevolume_1963-2025.csv')       # ac-ft
powell_inflow <- read.csv('inflow_1963-2025.csv')               # cfs
powell_outflow <- read.csv('totalrelease_1963-2025.csv')        # cfs
powell_precip <- read.csv('powell_precip.csv')                  # kg/m^2
powell_evap <- read.csv('powell_evap.csv')                      # kg/m^2
powell_temp <- read.csv('powell_temp.csv')                      # K
powell_rh <- read.csv('powell_rh.csv')                          # %
powell_uwind <- read.csv('powell_uwind.csv')                    # m/s
powell_vwind <- read.csv('powell_vwind.csv')                    # m/s

# Lake Folsom
setwd(datadir2)

folsom_generation <- read.csv('folsomgeneration_EIA.csv')       # MWh
folsom_storage <- read_xlsx('storage_1990-2025.xlsx')           # ac-ft
folsom_inflow <- read_xlsx('inflow_1999-2025.xlsx')             # cfs
folsom_outflow <- read_xlsx('outflow_1999-2025.xlsx')           # cfs
folsom_precip <- read.csv('folsom_precip.csv')                  # kg/m^2
folsom_evap <- read.csv('folsom_evap.csv')                      # kg/m^2
folsom_temp <- read.csv('folsom_temp.csv')                      # K
folsom_rh <- read.csv('folsom_rh.csv')                          # %
folsom_uwind <- read.csv('folsom_uwind.csv')                    # m/s
folsom_vwind <- read.csv('folsom_vwind.csv')                    # m/s

# Lake Oroville
setwd(datadir3)

oroville_generation <- read.csv('orovillegeneration.csv')       # MWh
oroville_storage <- read_xlsx('storage_1999-2025.xlsx')         # ac-ft
oroville_inflow <- read_xlsx('inflow_1999-2025.xlsx')           # cfs
oroville_outflow <- read_xlsx('outflow_1989-2025.xlsx')         # cfs
oroville_precip <- read.csv('oroville_precip.csv')              # kg/m^2
oroville_evap <- read.csv('oroville_evap.csv')                  # kg/m^2
oroville_temp <- read.csv('oroville_temp.csv')                  # K
oroville_rh <- read.csv('oroville_rh.csv')                      # %
oroville_uwind <- read.csv('oroville_uwind.csv')                # m/s
oroville_vwind <- read.csv('oroville_vwind.csv')                # m/s

# Lake Shasta
setwd(datadir4)

shasta_generation <- read.csv('shastageneration_EIA.csv')       # MWh
shasta_storage <- read_xlsx('storage_1990-2025.xlsx')           # ac-ft
shasta_inflow <- read_xlsx('inflow_1999-2025.xlsx')             # cfs
shasta_outflow <- read_xlsx('outflow_1990-2025.xlsx')           # cfs
shasta_precip <- read.csv('shasta_precip.csv')                  # kg/m^2
shasta_evap <- read.csv('shasta_evap.csv')                      # kg/m^2
shasta_temp <- read.csv('shasta_temp.csv')                      # K
shasta_rh <- read.csv('shasta_rh.csv')                          # %
shasta_uwind <- read.csv('shasta_uwind.csv')                    # m/s
shasta_vwind <- read.csv('shasta_vwind.csv')                    # m/s

# Lake Mead
setwd(datadir5)
 
mead_generation <- read.csv('hoovergeneration.csv')             # MWh
mead_storage <- read.csv('storage_1937-2025.csv')               # ac-ft
mead_inflow <- read.csv('powell_release_1963-2025.csv')         # cfs
mead_outflow <- read.csv('totalrelease_1934-2025.csv')          # cfs
mead_precip <- read.csv('mead_precip.csv')                      # kg/m^2
mead_evap <- read.csv('mead_evap.csv')                          # kg/m^2
mead_temp <- read.csv('mead_temp.csv')                          # K
mead_rh <- read.csv('mead_rh.csv')                              # %
mead_uwind <- read.csv('mead_uwind.csv')                        # m/s
mead_vwind <- read.csv('mead_vwind.csv')                        # m/s

# Future Simulations
setwd(datadir6)

future_powell_inflow <- read.csv('powell_inflow.csv')       # m^3/s
future_powell_outflow <- read.csv('powell_outflow.csv')     # m^3/s
future_folsom_inflow <- read.csv('folsom_inflow.csv')       # m^3/s
future_folsom_outflow <- read.csv('folsom_outflow.csv')     # m^3/s
future_oroville_inflow <- read.csv('oroville_inflow.csv')   # m^3/s
future_oroville_outflow <- read.csv('oroville_outflow.csv') # m^3/s
future_shasta_inflow <- read.csv('shasta_inflow.csv')       # m^3/s
future_shasta_outflow <- read.csv('shasta_outflow.csv')     # m^3/s
future_mead_inflow <- read.csv('mead_inflow.csv')           # m^3/s
future_mead_outflow <- read.csv('mead_outflow.csv')         # m^3/s

setwd(datadir7)
future_powell_climate <- read.csv('powell_climatevars.csv')
future_folsom_climate <- read.csv('folsom_climatevars.csv')
future_oroville_climate <- read.csv('oroville_climatevars.csv')
future_shasta_climate <- read.csv('shasta_climatevars.csv')
future_mead_climate <- read.csv('mead_climatevars.csv')

# save as rdata
setwd(rdatadir)
save.image('rawdata.rdata')

############# DATA PRE-PROCESSING #############

setwd(rdatadir)
load('rawdata.rdata')

# Observed Data

# Generation data
powell_generation <- powell_generation %>% mutate(Month = recode(Month, January = 1, February = 2, March = 3, April = 4, May = 5, June = 6, July = 7, August = 8, September = 9, October = 10, November = 11, December = 12)) %>% 
  mutate(Date = make_date(Year, Month)) 
folsom_generation <- folsom_generation %>% mutate(Month = recode(Month, January = 1, February = 2, March = 3, April = 4, May = 5, June = 6, July = 7, August = 8, September = 9, October = 10, November = 11, December = 12)) %>% 
  mutate(Date = make_date(Year, Month)) 
oroville_generation <- oroville_generation %>% mutate(Month = recode(Month, January = 1, February = 2, March = 3, April = 4, May = 5, June = 6, July = 7, August = 8, September = 9, October = 10, November = 11, December = 12)) %>% 
  mutate(Date = make_date(Year, Month)) 
shasta_generation <- shasta_generation %>% mutate(Month = recode(Month, January = 1, February = 2, March = 3, April = 4, May = 5, June = 6, July = 7, August = 8, September = 9, October = 10, November = 11, December = 12)) %>% 
  mutate(Date = make_date(Year, Month)) 
mead_generation <- mead_generation %>% mutate(Month = recode(Month, January = 1, February = 2, March = 3, April = 4, May = 5, June = 6, July = 7, August = 8, September = 9, October = 10, November = 11, December = 12)) %>% 
  mutate(Date = make_date(Year, Month)) 

# Storage Data
powell_storage$Date <- mdy(powell_storage$datetime); powell_storage$Date[which(year(powell_storage$Date) > 2060)] <- powell_storage$Date[which(year(powell_storage$Date) > 2060)]  - years(100)
folsom_storage$VALUE <- gsub(",","",folsom_storage$VALUE) %>% as.numeric(folsom_storage$VALUE); folsom_storage$Date <- date(ymd_hm(folsom_storage$`OBS DATE`)); names(folsom_storage)[7] <- 'storage_acft'
oroville_storage$VALUE <- gsub(",","",oroville_storage$VALUE) %>% as.numeric(oroville_storage$VALUE); oroville_storage$Date <- date(ymd_hm(oroville_storage$`OBS DATE`)); names(oroville_storage)[7] <- 'storage_acft'
shasta_storage$VALUE <- gsub(",","",shasta_storage$VALUE) %>% as.numeric(shasta_storage$VALUE); shasta_storage$Date <- date(ymd_hm(shasta_storage$`OBS DATE`)); names(shasta_storage)[7] <- 'storage_acft'
mead_storage$Date <- mdy(mead_storage$datetime); mead_storage$Date[which(year(mead_storage$Date) > 2030)] <- mead_storage$Date[which(year(mead_storage$Date) > 2030)]  - years(100)

# Inflow Data
powell_inflow$Date <- mdy(powell_inflow$datetime); powell_inflow$Date[which(year(powell_inflow$Date) > 2060)] <- powell_inflow$Date[which(year(powell_inflow$Date) > 2060)]  - years(100); powell_inflow$inflow_m3s <- powell_inflow$inflow_cfs * 0.0283168
folsom_inflow$VALUE <- gsub(",","",folsom_inflow$VALUE) %>% as.numeric(folsom_inflow$VALUE); folsom_inflow$Date <- date(ymd_hm(folsom_inflow$`OBS DATE`)); names(folsom_inflow)[7] <- 'inflow_cfs'; folsom_inflow$inflow_m3s <- folsom_inflow$inflow_cfs * 0.0283168
oroville_inflow$VALUE <- gsub(",","",oroville_inflow$VALUE) %>% as.numeric(oroville_inflow$VALUE); oroville_inflow$Date <- date(ymd_hm(oroville_inflow$`OBS DATE`)); names(oroville_inflow)[7] <- 'inflow_cfs'; oroville_inflow$inflow_m3s <- oroville_inflow$inflow_cfs * 0.0283168
shasta_inflow$VALUE <- gsub(",","",shasta_inflow$VALUE) %>% as.numeric(shasta_inflow$VALUE); shasta_inflow$Date <- date(ymd_hm(shasta_inflow$`OBS DATE`)); names(shasta_inflow)[7] <- 'inflow_cfs'; shasta_inflow$inflow_m3s <- shasta_inflow$inflow_cfs * 0.0283168
mead_inflow$Date <- mdy(mead_inflow$datetime); mead_inflow$Date[which(year(mead_inflow$Date) > 2030)] <- mead_inflow$Date[which(year(mead_inflow$Date) > 2030)]  - years(100); names(mead_inflow)[2] <- 'inflow_cfs'; mead_inflow$inflow_m3s <- mead_inflow$inflow_cfs * 0.0283168

# Outflow Data
powell_outflow$Date <- mdy(powell_outflow$datetime); powell_outflow$Date[which(year(powell_outflow$Date) > 2060)] <- powell_outflow$Date[which(year(powell_outflow$Date) > 2060)]  - years(100); names(powell_outflow)[2] <- 'outflow_cfs'; powell_outflow$outflow_m3s <- powell_outflow$outflow_cfs * 0.0283168
folsom_outflow$VALUE <- gsub(",","",folsom_outflow$VALUE) %>% as.numeric(folsom_outflow$VALUE); folsom_outflow$Date <- date(ymd_hm(folsom_outflow$`OBS DATE`)); names(folsom_outflow)[7] <- 'outflow_cfs'; folsom_outflow$outflow_m3s <- folsom_outflow$outflow_cfs * 0.0283168
oroville_outflow$VALUE <- gsub(",","",oroville_outflow$VALUE) %>% as.numeric(oroville_outflow$VALUE); oroville_outflow$Date <- date(ymd_hm(oroville_outflow$`OBS DATE`)); names(oroville_outflow)[7] <- 'outflow_cfs'; oroville_outflow$outflow_m3s <- oroville_outflow$outflow_cfs * 0.0283168
shasta_outflow$VALUE <- gsub(",","",shasta_outflow$VALUE) %>% as.numeric(shasta_outflow$VALUE); shasta_outflow$Date <- date(ymd_hm(shasta_outflow$`OBS DATE`)); names(shasta_outflow)[7] <- 'outflow_cfs'; shasta_outflow$outflow_m3s <- shasta_outflow$outflow_cfs * 0.0283168
mead_outflow$Date <- mdy(mead_outflow$datetime); mead_outflow$Date[which(year(mead_outflow$Date) > 2030)] <- mead_outflow$Date[which(year(mead_outflow$Date) > 2030)]  - years(100); names(mead_outflow)[2] <- 'outflow_cfs'; mead_outflow$outflow_m3s <- mead_outflow$outflow_cfs * 0.0283168

# Evaporation Data
powell_evap$Date <- date(ymd_hms(powell_evap$Date, truncated = 3)); powell_evap <- aggregate(powell_evap$evap_kgPm2, by = list(powell_evap$Date), FUN = sum); names(powell_evap) <- c('Date','evap_kgPm2')
folsom_evap$Date <- date(ymd_hms(folsom_evap$Date, truncated = 3)); folsom_evap <- aggregate(folsom_evap$evap_kgPm2, by = list(folsom_evap$Date), FUN = sum); names(folsom_evap) <- c('Date','evap_kgPm2')
oroville_evap$Date <- date(ymd_hms(oroville_evap$Date, truncated = 3)); oroville_evap <- aggregate(oroville_evap$evap_kgPm2, by = list(oroville_evap$Date), FUN = sum); names(oroville_evap) <- c('Date','evap_kgPm2')
shasta_evap$Date <- date(ymd_hms(shasta_evap$Date, truncated = 3)); shasta_evap <- aggregate(shasta_evap$evap_kgPm2, by = list(shasta_evap$Date), FUN = sum); names(shasta_evap) <- c('Date','evap_kgPm2')
mead_evap$Date <- date(ymd_hms(mead_evap$Date, truncated = 3)); mead_evap <- aggregate(mead_evap$evap_kgPm2, by = list(mead_evap$Date), FUN = sum); names(mead_evap) <- c('Date','evap_kgPm2')

# Precipitation Data
powell_precip$Date <- date(ymd_hms(powell_precip$Date)); powell_precip <- aggregate(powell_precip$precip_kgPm2, by = list(powell_precip$Date), FUN = sum); names(powell_precip) <- c('Date','precip_kgPm2')
folsom_precip$Date <- date(ymd_hms(folsom_precip$Date)); folsom_precip <- aggregate(folsom_precip$precip_kgPm2, by = list(folsom_precip$Date), FUN = sum); names(folsom_precip) <- c('Date','precip_kgPm2')
oroville_precip$Date <- date(ymd_hms(oroville_precip$Date)); oroville_precip <- aggregate(oroville_precip$precip_kgPm2, by = list(oroville_precip$Date), FUN = sum); names(oroville_precip) <- c('Date','precip_kgPm2')
shasta_precip$Date <- date(ymd_hms(shasta_precip$Date)); shasta_precip <- aggregate(shasta_precip$precip_kgPm2, by = list(shasta_precip$Date), FUN = sum); names(shasta_precip) <- c('Date','precip_kgPm2')
mead_precip$Date <- date(ymd_hms(mead_precip$Date)); mead_precip <- aggregate(mead_precip$precip_kgPm2, by = list(mead_precip$Date), FUN = sum); names(mead_precip) <- c('Date','precip_kgPm2')

# Relative Humidity Data
powell_rh$Date <- date(ymd_hms(powell_rh$Date)); powell_rh <- aggregate(powell_rh$rh_perc, by = list(powell_rh$Date), FUN = mean); names(powell_rh) <- c('Date','rh_perc')
folsom_rh$Date <- date(ymd_hms(folsom_rh$Date)); folsom_rh <- aggregate(folsom_rh$rh_perc, by = list(folsom_rh$Date), FUN = mean); names(folsom_rh) <- c('Date','rh_perc')
oroville_rh$Date <- date(ymd_hms(oroville_rh$Date)); oroville_rh <- aggregate(oroville_rh$rh_perc, by = list(oroville_rh$Date), FUN = mean); names(oroville_rh) <- c('Date','rh_perc')
shasta_rh$Date <- date(ymd_hms(shasta_rh$Date)); shasta_rh <- aggregate(shasta_rh$rh_perc, by = list(shasta_rh$Date), FUN = mean); names(shasta_rh) <- c('Date','rh_perc')
mead_rh$Date <- date(ymd_hms(mead_rh$Date)); mead_rh <- aggregate(mead_rh$rh_perc, by = list(mead_rh$Date), FUN = mean); names(mead_rh) <- c('Date','rh_perc')

# Temperature Data
powell_temp$Date <- date(ymd_hms(powell_temp$Date)); powell_temp <- aggregate(powell_temp$temp_K, by = list(powell_temp$Date), FUN = mean); names(powell_temp) <- c('Date','temp_K'); powell_temp$temp_C <- powell_temp$temp_K - 273.15
folsom_temp$Date <- date(ymd_hms(folsom_temp$Date)); folsom_temp <- aggregate(folsom_temp$temp_K, by = list(folsom_temp$Date), FUN = mean); names(folsom_temp) <- c('Date','temp_K'); folsom_temp$temp_C <- folsom_temp$temp_K - 273.15
oroville_temp$Date <- date(ymd_hms(oroville_temp$Date)); oroville_temp <- aggregate(oroville_temp$temp_K, by = list(oroville_temp$Date), FUN = mean); names(oroville_temp) <- c('Date','temp_K'); oroville_temp$temp_C <- oroville_temp$temp_K - 273.15
shasta_temp$Date <- date(ymd_hms(shasta_temp$Date)); shasta_temp <- aggregate(shasta_temp$temp_K, by = list(shasta_temp$Date), FUN = mean); names(shasta_temp) <- c('Date','temp_K'); shasta_temp$temp_C <- shasta_temp$temp_K - 273.15
mead_temp$Date <- date(ymd_hms(mead_temp$Date)); mead_temp <- aggregate(mead_temp$temp_K, by = list(mead_temp$Date), FUN = mean); names(mead_temp) <- c('Date','temp_K'); mead_temp$temp_C <- mead_temp$temp_K - 273.15

# Wind Speed Data
powell_uwind$Date <- date(ymd_hms(powell_uwind$Date)); powell_uwind <- aggregate(powell_uwind$uwind_mPs, by = list(powell_uwind$Date), FUN = mean); names(powell_uwind) <- c('Date','uwind_mPs')
folsom_uwind$Date <- date(ymd_hms(folsom_uwind$Date)); folsom_uwind <- aggregate(folsom_uwind$uwind_mPs, by = list(folsom_uwind$Date), FUN = mean); names(folsom_uwind) <- c('Date','uwind_mPs')
oroville_uwind$Date <- date(ymd_hms(oroville_uwind$Date)); oroville_uwind <- aggregate(oroville_uwind$uwind_mPs, by = list(oroville_uwind$Date), FUN = mean); names(oroville_uwind) <- c('Date','uwind_mPs')
shasta_uwind$Date <- date(ymd_hms(shasta_uwind$Date)); shasta_uwind <- aggregate(shasta_uwind$uwind_mPs, by = list(shasta_uwind$Date), FUN = mean); names(shasta_uwind) <- c('Date','uwind_mPs')
mead_uwind$Date <- date(ymd_hms(mead_uwind$Date)); mead_uwind <- aggregate(mead_uwind$uwind_mPs, by = list(mead_uwind$Date), FUN = mean); names(mead_uwind) <- c('Date','uwind_mPs')

powell_vwind$Date <- date(ymd_hms(powell_vwind$Date)); powell_vwind <- aggregate(powell_vwind$vwind_mPs, by = list(powell_vwind$Date), FUN = mean); names(powell_vwind) <- c('Date','vwind_mPs')
folsom_vwind$Date <- date(ymd_hms(folsom_vwind$Date)); folsom_vwind <- aggregate(folsom_vwind$vwind_mPs, by = list(folsom_vwind$Date), FUN = mean); names(folsom_vwind) <- c('Date','vwind_mPs')
oroville_vwind$Date <- date(ymd_hms(oroville_vwind$Date)); oroville_vwind <- aggregate(oroville_vwind$vwind_mPs, by = list(oroville_vwind$Date), FUN = mean); names(oroville_vwind) <- c('Date','vwind_mPs')
shasta_vwind$Date <- date(ymd_hms(shasta_vwind$Date)); shasta_vwind <- aggregate(shasta_vwind$vwind_mPs, by = list(shasta_vwind$Date), FUN = mean); names(shasta_vwind) <- c('Date','vwind_mPs')
mead_vwind$Date <- date(ymd_hms(mead_vwind$Date)); mead_vwind <- aggregate(mead_vwind$vwind_mPs, by = list(mead_vwind$Date), FUN = mean); names(mead_vwind) <- c('Date','vwind_mPs')

powell_ws <- merge(powell_uwind, powell_vwind, on = 'Date'); powell_ws$ws_mPs <- sqrt(powell_ws$uwind_mPs^2 + powell_ws$vwind_mPs^2)
folsom_ws <- merge(folsom_uwind, folsom_vwind, on = 'Date'); folsom_ws$ws_mPs <- sqrt(folsom_ws$uwind_mPs^2 + folsom_ws$vwind_mPs^2)
oroville_ws <- merge(oroville_uwind, oroville_vwind, on = 'Date'); oroville_ws$ws_mPs <- sqrt(oroville_ws$uwind_mPs^2 + oroville_ws$vwind_mPs^2)
shasta_ws <- merge(shasta_uwind, shasta_vwind, on = 'Date'); shasta_ws$ws_mPs <- sqrt(shasta_ws$uwind_mPs^2 + shasta_ws$vwind_mPs^2)
mead_ws <- merge(mead_uwind, mead_vwind, on = 'Date'); mead_ws$ws_mPs <- sqrt(mead_ws$uwind_mPs^2 + mead_ws$vwind_mPs^2)

# merge by lake
powelldatafiles <- list(powell_generation, powell_storage, powell_inflow, powell_outflow, powell_evap, powell_precip, powell_rh, powell_temp, powell_ws)
dailypowelldata <- reduce(.x = powelldatafiles, merge, by = c('Date'), all = T); dailypowelldata <- dailypowelldata[which(year(dailypowelldata$Date) >= 2001),c(1,9,11,14,17:20,22,25)]

folsomdatafiles <- list(folsom_generation, folsom_storage, folsom_inflow, folsom_outflow, folsom_evap, folsom_precip, folsom_rh, folsom_temp, folsom_ws)
dailyfolsomdata <- reduce(.x = folsomdatafiles, merge, by = c('Date'), all = T); dailyfolsomdata <- dailyfolsomdata[which(year(dailyfolsomdata$Date) >= 2001),c(1,9,16,28,38:41,43,46)]

orovilledatafiles <- list(oroville_generation, oroville_storage, oroville_inflow, oroville_outflow, oroville_evap, oroville_precip, oroville_rh, oroville_temp, oroville_ws)
dailyorovilledata <- reduce(.x = orovilledatafiles, merge, by = c('Date'), all = T); dailyorovilledata <- dailyorovilledata[which(year(dailyorovilledata$Date) >= 2001), c(1,8,15,27,37:40,42,45)]

shastadatafiles <- list(shasta_generation, shasta_storage, shasta_inflow, shasta_outflow, shasta_evap, shasta_precip, shasta_rh, shasta_temp, shasta_ws)
dailyshastadata <- reduce(.x = shastadatafiles, merge, by = c('Date'), all = T); dailyshastadata <- dailyshastadata[which(year(dailyshastadata$Date) >= 2001),c(1,9,16,28,38:41,43,46)]

meaddatafiles <- list(mead_generation, mead_storage, mead_inflow, mead_outflow, mead_evap, mead_precip, mead_rh, mead_temp, mead_ws)
dailymeaddata <- reduce(.x = meaddatafiles, merge, by = c('Date'), all = T); dailymeaddata <- dailymeaddata[which(year(dailymeaddata$Date) >= 2001),c(1,9,11,14,17:20,22,25)]

# convert to monthly data
monthlypowelldata <- dailypowelldata %>% mutate(Month = month(Date), YearMonth = format(Date, "%Y-%m")) %>%
  group_by(YearMonth, Month) %>%
  summarise(generation_MWh = mean(GenerationMWh, na.rm = T), 
            avg_storage_acft = mean(storage_acft),
            min_inflow_m3s = min(inflow_m3s), avg_inflow_m3s = mean(inflow_m3s), max_inflow_m3s = max(inflow_m3s),
            min_outflow_m3s = min(outflow_m3s), avg_outflow_m3s = mean(outflow_m3s), max_outflow_m3s = max(outflow_m3s),
            total_precip_kgPm2 = sum(precip_kgPm2), 
            min_rh_perc = min(rh_perc), avg_rh_perc = mean(rh_perc), max_rh_perc = max(rh_perc),
            min_temp_C = min(temp_C), avg_temp_C = mean(temp_C), max_temp_C = max(temp_C),
            min_ws_mPs = min(ws_mPs), avg_ws_mPs = mean(ws_mPs), max_ws_mPs = max(ws_mPs)) %>%
  na.omit()

monthlyfolsomdata <- dailyfolsomdata %>% mutate(Month = month(Date), YearMonth = format(Date, "%Y-%m")) %>%
  group_by(YearMonth, Month) %>%
  summarise(generation_MWh = mean(GenerationMWh, na.rm = T), 
            avg_storage_acft = mean(storage_acft, na.rm = T),
            min_inflow_m3s = min(inflow_m3s), avg_inflow_m3s = mean(inflow_m3s), max_inflow_m3s = max(inflow_m3s),
            min_outflow_m3s = min(outflow_m3s), avg_outflow_m3s = mean(outflow_m3s), max_outflow_m3s = max(outflow_m3s),
            total_precip_kgPm2 = sum(precip_kgPm2, na.rm = T), 
            min_rh_perc = min(rh_perc, na.rm = T), avg_rh_perc = mean(rh_perc, na.rm = T), max_rh_perc = max(rh_perc, na.rm = T),
            min_temp_C = min(temp_C, na.rm = T), avg_temp_C = mean(temp_C, na.rm = T), max_temp_C = max(temp_C, na.rm = T),
            min_ws_mPs = min(ws_mPs, na.rm = T), avg_ws_mPs = mean(ws_mPs, na.rm = T), max_ws_mPs = max(ws_mPs, na.rm = T)) %>%
  na.omit()

monthlyorovilledata <- dailyorovilledata %>% mutate(Month = month(Date), YearMonth = format(Date, "%Y-%m")) %>%
  group_by(YearMonth, Month) %>%
  summarise(generation_MWh = mean(GenerationMWh, na.rm = T), 
            avg_storage_acft = mean(storage_acft, na.rm = T),
            min_inflow_m3s = min(inflow_m3s), avg_inflow_m3s = mean(inflow_m3s), max_inflow_m3s = max(inflow_m3s),
            min_outflow_m3s = min(outflow_m3s), avg_outflow_m3s = mean(outflow_m3s), max_outflow_m3s = max(outflow_m3s),
            total_precip_kgPm2 = sum(precip_kgPm2, na.rm = T), 
            min_rh_perc = min(rh_perc, na.rm = T), avg_rh_perc = mean(rh_perc, na.rm = T), max_rh_perc = max(rh_perc, na.rm = T),
            min_temp_C = min(temp_C, na.rm = T), avg_temp_C = mean(temp_C, na.rm = T), max_temp_C = max(temp_C, na.rm = T),
            min_ws_mPs = min(ws_mPs, na.rm = T), avg_ws_mPs = mean(ws_mPs, na.rm = T), max_ws_mPs = max(ws_mPs, na.rm = T)) %>%
  na.omit()
monthlyorovilledata <- monthlyorovilledata[1:292,]

monthlyshastadata <- dailyshastadata %>% mutate(Month = month(Date), YearMonth = format(Date, "%Y-%m")) %>%
  group_by(YearMonth, Month) %>%
  summarise(generation_MWh = mean(GenerationMWh, na.rm = T), 
            avg_storage_acft = mean(storage_acft, na.rm = T),
            min_inflow_m3s = min(inflow_m3s), avg_inflow_m3s = mean(inflow_m3s), max_inflow_m3s = max(inflow_m3s),
            min_outflow_m3s = min(outflow_m3s), avg_outflow_m3s = mean(outflow_m3s), max_outflow_m3s = max(outflow_m3s),
            total_precip_kgPm2 = sum(precip_kgPm2, na.rm = T), 
            min_rh_perc = min(rh_perc, na.rm = T), avg_rh_perc = mean(rh_perc, na.rm = T), max_rh_perc = max(rh_perc, na.rm = T),
            min_temp_C = min(temp_C, na.rm = T), avg_temp_C = mean(temp_C, na.rm = T), max_temp_C = max(temp_C, na.rm = T),
            min_ws_mPs = min(ws_mPs, na.rm = T), avg_ws_mPs = mean(ws_mPs, na.rm = T), max_ws_mPs = max(ws_mPs, na.rm = T)) %>%
  na.omit()

monthlymeaddata <- dailymeaddata %>% mutate(Month = month(Date), YearMonth = format(Date, "%Y-%m")) %>%
  group_by(YearMonth, Month) %>%
  summarise(generation_MWh = mean(GenerationMWh, na.rm = T), 
            avg_storage_acft = mean(storage_acft, na.rm = T),
            min_inflow_m3s = min(inflow_m3s), avg_inflow_m3s = mean(inflow_m3s), max_inflow_m3s = max(inflow_m3s),
            min_outflow_m3s = min(outflow_m3s), avg_outflow_m3s = mean(outflow_m3s), max_outflow_m3s = max(outflow_m3s),
            total_precip_kgPm2 = sum(precip_kgPm2, na.rm = T), 
            min_rh_perc = min(rh_perc, na.rm = T), avg_rh_perc = mean(rh_perc, na.rm = T), max_rh_perc = max(rh_perc, na.rm = T),
            min_temp_C = min(temp_C, na.rm = T), avg_temp_C = mean(temp_C, na.rm = T), max_temp_C = max(temp_C, na.rm = T),
            min_ws_mPs = min(ws_mPs, na.rm = T), avg_ws_mPs = mean(ws_mPs, na.rm = T), max_ws_mPs = max(ws_mPs, na.rm = T)) %>%
  na.omit()

# Future Simulations

# Inflow/Outflow
future_powell_inflow$Date <- ymd(future_powell_inflow$Date); names(future_powell_inflow)[3] <- 'inflow_m3s'
future_powell_outflow$Date <- ymd(future_powell_outflow$Date); names(future_powell_outflow)[3] <- 'outflow_m3s'

future_folsom_inflow$Date <- ymd(future_folsom_inflow$Date); names(future_folsom_inflow)[3] <- 'inflow_m3s'
future_folsom_outflow$Date <- ymd(future_folsom_outflow$Date); names(future_folsom_outflow)[3] <- 'outflow_m3s'

future_oroville_inflow$Date <- ymd(future_oroville_inflow$Date); names(future_oroville_inflow)[3] <- 'inflow_m3s'
future_oroville_outflow$Date <- ymd(future_oroville_outflow$Date); names(future_oroville_outflow)[3] <- 'outflow_m3s'

future_shasta_inflow$Date <- ymd(future_shasta_inflow$Date); names(future_shasta_inflow)[3] <- 'inflow_m3s'
future_shasta_outflow$Date <- ymd(future_shasta_outflow$Date); names(future_shasta_outflow)[3] <- 'outflow_m3s'

future_mead_inflow$Date <- ymd(future_mead_inflow$Date); names(future_mead_inflow)[3] <- 'inflow_m3s'
future_mead_outflow$Date <- ymd(future_mead_outflow$Date); names(future_mead_outflow)[3] <- 'outflow_m3s'

# Climate Variables
future_powell_climate <- future_powell_climate %>% select(!X) %>% pivot_wider(names_from = WeatherVar, values_from = WeatherVal) %>%
  mutate(temp_C = tas - 273.15, precip_kgPm2 = pr * 86400, 
         Model = case_when(Model == 'GFDL-ESM2M' ~ 'gfdl', 
                           Model == 'HadGEM2-ES' ~ 'hadgem',
                           Model == 'IPSL-CM5A-LR' ~ 'ipsl',
                           Model == 'MIROC5' ~ 'miroc'),
         ModelRun = case_when(ModelRun == 'historical' ~ 'hist',
                              ModelRun == 'rcp26' ~ 'rcp2p6',
                              ModelRun == 'rcp45' ~ 'rcp4p5',
                              ModelRun == 'rcp60' ~ 'rcp6p0',
                              ModelRun == 'rcp85' ~ 'rcp8p5')) 

future_folsom_climate <- future_folsom_climate %>% select(!X) %>% pivot_wider(names_from = WeatherVar, values_from = WeatherVal) %>%
  mutate(temp_C = tas - 273.15, precip_kgPm2 = pr * 86400,
         Model = case_when(Model == 'GFDL-ESM2M' ~ 'gfdl', 
                           Model == 'HadGEM2-ES' ~ 'hadgem',
                           Model == 'IPSL-CM5A-LR' ~ 'ipsl',
                           Model == 'MIROC5' ~ 'miroc'),
         ModelRun = case_when(ModelRun == 'historical' ~ 'hist',
                              ModelRun == 'rcp26' ~ 'rcp2p6',
                              ModelRun == 'rcp45' ~ 'rcp4p5',
                              ModelRun == 'rcp60' ~ 'rcp6p0',
                              ModelRun == 'rcp85' ~ 'rcp8p5')) 

future_oroville_climate <- future_oroville_climate %>% select(!X) %>% pivot_wider(names_from = WeatherVar, values_from = WeatherVal) %>%
  mutate(temp_C = tas - 273.15, precip_kgPm2 = pr * 86400,
         Model = case_when(Model == 'GFDL-ESM2M' ~ 'gfdl', 
                           Model == 'HadGEM2-ES' ~ 'hadgem',
                           Model == 'IPSL-CM5A-LR' ~ 'ipsl',
                           Model == 'MIROC5' ~ 'miroc'),
         ModelRun = case_when(ModelRun == 'historical' ~ 'hist',
                              ModelRun == 'rcp26' ~ 'rcp2p6',
                              ModelRun == 'rcp45' ~ 'rcp4p5',
                              ModelRun == 'rcp60' ~ 'rcp6p0',
                              ModelRun == 'rcp85' ~ 'rcp8p5')) 

future_shasta_climate <- future_shasta_climate %>% select(!X) %>% pivot_wider(names_from = WeatherVar, values_from = WeatherVal) %>%
  mutate(temp_C = tas - 273.15, precip_kgPm2 = pr * 86400,
         Model = case_when(Model == 'GFDL-ESM2M' ~ 'gfdl', 
                           Model == 'HadGEM2-ES' ~ 'hadgem',
                           Model == 'IPSL-CM5A-LR' ~ 'ipsl',
                           Model == 'MIROC5' ~ 'miroc'),
         ModelRun = case_when(ModelRun == 'historical' ~ 'hist',
                              ModelRun == 'rcp26' ~ 'rcp2p6',
                              ModelRun == 'rcp45' ~ 'rcp4p5',
                              ModelRun == 'rcp60' ~ 'rcp6p0',
                              ModelRun == 'rcp85' ~ 'rcp8p5')) 

future_mead_climate <- future_mead_climate %>% select(!X) %>% pivot_wider(names_from = WeatherVar, values_from = WeatherVal) %>%
  mutate(temp_C = tas - 273.15, precip_kgPm2 = pr * 86400,
         Model = case_when(Model == 'GFDL-ESM2M' ~ 'gfdl', 
                           Model == 'HadGEM2-ES' ~ 'hadgem',
                           Model == 'IPSL-CM5A-LR' ~ 'ipsl',
                           Model == 'MIROC5' ~ 'miroc'),
         ModelRun = case_when(ModelRun == 'historical' ~ 'hist',
                              ModelRun == 'rcp26' ~ 'rcp2p6',
                              ModelRun == 'rcp45' ~ 'rcp4p5',
                              ModelRun == 'rcp60' ~ 'rcp6p0',
                              ModelRun == 'rcp85' ~ 'rcp8p5')) 

# merge by lake
powelldatafiles <- list(future_powell_inflow, future_powell_outflow, future_powell_climate)
futuredailypowelldata <- reduce(.x = powelldatafiles, merge, by = c('Date', 'Model', 'ModelRun'), all = T); futuredailypowelldata <- futuredailypowelldata[which(year(futuredailypowelldata$Date) >= 1976),c(1:3,5,7:9,15,16)]

folsomdatafiles <- list(future_folsom_inflow, future_folsom_outflow, future_folsom_climate)
futuredailyfolsomdata <- reduce(.x = folsomdatafiles, merge, by = c('Date', 'Model', 'ModelRun'), all = T); futuredailyfolsomdata <- futuredailyfolsomdata[which(year(futuredailyfolsomdata$Date) >= 1976),c(1:3,5,7:9,15,16)]

orovilledatafiles <- list(future_oroville_inflow, future_oroville_outflow, future_oroville_climate)
futuredailyorovilledata <- reduce(.x = orovilledatafiles, merge, by = c('Date', 'Model', 'ModelRun'), all = T); futuredailyorovilledata <- futuredailyorovilledata[which(year(futuredailyorovilledata$Date) >= 1976),c(1:3,5,7:9,15,16)]

shastadatafiles <- list(future_shasta_inflow, future_shasta_outflow, future_shasta_climate)
futuredailyshastadata <- reduce(.x = shastadatafiles, merge, by = c('Date', 'Model', 'ModelRun'), all = T); futuredailyshastadata <- futuredailyshastadata[which(year(futuredailyshastadata$Date) >= 1976),c(1:3,5,7:9,15,16)]

meaddatafiles <- list(future_mead_inflow, future_mead_outflow, future_mead_climate)
futuredailymeaddata <- reduce(.x = meaddatafiles, merge, by = c('Date', 'Model', 'ModelRun'), all = T); futuredailymeaddata <- futuredailymeaddata[which(year(futuredailymeaddata$Date) >= 1976),c(1:3,5,7:9,15,16)]

# convert to monthly data
futuremonthlypowelldata <- futuredailypowelldata %>% mutate(Year = year(Date), Month = month(Date), YearMonth = format(Date, "%Y-%m")) %>%
  group_by(YearMonth, Month, Year, Model, ModelRun) %>%
  summarise(min_inflow_m3s = min(inflow_m3s, na.rm = T), avg_inflow_m3s = mean(inflow_m3s, na.rm = T), max_inflow_m3s = max(inflow_m3s, na.rm = T),
            min_outflow_m3s = min(outflow_m3s, na.rm = T), avg_outflow_m3s = mean(outflow_m3s, na.rm = T), max_outflow_m3s = max(outflow_m3s, na.rm = T),
            total_precip_kgPm2 = sum(precip_kgPm2, na.rm = T), 
            min_rh_perc = min(hurs, na.rm = T), avg_rh_perc = mean(hurs, na.rm = T), max_rh_perc = max(hurs, na.rm = T),
            min_temp_C = min(temp_C, na.rm = T), avg_temp_C = mean(temp_C, na.rm = T), max_temp_C = max(temp_C, na.rm = T),
            min_ws_mPs = min(sfcWind, na.rm = T), avg_ws_mPs = mean(sfcWind, na.rm = T), max_ws_mPs = max(sfcWind, na.rm = T)
            ) %>% na.omit()

futuremonthlyfolsomdata <- futuredailyfolsomdata %>% mutate(Year = year(Date), Month = month(Date), YearMonth = format(Date, "%Y-%m")) %>%
  group_by(YearMonth, Month, Year, Model, ModelRun) %>%
  summarise(min_inflow_m3s = min(inflow_m3s, na.rm = T), avg_inflow_m3s = mean(inflow_m3s, na.rm = T), max_inflow_m3s = max(inflow_m3s, na.rm = T),
            min_outflow_m3s = min(outflow_m3s, na.rm = T), avg_outflow_m3s = mean(outflow_m3s, na.rm = T), max_outflow_m3s = max(outflow_m3s, na.rm = T),
            total_precip_kgPm2 = sum(precip_kgPm2, na.rm = T), 
            min_rh_perc = min(hurs, na.rm = T), avg_rh_perc = mean(hurs, na.rm = T), max_rh_perc = max(hurs, na.rm = T),
            min_temp_C = min(temp_C, na.rm = T), avg_temp_C = mean(temp_C, na.rm = T), max_temp_C = max(temp_C, na.rm = T),
            min_ws_mPs = min(sfcWind, na.rm = T), avg_ws_mPs = mean(sfcWind, na.rm = T), max_ws_mPs = max(sfcWind, na.rm = T)
            ) %>% na.omit()

futuremonthlyorovilledata <- futuredailyorovilledata %>% mutate(Year = year(Date), Month = month(Date), YearMonth = format(Date, "%Y-%m")) %>%
  group_by(YearMonth, Month, Year, Model, ModelRun) %>%
  summarise(min_inflow_m3s = min(inflow_m3s, na.rm = T), avg_inflow_m3s = mean(inflow_m3s, na.rm = T), max_inflow_m3s = max(inflow_m3s, na.rm = T),
           min_outflow_m3s = min(outflow_m3s, na.rm = T), avg_outflow_m3s = mean(outflow_m3s, na.rm = T), max_outflow_m3s = max(outflow_m3s, na.rm = T),
           total_precip_kgPm2 = sum(precip_kgPm2, na.rm = T), 
           min_rh_perc = min(hurs, na.rm = T), avg_rh_perc = mean(hurs, na.rm = T), max_rh_perc = max(hurs, na.rm = T),
           min_temp_C = min(temp_C, na.rm = T), avg_temp_C = mean(temp_C, na.rm = T), max_temp_C = max(temp_C, na.rm = T),
           min_ws_mPs = min(sfcWind, na.rm = T), avg_ws_mPs = mean(sfcWind, na.rm = T), max_ws_mPs = max(sfcWind, na.rm = T)
  ) %>% na.omit()

futuremonthlyshastadata <- futuredailyshastadata %>% mutate(Year = year(Date), Month = month(Date), YearMonth = format(Date, "%Y-%m")) %>%
  group_by(YearMonth, Month, Year, Model, ModelRun) %>%
  summarise(min_inflow_m3s = min(inflow_m3s, na.rm = T), avg_inflow_m3s = mean(inflow_m3s, na.rm = T), max_inflow_m3s = max(inflow_m3s, na.rm = T),
            min_outflow_m3s = min(outflow_m3s, na.rm = T), avg_outflow_m3s = mean(outflow_m3s, na.rm = T), max_outflow_m3s = max(outflow_m3s, na.rm = T),
            total_precip_kgPm2 = sum(precip_kgPm2, na.rm = T), 
            min_rh_perc = min(hurs, na.rm = T), avg_rh_perc = mean(hurs, na.rm = T), max_rh_perc = max(hurs, na.rm = T),
            min_temp_C = min(temp_C, na.rm = T), avg_temp_C = mean(temp_C, na.rm = T), max_temp_C = max(temp_C, na.rm = T),
            min_ws_mPs = min(sfcWind, na.rm = T), avg_ws_mPs = mean(sfcWind, na.rm = T), max_ws_mPs = max(sfcWind, na.rm = T)
  ) %>% na.omit()

futuremonthlymeaddata <- futuredailymeaddata %>% mutate(Year = year(Date), Month = month(Date), YearMonth = format(Date, "%Y-%m")) %>%
  group_by(YearMonth, Month, Year, Model, ModelRun) %>%
  summarise(min_inflow_m3s = min(inflow_m3s, na.rm = T), avg_inflow_m3s = mean(inflow_m3s, na.rm = T), max_inflow_m3s = max(inflow_m3s, na.rm = T),
            min_outflow_m3s = min(outflow_m3s, na.rm = T), avg_outflow_m3s = mean(outflow_m3s, na.rm = T), max_outflow_m3s = max(outflow_m3s, na.rm = T),
            total_precip_kgPm2 = sum(precip_kgPm2, na.rm = T), 
            min_rh_perc = min(hurs, na.rm = T), avg_rh_perc = mean(hurs, na.rm = T), max_rh_perc = max(hurs, na.rm = T),
            min_temp_C = min(temp_C, na.rm = T), avg_temp_C = mean(temp_C, na.rm = T), max_temp_C = max(temp_C, na.rm = T),
            min_ws_mPs = min(sfcWind, na.rm = T), avg_ws_mPs = mean(sfcWind, na.rm = T), max_ws_mPs = max(sfcWind, na.rm = T)
  ) %>% na.omit()

# save rdata
setwd(rdatadir)
save(monthlyfolsomdata, monthlymeaddata, monthlyorovilledata, monthlypowelldata, monthlyshastadata, 
     futuremonthlyfolsomdata, futuremonthlymeaddata, futuremonthlyorovilledata, futuremonthlypowelldata, futuremonthlyshastadata, 
     file = 'preprocesseddata.rdata')

############# MODEL DEVELOPMENT #############

setwd(rdatadir)
load('preprocesseddata.rdata')

# labels for reservoirs
reservoirnames <- c('Powell', 'Folsom', 'Oroville', 'Shasta', 'Mead')

# labels for seasons [summer, winter]
seasonindices <- list(c(4:9), c(10:12,1:3))
seasonnames <- c('summer', 'winter')

# labels for response variables
responseindices <- list(1, 2, c(1, 2))
responsenames <- list('generation_MWh', 'storage_acft', c('generation_MWh', 'storage_acft'))
modelnames <- c('generation', 'storage', 'multi-outcome')

# combine all data
allmonthlydata <- list(monthlypowelldata, monthlyfolsomdata, monthlyorovilledata, monthlyshastadata, monthlymeaddata)

# initialize variables
predictiondata_reservoirs <- list()

# loop through reservoirs
for (i in 1:length(allmonthlydata)) {
  reservoirofinterest <- allmonthlydata[[i]]
  
  # initialize variables
  predictiondata_seasons <- list()
  
  # loop through seasons
  for (t in 1:2) {
    seasonofinterest <- reservoirofinterest[reservoirofinterest$Month %in% seasonindices[[t]],-c(1,2)]
    
    # initialize variables
    predictiondata_models <- list()
    
    # loop through models
    for (m in 1:3) {
      # set up cross validation with selected variables
      n <- nrow(seasonofinterest)
      set.seed(11)
      updateddata <- seasonofinterest[sample(n),]
      k <- 5
      folds <- cut(seq(1,n), breaks = k, labels = FALSE)
      
      # extract response and predictor variables
      responsevars <- updateddata[,responseindices[[m]]]
      predictorvars <- updateddata[,3:18]
      
      # initialize variables
      yhat_all <- c(); ytest_all <- c()
      
      # run models 
      for (j in 1:k) {
        # split into training and test dataset
        testIndex <- which(folds == j, arr.ind = TRUE)
        trainingX <- as.matrix(predictorvars[-testIndex,])
        trainingY <- as.matrix(responsevars[-testIndex,])
        testingX <- as.matrix(predictorvars[testIndex,])
        testingY <- as.matrix(responsevars[testIndex,])
        
        # build model & generate predictions
        ypred <- build_forest_predict(trainingX, trainingY, n_tree = 100, m_feature = 5, min_leaf = 10, testX = testingX)
        
        # store data 
        yhat_all <- rbind(yhat_all, ypred)
        ytest_all <- rbind(ytest_all, testingY)
      }
      
      # store all model data
      predictiondata_models[[modelnames[m]]] <- data.frame('predicted' = yhat_all, 'actual' = ytest_all)
    }
    
    # store all season data
    predictiondata_seasons[[seasonnames[t]]] <- predictiondata_models
  }
  
  # store all reservoir data
  predictiondata_reservoirs[[reservoirnames[i]]] <- predictiondata_seasons
} 

# save as rdata
setwd(rdatadir)
save(allmonthlydata, predictiondata_reservoirs, file = 'observationalresults.rdata')

############# CLIMATE CHANGE PROJECTIONS #############

setwd(rdatadir)
load('preprocesseddata.rdata')

# labels for reservoirs
reservoirnames <- c('Powell', 'Folsom', 'Oroville', 'Shasta', 'Mead')

# labels for seasons [summer, winter]
seasonindices <- list(c(4:9), c(10:12,1:3))
seasonnames <- c('summer', 'winter')

# labels for response variables
responseindices <- list(1, 2, c(1, 2))
responsenames <- list('generation_MWh', 'storage_acft', c('generation_MWh', 'storage_acft'))
modelnames <- c('generation', 'storage', 'multi-outcome')

# labels for GCMs
gcmnames <- c('gfdl', 'hadgem', 'ipsl', 'miroc')

# labels for scenarios
scenarionames <- c('hist', 'rcp2p6', 'rcp4p5', 'rcp6p0', 'rcp8p5')

# combine all data
allmonthlydata <- list(monthlypowelldata, monthlyfolsomdata, monthlyorovilledata, monthlyshastadata, monthlymeaddata)
allfuturedata <- list(futuremonthlypowelldata, futuremonthlyfolsomdata, futuremonthlyorovilledata, futuremonthlyshastadata, futuremonthlymeaddata)

# initialize variables
projectiondata_reservoirs <- list()

# loop through reservoirs
for (i in 1:length(allmonthlydata)) {
  reservoirofinterest <- allmonthlydata[[i]]
  futurereservoirofinterest <- allfuturedata[[i]]
  
  # initialize variables
  projectiondata_seasons <- list()
  
  # loop through seasons
  for (t in 1:2) {
    seasonofinterest <- reservoirofinterest[reservoirofinterest$Month %in% seasonindices[[t]],-c(1,2)]
    futureseasonofinterest <- futurereservoirofinterest[futurereservoirofinterest$Month %in% seasonindices[[t]],]
    
    # initialize variables
    projectiondata_models <- list()
    
    # loop through models
    for (m in 1:3) {
      # extract response and predictor variables
      responsevars <- seasonofinterest[,responseindices[[m]]]
      predictorvars <- seasonofinterest[,3:18]
      
      # initialize variables
      projectiondata_gcms <- list()
      
      for (g in 1:length(gcmnames)) {
        gcmofinterest <- futureseasonofinterest[which(futureseasonofinterest$Model == gcmnames[g]),]
        
        # initialize variables
        projectiondata_scenarios <- list()
        
        for (s in 1:length(scenarionames)) {
          scenarioofinterest <- gcmofinterest[which(gcmofinterest$ModelRun == scenarionames[s]),]
          
          # separate observational data for training and simulated data for projecting
          trainingX <- as.matrix(predictorvars)
          trainingY <- as.matrix(responsevars)
          testingX <- as.matrix(scenarioofinterest[,-c(1:5)])
          
          # build model & generate predictions
          ypred <- build_forest_predict(trainingX, trainingY, n_tree = 100, m_feature = 5, min_leaf = 10, testX = testingX)
          
          # store scenario data
          projectiondata_scenarios[[scenarionames[s]]] <- data.frame('date' = scenarioofinterest$YearMonth, 'year' = scenarioofinterest$Year, 'projections' = ypred)
        }
        
        # store gcm data
        projectiondata_gcms[[gcmnames[g]]] <- projectiondata_scenarios
      }
      
      # store all model data
      projectiondata_models[[modelnames[m]]] <- projectiondata_gcms
    }
    
    # store all season data
    projectiondata_seasons[[seasonnames[t]]] <- projectiondata_models
  }
  
  # store all reservoir data
  projectiondata_reservoirs[[reservoirnames[i]]] <- projectiondata_seasons
} 

# save as rdata
setwd(rdatadir)
save(allfuturedata, projectiondata_reservoirs, file = 'projectionresults.rdata')


############# INTERPRETATION #############

setwd(rdatadir)
load('observationalresults.rdata')
load('projectionresults.rdata')

# Model Performance (Observations)

# initialize variables
allnrmse <- data.frame('NRMSE' = numeric(), 'Variable' = character(), 'Model' = character(), 'Season' = character(), 'Reservoir' = character())

for (r in 1:length(predictiondata_reservoirs)) {
  reservoirofinterest <- predictiondata_reservoirs[[r]]
  
  for (t in 1:length(reservoirofinterest)) {
    seasonofinterest <- reservoirofinterest[[t]]
    
    for (m in 1:length(seasonofinterest)) {
      modelofinterest <- seasonofinterest[[m]]
      
      if (length(modelofinterest) < 3) {
        nrmsecalc <- data.frame('NRMSE' = nrmse(obs = modelofinterest[,2], sim = modelofinterest[,1], norm = 'maxmin'), 'Variable' = names(seasonofinterest)[m], 'Model' = 'Single', 'Season' = names(reservoirofinterest)[t], 'Reservoir' = names(predictiondata_reservoirs[r]))
      } else {
        nrmsecalc <- data.frame('NRMSE' = nrmse(obs = modelofinterest[,c(3,4)], sim = modelofinterest[,c(1,2)], norm = 'maxmin'), 'Variable' = names(modelofinterest)[c(3,4)], 'Model' = 'Multi', 'Season' = names(reservoirofinterest)[t], 'Reservoir' = names(predictiondata_reservoirs[r]))
        rownames(nrmsecalc) <- NULL
      }
      
      allnrmse <- rbind(allnrmse, nrmsecalc)
    }
  }
}

# rename variable items
allnrmse <- allnrmse %>% mutate(Variable = case_when(allnrmse$Variable == 'generation' ~ 'Generation', 
                                                     allnrmse$Variable == 'storage' ~ 'Storage',
                                                     allnrmse$Variable == 'actual.generation_MWh' ~ 'Generation',
                                                     allnrmse$Variable == 'actual.avg_storage_acft' ~ 'Storage'))

# write to csv file
setwd(outputdir)
write.csv(allnrmse, 'predictions_nrmse.csv')

# t-tests for significance (Observations)

# initialize variables
allpvalues <- data.frame('p-value' = numeric(), 'Variable' = character(), 'Model' = character(), 'Season' = character(), 'Reservoir' = character())

for (r in 1:length(predictiondata_reservoirs)) {
  reservoirofinterest <- predictiondata_reservoirs[[r]]
  
  for (t in 1:length(reservoirofinterest)) {
    seasonofinterest <- reservoirofinterest[[t]]
    
    for (v in 1:2) {
      variableofinterest <- seasonofinterest[[v]]
      
      # calculate p-value
      pvaluecalc <- data.frame('p-value' = t.test(variableofinterest$predicted, seasonofinterest[[3]][,v])$p.value, 'Variable' = names(seasonofinterest)[v], 'Season' = names(reservoirofinterest)[t], 'Reservoir' = names(predictiondata_reservoirs[r]))
      
      # store
      allpvalues <- rbind(allpvalues, pvaluecalc)
    }
  }
}

# write to csv file
setwd(outputdir)
write.csv(allpvalues, 'predictions_pvalues.csv')

# Percent Change (Projections)

allpc <- percentchange <- data.frame('PC' = numeric(), 'Reservoir' = character(), 'Season' = character(), 'Model' = character(), 'Variable' = character(), 'GCM' = character(), 'Scenario' = character())

for (r in 1:length(predictiondata_reservoirs)) {
  reservoirofinterest <- projectiondata_reservoirs[[r]]
  
  for (t in 1:length(reservoirofinterest)) {
    seasonofinterest <- reservoirofinterest[[t]]
    
    for (m in 1:length(seasonofinterest)) {
      modelofinterest <- seasonofinterest[[m]]
      
      for (g in 1:length(modelofinterest)) {
        gcmofinterest <- modelofinterest[[g]]
        
        for (s in 2:length(gcmofinterest)) {
          scenarioofinterst <- gcmofinterest[[s]]
          
          # extract years of interest
          historicaldata <- rbind(gcmofinterest[[1]][which(gcmofinterest[[1]]$year > 2000),], scenarioofinterst[which(scenarioofinterst$year <= 2020),])
          futuredata <- scenarioofinterst[which(scenarioofinterst$year > 2040 & scenarioofinterst$year <= 2060),]
          
          if (length(scenarioofinterst) < 4) {
            # PC = (new - old) / old * 100
            percentchange <- data.frame('PC' = (mean(futuredata[,3]) - mean(historicaldata[,3]))/mean(historicaldata[,3]) * 100, 
                                        'Reservoir' = names(projectiondata_reservoirs)[r], 'Season' = names(reservoirofinterest)[t], 
                                        'Model' = 'Single', 'Variable' = names(seasonofinterest)[m], 'GCM' = names(modelofinterest)[g], 
                                        'Scenario' = names(gcmofinterest)[s])
          } else {
            percentchange <- data.frame('PC' = c((mean(futuredata[,3]) - mean(historicaldata[,3]))/mean(historicaldata[,3]) * 100, 
                                                 (mean(futuredata[,4]) - mean(historicaldata[,4]))/mean(historicaldata[,4]) * 100), 
                                        'Reservoir' = names(projectiondata_reservoirs)[r], 'Season' = names(reservoirofinterest)[t], 
                                        'Model' = 'Multi', 'Variable' = names(seasonofinterest)[c(1,2)], 'GCM' = names(modelofinterest)[g], 
                                        'Scenario' = names(gcmofinterest)[s])
          }
          
          allpc <- rbind(allpc, percentchange)
        }
      }
      
    }
  }
}

# write to csv file
setwd(outputdir)
write.csv(allpc, 'projections_pc.csv')

# save as rdata
setwd(rdatadir)
save(allnrmse, allpc, file = 'interpretation.rdata')


############# FIGURES AND TABLES #############

setwd(rdatadir)
load('observationalresults.rdata')
load('projectionresults.rdata')
load('interpretation.rdata')

# FIGURES - NRMSE FOR SINGLE OUTCOME MODELS

plotdata <- allnrmse[which(allnrmse$Model == 'Single'),]

pdf('singlenrmse_SW.pdf', width = 6, height = 5)
ggplot(plotdata) + geom_bar(aes(x = Reservoir, y = NRMSE, fill = Season), position = 'dodge', stat = 'identity') +
  facet_wrap(~Variable, scales = 'free', nrow = 2) + theme_light() + theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('#d8b365', '#5ab4ac'), labels = c('Summer', 'Winter'))
dev.off()

pdf('singlenrmse_GS.pdf', width = 6, height = 5)
ggplot(plotdata) + geom_bar(aes(x = Reservoir, y = NRMSE, fill = Variable), position = 'dodge', stat = 'identity') +
  facet_wrap(~Season, scales = 'free', nrow = 2) + theme_light() + theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('#fdae61', '#66c2a5'))
dev.off()

# FIGURES - NRMSE FOR MULTI OUTCOME MODELS

plotdata <- allnrmse[which(allnrmse$Model == 'Multi'),]

pdf('multinrmse_SW.pdf', width = 6, height = 5)
ggplot(plotdata) + geom_bar(aes(x = Reservoir, y = NRMSE, fill = Season), position = 'dodge', stat = 'identity') +
  facet_wrap(~Variable, scales = 'free', nrow = 2) + theme_light() + theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('#d8b365', '#5ab4ac'), labels = c('Summer', 'Winter'))
dev.off()

pdf('multinrmse_GS.pdf', width = 6, height = 5)
ggplot(plotdata) + geom_bar(aes(x = Reservoir, y = NRMSE, fill = Variable), position = 'dodge', stat = 'identity') +
  facet_wrap(~Season, scales = 'free', nrow = 2) + theme_light() + theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('#fdae61', '#66c2a5'))
dev.off()

# FIGURE - NRMSE COMPARING SINGLE AND MULTI OUTCOME MODELS

plotdata <- allnrmse

pdf('comparisonnrmse.pdf', width = 6, height = 5)
ggplot(plotdata) + geom_bar(aes(x = Reservoir, y = NRMSE, fill = Model), position = 'dodge', stat = 'identity') +
  facet_wrap(~Season + Variable, scales = 'free', nrow = 2) + theme_light() + theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('#7fc97f', '#beaed4'), labels = c('Multi-Outcome', 'Single Outcome'))
dev.off()

# FIGURES - PREDICTED VS ACTUAL FOR SINGLE AND MULTI-OUTCOME MODELS

# initialize variables
allvalues <- data.frame('predicted' = numeric(), 'actual' = numeric(), 'Variable' = character(), 'Model' = character(), 'Season' = character(), 'Reservoir' = character())

for (r in 1:length(predictiondata_reservoirs)) {
  reservoirofinterest <- predictiondata_reservoirs[[r]]
  
  for (t in 1:length(reservoirofinterest)) {
    seasonofinterest <- reservoirofinterest[[t]]
    
    for (m in 1:length(seasonofinterest)) {
      modelofinterest <- seasonofinterest[[m]]
      
      # extract and store values of interest
      if (m < 3) {
        modelvalues <- data.frame('predicted' = modelofinterest[,1], 'actual' = modelofinterest[,2], 'Variable' = names(modelofinterest)[2], 'Model' = 'Single', 'Season' = names(reservoirofinterest)[t], 'Reservoir' = names(predictiondata_reservoirs[r]))
      } else {
        modelvalues <- data.frame('predicted' = c(modelofinterest[,1], modelofinterest[,2]), 'actual' = c(modelofinterest[,3], modelofinterest[,4]), 'Variable' = c(rep('generation_MWh', length(modelofinterest[,3])), rep('avg_storage_acft', length(modelofinterest[,4]))), 'Model' = 'Multi', 'Season' = names(reservoirofinterest)[t], 'Reservoir' = names(predictiondata_reservoirs[r]))
      }
      
      # merge
      allvalues <- rbind(allvalues, modelvalues)
    }
  }
}

allvalues$Variable[which(allvalues$Variable == 'generation_MWh')] <- "Generation"
allvalues$Variable[which(allvalues$Variable == 'avg_storage_acft')] <- "Storage"

pdf('comparison_pVa.pdf', width = 6, height = 5)
ggplot(allvalues) + geom_point(aes(x = actual/100000, y = predicted/100000, color = Reservoir, shape = Model), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + ylab('Predicted Values') + xlab('Actual Values') +
  facet_wrap(~Season + Variable, scales = 'free', nrow = 2) + theme_light() + theme(legend.position = 'bottom') +
  scale_color_manual(values = c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c', '#fb9a99'), labels = c('Folsom', 'Mead', 'Oroville', 'Powell', 'Shasta')) +
  scale_shape_discrete(guide = 'none') # circle = multi; triangle = single
dev.off()


# FIGURES - PERCENT CHANGE FOR SINGLE OUTCOME MODELS 

allpc$Variable[which(allpc$Variable == 'generation')] <- "Generation"
allpc$Variable[which(allpc$Variable == 'storage')] <- "Storage"

plotdata <- allpc[which(allpc$Model == 'Single'),]

plotdata <- plotdata %>% group_by(Reservoir, Season, Variable, Scenario) %>%
  summarize(mins = min(PC, na.rm = T), maxs = max(PC, na.rm = T), means = mean(PC, na.rm = T), medians = median(PC, na.rm = T))

pdf('singlepc_avg_summer.pdf', width = 7.5, height = 5.5)
ggplot(plotdata[which(plotdata$Season == 'summer'),], aes(x = Reservoir, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = means)) + geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = Scenario), stat = 'identity') + ylab('Percent Change') +
  facet_wrap(~Variable, scales = 'free', nrow = 2) + theme_light() + theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('#fef0d9', '#fdcc8a', '#fc8d59', '#d7301f'), labels = c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5'))
dev.off()

pdf('singlepc_med_summer.pdf', width = 7.5, height = 5.5)
ggplot(plotdata[which(plotdata$Season == 'summer'),], aes(x = Reservoir, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = medians)) + geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = Scenario), stat = 'identity') + ylab('Percent Change') +
  facet_wrap(~Variable, scales = 'free', nrow = 2) + theme_light() + theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('#fef0d9', '#fdcc8a', '#fc8d59', '#d7301f'), labels = c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5'))
dev.off()

pdf('singlepc_avg_winter.pdf', width = 7.5, height = 5.5)
ggplot(plotdata[which(plotdata$Season == 'winter'),], aes(x = Reservoir, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = means)) + geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = Scenario), stat = 'identity') + ylab('Percent Change') +
  facet_wrap(~Variable, scales = 'free', nrow = 2) + theme_light() + theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('#fef0d9', '#fdcc8a', '#fc8d59', '#d7301f'), labels = c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5'))
dev.off()

pdf('singlepc_med_winter.pdf', width = 7.5, height = 5.5)
ggplot(plotdata[which(plotdata$Season == 'winter'),], aes(x = Reservoir, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = medians)) + geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = Scenario), stat = 'identity') + ylab('Percent Change') +
  facet_wrap(~Variable, scales = 'free', nrow = 2) + theme_light() + theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('#fef0d9', '#fdcc8a', '#fc8d59', '#d7301f'), labels = c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5'))
dev.off()

# FIGURES - PERCENT CHANGE FOR MULTI OUTCOME MODELS 

plotdata <- allpc[which(allpc$Model == 'Multi'),]

plotdata <- plotdata %>% group_by(Reservoir, Season, Variable, Scenario) %>%
  summarize(mins = min(PC, na.rm = T), maxs = max(PC, na.rm = T), means = mean(PC, na.rm = T), medians = median(PC, na.rm = T))

pdf('multipc_avg_summer.pdf', width = 7.5, height = 5.5)
ggplot(plotdata[which(plotdata$Season == 'summer'),], aes(x = Reservoir, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = means)) + geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = Scenario), stat = 'identity') + ylab('Percent Change') +
  facet_wrap(~Variable, scales = 'free', nrow = 2) + theme_light() + theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('#fef0d9', '#fdcc8a', '#fc8d59', '#d7301f'), labels = c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5'))
dev.off()

pdf('multipc_med_summer.pdf', width = 7.5, height = 5.5)
ggplot(plotdata[which(plotdata$Season == 'summer'),], aes(x = Reservoir, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = medians)) + geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = Scenario), stat = 'identity') + ylab('Percent Change') +
  facet_wrap(~Variable, scales = 'free', nrow = 2) + theme_light() + theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('#fef0d9', '#fdcc8a', '#fc8d59', '#d7301f'), labels = c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5'))
dev.off()

pdf('multipc_avg_winter.pdf', width = 7.5, height = 5.5)
ggplot(plotdata[which(plotdata$Season == 'winter'),], aes(x = Reservoir, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = means)) + geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = Scenario), stat = 'identity') + ylab('Percent Change') +
  facet_wrap(~Variable, scales = 'free', nrow = 2) + theme_light() + theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('#fef0d9', '#fdcc8a', '#fc8d59', '#d7301f'), labels = c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5'))
dev.off()

pdf('multipc_med_winter.pdf', width = 7.5, height = 5.5)
ggplot(plotdata[which(plotdata$Season == 'winter'),], aes(x = Reservoir, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = medians)) + geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = Scenario), stat = 'identity') + ylab('Percent Change') +
  facet_wrap(~Variable, scales = 'free', nrow = 2) + theme_light() + theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('#fef0d9', '#fdcc8a', '#fc8d59', '#d7301f'), labels = c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5'))
dev.off()

# FIGURES - COMPARISON OF PERCENT CHANGE 

allpc$Scenario[which(allpc$Scenario == 'rcp2p6')] <- 'RCP2.6'
allpc$Scenario[which(allpc$Scenario == 'rcp4p5')] <- 'RCP4.5'
allpc$Scenario[which(allpc$Scenario == 'rcp6p0')] <- 'RCP6.0'
allpc$Scenario[which(allpc$Scenario == 'rcp8p5')] <- 'RCP8.5'

plotdata <- allpc %>% group_by(Reservoir, Season, Model, Variable, Scenario) %>%
  summarize(mins = min(PC, na.rm = T), maxs = max(PC, na.rm = T), means = mean(PC, na.rm = T), medians = median(PC, na.rm = T))

pdf('comppc_avg_summer.pdf', width = 8, height = 6)
ggplot(plotdata[which(plotdata$Season == 'summer'),], aes(x = Reservoir, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = means)) + geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = Model), stat = 'identity') +
  facet_wrap(~Variable + Scenario, scales = 'free', nrow = 2) + theme_light() + 
  theme(legend.position = 'bottom', axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c('#7fc97f', '#beaed4'), labels = c('Multi-Outcome', 'Single Outcome'))
dev.off()

pdf('comppc_med_summer.pdf', width = 8, height = 6)
ggplot(plotdata[which(plotdata$Season == 'summer'),], aes(x = Reservoir, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = medians)) + geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = Model), stat = 'identity') +
  facet_wrap(~Variable + Scenario, scales = 'free', nrow = 2) + theme_light() + 
  theme(legend.position = 'bottom', axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c('#7fc97f', '#beaed4'), labels = c('Multi-Outcome', 'Single Outcome'))
dev.off()

pdf('comppc_avg_winter.pdf', width = 8, height = 6)
ggplot(plotdata[which(plotdata$Season == 'winter'),], aes(x = Reservoir, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = means)) + geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = Model), stat = 'identity') +
  facet_wrap(~Variable + Scenario, scales = 'free', nrow = 2) + theme_light() + 
  theme(legend.position = 'bottom', axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c('#7fc97f', '#beaed4'), labels = c('Multi-Outcome', 'Single Outcome'))
dev.off()

pdf('comppc_med_winter.pdf', width = 8, height = 6)
ggplot(plotdata[which(plotdata$Season == 'winter'),], aes(x = Reservoir, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = medians)) + geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = Model), stat = 'identity') +
  facet_wrap(~Variable + Scenario, scales = 'free', nrow = 2) + theme_light() + 
  theme(legend.position = 'bottom', axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c('#7fc97f', '#beaed4'), labels = c('Multi-Outcome', 'Single Outcome'))
dev.off()

# FIGURE - ACTUAL MWH/ACFT PROJECTIONS

allgcmproj <- data.frame('Change' = numeric(), 'Reservoir' = character(), 'Season' = character(), 'Model' = character(), 'Variable' = character(), 'GCM' = character(), 'Scenario' = character())

for (r in 1:length(predictiondata_reservoirs)) {
  reservoirofinterest <- projectiondata_reservoirs[[r]]
  
  for (t in 1:length(reservoirofinterest)) {
    seasonofinterest <- reservoirofinterest[[t]]
    
    for (m in 1:length(seasonofinterest)) {
      modelofinterest <- seasonofinterest[[m]]
      
      for (g in 1:length(modelofinterest)) {
        gcmofinterest <- modelofinterest[[g]]
        
        for (s in 2:length(gcmofinterest)) {
          scenarioofinterst <- gcmofinterest[[s]]
          
          # extract years of interest
          historicaldata <- rbind(gcmofinterest[[1]][which(gcmofinterest[[1]]$year > 2000),], scenarioofinterst[which(scenarioofinterst$year <= 2020),])
          futuredata <- scenarioofinterst[which(scenarioofinterst$year > 2040 & scenarioofinterst$year <= 2060),]
          
          if (length(scenarioofinterst) < 4) {
            # PC = (new - old) / old * 100
            change <- data.frame('Change' = mean(futuredata[,3]) - mean(historicaldata[,3]), 
                                 'Reservoir' = names(projectiondata_reservoirs)[r], 'Season' = names(reservoirofinterest)[t], 
                                 'Model' = 'Single', 'Variable' = names(seasonofinterest)[m], 'GCM' = names(modelofinterest)[g], 
                                 'Scenario' = names(gcmofinterest)[s])
          } else {
            change <- data.frame('Change' = c(mean(futuredata[,3]) - mean(historicaldata[,3]), mean(futuredata[,4]) - mean(historicaldata[,4])), 
                                 'Reservoir' = names(projectiondata_reservoirs)[r], 'Season' = names(reservoirofinterest)[t], 
                                 'Model' = 'Multi', 'Variable' = names(seasonofinterest)[c(1,2)], 'GCM' = names(modelofinterest)[g], 
                                 'Scenario' = names(gcmofinterest)[s])
          }
          
          allgcmproj <- rbind(allgcmproj, change)
        }
      }
      
    }
  }
}

allgcmproj$Variable[which(allgcmproj$Variable == 'generation')] <- "Generation"
allgcmproj$Variable[which(allgcmproj$Variable == 'storage')] <- "Storage"

plotdata <- allgcmproj[which(allgcmproj$Model == 'Multi'),]

plotdata <- plotdata %>% group_by(Reservoir, Season, Variable, Scenario) %>%
  summarize(mins = min(Change, na.rm = T), maxs = max(Change, na.rm = T), means = mean(Change, na.rm = T), medians = median(Change, na.rm = T))

pdf('multi_abschange_med_summer.pdf', width = 7.5, height = 5.5)
ggplot(plotdata[which(plotdata$Season == 'summer'),], aes(x = Reservoir, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = means)) + geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = Scenario), stat = 'identity') + ylab('Absolute Change') +
  facet_wrap(~Variable, scales = 'free', nrow = 2) + theme_light() + theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('#fef0d9', '#fdcc8a', '#fc8d59', '#d7301f'), labels = c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5'))
dev.off()

pdf('multi_abschange_med_winter.pdf', width = 7.5, height = 5.5)
ggplot(plotdata[which(plotdata$Season == 'winter'),], aes(x = Reservoir, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = means)) + geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = Scenario), stat = 'identity') + ylab('Absolute Change') +
  facet_wrap(~Variable, scales = 'free', nrow = 2) + theme_light() + theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('#fef0d9', '#fdcc8a', '#fc8d59', '#d7301f'), labels = c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5'))
dev.off()

plotdata <- allgcmproj[which(allgcmproj$Model == 'Single'),]

plotdata <- plotdata %>% group_by(Reservoir, Season, Variable, Scenario) %>%
  summarize(mins = min(Change, na.rm = T), maxs = max(Change, na.rm = T), means = mean(Change, na.rm = T), medians = median(Change, na.rm = T))

pdf('single_abschange_med.pdf', width = 10, height = 7)
ggplot(plotdata, aes(x = Reservoir, ymin = mins, ymax = maxs, lower = mins, upper = maxs, middle = means)) + geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
  geom_boxplot(aes(fill = Scenario), stat = 'identity') + ylab('Absolute Change') +
  facet_wrap(~Season + Variable, scales = 'free', nrow = 2) + theme_light() + theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('#fef0d9', '#fdcc8a', '#fc8d59', '#d7301f'), labels = c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5'))
dev.off()

# FIGURE - STUDY AREA

states <- ne_states(country = 'united states of america', returnclass = "sf")
sw_states <- states[c(13,14,35,37),] # AZ, CA, UT, NV

rivers <- st_read('/Users/rqo5125/Library/Mobile Documents/com~apple~CloudDocs/Documents/Research/data/shapefiles/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp', 'ne_10m_rivers_lake_centerlines')
rivers_supp <- st_read('/Users/rqo5125/Library/Mobile Documents/com~apple~CloudDocs/Documents/Research/data/shapefiles/ne_10m_rivers_north_america/ne_10m_rivers_north_america.shp', 'ne_10m_rivers_north_america')

rivers<- subset(rivers, name %in% c('Colorado', 'San Juan', 'Green', 'Sacramento', 'San Joaquin'))
rivers2 <- subset(rivers_supp, name %in% c('American', 'Feather'))
rivers_df <- rbind(fortify(rivers), fortify(rivers2[,-c(8,10,11)]))

reservoirs <- data.frame(rbind(c('Lake Powell', 37.0683, -111.2433),
                               c('Lake Mead', 36.1435, -114.4144),
                               c('Lake Oroville', 39.5625, -121.4513), 
                               c('Lake Folsom', 38.7292, -121.1195), 
                               c('Lake Shasta', 40.6804, -122.370)))
names(reservoirs) <- c('Reservoir','lat','long')
reservoirs$lat <- as.numeric(reservoirs$lat); reservoirs$long <- as.numeric(reservoirs$long)

pdf('studyarea.pdf', width = 7, height = 4.5)
ggplot() + geom_sf(data = sw_states, fill = 'white') + xlab("") + ylab("") + theme_light() +
  geom_sf(data = rivers_df, col = 'blue') +
  coord_sf(xlim = c(-125, -100), ylim = c(30, 45)) +
  geom_point(data = reservoirs, aes(x = long, y = lat), pch = 19, size = 2) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.grid.major = element_blank(), panel.border = element_blank()) +
  geom_text_repel(data = reservoirs, aes(x = long, y = lat, label = Reservoir), 
                  fontface = "bold", nudge_x = c(-0.25, -1.75, 4.75, 2.75, 5), 
                  nudge_y = c(-3, 2, 0, -3, 0), segment.size = 0.5, 
                  segment.linetype = 2) 
dev.off()
