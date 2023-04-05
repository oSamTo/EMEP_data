source("./R/workspace.R")
source("./R/process_functions.R")

###########################################################
####                                                   ####
#### THIS MASTER SCRIPT WILL CREATE EMISSIONS SURFACES ####
#### FOR THE UK, EIRE (and combined) FOR CHOSEN YEARS  ####  # combined??
#### AND POLLUTANTS/GHGs. THE FILES WILL BE WRITTEN AS ####
####    1KM CSV FILES IN BNG IN BOTH SNAP & GNFR.      ####
#### A FURTHER FUNCTION CONVERTS ALL DATA TO LATLONG   ####
####                                                   ####
###########################################################

##### input variables: #######
v_pollutants <- c("nh3", "nox", "sox", "ch4", "co2", "n2o", "co", "nmvoc", "pm10", "pm2.5", "cd", "cu", "ni", "pb", "zn")  #   - choose one or more "nh3", "nox", "sox", "ch4", "co2", "n2o", "co", "nmvoc", "pm10", "pm2.5", "cd", "cu", "ni", "pb", "zn"
v_years <- 2020 # emissions year(s) to run; single or range
map_yr <- 2020 # the latest available emissions maps and tables

############################################################################
# Produce Emission Surfaces -----------------------------------------------
# can lapply across pollutants. Function will apply across vector of years. 
lapply(v_pollutants, produce_emis_surfaces, v_years = v_years, map_yr = map_yr, emis_release = 2020)

#############################################################################
# Separate function to create PMco surfaces -------------------------------
lapply(2020, createPMCOsurfaces, map_yr = 2020, emis_release = 2020)


#j <- list.files(paste0(data_dir,"/Emissions_grids_plain/LL"), pattern=".tif$", full.names = T, recursive = T)
#length(j)
#do.call(file.remove, list(j))
#j <- list.files(paste0(data_dir,"/Emissions_grids_plain/BNG"), pattern=".tif$", full.names = T, recursive = T)
#length(j)
#do.call(file.remove, list(j))