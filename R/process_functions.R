#########################################################
#### series of functions to process NAEI and MapEire ####
#### data into .tif files in both BNG and LatLon     ####
#########################################################

########################
#### RASTER DOMAINS ####
#########################################################################################
#### function to set extents for various domains
setDomain <- function(area = c("UK","EIRE","EU_EMEP"), crs = c("BNG","LL")){
  
  area  <- match.arg(area)  # area to process
  crs   <- match.arg(crs)   # crs to return in
  
  # if statements to return correct domain
  if(area == "UK"){
    
    if(crs == "BNG"){
      
      r <- rast(xmin = -50000, xmax = 800000, ymin = -50000, ymax = 1350000,
                res = 1000, crs = "epsg:27700", vals = NA)
      
    }else{
     
      r <- rast(xmin = -10.55, xmax = 5.61, ymin = 49.28, ymax = 62.03,
                              res = 0.01, crs = "epsg:4326", vals = NA)
       
    }
    
  }else if(area == "EIRE"){
    
    if(crs == "BNG"){
      
      r <- rast(xmin = -230000, xmax = 300000, ymin = -50000, ymax = 800000,
                              res = 1000, crs = "epsg:27700", vals = NA)
      
    }else{
      
      r <- rast(xmin = -12.31, xmax = -3.37, ymin = 49.12, ymax = 57.08,
                              res = 0.01, crs = "epsg:4326", vals = NA)
      
    }
    
  }else if(area == "EU_EMEP"){
    
    if(crs == "BNG"){
      
      stop(print("Cannot have BNG crs for the EU EMEP domain"))
      
    }else{
      
      r <- rast(xmin = -30, xmax = 90, ymin = 30, ymax = 82, 
                        res = 0.1, crs = "epsg:4326", vals = NA)
      
    }
    
  }
  
  return(r)
}

#### functions to create quick/simple lookups between GNFR and SNAP for converting sector maps

GNFRtoSNAP <- function(){
  
  dt <- data.table(GNFR = c("A_PublicPower","B_Industry","C_OtherStationaryComb","D_Fugitive","E_Solvents","F_RoadTransport","G_Shipping","H_Aviation","I_Offroad","J_Waste","K_AgriLivestock","L_AgriOther","N_Natural","O_AviCruise","P_IntShipping","q_LULUCF"), SNAP = c(1,3,2,5,6,7,8,8,8,9,10,10,11,NA,8,11))
  
  return(dt)
}

SNAPtoGNFR <- function(){
  
  dt <- data.table(SNAP = c(1,3,4,2,5,6,7,NA,8,NA,9,10,NA,11,NA,NA,NA), 
                   GNFR = c("A_PublicPower","B_Industry","B_Industry","C_OtherStationaryComb","D_Fugitive","E_Solvents","F_RoadTransport","G_Shipping","H_Aviation","I_Offroad","J_Waste","K_AgriLivestock","L_AgriOther","N_Natural","O_AviCruise","P_IntShipping","q_LULUCF"))
  
  return(dt)
}

#########################
#### VECTOR CREATION ####
#########################################################################################
#### function to set the pollutant vector as a target
vectorPolls <- function(dt_PID, class = c("ceh","emep","eire","naei")){
  
  class <- match.arg(class)
  
  dt_PID <- dt_PID[!is.na(PollutantID)]
  
  if(class == "naei"){
    v <- dt_PID[,PollutantID]
  }else{
    v <- dt_PID[,get(paste0(class,"_poll"))]
  }
  
  return(v)
  
}

#### function to set the pollutant vector as a target
vectorSecs <- function(v_sector_maps){
  
  v <- v_sector_maps
  return(v)
  
}

#### function to set the pollutant vector as a target
vectorSecs_fnames <- function(v_sector_maps, endYear){
  
  v <- paste0("NAEI_totals_1970_",endYear,"_uk_",v_sector_maps,"_t.csv")
  return(v)
  
}

############################
#### DIRECTORY CREATION ####
#########################################################################################
#### function to create directories



########################
#### DATA DOWNLOADS ####
#########################################################################################
#### function(s) to download EMEP emissions totals directly from the web (and write)
#### no formatting. 

## sub function to make URL and download emissions from EMEP
urlEMEP <- function(species, v_territories, endYear, invYear_EMEP){
  
  terr_string <- gsub(", ","", toString(paste0("&countries=",v_territories)))
  
  emis_yr_string <- gsub(", ","", toString(paste0("&years=",1990:endYear)))
  
  emep_url <- paste0("https://webdab01.umweltbundesamt.at/cgi-bin/wedb2_controller.pl?sectordefinitions=NFR2014_L2&State=official&database=web&reportyear=",invYear_EMEP, emis_yr_string, terr_string,"&pollutants=",species,"&datatype=national_csv&unit=Gg")
  
  dt <- fread(emep_url)
  #dt[, `NUMBER/FLAG` := as.numeric(`NUMBER/FLAG`)]
  
  return(dt)
  
}

downloadEMEPtotals <- function(species, endYear, invYear_EMEP, dt_PID, dt_ISO){
  
  dt_PID <- dt_PID[!is.na(PollutantID)]
  
  # EMEP pollutant to download, and countries
  #EMEPpoll <- dt_PID[ceh_poll == species, emep_poll]
  if(species == "PM_5") species <- "PM2.5" # look to change dt_PID - check EMEP maps & EIRE maps
  
  v_territories <- unique(dt_ISO[, EMEP_iso])
  
  dt_emis <- urlEMEP(species, v_territories, endYear, invYear_EMEP)
  
  fname <- paste0("C:/FastProcessingSam/dump/zztar_EMEP/totals/EMEPtotals_",species,"_1990-",endYear,"_inv",invYear_EMEP,"_Gg.csv")
  
  fwrite(dt, fname)
  
  return(fname)
  
}

#### function(s) to download EMEP gridded emissions directly from the web (and write)
#### no formatting. As .txt files.  They only exist as GNFR maps. 
downloadEMEPmaps <- function(species, endYear, invYear_EMEP, dt_PID){
  
  dt_PID <- dt_PID[!is.na(PollutantID)]
 
  # create URL
  emep_url <- paste0("https://webdab01.umweltbundesamt.at/download/gridding",invYear_EMEP,"/",endYear,"/",species,"_",invYear_EMEP,"_GRID_",endYear,".zip")
  
  # set file name for zip file
  folname   <- paste0("EMEP_",species,"_GRID_inv",invYear_EMEP,"_emis",endYear)
  fname_zip <- paste0(folname,".zip")
  
  # download 
  download.file(url = emep_url,
                destfile = paste0("C:/FastProcessingSam/dump/zztar_EMEP/maps/",fname_zip),
                quiet = T)
  
  # unzip the raw .txt data download. No formatting here. 
  unzip(paste0("C:/FastProcessingSam/dump/zztar_EMEP/maps/",fname_zip), overwrite = T,  exdir = paste0("C:/FastProcessingSam/dump/zztar_EMEP/maps/",folname))
  
  # list files and return
  v_fnames <- list.files(paste0("C:/FastProcessingSam/dump/zztar_EMEP/maps/",folname), pattern = ".txt$", full.names = T)
  return(v_fnames)
  
}

#### function(s) to download EMEP gridded emissions directly from the web (and write)
#### no formatting. As .nc files.  They only exist as GNFR maps. 
downloadEMEPnc <- function(species, endYear, invYear_EMEP, dt_PID){
 
  ## download .nc files?
  # might not be any point - EMEP4UK requires variables as country/sector specific, which is in .txt files
  
}


#### function(s) to download NAEI emissions totals directly from the web (and write)
#### no formatting. 
downloadNAEItotals <- function(species, endYear, invYear_NAEI, dt_PID){
  
  ## At the moment this is done with a query number generated by actually doing the
  ## data request on the website. Cumbersome and non-programmtic. 
  ## Perhaps just transfer over all extant tables. 
  
  # list files and return
  v_fnames <- list.files(paste0("C:/FastProcessingSam/dump/zztar_NAEI/maps/",folname), pattern = ".asc$", full.names = T)
  return(v_fnames)

}

#### function(s) to download NAEI point source emissions directly from the web (and write)
#### no formatting. 
downloadNAEIpoints <- function(ptsYear_UK, invYear_NAEI){
  
  # all pollutants are in one file. 
  # only latest year is available, the download year should be determined in PARAMETERS.R
  
  # create URL
  fname <- paste0("NAEIPointsSources_",ptsYear_UK,".xlsx")
  naei_url <- paste0("https://naei.beis.gov.uk/mapping/mapping_",ptsYear_UK,"/",fname)
  
  # download 
  download.file(url = naei_url,
                destfile = paste0("C:/FastProcessingSam/dump/zztar_NAEI/points/",fname),
                quiet = T)
  
  fname <- paste0("C:/FastProcessingSam/dump/zztar_NAEI/points/",fname)
  return(fname)
 # (copy over captured versions from P:/)
}

#### function(s) to download NAEI gridded emissions directly from the web (and write)
#### no formatting. 
downloadNAEImaps <- function(species, mapYear_UK, invYear_NAEI, dt_PID){
  
  # only latest map year is available, the map year should be determined in PARAMETERS.R
  
  dt_PID <- dt_PID[!is.na(PollutantID)]
  
  # create URL
  naei_url <- paste0("https://naei.beis.gov.uk/mapping/mapping_",mapYear_UK,"/",species,".zip")
  
  # set file name for zip file
  folname   <- paste0("NAEI_",species,"_GRID_inv",invYear_EMEP,"_emis",mapYear_UK)
  fname_zip <- paste0(folname,".zip")
  
  # download 
  download.file(url = naei_url,
                destfile = paste0("C:/FastProcessingSam/dump/zztar_NAEI/maps/",fname_zip),
                quiet = T)
  
  # unzip the raw .asc/.prj data download. No formatting here. 
  unzip(paste0("C:/FastProcessingSam/dump/zztar_NAEI/maps/",fname_zip), overwrite = T,  exdir = paste0("C:/FastProcessingSam/dump/zztar_NAEI/maps/",folname))
  
  # list files and return
  v_fnames <- list.files(paste0("C:/FastProcessingSam/dump/zztar_NAEI/maps/",folname), pattern = ".asc$", full.names = T)
  return(v_fnames)
  
}

#### function(s) to download MapEire gridded emissions directly from the web (and write)
#### no formatting.

downloadEIREmaps <- function(mapYear_IE){
  
  ## ONLY 2019 maps available (and 2016 - out of date). Have to just check every year. 
  ## EIRE maps are NOT by pollutant, but by GNFR shapefiles with all pollutants within them. 
  
  ## EIRE maps have been copied across - there is a 'SSL connect error' that prevents download
  
  # list files and return
  v_fnames <- list.files(paste0("C:/FastProcessingSam/dump/zztar_NAEI/maps/",folname), pattern = ".asc$", full.names = T)
  return(v_fnames)
  
}

# https://projects.au.dk/fileadmin/projects/mapeire/Download_Shapefiles/2019_A_PublicPower.zip

#########################
#### DATA FORMATTING ####
#########################################################################################
#### function to format EMEP downloaded totals. Data in Gg/kt
formatEUtotals <- function(fname, endYear, invYear_EMEP, dt_PID){
  
  dt_PID <- dt_PID[!is.na(PollutantID)]
  
  # read data. Cant really get pollutant name from filename in case it has `_` in the poll name
  dt <- fread(fname)
  
  # set names, choose columns 
  dt[, c("UNIT") := NULL]
  setnames(dt, c("# Format: ISO2","YEAR","SECTOR","POLLUTANT","NUMBER/FLAG"), c("ISO2","Year","NFR19","Pollutant","emis_kt"))
  dt[, emis_kt := suppressWarnings(as.numeric(emis_kt)) ]
  
  # extract and set pollutant name
  EMEPpoll <- unique(dt[,Pollutant])
  CEHpoll  <- dt_PID[emep_poll == EMEPpoll, ceh_poll]
  dt[, Pollutant := CEHpoll]
  
  # remove particular EMEP NFR codes, then remove any NA
  v_removal <- c("NT COMPLIANCE","NT COMPL (NECD)","ADJUSTMENTS","ADJ AND FLEX", unique(dt[,NFR19][grep("(fu)", dt[,NFR19])]))
  dt <- dt[!(NFR19 %in% v_removal)]
  dt <- dt[!(is.na(emis_kt))]
  
  # convert to tonnes and write
  dt[, "emis_t" := emis_kt * 1000] %>% .[,"emis_kt" := NULL]
  
  fname <- paste0("C:/FastProcessingSam/dump/zztar_EMEP/totals/EMEPtotals_",CEHpoll,"_NFR_1990-",endYear,"_inv",invYear_EMEP,"_Mg.csv")
  
  fwrite(dt, fname)
  
  return(fname)
  
}

#########################################################################################
#### function to aggregate the formatted EU totals to other sector mapping
aggEUtotals <- function(fname, ){
  
 
  
   
  
}

#########################################################################################
#### function to format EMEP downloaded maps. Data in .txt files, 0.1 degree, tonnes cell-1
formatEUmaps <- function(fname, endYear, invYear_EMEP, dt_PID){
  
  
  # read data. Cant really get pollutant name from filename in case it has `_` in the poll name
  dt <- fread(fname)
  
  # set names, choose columns 
  dt[, c("UNIT") := NULL]
  setnames(dt, c("# Format: ISO2","YEAR","SECTOR","POLLUTANT","LONGITUDE","LATITUDE","EMISSION"), c("ISO2","Year","GNFR","Pollutant","Lon","Lat","emis_t"))
  dt[, GNFR := gsub("N14 ","",GNFR)]
  dt[, emis_t := suppressWarnings(as.numeric(emis_t)) ]
  
  # extract and set pollutant name
  EMEPpoll <- unique(dt[,Pollutant])
  CEHpoll  <- dt_PID[emep_poll == EMEPpoll, ceh_poll]
  dt[, Pollutant := CEHpoll]
  
  # filename and write
  fname <- paste0("C:/FastProcessingSam/dump/zztar_EMEP/maps/",CEHpoll,"_F_RoadTransport_2022_GRID_2020.csv")
  
  EMEPtotals_",CEHpoll,"_NFR_1990-",endYear,"_inv",invYear_EMEP,"_Mg.csv
  
  fwrite(dt, fname)
  
  return(fname)
  
}

#### function to read in UK NAEI points data and process it, write output, over all pollutants.
#### Only UK. Eire and EU have total emissions gridded. E-PRTR is not a complete register (plus not needed)
formatUKpoints <- function(endYear, lookup_NFR, lookup_SIC, lookup_PID, dt_SNAPGNFR){
  
  # process the point data for all years and for all pollutants.
  # There's no point to do this over and over for years/pollutants
  # it's a bit ugly due to changing data formats, but is what it is. 
  
  dt_NFR <- fread(lookup_NFR)
  dt_SIC <- fread(lookup_SIC)[!(is.na(PollutantID))] # point to sector
  dt_PID <- fread(lookup_PID)[!is.na(PollutantID)]
  
  #####################
  ## Pre 2011 points ##
  dt_pts_pre11 <- setDT(read_excel("//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/NAEI_data/point/raw_data/NAEIPointsSources_1990-2018_EMEPpolls.xlsx", sheet = "Data"))
  setnames(dt_pts_pre11, c("Pollutant Name", "PollCode","SectorName"), c("Pollutant","PollutantID","Sector"))
  dt_pts_pre11[,c("PlantID","Operator","LocusID","Region","Unit","Datatype","Site",as.character(2011:2018)) := NULL]
  
  # sum together some lines
  dt_pts_pre11 <- dt_pts_pre11[, lapply(.SD, sum, na.rm=TRUE), by=.(Pollutant, PollutantID, SectorID, Sector, Easting, Northing), .SDcols= as.character(1990:2010)]
  
  # melt long to get ready to join to later years
  dt_pts_pre11 <- melt(dt_pts_pre11, id.vars = c("Easting","Northing","Pollutant","PollutantID","SectorID","Sector"), variable.name = "Year", value.name = "emis_t")
  dt_pts_pre11[, emis_t := as.numeric(emis_t)]
  
  # join SNAP
  dt_pts_pre11 <- dt_SIC[dt_pts_pre11, on = c("PollutantID","SectorID")]
  dt_pts_pre11[, c("Group","NFR","i.Pollutant","i.Sector","SectorID") := NULL]
  
  # set pollutant names to CEH names
  dt_pts_pre11[, Pollutant := plyr::mapvalues(Pollutant, dt_PID$naei_pt_name, dt_PID$ceh_poll)]
  
  #########################
  ## 2011 to 2015 points ##
  v_fnames <- paste0("//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/NAEI_data/point/raw_data/NAEIPointsSources_",2011:2015,".xlsx")
  
  l_pts_1115 <- lapply(v_fnames, function(x) as.data.table(read_excel(x, sheet = "Data"))) 
  
  # reset some wonky names, this is just manual stuff
  invisible(lapply(l_pts_1115, function(x) setnames(x, "PollCode", "PollutantID", skip_absent = T)))
  invisible(lapply(l_pts_1115, function(x) setnames(x, "Emission", "emis_t", skip_absent = T)))
  invisible(lapply(l_pts_1115, function(x) setnames(x, "SourceName", "Sector", skip_absent = T)))
  
  # tidy up, set col names etc
  colskeep <- c("Easting","Northing","Pollutant","PollutantID","Sector","SNAP","Year","emis_t")
  l_pts_1115 <- lapply(l_pts_1115, function(x) x[ ,..colskeep])
  
  # bind to one table, keep SNAP. Join points table to get standard pollutant name. 
  dt_pts_1115 <- rbindlist(l_pts_1115, use.names = T)
  dt_pts_1115 <- dt_SIC[!duplicated(Pollutant, PollutantID), c("Pollutant","PollutantID")][dt_pts_1115, on = c("PollutantID")]
  dt_pts_1115[,c("i.Pollutant") := NULL]
  
  # set pollutant names to CEH names
  dt_pts_1115[, Pollutant := plyr::mapvalues(Pollutant, dt_PID$naei_pt_name, dt_PID$ceh_poll)]
  
  #########################
  ## 2016 onwards points ##
  # vector of names and read
  v_fnames <- paste0("//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/NAEI_data/point/raw_data/NAEIPointsSources_",2016:endYear,".xlsx")
  
  l_pts_16on <- lapply(v_fnames, function(x) as.data.table(read_excel(x, sheet = "Data"))) 
  
  # reset some wonky names, this is just manual stuff
  invisible(lapply(l_pts_16on, function(x) setnames(x, "PollutantD", "PollutantID", skip_absent = T))) # just one has this in
  invisible(lapply(l_pts_16on, function(x) setnames(x, "Emission", "emis_t", skip_absent = T)))
  
  # tidy up, set col names etc
  colskeep <- c("Easting","Northing","Pollutant","PollutantID","Sector","SectorID","Year","emis_t")
  l_pts_16on <- lapply(l_pts_16on, function(x) x[ ,..colskeep])
  
  # bind to one table, keep SNAP. Join points table to get standard pollutant name. 
  dt_pts_16on <- rbindlist(l_pts_16on, use.names = T)
  dt_pts_16on <- dt_SIC[dt_pts_16on, on = c("PollutantID","SectorID")]
  dt_pts_16on[,c("SectorID","Group","NFR","i.Pollutant","i.Sector") := NULL]
  
  # set pollutant names to CEH names
  dt_pts_16on[, Pollutant := plyr::mapvalues(Pollutant, dt_PID$naei_pt_name, dt_PID$ceh_poll)]
  
  # combine all the points data
  dt_pts <- rbindlist(list(dt_pts_pre11, dt_pts_1115, dt_pts_16on), use.names = T)
  dt_pts[, emis_t := as.numeric(emis_t)]
  
  # remove points with 0,0 location, NA emissions, NA SNAP
  dt_pts <- dt_pts[Easting != 0 & Northing != 0] # ~5,000 rows
  dt_pts <- dt_pts[!is.na(Easting)]
  dt_pts <- dt_pts[!is.na(Northing)]
  dt_pts <- dt_pts[!is.na(emis_t)]
  dt_pts <- dt_pts[emis_t > 0] # 370k rows
  
  # subset points to pollutants in the pollutants file
  dt_pts <- dt_pts[Pollutant %in% dt_PID[,ceh_poll]]
  
  # add GNFR
  dt_pts <- dt_SNAPGNFR[dt_pts, on = "SNAP"]
  
  # add power station flag
  dt_pts[Sector == "Major power producers", powFlag := 1]
  
  # set Area
  dt_pts[, AREA := "UK"]
  dt_pts <- dt_pts[, c("Easting","Northing","Pollutant","PollutantID","Sector","emis_t","Year","SNAP","GNFR","AREA", "powFlag")]
  
  fname <- paste0("C:/FastProcessingSam/dump/NAEI_pts_1990_",endYear,"_uk_SNAPGNFR_t_BNG.csv")
  fwrite(dt_pts, fname)
  
  return(fname)
  
}

#########################################################################################
#### function to convert UK NAEI point data into LL data

transformUKpts <- function(fname){
  
  dt <- fread(fname)
  
  sf_BNG <- st_as_sf(dt, coords = c("Easting","Northing"), crs = "epsg:27700")
  sf_LL <- st_transform(sf_BNG, crs = "epsg:4326")
  
  dt_LL <- as.data.table(st_set_geometry(sf_LL, NULL))[, c("Easting","Northing") := list(as.vector(st_coordinates(sf_LL)[,1]) , as.vector(st_coordinates(sf_LL)[,2]))]
  
  dt_LL <- dt_LL[, c("Easting","Northing","Pollutant","PollutantID","Sector","emis_t","Year","SNAP","GNFR","AREA", "powFlag")]
  
  fname_LL <- gsub("_BNG","_LL",fname)
  fwrite(dt_LL, fname_LL)
  return(fname_LL)
  
}

#########################################################################################
#### function to read in the UK NAEI inventory totals
#### write out NFR level table of all pollutants & years

totalsUKformatted <- function(endYear, lookup_NFR, lookup_SIC, lookup_PID){

  dt_NFR <- fread(lookup_NFR)
  dt_SIC <- fread(lookup_SIC)[!(is.na(PollutantID))] # point to sector
  dt_PID <- fread(lookup_PID)[!is.na(PollutantID)]
  
  # bring in NAEI data
  v_fnames <- paste0("//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/NAEI_data/diffuse/",dt_PID$ceh_poll,"/NFC_time_series/naei_",dt_PID$ceh_poll,"_",dt_PID$invStart,"-",endYear,".csv")
  l_naei <- lapply(v_fnames, fread, na.strings = "-", header=T)
  names(l_naei) <- dt_PID$ceh_poll
  
  # set some names, select columns
  l_naei <- lapply(l_naei, function(x) setnames(x, c("Gas", "NFR/CRF Group"), c("Pollutant", "NFR19")))
  l_naei <- lapply(l_naei, function(x) x[Source != ""])
  l_naei <- lapply(l_naei, function(x) x[,"Units" := NULL])
  
  # melt to long, set emissions to numeric
  l_naei <- lapply(l_naei, function(x) suppressWarnings(melt(x, id.vars = c("Pollutant","NFR19","Source","Activity"), variable.name = "Year", value.name = "emis_kt") )) 
  
  l_naei <- lapply(l_naei, function(x) suppressWarnings(x[,emis_kt := as.numeric(emis_kt)]) )
  
  # convert to one table
  dt_naei <- rbindlist(l_naei, use.names = T)
  
  # subset some NAs
  dt_naei <- dt_naei[!is.na(emis_kt)] 
  dt_naei <- dt_naei[Source != ""] 
  dt_naei <- dt_naei[Activity != ""] 
  
  # set pollutant names to CEH names
  dt_naei[, Pollutant := plyr::mapvalues(Pollutant, dt_PID$naei_totals_long, dt_PID$ceh_poll)]
  
  # insert some new attributes
  dt_naei[, c("AREA","emis_t") := list("UK", emis_kt * 1000)]
  dt_naei[, "emis_kt" := NULL]
  
  # name and write
  fname <- paste0("C:/FastProcessingSam/dump/NAEI_totals_1970_",endYear,"_uk_NFR_t.csv")
  
  fwrite(dt_naei, fname)
  
  return(fname)
  
}

#########################################################################################
#### function to aggregate the formatted UK totals to other sector mapping
totalsUKagg <- function(fname, classification, endYear, lookup_NFR){
  
  cols <- c("NFR19","Source","Activity",classification)
  dt_NFR <- fread(lookup_NFR)[,..cols]
  
  # read data
  dt <- fread(fname)
  
  # join sectors
  dt_joined <- dt_NFR[dt, on = c("NFR19","Source","Activity")]
  dt_joined <- dt_joined[!is.na(emis_t)]
  
  # aggregate by classification (from dynamic target using v_aggregations)
  dt_agg <- dt_joined[, .(emis_t = sum(emis_t, na.rm=T)), by=.(Pollutant, Year, get(classification), AREA)]
  
  # name and write
  fname <- paste0("C:/FastProcessingSam/dump/NAEI_totals_1970_",endYear,"_uk_",classification,"_t.csv")
  
  fwrite(dt_agg, fname)
  
}

#########################################################################################
#### function to take EMEP downloads for Eire and create standardised table
#### EMEP downloads are in GNFR because they can be provided in this way
#### therefore no NFR19 table is created, and only a SNAP conversion is needed

totalsIEformat <- function(endYear, lookup_NFR, lookup_SIC, lookup_PID){
  
  dt_NFR <- fread(lookup_NFR)
  dt_SIC <- fread(lookup_SIC)[!(is.na(PollutantID))] # point to sector
  dt_PID <- fread(lookup_PID)[!is.na(PollutantID)]
  
  # read in data
  v_fnames <- paste0("//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/EIRE_data/diffuse/EMEP_totals/",dt_PID$ceh_poll,"/EMEP_",dt_PID$ceh_poll,"_ann.tots_GNFR_1990-",endYear,".txt")
  
  l_eire <- lapply(v_fnames, fread, na.strings = "-", header=T)
  names(l_eire) <- dt_PID$ceh_poll
  
  # set some names, select columns
  l_eire <- lapply(l_eire, function(x) setnames(x, c("YEAR","NUMBER/FLAG","SECTOR","POLLUTANT"),c("Year","emis_kt","GNFR","Pollutant")))
  l_eire <- lapply(l_eire, function(x) x[,c("UNIT","# Format: ISO2") := NULL])
  
  # set emissions to numeric
  l_eire <- lapply(l_eire, function(x) suppressWarnings(x[,emis_kt := as.numeric(emis_kt)]) )
  
  # convert to one table
  dt_eire <- rbindlist(l_eire, use.names = T)
  
  # subset some NAs
  dt_eire <- dt_eire[!is.na(emis_kt)] 
  
  # set pollutant names to CEH names
  dt_eire[, Pollutant := plyr::mapvalues(Pollutant, dt_PID$emep_poll, dt_PID$ceh_poll)]
  # make exception for PM2.5 as this is named differently FOR SOME REASON
  dt_eire[Pollutant == "PM2.5", Pollutant := "pm25"]
  
  # insert some new attributes
  dt_eire[, c("AREA","emis_t") := list("IE", emis_kt * 1000)]
  dt_eire[, "emis_kt" := NULL]
  
  # name and write
  fname <- paste0("C:/FastProcessingSam/dump/EMEP_totals_1990_",endYear,"_ie_GNFR_t.csv")
  
  fwrite(dt_eire, fname)
  
  return(fname)
  
  # remove "Other" from data (no mapping)
  #dt_EIRE <- dt_EIRE[!(GNFR %in% c("M_Other","z_Memo"))]
  
}







#########################################################################################
#### function to create csv files for diffuse and point emissions (UK is split, EIRE is total)
## surfaces are created for EIRE and UK for BNG and LL

produce_emis_surfaces <- function(species = c("bap","bz","cd","ch4","co","co2","cu","hcl","hg","n2o","nh3","ni","nmvoc","nox","pb","pm0.1","pm1","pm10","pm2.5","sox","zn"), 
                                  v_years = 1990:2020, map_yr = 2018:2020, emis_release = 2010:2020){
  
  # PMCO not created in this workflow, as it is model specific (EMEP4UK)
  
  ###################
  species  <- match.arg(species)  # species to process
  #v_years  <- match.arg(v_years)     # year to process
  if(length(map_yr) > 1) stop("Map reference year needs to be single year")
  ###################
  
  print(paste0(Sys.time(),": Creating emissions surfaces (SNAP & GNFR) for ", species,"..."))
  
  ## create directories
  lapply(v_years, createDirs, species = species)
  
  #########################
  #### POINT EMISSIONS ####
  #########################
  
  ## WORKING ACROSS MULTPLE YEARS
  ## subset and format and write UK points. Eire (E-PRTR) points not being used.
  l_pts_BNG <- lapply(v_years, readUKpoints, species = species)
  names(l_pts_BNG) <- as.character(v_years)
  l_pts_LL  <- lapply(v_years, transformUKpts, l_dt = l_pts_BNG, species = species)
  names(l_pts_LL) <- as.character(v_years)
  
  # summary tables of point data
  l_pts_summary  <- lapply(v_years, summarisePoints, species = species, pt_data = l_pts_BNG)
  names(l_pts_summary) <- as.character(v_years)
  
  ###########################
  #### DIFFUSE EMISSIONS ####
  ###########################
  
  ## WORKING ACROSS MULTIPLE YEARS
  # fetch stated year of emissions tables for UK - SNAP & GNFR
  l_emis_sums_UK <- lapply(v_years, readUKtotals, species = species, emis_release = emis_release)
  names(l_emis_sums_UK) <- as.character(v_years)
  
  # fetch stated year of emissions tables for EIRE - SNAP & GNFR
  l_emis_sums_EIRE <- lapply(v_years, readEIREtotals, species = species, emis_release = emis_release)
  names(l_emis_sums_EIRE) <- as.character(v_years)
  
  # before scaling the diffuse maps, the SNAP totals must have point totals removed.
  # (Diffuse maps don't have points in, sector totals sheet does)
  # Only UK at the moment. This is only done for SNAP, because GNFR totals are so different.
  l_diff_UK <- lapply(v_years, adjustToDiffuse, area = "uk", tot_data = l_emis_sums_UK, pt_data = l_pts_summary)
  names(l_diff_UK) <- as.character(v_years)
  
  l_diff_EIRE <- copy(l_emis_sums_EIRE) # not needed for Eire, until further notice
  
  ## Read in the diffuse maps, using reference year;
  # scale NAEI SNAP maps to each year's SNAP total
  # convert the SNAP maps to GNFR, and vice-versa for Eire
  l_maps_UK   <- lapply(v_years, readScaleWriteMaps, area = "uk", species, map_yr, emis_release, diff_sums = l_diff_UK)
  names(l_maps_UK) <- as.character(v_years)
  
  l_maps_EIRE <- lapply(v_years, readScaleWriteMaps, area = "eire", species, map_yr, emis_release, diff_sums = l_diff_EIRE)
  names(l_maps_EIRE) <- as.character(v_years)
  
  # re-project to Lat Lon
  l_maps_UK_LL <- lapply(v_years, reprojectMaps, area = "uk", species, map_yr, maps = l_maps_UK, emis_release = emis_release)
  names(l_maps_UK_LL) <- as.character(v_years)
  
  l_maps_EIRE_LL <- lapply(v_years, reprojectMaps, area = "eire", species, map_yr, maps = l_maps_EIRE, emis_release = emis_release)
  names(l_maps_EIRE_LL) <- as.character(v_years)
  
  
  # summarise point data, maps and write the tables.
  lapply(v_years, summariseAllData, species, emis_release,
         uk_rep = l_emis_sums_UK, uk_diff = l_diff_UK, uk_pts = l_pts_summary, uk_BNG = l_maps_UK, uk_LL = l_maps_UK_LL, 
         eire_rep = l_emis_sums_EIRE, eire_diff = l_diff_EIRE, eire_pts = NULL, eire_BNG = l_maps_EIRE, eire_LL = l_maps_EIRE_LL)
  
  # process & create a PMCO surface if option selected
  # createPMCOsurfaces
  
}

#########################################################################################
#### function to create the needed directories for writing out
createDirs <- function(species, year){
  
  suppressWarnings(dir.create(paste0(data_dir,"/Emissions_grids_plain/BNG/",species,"/point/",year), recursive = T))
  suppressWarnings(dir.create(paste0(data_dir,"/Emissions_grids_plain/LL/",species,"/point/",year), recursive = T))
  
  suppressWarnings(dir.create(paste0(data_dir,"/Emissions_grids_plain/BNG/",species,"/diffuse/",year,"/rasters_GNFR"), recursive = T))
  suppressWarnings(dir.create(paste0(data_dir,"/Emissions_grids_plain/BNG/",species,"/diffuse/",year,"/rasters_SNAP"), recursive = T))
  
  suppressWarnings(dir.create(paste0(data_dir,"/Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_GNFR"), recursive = T))
  suppressWarnings(dir.create(paste0(data_dir,"/Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_SNAP"), recursive = T))
  
  suppressWarnings(dir.create(paste0(data_dir,"/Emissions_grids_plain/summaries/",species), recursive = T))
}

#########################################################################################
#### function to process NAEI UK points to formatted table, with power station flag
readUKpoints <- function(species, year){
  
  if(year < 2011){
    
    dt_pts <- setDT(read_excel(paste0(data_dir, "/NAEI_data/point/raw_data/NAEIPointsSources_1990-2018_EMEPpolls.xlsx", sheet = "Data")))
    setnames(dt_pts, c("Pollutant Name", "PollCode", paste0(year)), c("Pollutant","PollutantID", "Emission"))
    dt_pts[,c("PlantID","Operator","SectorName","LocusID","Region","Unit","Datatype",as.character(setdiff(1990:2018, year))) := NULL]
    
    # sum together some lines
    dt_pts <- dt_pts[, lapply(.SD, sum, na.rm=TRUE), by=.(Pollutant, PollutantID,Site,SectorID,Easting,Northing), .SDcols="Emission"]
    dt_pts[, Year := year]
    
  }else{
    
    dt_pts <- as.data.table(read_excel(paste0(data_dir,"/NAEI_data/point/raw_data/NAEIPointsSources_",year,".xlsx"), sheet = "Data"))
    
    if("PollCode" %in% names(dt_pts)) setnames(dt_pts, "PollCode", "PollutantID")
    
  }
  
  
  # remove points with 0,0 location and NA emissions
  dt_pts <- dt_pts[Easting != 0 & Northing != 0]
  dt_pts <- dt_pts[!is.na(Easting)]
  dt_pts <- dt_pts[!is.na(Northing)]
  dt_pts <- dt_pts[!is.na(Emission)]
  
  # subset to species
  dt_pts <- dt_pts[PollutantID == dt_poll_IDs[pollutant == species, pollutant_id]]
  
  # rename species
  dt_pts[, Pollutant := species]
  
  # add SNAP/GNFR
  dt_pts <- dt_ptsec[Pollutant == species][dt_pts, on = "SectorID"]
  
  # add power station flag
  dt_pts[SectorID == 18, powFlag := 1]
  
  # set Area
  dt_pts[, AREA := "UK"]
  
  # tidy up
  dt_pts[, Emission := as.numeric(as.character(Emission))]
  col.keep <- c("Easting","Northing","Pollutant","Emission","Year","SNAP","GNFR","AREA","powFlag")
  dt_pts <- dt_pts[,..col.keep]
  setnames(dt_pts, "Emission", "pt_emis_t")
  
  fwrite(dt_pts, paste0(data_dir,"/Emissions_grids_plain/BNG/",species,"/point/",year,"/",species,"_pt_",year,"_uk_SNAPGNFR_t_BNG.csv"))
  
  
  return(dt_pts)
}

#########################################################################################
#### function to convert UK points into Lat lon, for writing purposes
#transformUKpts <- function(l_dt, species, year){
#  
#  dt <- l_dt[[as.character(year)]]
#  
#  sf_BNG <- st_as_sf(dt, coords = c("Easting","Northing"), crs = "epsg:27700")
#  sf_LL <- st_transform(sf_BNG, crs = "epsg:4326")
#  
#  dt_LL <- as.data.table(st_set_geometry(sf_LL, NULL))[, c("Easting","Northing") := list(as.vector(st_coordinates(sf_LL)[,1]) , as.vector(st_coordinates(sf_LL)[,2]))]
#  
#  dt_LL <- dt_LL[, c("Easting","Northing","Pollutant","pt_emis_t","Year","SNAP","GNFR","AREA", "powFlag")]
#  
#  fwrite(dt_LL, paste0(data_dir,"/Emissions_grids_plain/LL/",species,"/point/",year,"/",species,"_pt_",year,"_uk_SNAPGNFR_t_LL.csv"))
#  
#  return(dt_LL)
#  
#  
#}

#########################################################################################
#### function to summarise point data and write
summarisePoints <- function(year, species, pt_data){
  
  dt <- pt_data[[as.character(year)]]
  
  dt_pts_SNAP_summary <- dt[, .(pt_emis_t = sum(pt_emis_t, na.rm=T)), by = SNAP][dt_SNAP, on = "SNAP"] %>% .[ , c("Pollutant","Year","AREA") := list(species, year, "UK")]
  
  dt_pts_GNFR_summary <- dt[, .(pt_emis_t = sum(pt_emis_t, na.rm=T)), by = GNFR][dt_GNFR, on = c(GNFR = "sector_name")] %>% .[ , c("Pollutant","Year","AREA") := list(species, year, "UK")] %>% .[, sector_name := tstrsplit(GNFR,"_",keep=2)] %>% .[, i.GNFR := NULL] %>%
    .[ , c("GNFR","pt_emis_t","sector_num","sector_name","Pollutant","Year","AREA")]
  

  l <- list(dt_pts_SNAP_summary,dt_pts_GNFR_summary)
  names(l) <- c("SNAP","GNFR")
  return(l)

}

#########################################################################################
#### function to read in NAEI UK totals and produce a formatted table (SNAP & GNFR)
readUKtotals <- function(year, species, emis_release){
  
  # emis_release is the year that the table of emissions (by NFR) was produced. 
  if(emis_release < year) stop("release of emissions data must be >= year being processed")
  
  # bring in NAEI data
  dt_NAEI <- fread(paste0(data_dir,"/NAEI_data/diffuse/",species,"/NFC_time_series/naei_",species,"_",dt_poll_IDs[pollutant == species, yearStart],"-",emis_release,".csv"), header=T)
  
  setnames(dt_NAEI, c("Gas", "NFR/CRF Group" , as.character(year)), c("Pollutant", "NFR19" , "tot_emis_kt"))
  
  cols_keep <- c("Pollutant", "NFR19","Source","Activity","tot_emis_kt")
  
  dt_NAEI <- dt_NAEI[, ..cols_keep]
  suppressWarnings(dt_NAEI[,tot_emis_kt := as.numeric(tot_emis_kt)])
  dt_NAEI[, c("Pollutant","Year","AREA","tot_emis_t") := list(species, year, "UK", tot_emis_kt * 1000)]
  
  # subset some NAs
  dt_NAEI <- dt_NAEI[!is.na(tot_emis_t)]
  dt_NAEI <- dt_NAEI[Source != ""]
  dt_NAEI <- dt_NAEI[Activity != ""]
  
  # join sectors
  dt_joined <- dt_NFR19[dt_NAEI, on = c("NFR19","Source","Activity")]
  dt_joined <- dt_joined[!is.na(tot_emis_t)]
  
  ## removing NA aggregated sectors and Aviation Cruise & `Other` (review NFR codes for NA aggregations)
  #suppressWarnings(dt_joined[, SNAP := as.numeric(SNAP)]) 
  #dt_joined <- dt_joined[!is.na(GNFR) ] %>% .[!is.na(SNAP)] %>% .[GNFR != "M_Other"]
  
  # summarise to sectors
  dt_SNAP_agg <- dt_joined[, .(tot_emis_t = sum(tot_emis_t, na.rm=T)), by=.(Pollutant, Year, SNAP, AREA)]
  dt_GNFR_agg <- dt_joined[, .(tot_emis_t = sum(tot_emis_t, na.rm=T)), by=.(Pollutant, Year, GNFR, AREA)]
  
  l <- list("SNAP" = dt_SNAP_agg, "GNFR" = dt_GNFR_agg)
  
  return(l)
  
}

#########################################################################################
#### function to read in EIRE totals and produce a formatted table (SNAP & GNFR)
readEIREtotals <- function(year, species, emis_release){
 
  if(!(species %in% c("ch4","n2o","co2"))){
    
    dt_EIRE <- fread(paste0(data_dir,"/EIRE_data/diffuse/EMEP_totals/",species,"/EMEP_",species,"_ann.tots_GNFR_1990-",emis_release,".txt"))

    setnames(dt_EIRE,c("# Format: ISO2","NUMBER/FLAG","SECTOR"),c("ISO2","tot_emis_kt","GNFR"))
    dt_EIRE <- dt_EIRE[ISO2=="IE" & YEAR == year]
    
    # change emissions to numeric and remove NA lines and superflous columns
    suppressWarnings(dt_EIRE <- dt_EIRE[, tot_emis_kt:=as.numeric(tot_emis_kt)])
    dt_EIRE[, tot_emis_t := tot_emis_kt * 1000]
    
    # remove "Other" from data (no mapping)
    dt_EIRE <- dt_EIRE[!(GNFR %in% c("M_Other","z_Memo"))]
    setnames(dt_EIRE, c("YEAR","POLLUTANT"), c("Year","Pollutant"))
    
    # join SNAP sectors
    dt_joined <- dt_GNFR_to_SNAP[dt_EIRE, on = c("GNFR")]
    
    dt_joined[, c("Pollutant","AREA") := list(species, "EIRE")]
    
    dt_joined[,c("ISO2","UNIT","tot_emis_kt") := NULL]
    
    dt_joined <- dt_joined[, c("Pollutant", "Year", "GNFR", "SNAP", "AREA", "tot_emis_t")]
    
    
  }else{
    ## GHGs for Eire
    
    ghg_file <- list.files(paste0(data_dir,"/EIRE_data/diffuse"), pattern = "^UNFCCC_GHG_to_", full.names = T)
    ghg_file_yr <- as.numeric(substr(ghg_file, nchar(ghg_file) - 7, nchar(ghg_file) - 4))
    if(year > ghg_file_yr) stop("Download latest GHG data from UNFCCC")
    
    dt_EIRE <- fread(ghg_file)
    setnames(dt_EIRE, c("Pollutant_name","Unit","emissions","Sector_code"),c("pollutant","Units","emission","CRF"))
    
    # subset to country, without NA values
    dt_EIRE <- dt_EIRE[Country_code == "IE" & !is.na(emission)]
    dt_EIRE[, pollutant := tolower(pollutant)]
    dt_EIRE[, Year := as.numeric(Year)]
    dt_EIRE <- dt_EIRE[pollutant == species & Year == year]
    
    # join GNFR codes
    dt_EIRE <- dt_CRF[dt_EIRE, on = "CRF"]
    dt_EIRE <- dt_EIRE[!is.na(GNFR)]
    dt_EIRE[, c("Country_code","Country","Format_name","pollutant","Sector_name","Parent_sector_code","Units","Notation","PublicationDate","DataSource","CRF") := NULL]
    
    dt_EIRE <- dt_EIRE[!(GNFR %in% c("M_Other","z_Memo"))]
    
    # join SNAP and format
    dt_EIRE <- dt_GNFR_to_SNAP[dt_EIRE, on = c("GNFR")]
    
    dt_joined <- copy(dt_EIRE)
    
    dt_joined[, c("Pollutant","AREA","tot_emis_t") := list(species, "EIRE", emission * 1000)]
    
    dt_joined[,c("emission") := NULL]
    
    dt_joined <- dt_joined[, c("Pollutant", "Year", "GNFR", "SNAP", "AREA", "tot_emis_t")]
    
    
  }
    
  
  # summarise to sectors
  dt_SNAP_agg <- dt_joined[, .(tot_emis_t = sum(tot_emis_t, na.rm=T)), by=.(Pollutant, Year, SNAP, AREA)]
  dt_GNFR_agg <- dt_joined[, .(tot_emis_t = sum(tot_emis_t, na.rm=T)), by=.(Pollutant, Year, GNFR, AREA)]
  
  l <- list("SNAP" = dt_SNAP_agg, "GNFR" = dt_GNFR_agg)
  
  return(l)
  
}

#########################################################################################
#### function to adjust the totals to diffuse totals by subtracting point emissions (UK)
adjustToDiffuse <- function(year, area = c("uk"), tot_data, pt_data){
  
  area  <- match.arg(area)
  f <- function(x,y) sum(x[1],na.rm=T) - sum(y[1],na.rm=T)
  
  # This is only done for SNAP as the GNFR maps are created later from final SNAP maps. 
  # The subtraction of points from diffuse does not work, a lot of data is lost in GNFR
  
  ## SNAP ##
  # subset the data lists for year
  dt_pts      <- pt_data[[as.character(year)]][["SNAP"]]
  dt_pts[,SNAP := as.character(SNAP)]
  dt_tot_data <- tot_data[[as.character(year)]][["SNAP"]]
  
  # join points and totals. reset any to 0 that need it.
  dt_emis_SNAP <- dt_pts[dt_tot_data, on = "SNAP"]
  dt_emis_SNAP[, diff_emis_t := f(tot_emis_t, pt_emis_t), by = seq_len(nrow(dt_emis_SNAP))]
  #extra_ems <- abs(dt_emis_SNAP[diff_emis_t < 0,sum(diff_emis_t)]) # make a sum of extra emissions for summary
  #dt_emis_SNAP[diff_emis_t < 0 , diff_emis_t := 0]
  dt_emis_SNAP[, c("i.Pollutant","i.Year","i.AREA") := NULL]

  return(dt_emis_SNAP)
  
}

#########################################################################################
#### function to take maps, scale to the amount required, convert to SNAP/GNFR, write & return
readScaleWriteMaps <- function(area = c("uk","eire"), year, species, map_yr, emis_release, diff_sums){
  
  area  <- match.arg(area)
  
  # everything on ifelse for UK vs Eire
  
  if(area == "uk"){
    
    v_SNAP <- list.files(paste0(data_dir,"/NAEI_data/diffuse/",species,"/maps/",map_yr,"/"), pattern = ".tif$", full.names = TRUE) 
    
    # remove "total" rasters
    v_SNAP <- v_SNAP[!grepl("tota",v_SNAP)]
    
    s_SNAP <- rast(v_SNAP)
    s_SNAP <- crop(extend(s_SNAP, ext(r_naei_1km_BNG)), ext(r_naei_1km_BNG))
    
    names(s_SNAP) <- gsub(paste0(dt_poll_IDs[pollutant == species, map_name],substr(map_yr,3,4)) , "", names(s_SNAP))
    names(s_SNAP) <- paste0(dt_SNAP[match(names(s_SNAP) , dt_SNAP[,sector_name]), sector_num])
    
    # re-order to SNAP order
    v_order <- as.character(dt_SNAP[,sector_num])
    v_order <- v_order[v_order %in% names(s_SNAP)]
    s_SNAP <- s_SNAP[[v_order]]
    
    # get the sums, ready for scaling. (SNAP in UK)
    # select the diffuse emission sums by year (in the lapply if > 1 year)
    dt_diff <- diff_sums[[paste0(year)]]
    
    v_diff_tots <- dt_diff[match(names(s_SNAP) , dt_diff[,SNAP]), diff_emis_t]
    
    v_scalars <- v_diff_tots / global(s_SNAP, sum, na.rm=T)
    
    # reset negative scalars to 1. This is where subtracted points were bigger than diffuse.
    # Creates extra emissions though. 
    v_scalars_adj <- copy(v_scalars)
    v_scalars_adj[v_scalars_adj < 0] <- 1
    
    s_SNAP_UK <- s_SNAP * v_scalars_adj$sum
    
    ## pad out the stack with missing surfaces
    
    v_missing <- dt_SNAP[!(as.character(SNAP) %in% names(s_SNAP_UK)), as.character(SNAP)]
    #v_missing <- v_missing[!(v_missing %in% c("O_AviCruise","P_IntShipping","q_LULUCF"))]
    if(length(v_missing) > 0){
      s_missing <- rep(r_naei_1km_BNG, length(v_missing))
      names(s_missing) <- v_missing
    }else{
      s_missing <- NULL
    }
    
    s_SNAP_UK <- c(s_SNAP_UK, s_missing)
    
    # write
    sapply(X = 1:nlyr(s_SNAP_UK), function(x) writeRaster(s_SNAP_UK[[x]] , paste0(data_dir,"/Emissions_grids_plain/BNG/",species,"/diffuse/",year,"/rasters_SNAP/",species,"_diff_",year,"_release",emis_release,"_uk_SNAP_S",str_pad(names(s_SNAP_UK[[x]]), 2, pad = "0"),"_t_1km_BNG_",map_yr,"NAEImap.tif"), overwrite=T))
    
    ## turn these into GNFR maps (UK)
    # copy the SNAP maps, rename to GNFR and sum where name is the same
    s_GNFR <- copy(s_SNAP_UK)
    names(s_GNFR) <- dt_SNAP_to_GNFR[match(names(s_GNFR) , dt_SNAP_to_GNFR[,SNAP]), GNFR]
    
    sum_index <- match(names(s_GNFR) , unique(names(s_GNFR)))
    
    s_GNFR2 <- tapp(s_GNFR, index = sum_index, fun = "sum", na.rm=T)
    names(s_GNFR2) <- unique(names(s_GNFR))
    
    ## pad out the stack with missing surfaces
    v_missing <- dt_GNFR[!(sector_name %in% names(s_GNFR2)), sector_name]
    if(length(v_missing) > 0){
      s_missing <- rep(r_naei_1km_BNG, length(v_missing))
      names(s_missing) <- v_missing
    }else{
      s_missing <- NULL
    }
    
    s_GNFR2 <- c(s_GNFR2, s_missing)
    
    s_GNFR_UK <- s_GNFR2[[ dt_GNFR[,sector_name][dt_GNFR[,sector_name] %in% names(s_GNFR2)] ]]
    
    sapply(X = 1:nlyr(s_GNFR_UK), function(x) writeRaster(s_GNFR_UK[[x]] , paste0(data_dir,"/Emissions_grids_plain/BNG/",species,"/diffuse/",year,"/rasters_GNFR/",species,"_diff_",year,"_release",emis_release,"_uk_GNFR_",names(s_GNFR_UK[[x]]),"_t_1km_BNG_",map_yr,"NAEImap.tif"), overwrite=T))
    
    
    l <- list(s_SNAP_UK , s_GNFR_UK)
    names(l) <- paste0(c("UK_SNAP_diff_","UK_GNFR_diff_"),year)
    return(l)
    
  }else{ # EIRE
    
    # Eire is always latest (2019) maps - might change to EMEP maps?
    v_GNFR <- list.files(paste0(data_dir,"/EIRE_data/diffuse/MapEire_data/ME_2019"), pattern = paste0("_",species,"_t_BNG.tif$"), full.names = T, recursive = T)
  
    # remove "total" rasters
    v_remove <- c("CRF_NationalTotal","NFR_NationalTotal")
    v_GNFR <- v_GNFR[grep(paste(v_remove,collapse="|"), v_GNFR, invert = T)]
    
    s_GNFR <- rast(v_GNFR)
    s_GNFR <- crop(extend(s_GNFR, ext(r_eire_1km_BNG)), ext(r_eire_1km_BNG))
    
    names(s_GNFR) <- gsub("2019_" , "", names(s_GNFR))
    names(s_GNFR) <- gsub(paste0("_",species,"_t_BNG") , "", names(s_GNFR))
    
    # get the sums, ready for scaling. 
    # select the diffuse emission sums by year (in the lapply if > 1 year)
    dt_diff <- diff_sums[[paste0(year)]][["GNFR"]]
    
    v_diff_tots <- dt_diff[match(names(s_GNFR) , dt_diff[,GNFR]), tot_emis_t]
    
    v_scalars <- v_diff_tots / global(s_GNFR, sum, na.rm=T)
    
    s_GNFR_EIRE <- s_GNFR * v_scalars$sum
    
    ## pad out the stack with missing surfaces
    v_missing <- dt_GNFR[!(sector_name %in% names(s_GNFR_EIRE)), sector_name]
    if(length(v_missing) > 0){
      s_missing <- rep(r_eire_1km_BNG, length(v_missing))
      names(s_missing) <- v_missing
    }else{
      s_missing <- NULL
    }
    
    s_GNFR_EIRE <- c(s_GNFR_EIRE, s_missing)
    
    s_GNFR_EIRE <- s_GNFR_EIRE[[ dt_GNFR[,sector_name][dt_GNFR[,sector_name] %in% names(s_GNFR_EIRE)] ]]
    
    # write
    sapply(X = 1:nlyr(s_GNFR_EIRE), function(x) writeRaster(s_GNFR_EIRE[[x]] , paste0(data_dir,"/Emissions_grids_plain/BNG/",species,"/diffuse/",year,"/rasters_GNFR/",species,"_diff_",year,"_release",emis_release,"_eire_GNFR_",names(s_GNFR_EIRE[[x]]),"_t_1km_BNG_2019MEmap.tif"), overwrite=T))
    
    
    ## turn these into SNAP maps (Eire)
    # copy the GNFR maps, rename to SNAP and sum where name is the same
    s_SNAP <- copy(s_GNFR_EIRE)
    
    names(s_SNAP) <- dt_GNFR_to_SNAP[match(names(s_SNAP) , dt_GNFR_to_SNAP[,GNFR]), SNAP]
    
    sum_index <- match(names(s_SNAP) , unique(names(s_SNAP)))
    
    s_SNAP2 <- tapp(s_SNAP, index = sum_index, fun = "sum", na.rm=T)
    
    names(s_SNAP2) <- unique(names(s_SNAP))
    
    ## pad out the stack with missing surfaces
    
    v_missing <- dt_SNAP[!(sector_num %in% names(s_SNAP2)), sector_num]
    if(length(v_missing) > 0){
      s_missing <- rep(r_eire_1km_BNG, length(v_missing))
      names(s_missing) <- v_missing
    }else{
      s_missing <- NULL
    }
    
    s_SNAP2 <- c(s_SNAP2, s_missing)
    
    s_SNAP_EIRE <- s_SNAP2[[ as.character(dt_SNAP[,sector_num][dt_SNAP[,sector_num] %in% names(s_SNAP2)]) ]]
    
    # write
    sapply(X = 1:nlyr(s_SNAP_EIRE), function(x) writeRaster(s_SNAP_EIRE[[x]] , paste0(data_dir,"/Emissions_grids_plain/BNG/",species,"/diffuse/",year,"/rasters_SNAP/",species,"_diff_",year,"_release",emis_release,"_eire_SNAP_S",str_pad(names(s_SNAP_EIRE[[x]]), 2, pad = "0"),"_t_1km_BNG_2019MEmap.tif"), overwrite=T))
    
    l <- list(s_SNAP_EIRE , s_GNFR_EIRE)
    names(l) <- paste0(c("EIRE_SNAP_diff_","EIRE_GNFR_diff_"),year)
    return(l)
    
  }
  
}

#########################################################################################
#### function to reproject maps into Lat Lon, and write at the same time
reprojectMaps <- function(year, area = c("uk","eire") , species, map_yr, maps, emis_release){
  
  area  <- match.arg(area)
  
  if(area == "uk"){
    r_area_LL <- copy(r_naei_0.01_LL)
    r_area_LL[] <- 0
  }else{
    r_area_LL <- copy(r_eire_0.01_LL)
    r_area_LL[] <- 0
  }
  
  ## SNAP
  s_SNAP_BNG <- maps[[paste(year)]][[paste0(toupper(area),"_SNAP_diff_",year)]]
  s_SNAP_BNG_tm2 <- s_SNAP_BNG/1000000
  
  s_SNAP_LL_tm2 <- terra::project(s_SNAP_BNG_tm2, r_area_LL, method = "near")
  s_SNAP_LL <- s_SNAP_LL_tm2 * terra::cellSize(r_area_LL, unit = "m")
  
  # adjust back to totals
  v_scalars   <- global(s_SNAP_BNG, sum, na.rm=T)$sum / global(s_SNAP_LL, sum, na.rm=T)$sum
  s_SNAP_LLsc <- s_SNAP_LL * v_scalars
  
  # write
  end_text <- ifelse(area == "uk", paste0(map_yr,"NAEImap"), "2019MEmap")
  sapply(X = 1:nlyr(s_SNAP_LLsc), function(x) writeRaster(s_SNAP_LLsc[[x]] , paste0(data_dir,"/Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_SNAP/",species,"_diff_",year,"_release",emis_release,"_",area,"_SNAP_S",str_pad(names(s_SNAP_LLsc[[x]]), 2, pad = "0"),"_t_0.01_LL_",end_text,".tif"), overwrite=T))
  
  ## GNFR
  s_GNFR_BNG <- maps[[paste(year)]][[paste0(toupper(area),"_GNFR_diff_",year)]]
  s_GNFR_BNG_tm2 <- s_GNFR_BNG/1000000
  
  s_GNFR_LL_tm2 <- terra::project(s_GNFR_BNG_tm2, r_area_LL, method = "near")
  s_GNFR_LL <- s_GNFR_LL_tm2 * terra::cellSize(r_area_LL, unit = "m")
  
  # adjust back to totals
  v_scalars   <- global(s_GNFR_BNG, sum, na.rm=T)$sum / global(s_GNFR_LL, sum, na.rm=T)$sum
  s_GNFR_LLsc <- s_GNFR_LL * v_scalars
  
  # write
  end_text <- ifelse(area == "uk", paste0(map_yr,"NAEImap"), "2019MEmap")
  sapply(X = 1:nlyr(s_GNFR_LLsc), function(x) writeRaster(s_GNFR_LLsc[[x]] , paste0(data_dir,"/Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_GNFR/",species,"_diff_",year,"_release",emis_release,"_",area,"_GNFR_",names(s_GNFR_LLsc[[x]]),"_t_0.01_LL_",end_text,".tif"), overwrite=T))
  
  l <- list(s_SNAP_LLsc, s_GNFR_LLsc)
  names(l) <- c(paste0(toupper(area),"_SNAP_diffLL_",year), paste0(toupper(area),"_GNFR_diffLL_",year))
  return(l)
}

#########################################################################################
#### function to summarise all of the data produced. 
summariseAllData <- function(year, species, emis_release,
                             uk_rep,   uk_diff,   uk_pts,   uk_BNG,   uk_LL, 
                             eire_rep, eire_diff, eire_pts, eire_BNG, eire_LL){
  
  ## SNAP ##
  
  uk_data_reported_snap <- uk_rep[[as.character(year)]][["SNAP"]]
  uk_points_totals_snap <- uk_pts[[as.character(year)]][["SNAP"]]
  uk_diffuse_totals_snap <- uk_diff[[as.character(year)]][,c("SNAP","sector_name","Pollutant","Year","diff_emis_t")]
  
  uk_year_maps_BNG_snap <- data.table(SNAP = names(uk_BNG[[as.character(year)]][[paste0("UK_SNAP_diff_",year)]]),Pollutant = species, Year = year, t_BNG = global(uk_BNG[[as.character(year)]][[paste0("UK_SNAP_diff_",year)]], sum, na.rm=T))
  uk_year_maps_LL_snap <- data.table(SNAP = names(uk_LL[[as.character(year)]][[paste0("UK_SNAP_diffLL_",year)]]),Pollutant = species, Year = year, t_LL = global(uk_LL[[as.character(year)]][[paste0("UK_SNAP_diffLL_",year)]], sum, na.rm=T))
  
  
  dt_uk_snap <- uk_year_maps_LL_snap[uk_year_maps_BNG_snap, on = c("SNAP","Pollutant","Year")][uk_points_totals_snap, on = c("SNAP","Pollutant","Year")][uk_diffuse_totals_snap, on = c("SNAP","Pollutant","Year")][uk_data_reported_snap, on = c("SNAP","Pollutant","Year")]
  
  dt_uk_snap[, c("i.sector_name","i.AREA") := NULL]
  dt_uk_snap <- dt_uk_snap[,c("Pollutant", "Year", "AREA", "SNAP", "sector_name", "sector_num", "tot_emis_t", "pt_emis_t", "diff_emis_t", "t_BNG.sum", "t_LL.sum")]
  
  
  eire_data_reported_snap <- eire_rep[[as.character(year)]][["SNAP"]]
  #eire_points_totals_snap <- data.table(SNAP = numeric(),Pollutant=character(),Year=numeric(),pt_emis_t=numeric())
  eire_diffuse_totals_snap <- eire_diff[[as.character(year)]][["SNAP"]][,c("SNAP","Pollutant","Year","AREA","tot_emis_t")] %>% setnames(., "tot_emis_t", "diff_emis_t")
  
  eire_year_maps_BNG_snap <- data.table(SNAP = as.numeric(names(eire_BNG[[as.character(year)]][[paste0("EIRE_SNAP_diff_",year)]])),Pollutant = species, Year = year, t_BNG = global(eire_BNG[[as.character(year)]][[paste0("EIRE_SNAP_diff_",year)]], sum, na.rm=T))
  eire_year_maps_LL_snap <- data.table(SNAP = as.numeric(names(eire_LL[[as.character(year)]][[paste0("EIRE_SNAP_diffLL_",year)]])),Pollutant = species, Year = year, t_LL = global(eire_LL[[as.character(year)]][[paste0("EIRE_SNAP_diffLL_",year)]], sum, na.rm=T))
  
  
  dt_eire_snap <- eire_year_maps_LL_snap[eire_year_maps_BNG_snap, on = c("SNAP","Pollutant","Year")][eire_diffuse_totals_snap, on = c("SNAP","Pollutant","Year")][eire_data_reported_snap, on = c("SNAP","Pollutant","Year")] %>% .[, pt_emis_t := NA]
  
  dt_eire_snap[, c("i.AREA") := NULL]
  dt_eire_snap <- dt_eire_snap[,c("Pollutant", "Year", "AREA", "SNAP", "tot_emis_t", "pt_emis_t", "diff_emis_t", "t_BNG.sum", "t_LL.sum")]
  
  summary_SNAP <- rbindlist(list(dt_uk_snap, dt_eire_snap), use.names = T, fill = T)
  
  fwrite(summary_SNAP, paste0(data_dir,"/Emissions_grids_plain/summaries/",species,"/",species,"_",year,"_ukeire_SNAP_SUMMARY.csv"))
  
  ## GNFR ##
  
  uk_data_reported_gnfr <- uk_rep[[as.character(year)]][["GNFR"]]
  uk_points_totals_gnfr <- uk_pts[[as.character(year)]][["GNFR"]]
  #uk_diffuse_totals_gnfr <- uk_diff[[as.character(year)]][,c("GNFR","sector_name","Pollutant","Year","diff_emis_t")]
  
  uk_year_maps_BNG_gnfr <- data.table(GNFR = names(uk_BNG[[as.character(year)]][[paste0("UK_GNFR_diff_",year)]]),Pollutant = species, Year = year, t_BNG = global(uk_BNG[[as.character(year)]][[paste0("UK_GNFR_diff_",year)]], sum, na.rm=T))
  uk_year_maps_LL_gnfr <- data.table(GNFR = names(uk_LL[[as.character(year)]][[paste0("UK_GNFR_diffLL_",year)]]),Pollutant = species, Year = year, t_LL = global(uk_LL[[as.character(year)]][[paste0("UK_GNFR_diffLL_",year)]], sum, na.rm=T))
  
  
  dt_uk_gnfr <- uk_year_maps_LL_gnfr[uk_year_maps_BNG_gnfr, on = c("GNFR","Pollutant","Year")][uk_points_totals_gnfr, on = c("GNFR","Pollutant","Year")][uk_data_reported_gnfr, on = c("GNFR","Pollutant","Year")]
  
  dt_uk_gnfr[, c("i.AREA") := NULL] %>% .[, diff_emis_t := NA]
  dt_uk_gnfr <- dt_uk_gnfr[,c("Pollutant", "Year", "AREA", "GNFR", "sector_name", "sector_num", "tot_emis_t", "pt_emis_t", "diff_emis_t", "t_BNG.sum", "t_LL.sum")]
  
  
  eire_data_reported_gnfr <- eire_rep[[as.character(year)]][["GNFR"]]
  #eire_points_totals_snap <- data.table(SNAP = numeric(),Pollutant=character(),Year=numeric(),pt_emis_t=numeric())
  eire_diffuse_totals_gnfr <- eire_diff[[as.character(year)]][["GNFR"]][,c("GNFR","Pollutant","Year","AREA","tot_emis_t")] %>% setnames(., "tot_emis_t", "diff_emis_t")
  
  eire_year_maps_BNG_gnfr <- data.table(GNFR = names(eire_BNG[[as.character(year)]][[paste0("EIRE_GNFR_diff_",year)]]),Pollutant = species, Year = year, t_BNG = global(eire_BNG[[as.character(year)]][[paste0("EIRE_GNFR_diff_",year)]], sum, na.rm=T))
  eire_year_maps_LL_gnfr <- data.table(GNFR = names(eire_LL[[as.character(year)]][[paste0("EIRE_GNFR_diffLL_",year)]]),Pollutant = species, Year = year, t_LL = global(eire_LL[[as.character(year)]][[paste0("EIRE_GNFR_diffLL_",year)]], sum, na.rm=T))
  
  
  dt_eire_gnfr <- eire_year_maps_LL_gnfr[eire_year_maps_BNG_gnfr, on = c("GNFR","Pollutant","Year")][eire_diffuse_totals_gnfr, on = c("GNFR","Pollutant","Year")][eire_data_reported_gnfr, on = c("GNFR","Pollutant","Year")] %>% .[, pt_emis_t := NA]
  
  dt_eire_gnfr[, c("i.AREA") := NULL]
  dt_eire_gnfr <- dt_eire_gnfr[,c("Pollutant", "Year", "AREA", "GNFR", "tot_emis_t", "pt_emis_t", "diff_emis_t", "t_BNG.sum", "t_LL.sum")]
  
  summary_GNFR <- rbindlist(list(dt_uk_gnfr, dt_eire_gnfr), use.names = T, fill = T)
  
  fwrite(summary_GNFR, paste0(data_dir,"/Emissions_grids_plain/summaries/",species,"/",species,"_",year,"_ukeire_GNFR_SUMMARY.csv"))
  
}

#########################################################################################
#### function to create pmco data based on pm25 and pm10
createPMCOsurfaces <- function(year = 1990:2020, map_yr = 2018:2020, emis_release = 2010:2020){

  ## need to read in PM10 data and subtract the PM2.5 data, to leave the coarse fraction. 
  
  ###################
  year  <- match.arg(year)     # year to process
  if(length(map_yr) > 1) stop("Map reference year needs to be single year")
  ###################
  
  print(paste0(Sys.time(),": Creating PMco surfaces (SNAP & GNFR) for ", year,"..."))
  
  createDirs(species = "pmco", year = year)
  
  #### point data - UK only. BNG ####
  if(year < 2011){
    
    dt_pts <- setDT(read_excel(paste0(data_dir, "/NAEI_data/point/raw_data/NAEIPointsSources_1990-2018_EMEPpolls.xlsx"), sheet = "Data"))
    setnames(dt_pts, c("Pollutant Name", "PollCode", paste0(year)), c("Pollutant","PollutantID", "Emission"))
    dt_pts[,c("Operator","SectorName","LocusID","Region","Unit","Datatype",as.character(setdiff(1990:2018, year))) := NULL]
    
    # sum together some lines
    dt_pts <- dt_pts[, lapply(.SD, sum, na.rm=TRUE), by=.(Pollutant, PollutantID, PlantID, Site, SectorID, Easting, Northing), .SDcols="Emission"]
    dt_pts[, Year := year]
    
  }else{
    
    dt_pts <- as.data.table(read_excel(paste0(data_dir,"/NAEI_data/point/raw_data/NAEIPointsSources_",year,".xlsx"), sheet = "Data"))
    
    if("PollCode" %in% names(dt_pts)) setnames(dt_pts, "PollCode", "PollutantID")
    
  }
  
  
  # remove points with 0,0 location and NA emissions
  dt_pts <- dt_pts[Easting != 0 & Northing != 0]
  dt_pts <- dt_pts[!is.na(Easting)]
  dt_pts <- dt_pts[!is.na(Northing)]
  dt_pts[, Emission := as.numeric(as.character(Emission))]
  dt_pts <- dt_pts[!is.na(Emission)]
  # add SNAP/GNFR - have to use PM10, there is no PMco sector
  dt_pts <- dt_ptsec[Pollutant == "pm10"][dt_pts, on = "SectorID"]
  
  # subset to species
  pt_2.5 <- dt_pts[PollutantID == 122] %>% setnames(., "Emission", "emis_2.5")
  pt_10 <- dt_pts[PollutantID == 24] %>% setnames(., "Emission", "emis_10")
  
  # join together by plantID
  pt_pm <- pt_10[pt_2.5, on = c("PlantID","Easting","Northing","SectorID")]
  pt_pm[is.na(emis_10), emis_10 := 0]
  pt_pm[is.na(emis_2.5), emis_2.5 := 0]
  
  if((pt_2.5[, sum(emis_2.5, na.rm=T)] / pt_pm[, sum(emis_2.5, na.rm=T)]) < 0.95 | 
     (pt_2.5[, sum(emis_2.5, na.rm=T)] / pt_pm[, sum(emis_2.5, na.rm=T)]) > 1.05) stop("extra PM2.5 emissions created")
  
  if((pt_10[, sum(emis_10, na.rm=T)] / pt_pm[, sum(emis_10, na.rm=T)]) < 0.95 | 
     (pt_10[, sum(emis_10, na.rm=T)] / pt_pm[, sum(emis_10, na.rm=T)]) > 1.05) stop("extra PM10 emissions created")
  
  # rename species
  pt_pm[, Pollutant := "pmco"]
  
  # add power station flag
  pt_pm[SectorID == 18, powFlag := 1]
  
  # set Area
  pt_pm[, AREA := "UK"]
  
  # calculate PMco and adjust if < 0
  pt_pm[, emis_co := emis_10 - emis_2.5]
  pt_pm[emis_co < 0, emis_co := 0]
  pt_pm <- pt_pm[!is.na(Year)]
  
  # tidy up
  
  col.keep <- c("Easting","Northing","Pollutant","emis_co","Year","SNAP","GNFR","AREA","powFlag")
  pt_pm <- pt_pm[,..col.keep]
  setnames(pt_pm, "emis_co", "pt_emis_t")
  
  fwrite(pt_pm, paste0(data_dir,"/Emissions_grids_plain/BNG/pmco/point/",year,"/pmco_pt_",year,"_uk_SNAPGNFR_t_BNG.csv"))
  
  l_pts_BNG <- list(pt_pm)
  names(l_pts_BNG) <- as.character(year)
  
  # transform to LL
  l_pts_LL  <- list(transformUKpts(l_dt = l_pts_BNG, species = "pmco", year = year))
  names(l_pts_LL) <- as.character(year)
  
  #### Diffuse ####
  ### subtract the PM25 maps from the PM10 maps
  ## loop through BNG and LL
  for(crs in c("LL","BNG")){
    
    res <- ifelse(crs == "BNG", "1km", "0.01")
    
    for(area in c("uk","eire")){
      
      map_suff <- ifelse(area == "uk", paste0(map_yr,"NAEImap"), "2019MEmap")
      
      # SNAP
      v_files_pm25 <- list.files(paste0(data_dir, "/Emissions_grids_plain/",crs,"/pm2.5/diffuse/",year,"/rasters_SNAP"), pattern = paste0("^pm2.5_diff_",year,"_release",emis_release,"_",area,"_SNAP"), full.names = T)
      if(length(v_files_pm25) != 11) stop("11 SNAP PM2.5 files NOT read in - check")
      s_pm25_SNAP <- rast(v_files_pm25)
      
      v_files_pm10 <- list.files(paste0(data_dir, "/Emissions_grids_plain/",crs,"/pm10/diffuse/",year,"/rasters_SNAP"), pattern = paste0("^pm10_diff_",year,"_release",emis_release,"_",area,"_SNAP"), full.names = T)
      if(length(v_files_pm10) != 11) stop("11 SNAP PM10 files NOT read in - check")
      s_pm10_SNAP <- rast(v_files_pm10)
      
      if(sum(names(s_pm25_SNAP) != names(s_pm10_SNAP)) > 0) stop("Check PM files - names don't match")
      
      s_pmco_SNAP <- s_pm10_SNAP - s_pm25_SNAP
      s_pmco_SNAP[s_pmco_SNAP < 0] <- 0
      
      # write
      sapply(X = 1:nlyr(s_pmco_SNAP), function(x) writeRaster(s_pmco_SNAP[[x]] , paste0(data_dir,"/Emissions_grids_plain/",crs,"/pmco/diffuse/",year,"/rasters_SNAP/pmco_diff_",year,"_release",emis_release,"_",area,"_SNAP_S",str_pad(names(s_pmco_SNAP[[x]]), 2, pad = "0"),"_t_",res,"_",crs,"_",map_suff,".tif"), overwrite=T))
      
      # GNFR
      v_files_pm25 <- list.files(paste0(data_dir, "/Emissions_grids_plain/",crs,"/pm2.5/diffuse/",year,"/rasters_GNFR"), pattern = paste0("^pm2.5_diff_",year,"_release",emis_release,"_",area,"_GNFR"), full.names = T)
      if(length(v_files_pm25) != 16) stop("16 GNFR PM2.5 files NOT read in - check")
      s_pm25_GNFR <- rast(v_files_pm25)
      
      v_files_pm10 <- list.files(paste0(data_dir, "/Emissions_grids_plain/",crs,"/pm10/diffuse/",year,"/rasters_GNFR"), pattern = paste0("^pm10_diff_",year,"_release",emis_release,"_",area,"_GNFR"), full.names = T)
      if(length(v_files_pm10) != 16) stop("16 GNFR PM10 files NOT read in - check")
      s_pm10_GNFR <- rast(v_files_pm10)
      
      if(sum(names(s_pm25_GNFR) != names(s_pm10_GNFR)) > 0) stop("Check PM files - names don't match")
      
      s_pmco_GNFR <- s_pm10_GNFR - s_pm25_GNFR
      
      s_pmco_GNFR[s_pmco_GNFR < 0] <- 0
      
      # write
      sapply(X = 1:nlyr(s_pmco_GNFR), function(x) writeRaster(s_pmco_GNFR[[x]] , paste0(data_dir,"/Emissions_grids_plain/",crs,"/pmco/diffuse/",year,"/rasters_GNFR/pmco_diff_",year,"_release",emis_release,"_",area,"_GNFR_",names(s_pmco_GNFR[[x]]),"_t_",res,"_",crs,"_",map_suff,".tif"), overwrite=T))
      
    } # UK/Ireland
    
  } # BNG/LL
  
  
}

#########################################################################################
