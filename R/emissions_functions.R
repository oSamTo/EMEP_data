point.source.inputs <- function(year, species, SNAP.to.GNFR, GNFR.to.SNAP){
  
  ###############################################################
  ####   Create a point source data table for UK and Eire    ####
  ###############################################################
  
  # quick lookup table
  NAEI.species.lookup <- data.table(orig = c("sox","nox","nh3","ch4","n2o","co2","nmvoc","co","pm10","pm2.5","cu","cd","ni","pb","zn"), NAEI.species = c("SO2","NOx","NH3","CH4","N2O","Carbon Dioxide as Carbon","VOC","CO","PM10","PM2.5","Cu","Cd","Ni","Pb","Zn"))
  
    print(paste0(Sys.time(),":           UK & EIRE..."))
   
    # read the relevant point sector to SNAP coversion table
    pts.sec.to.snap <- fread("./lookups/points_sectors_to_SNAP.csv")
    # attach the NAEI lookup names and remove NA
    pts.sec.to.snap$NAEI.species <- NAEI.species.lookup[,NAEI.species][match(pts.sec.to.snap$Pollutant, NAEI.species.lookup[,orig], nomatch = NA)]
    pts.sec.to.snap <- pts.sec.to.snap[!is.na(NAEI.species)]
    #pts.sec.to.snap <- pts.sec.to.snap[Pollutant == species]
    pts.sec.to.snap[,c("Pollutant","Sector","Group","NFR") := NULL]
    #pts.sec.to.snap[!duplicated(pts.sec.to.snap)]
    setnames(pts.sec.to.snap,"NAEI.species","Pollutant")
    
    # create a FRAME input file for given year & species, for UK and Ireland
  
    ###################################
    #### UK POINTS from NAEI data  ####
    ###################################
    
    ### NAEI point data - in tonnes ###
    # if the year is 2017 or 2018, use NAEO downloads, otherwise use the long time series of points
    
    if(year < 2017){
      
      dt_pts_2000_18_all <- setDT(read_excel("./NAEI_data/point/raw_data/NAEIPointsSources_1990-2018_v2_full.xlsx", sheet = "Data"))
      dt_pts_2000_18_all[,c("PollCode","PlantID","Operator","SectorName","LocusID","Region","Unit","Datatype",as.character(1990:1999)) := NULL]
      setnames(dt_pts_2000_18_all, c("Easting","Northing","Pollutant Name"), c("x","y","Pollutant"))
      #dt_pts_2000_18_all[Pollutant == "VOC", Pollutant := "NMVOC"]
      
      cols.keep <- c("Pollutant","Site","SectorID","x","y",as.character(year))
      
      dt_pts_y <- dt_pts_2000_18_all[ , ..cols.keep ]
      
      setnames(dt_pts_y, c(as.character(year),"x","y"), c("Emission","Easting","Northing"))
      
      dt_pts_y <- dt_pts_y[!is.na(Emission)]
      
      # remove points with 0,0 location
      dt_pts_y <- dt_pts_y[!is.na(Easting)]
      dt_pts_y <- dt_pts_y[!is.na(Northing)]
      
      dt_pts_y_collapse <- dt_pts_y[, lapply(.SD, sum, na.rm=TRUE), by=.(Pollutant,Site,SectorID,Easting,Northing), .SDcols="Emission"]
      
      NAEI.pts <- dt_pts_y_collapse
      
    }else{
    
      NAEI.pts <- as.data.table(read_excel(paste0("./NAEI_data/point/raw_data/NAEIPointsSources_",year,".xlsx"), sheet = "Data"))
      
      # remove points with 0,0 location
      NAEI.pts <- NAEI.pts[Easting != 0 & Northing != 0]
    
    }
    
    #### SET FLAG FOR POWER STATIONS ####
    
    NAEI.pts[SectorID == 18, pow.flag := 1]
    
    # set Area and set NAs
    NAEI.pts[,AREA:="UK"]
    
    
    ## If point data is 2016 onwards, get SNAP info from sector ID table, otherwise just subset SNAP column
    # join SNAP data based on SectorID
    NAEI.SNAP <- NAEI.pts[pts.sec.to.snap, on = c("Pollutant","SectorID")]
    NAEI.SNAP <- NAEI.SNAP[!is.na(Emission)]
    
    # format to final shape
    col.keep <- c("Easting","Northing","Pollutant","Emission","SNAP","AREA", "pow.flag")
    NAEI.format <- NAEI.SNAP[,..col.keep]
    NAEI.format[, Emission := as.numeric(as.character(Emission))]
    
    #### SUBSETTING AND CHECKING PM point sources ####
    # need to ensure PM10 is bigger than PM2.5 for a point source (there are some that are in error)
    if(species %in% c("pm10","pm2.5")){
      
      NAEI.PM <- NAEI.format[Pollutant %in% c("PM10","PM2.5")]
      NAEI.PM <- NAEI.PM[,list(Emission = sum(Emission, na.rm=T)), by = .(Easting, Northing, Pollutant, SNAP, AREA, pow.flag)]
      
      temp.wide <- dcast(NAEI.PM, ... ~ Pollutant, value.var = "Emission", aggregate = "sum")
      
      temp.wide[, ratio := PM10 / PM2.5]
      
      PM10ok <- temp.wide[ratio > 0.99999]
      
      PM10ok.m <- melt(PM10ok, id.vars=c("Easting","Northing","SNAP","AREA","pow.flag","ratio"))
      setnames(PM10ok.m,c("variable","value"),c("Pollutant","Emission"))
      PM10ok.m[,ratio := NULL]
      
      NAEI.final <- PM10ok.m[Pollutant == NAEI.species.lookup[orig == species,NAEI.species]]
            
      
    }else{
      NAEI.final <- NAEI.format[Pollutant == NAEI.species.lookup[orig == species,NAEI.species]]
    }
    
    NAEI.final <- NAEI.final[,c("Easting","Northing","Emission","SNAP","AREA","pow.flag")]
    ## UK point data in GNFR codes
    NAEI.in.GNFR <- SNAP.to.GNFR[NAEI.final, on = c("SNAP")]
    NAEI.in.GNFR <- NAEI.in.GNFR[,c("Easting","Northing","Emission","GNFR","AREA","pow.flag")]
    
    
    ## UK POINT SUMMARY ##
    # sum all point emissions in NAEI
    naei.tot <- data.table(Area = "UK", Sector = "SNAP Total", emis.kt = sum(NAEI.final[,Emission/1000]), row.names = NULL)    
    gnfr.tot <- data.table(Area = "UK", Sector = "GNFR Total", emis.kt = sum(NAEI.in.GNFR[,Emission/1000]), row.names = NULL)    
    #setkey(naei.tot, Area,Sector)
    
    # sum all point emissions by SNAP following joins
    naei.snap.tots <- cbind(rep("UK",length(unique(NAEI.final[,SNAP]))),NAEI.final[,sum(Emission)/1000,by=SNAP])
    names(naei.snap.tots) <- c("Area","Sector","emis.kt")
    
    #zero.loc.tots <- data.frame(Area = "UK", Sector = "Zero.coords", emis.kt = sum(NAEI.zero.loc[,Emission])/1000)
    
    naei.gnfr.tots <- cbind(rep("UK",length(unique(NAEI.in.GNFR[,GNFR]))),NAEI.in.GNFR[,sum(Emission)/1000,by=GNFR])
    names(naei.gnfr.tots) <- c("Area","Sector","emis.kt")
    
    pt.summary.uk <- do.call("rbind",list(naei.snap.tots, unname(naei.tot[1,]), naei.gnfr.tots, unname(gnfr.tot[1,])))
    setkey(pt.summary.uk, Area,Sector)
    
    ###################################################
    #### IRISH POINTS - same input file as UK data ####
    ###################################################

    # taking from the EPRTR database - the European point source register
    # extract of EPRTR database must have already taken place via;
    # ...\NAEI_data_and_SNAPS\E-PRTR_point_data\EPRTR_extract.R
    
    ### 17/11/20 : NO EIRE POINTS ANYMORE - WE ARE ONLY USING MAPEIRE DIFFUSE DATA
    ### UNFORTUNATELY THIS MEANS EXTRA INFO OF POINTS IS NOT USED
    ### BUT EPRTR IS NOT COMPLETE
    EIRE.final <- data.table(Easting = integer(), Northing = integer(), Emission = numeric(), GNFR = character(), AREA = character(), pow.flag = integer())
      
    EIRE.in.SNAP <- data.table(Easting = integer(), Northing = integer(), Emission = numeric(), SNAP = character(), AREA = character(), pow.flag = integer())
      
    pt.summary.ire <- NULL
    
    ## THERE ARE NO PM2.5 data in the E-PRTR - skip
    #if(species == "pm2.5" & year < 2018){print("NO PM2.5 POINT DATA in E-PRTR, making blank point file...")
    #  
    #  EIRE.final <- data.table(Easting = integer(), Northing = integer(), Emission = numeric(), GNFR = character(), AREA = character(), pow.flag = integer())
    #  
    #  EIRE.in.SNAP <- data.table(Easting = integer(), Northing = integer(), Emission = numeric(), SNAP = character(), AREA = character(), pow.flag = integer())
    #  
    #  pt.summary.ire <- NULL
    #  
    #  }else{
    #
    #    
    # read E-PRTR data - data is in kg
    #EPRTR.pts <- fread(paste0("./E-PRTR_point_data/",species,"/",species,"_pts_",year,"_E-PRTR.csv"))
    #
    # subset for Ireland
    #EPRTR.Ire <- EPRTR.pts[CountryName == "Ireland"]
    #
    # convert units to tonnes
    #EPRTR.Ire[, Emission := (TotalQuantity / 1000) ]
    #
    # set Area
    #EPRTR.Ire[,AREA:="EIRE"]
    #
    # change some column names
    #setnames(EPRTR.Ire,c("Xbng","Ybng","GNFR14"), c("Easting","Northing","GNFR"))
    #
    # format to final shape
    #col.keep <- c("Easting","Northing","Emission","GNFR","AREA")
    #EIRE.final <- EPRTR.Ire[,..col.keep]
    #
    #### SET FLAG FOR POWER STATIONS - all in GNFR A in Eire ####
    #EIRE.final[GNFR == "A_PublicPower", pow.flag := 1]
    #
    ## Eire point data in SNAP codes
    #EIRE.in.SNAP <- GNFR.to.SNAP[EIRE.final, on = c("GNFR")]
    #EIRE.in.SNAP <- EIRE.in.SNAP[,c("Easting","Northing","Emission","SNAP","AREA","pow.flag")]
    
    ## MINI EIRE SUMMARY ##
    # sum all NOx point emissions in E-PRTR
    #eprtr.tot <- data.table(Area = "EIRE", Sector = "GNFR Total", emis.kt = sum(EPRTR.Ire[,Emission/1000]), row.names = NULL)    
    #ire.snap.tot <- data.table(Area = "EIRE", Sector = "SNAP Total", emis.kt = sum(EIRE.in.SNAP[,Emission/1000]), row.names = NULL)    
    
    # sum all point emissions in E-PRTR by GNFR grouping (EMEP version of SNAP)
    #eprtr.gnfr.tots <- cbind(rep("EIRE",length(unique(EPRTR.Ire[,GNFR]))),EPRTR.Ire[,sum(Emission)/1000,by=GNFR])
    #names(eprtr.gnfr.tots) <- c("Area","Sector","emis.kt")
    
    #ire.snap.tots <- cbind(rep("EIRE",length(unique(EIRE.in.SNAP[,SNAP]))),EIRE.in.SNAP[,sum(Emission)/1000,by=SNAP])
    #names(ire.snap.tots) <- c("Area","Sector","emis.kt")
    
    #pt.summary.ire <- do.call("rbind",list(eprtr.gnfr.tots, unname(eprtr.tot[1,]), ire.snap.tots, unname(ire.snap.tot[1,])))
    #setkey(pt.summary.ire, Area,Sector)
      
    #}  
    
    #########################
    ### Combine UK & EIRE ###
    #########################
    
    # combine datasets
    all.pts.snap <- rbindlist(list(NAEI.final,EIRE.in.SNAP))
    all.pts.gnfr <- rbindlist(list(NAEI.in.GNFR,EIRE.final))
    
    ## SUMMARY OF ALL UK & EIRE ##
    pt.summary.all.snap <- all.pts.snap[,sum(Emission)/1000, by=.(AREA,SNAP)]
    names(pt.summary.all.snap) <- c("Area","Sector","processed.kt")
    
    pt.summary.all.gnfr <- all.pts.gnfr[,sum(Emission)/1000, by=.(AREA,GNFR)]
    names(pt.summary.all.gnfr) <- c("Area","Sector","processed.kt")
    
    
    pt.summary.all <- rbind(pt.summary.all.snap, 
                            data.frame(Area = "ALL", Sector = "Total", pt.summary.all.snap[,list(processed.kt = sum(processed.kt))]),
                            pt.summary.all.gnfr,
                            data.frame(Area = "ALL", Sector = "Total", pt.summary.all.gnfr[,list(processed.kt = sum(processed.kt))]))
    
    setkey(pt.summary.all, Area,Sector)
    
    #################
    ## return data ##
    
    pt.data <- list("uk.snap" = NAEI.final, "uk.gnfr" = NAEI.in.GNFR, "eire.snap" = EIRE.in.SNAP, "eire.gnfr" = EIRE.final, "ukeire.snap" = all.pts.snap, "ukeire.gnfr" = all.pts.gnfr)
    pt.summary <- list("uk" = pt.summary.uk, "eire" = pt.summary.ire, "ukeire" = pt.summary.all)
    
    pts.list <- list("data" = pt.data, "summary" = pt.summary)
    
    return(pts.list)
    
    
    
} # end of point emissions function

############################################################################################


diffuse.source.emissions <- function(year, latest.avail, species, pt.file, NFR.SNAP, CRF.GNFR, SNAP.to.GNFR, GNFR.to.SNAP){
  
  #############################
  #### CREATE UK EMISSIONS ####
  #############################
  print(paste0(Sys.time(),":           UK..."))

    ## first a summary on what the NAEI NFR tables says on totals
    NAEIdata <- fread(paste0("./NAEI_data/diffuse/",species,"/NFC_time_series/naei_",species,"_1990-",latest.avail,".csv"),header=T)
    NAEIdata <- NAEIdata[Source != ""]
    
    # subset
    cols <- c("NFR/CRF Group",year)
    NAEIdata17 <- NAEIdata[,..cols]
    setnames(NAEIdata17,c("NFR19","ems_kt"))
    
    # Associate SNAP to NFR19 category and remove NA matches
    NAEIdata17 <- NFR.SNAP[NAEIdata17, on =c("NFR19")]
    
    # Convert emissions to numeric (allows summing)
    # Complete cases: Remove NA SNAP and remove NA emissions - stuff like aviation cruise and randoms
    suppressWarnings(NAEIdata17[,ems_kt:= as.numeric(as.character(ems_kt))])
    NAEIdata17.SNtot <- NAEIdata17[, .(SNAPsum = sum(ems_kt,na.rm=T)), by = SNAP]
    NAEIdata17.SNtot <- rbindlist(list(NAEIdata17.SNtot, data.table(SNAP="8.no.intl.ship",SNAPsum=NAEIdata17.SNtot[SNAP=="8",SNAPsum]-sum(NAEIdata17[NFR19=="z_1A3di(i)",ems_kt]))))
    
    #### Reading in the diffuse maps
    
    ## NAEI Area data - tonnes per cell
    naei_all_files <- list.files(paste0("./NAEI_data/diffuse/",species,"/maps/",latest.avail,"/"), pattern = ".tif$", full.names = TRUE)
    
    # remove "total" rasters
    naei_all_files <- naei_all_files[!grepl("tota",naei_all_files)]
    
    # in a loop, read in each NAEI SNAP map and stack original - crop by extended domain (very little data loss)
    allSNAP.st <- stack()
    allSNAP.st.orig <- stack()
    
    # when reading in each SNAP, if the year = latest year, simply take surface, crop and stack
    # if year != latest year, the map needs to be scaled by the correct amount of diffuse emissions
          # i.e. totals from table need point totals deducting to make new diffuse totals
    
    # point snap totals
    pt.file.SNtot <- pt.file[["uk.snap"]][AREA == "UK", .(SNAPsumpt = sum(Emission,na.rm=T)/1000), by = SNAP]
    pt.file.SNtot[, SNAP := as.character(SNAP)]
    # combine point and table totals and make new diffuse total for specific year (i.e. like the latest map)
    all.SNtot <- pt.file.SNtot[NAEIdata17.SNtot, on = c("SNAP")]
    all.SNtot[is.na(SNAPsumpt), SNAPsumpt := 0]
    all.SNtot[,new.diff_ems := SNAPsum - SNAPsumpt]
    all.SNtot[new.diff_ems < 0, new.diff_ems:= 0]
    
    for(i in 1:length(naei_all_files)){
      
      r <- raster(naei_all_files[i])
      
      # change the names to SNAP sectors
      names(r) <- ifelse(grepl("energyprod",names(r)), "S1", ifelse(grepl("domcom",names(r)), "S2", ifelse(grepl("indcom",names(r)), "S3", ifelse(grepl("indproc",names(r)), "S4", ifelse(grepl("offshore",names(r)), "S5", ifelse(grepl("solvent",names(r)), "S6", ifelse(grepl("roadtran",names(r)), "S7", ifelse(grepl("othertran",names(r)), "S8", ifelse(grepl("waste",names(r)), "S9", ifelse(grepl("agric",names(r)), "S10", "S11"))))))))))
      
      # NEED TO SCALE RASTER HERE IF NOT LATEST YEAR
      if(year != latest.avail){
        
        new.total.kt <- (all.SNtot[SNAP == substr(names(r),2,nchar(names(r))), new.diff_ems]) * 1000
        latest.total.kt <- cellStats(r,sum)
        #rescale
        r <- (r / latest.total.kt) * new.total.kt
        
      }else{}
      
      c <- crop(extend(r, uk.domain.1km), uk.domain.1km)
      
      
      allSNAP.st.orig <- stack(allSNAP.st.orig, r)
      allSNAP.st <- stack(allSNAP.st, c)
    }
    
   
    ### UK diffuse data now stacked in SNAP codes in tonnes cell-1 ###
    ## !! UNLESS IT'S A METAL, WHICH IS IN KG CELL-1 !! ##
    
    if(species %in% c("cd", "cu", "ni", "pb", "zn")){
      # convert metals to t cell-1, from kg cell-1
      
      allSNAP.st <- allSNAP.st/1000
      allSNAP.st.orig <- allSNAP.st.orig/1000
      
    }else{}
    
    # keep as tonnes cell-1
    uk.diffuse.snap <- allSNAP.st
    crs(uk.diffuse.snap) <- BNG
    
    ## Convert it to rough GNFR sectors
    uk.diffuse.gnfr <- uk.diffuse.snap
    
    for(h in names(uk.diffuse.gnfr)){
      
      names(uk.diffuse.gnfr)[names(uk.diffuse.gnfr) == h] <- SNAP.to.GNFR[SNAP %in% gsub("S","",h), GNFR]
      
    }
    
    # collpase the 2 industry layers (if SNAPs 3 & 4 were in original data)
    if(length(grep("B_Industry", names(uk.diffuse.gnfr)))>1){
      
      temp <- uk.diffuse.gnfr[[grep("B_Industry", names(uk.diffuse.gnfr))]]
      temp.s <- calc(temp, sum, na.rm=T)
      names(temp.s) <- "B_Industry"
      
      uk.diffuse.gnfr <- stack(uk.diffuse.gnfr[[grep("B_Industry", names(uk.diffuse.gnfr), invert = T)]], temp.s)
      
    }else{}
    
    crs(uk.diffuse.gnfr) <- BNG
    
    if(sum(cellStats(uk.diffuse.snap, sum)/1000) / sum(cellStats(uk.diffuse.gnfr, sum)/1000) < 0.999 | sum(cellStats(uk.diffuse.snap, sum)/1000) / sum(cellStats(uk.diffuse.gnfr, sum)/1000) > 1.001){stop(print("Conversion of UK diffuse data to GNFR has caused a summing error!"))}else{}
      
        
  
  uk.diffuse.midway.list <- list("NAEI.SNAP.tots" = NAEIdata17.SNtot, "pts" = pt.file, "NAEI.snap.ras" = allSNAP.st.orig, "NAEI.snap.ras.crop" = uk.diffuse.snap, "NAEI.gnfr.ras.crop" = uk.diffuse.gnfr)
  
  # mid way stats for UK
  uk.stats.midway <- midpoint.diffuse.summary.stats.UK(midway.data = uk.diffuse.midway.list, species = species, year = year)
  
  
  
  ########################## 
  ## Irish AREA EMISSIONS ##
  ##########################
  
  ## latest distribution = 2016 from MapEire.
  
  ## create a raster on the new UK FRAME domain, and add to stack.
  
  ## FRAME can take any number of columns - using combined sectors to map GNFR to SNAP sector (try to)
  ## CEIP: "No direct conversion from SNAP to GNFR possible"
  
  ## Irish data is NFR to GNFR based (the maps are done by MapEire project) - lastest are 2016 (1/8/19)
  
  #### 17/11/20: NO LONGER REMOVING POINTS FROM MAP-ERIE DATA SURFACES ####
  
  ## THIS FOLLOWING TEXT IS DEPRECATED:
  #!DEP!# the points must be removed SPATIALLY as GNFR maps done with all sources included. Extra step than UK.  
  
  #!DEP!# 1. Read in MapEire spatial distribution (t NOx/SOx/NH3 km-2 yr-1)  
  #!DEP!# 2. Update MapEire distributions with required year data
  #!DEP!# 3. Read in Irish point data (Gg N/S yr-1)
  #!DEP!# 4. Subtract point data from the MapEire data, spatially, by GNFR
  #!DEP!# 5. Scale remaining spatial data by the totals from EMEP/UNFCCC and add together GNFR maps to combined sector
  #!DEP!# (In Irish data, point removal is done at GNFR before map aggregation, as that data is available to)
  
  print(paste0(Sys.time(),":           EIRE..."))
  
  
  
  if(species %in% c("nox","sox","nh3","co","nmvoc","pm10","pm2.5","cd", "cu", "ni", "pb", "zn")){
    # get EMEP air pollutant data in kt
    emep <- fread(paste0("./EIRE_data/diffuse/EMEP_totals/",species,"/EMEP_",species,"_ann.tots_GNFR_90-",substr(latest.avail,3,4),".txt"),skip=1)
    setnames(emep,c("# Format: ISO2","NUMBER/FLAG","SECTOR"),c("ISO2","EMISSION","GNFR14"))
    emep <- emep[ISO2=="IE" & YEAR == year]
    
    # change emissions to numeric and remove NA lines and superflous columns
    suppressWarnings(emep <- emep[, EMISSION:=as.numeric(EMISSION)])
    #emep <- emep[!is.na(EMISSION)]
    emep[,GNFR14 := gsub("N14 ","",GNFR14)]
    
    # remove "Other" from data (no mapping)
    if("M_Other" %in% emep[,GNFR14]){
      emep <- emep[GNFR14 != "M_Other"]
    } else{NULL}
    
    emep[,c("ISO2","YEAR","POLLUTANT","UNIT") := NULL]
    setnames(emep, "GNFR14","GNFR")
    emep <- emep[GNFR != "z_Memo"]
    ire.scale.data <- emep
    
    
    
  }else if(species %in% c("ch4","co2","n2o")){
    # get EMEP/UNFCCC total emissions data - to scale MapEire data  
    unfccc.ghgs <- fread(paste0("./EIRE_data/diffuse/UNFCCC_GHG_emis_1990-",latest.avail,".csv"))
    unfccc.17 <- unfccc.ghgs[Pollutant_name == upper.spec & Year == year & Country == "Ireland" & emissions > 0]
    setnames(unfccc.17,"Sector_code", "CRF")
    # join codes & remove shipping and NA
    unfccc.gnfr <- unfccc.17[CRF.GNFR, on = "CRF"]
    unfccc.gnfr <- unfccc.gnfr[!(CRF %in% c("1.D.1.a","1.D.1.b"))]
    unfccc.gnfr <- unfccc.gnfr[!is.na(emissions)]
    
    # remove "Other" from data (no mapping)
    if("M_Other" %in% unfccc.gnfr[,GNFR19]){
      unfccc.gnfr <- unfccc.gnfr[GNFR19 != "M_Other"]
    } else{NULL}
    unfccc.gnfr[,emissions:=as.numeric(emissions)]
    # remove some columns
    #unfccc.gnfr[,c("ISO2","YEAR","POLLUTANT","UNIT") := NULL]
    setnames(unfccc.gnfr, c("GNFR19","emissions"),c("GNFR","EMISSION"))
    unfccc.gnfr <- unfccc.gnfr[GNFR != "z_Memo"]
    unfccc.gnfr[GNFR == "C_OtherStationaryComb", GNFR := "C_OtherStatComb"]
    ire.scale.data <- unfccc.gnfr[,list(EMISSION = sum(EMISSION)),by="GNFR"]
    
  }
  
  
  #### 17/11/20 :  NO LONGER USING POINT SOURCES, JUST OUTPUTTING THE SCALED MAPEIRE SURFACE ####
  
  ## read in Irish FRAME point source emissions - use points created under NFC already
  ## these have to be subtracted from the MapEire maps SPATIALLY, by GNFR sector (only need GNFR here)
  ## then diffuse maps can be changed to SNAP
  pointdata.ire <- pt.file[["eire.gnfr"]][AREA=="EIRE"]
  
  ##################################################
  ## SCALING TO EMEP YEAR AND SPATIAL SUBTRACTION ##
  
  # Bring in GNFR map(s), sum if needed, remove points, and scale combined map by latest EMEP data
  #! For NOx/SOx K_Agri & L_Agri, use NH3 surface !#
  #! Skip if MapEire total is 0!#
  
  eire.scaled.list <- list()
  eire.scaled.no.pts.list <- list()
  
  req.ext <- extent(uk.domain.1km)
  iri.diffuse.gnfr <- stack()
  
  dirs <- list.dirs(path = paste0("./EIRE_data/diffuse/MapEire_data/ME_2016"), full.names = TRUE, recursive = TRUE)[-1]
  
  
  # loop through combined sectors from EMEP/UNFCCC data agg
  for(sector in ire.scale.data[,GNFR]){
    
    # check if the sector exists in the combined sectors list. If it doesnt; create blank and move onto next
    # Do same and skip for 'Natural' (no distribution for Eire)
    if(sector == "N_Natural"){
      # use blank 
      scaled.MapEire <- uk.domain.1km
      scaled.MapEire[] <- NA
      
      # name and stack
      scaled.MapEire.ext <- crop(extend(scaled.MapEire, req.ext, value=NA),req.ext)
      names(scaled.MapEire.ext) <- "N_Natural"
      
      iri.diffuse.gnfr <- stack(iri.diffuse.gnfr,scaled.MapEire.ext)
      next
      
    }else{
      NULL
    }
    
    # get required GNFR sectors that make up the combined sector
   
    req.MapEire.map <- crop(extend(raster(paste0("./EIRE_data/diffuse/MapEire_data/ME_2016/2016_",sector,"/2016_",sector,"_",upper.spec,"_t_BNG.tif")), req.ext, value=NA),req.ext)
   
    # scale map - has to be done pre-point removal. If it's NH3 agriculture, scale both separately then sum.
    scaled.MapEire <- (req.MapEire.map/cellStats(req.MapEire.map, sum)) * ire.scale.data[GNFR==sector,EMISSION * 1000]
    
    # add sum to a list
    eire.scaled.list[[sector]] <- cellStats(scaled.MapEire, sum)/1000
    
    
    # subset the point file read in above to GNFR
    #ire.pts.gnfr <- pointdata.ire[GNFR == sector]
    ire.pts.gnfr <- data.table()
    
    if(nrow(ire.pts.gnfr)==0){
      
      # name and stack
      scaled.MapEire.ext <- scaled.MapEire
      names(scaled.MapEire.ext) <- sector
      
      iri.diffuse.gnfr <- stack(iri.diffuse.gnfr, scaled.MapEire.ext)
      
      # add total to a list (no points to remove)
      eire.scaled.no.pts.list[[sector]] <- cellStats(scaled.MapEire.ext, sum)/1000
      
    } else {
      ## THIS SECTION IS CURRENTLY NOT USED ##
      ## THIS SECTION REMOVES POINTS FROM SPATIAL SURFACE ##
      
      # make spatial points dataframe
      coordinates(ire.pts.gnfr) <- ~Easting+Northing
      
      # rasterize the points to the same grid as the GNFR map
      ire.pts.gnfr.r <- rasterize(ire.pts.gnfr, field = "Emission", uk.domain.1km, sum)
      
      # focally subtract points from surfaces - points dont always align with surfaces (could be adjacent cell etc) so need to subtract the point from the grid of 9 area cells it falls within, thus ensuring minimal/no data duplication
      
      if(nrow(ire.pts.gnfr)==1){
        pts.as.cell.nums <- data.frame(layer=getValues(ire.pts.gnfr.r)[!is.na(getValues(ire.pts.gnfr.r))])
        row.names(pts.as.cell.nums) <- match(getValues(ire.pts.gnfr.r)[!is.na(getValues(ire.pts.gnfr.r))],getValues(ire.pts.gnfr.r))
      }else{
        pts.as.cell.nums <- as.data.frame(ire.pts.gnfr.r, cellnumbers=TRUE, na.rm=T,row.names = T)
      }
      
      # loop through cells with point sources in
      for(p in 1:nrow(pts.as.cell.nums)){
        
        # skip if all 9 values below are NA - point is extra data
        if((sum(is.na(scaled.MapEire[adjacent(ire.pts.gnfr.r, as.numeric(rownames(pts.as.cell.nums)[p]), directions=8, pairs=FALSE, include=T)]))==9)==T){next}else{}
        
        # convert point value to tonnes
        point.val <- pts.as.cell.nums[p,]
        
        # get same cell value from area raster
        area.val <- scaled.MapEire[as.numeric(rownames(pts.as.cell.nums)[p])]
        area.val <- ifelse(is.na(area.val),0,area.val)
        
        if(area.val >= point.val){
          # simply remove point value from underlying raster value and move on in the loop
          scaled.MapEire[as.numeric(rownames(pts.as.cell.nums)[p])] <- (area.val - point.val)
        }else{
          
          # calculate the excess emissions to spread, and set target cell to 0
          excess <- abs(area.val - point.val)
          scaled.MapEire[as.numeric(rownames(pts.as.cell.nums)[p])] <- 0
          
          # extract cell numbers of surrounding 8 cells
          surr.cells <- adjacent(ire.pts.gnfr.r, as.numeric(rownames(pts.as.cell.nums)[p]), directions=8, pairs=FALSE, include=F) 
          
          # get the values of those 8 cells, and work out proportion they represent
          surr.cells.values <- scaled.MapEire[surr.cells]
          surr.cells.props <- surr.cells.values/sum(surr.cells.values,na.rm=T)
          
          # calculate the amount each cell must take by their proportion
          values.to.subtract <- surr.cells.props * excess
          
          # subtract from actual cell values. If any < 0, reset to 0. It will be excess data. 
          new.values.insert <- surr.cells.values - values.to.subtract
          new.values.insert[new.values.insert<0] <- 0
          
          # insert back into raster
          scaled.MapEire[surr.cells] <- new.values.insert
          
        }
        
      }# end of point removal loop
      
      # After removing point data, name it and stack it
      scaled.MapEire.ext <- scaled.MapEire
      names(scaled.MapEire.ext) <- sector
      
      # add total to a list (points removed)
      eire.scaled.no.pts.list[[sector]] <- cellStats(scaled.MapEire.ext, sum)/1000
      
      iri.diffuse.gnfr <- stack(iri.diffuse.gnfr,scaled.MapEire.ext)
      
    }  
    
  }
  
  # drop the O_AviCruise layer
  iri.diffuse.gnfr <- dropLayer(iri.diffuse.gnfr, "O_AviCruise")
  
  # Irish data stacked in tonnes km-2
  crs(iri.diffuse.gnfr) <- BNG
  
  ## Convert it to rough GNFR sectors
  iri.diffuse.snap <- iri.diffuse.gnfr
  
  for(h in names(iri.diffuse.snap)){
    
    # change name
    names(iri.diffuse.snap)[names(iri.diffuse.snap) == h] <- paste0("S",GNFR.to.SNAP[GNFR %in% h, SNAP])
    
  }
  
  # collpase the 2 industry layers (if SNAPs 3 & 4 were in original data)
  for(multi in c("S8","S10")){
  
  if(length(grep(multi, names(iri.diffuse.snap)))>1){
    
    temp <- iri.diffuse.snap[[grep(multi, names(iri.diffuse.snap))]]
    temp.s <- calc(temp, sum, na.rm=T)
    names(temp.s) <- multi
    
    iri.diffuse.snap <- stack(iri.diffuse.snap[[grep(multi, names(iri.diffuse.snap), invert = T)]], temp.s)
    
  }else{}
    
  } # end of collapsing multiple snaps
  
  crs(iri.diffuse.snap) <- BNG
  
  if(sum(cellStats(iri.diffuse.gnfr, sum)/1000)/sum(cellStats(iri.diffuse.snap, sum)/1000) < 0.999 | sum(cellStats(iri.diffuse.gnfr, sum)/1000)/sum(cellStats(iri.diffuse.snap, sum)/1000) > 1.001){stop(print("Conversion of Irish diffuse data to SNAP has caused a summing error!"))}else{}
  
  
  
  ## summary data list
  eire.diffuse.midway.list <- list("eire.annual.tots" = ire.scale.data, "pts" = pointdata.ire, "eire.scaled" = eire.scaled.list, "eire.scaled.no.pts" = eire.scaled.no.pts.list, "eire.in.snap" = iri.diffuse.snap)
  
  # mid way stats for EIRE
  eire.stats.midway <- midpoint.diffuse.summary.stats.EIRE(midway.data = eire.diffuse.midway.list, species = species, year = year)
  
  
  ##### BOTH STACKS NOW READY TO BE COMBINED #####  
  
  ######################################### 
  ## Combining UK & Irish AREA EMISSIONS ##
  #########################################
  
  print(paste0(Sys.time(),":           COMBINING UK & EIRE..."))
  
 
      # combine both;
      # 1. the UK SNAP and adjusted Eire SNAP
      # 2. the adjusted UK GNFR and Irish GNFR
      
      ### SNAP STACK ###
      snap_order <- c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10", "S11")
      # pad out snap stacks with any missing layers, and then put in correct order of SNAP 1 to SNAP 11
      # UK
      if(length(paste0("S",1:11)[!(paste0("S",1:11) %in% names(uk.diffuse.snap))]) > 0){
        
      for(l in paste0("S",1:11)[!(paste0("S",1:11) %in% names(uk.diffuse.snap))]){
        
        temp <- uk.domain.1km
        names(temp) <- l
        uk.diffuse.snap <- stack(uk.diffuse.snap,temp)
        
        }  
      
      }else{}
      # re-order
      uk.diffuse.snap <- uk.diffuse.snap[[snap_order]]
      
      # Eire
      if(length(paste0("S",1:11)[!(paste0("S",1:11) %in% names(iri.diffuse.snap))]) > 0){
        
        for(l in paste0("S",1:11)[!(paste0("S",1:11) %in% names(iri.diffuse.snap))]){
          
          temp <- uk.domain.1km
          names(temp) <- l
          iri.diffuse.snap <- stack(iri.diffuse.snap,temp)
          
        }  
        
      }else{}
      # re-order
      iri.diffuse.snap <- iri.diffuse.snap[[snap_order]]
      
      ## add the two sets of raster stacks together into 1
      
      if(identical(names(uk.diffuse.snap),names(iri.diffuse.snap))){
        
        full.snap.st <- stack()
        
        for(l in paste0("S",1:11)){
          
          full.layer <- calc(stack(uk.diffuse.snap[[l]],iri.diffuse.snap[[l]]), sum, na.rm=T)
          names(full.layer) <- l
          full.snap.st <- stack(full.snap.st,full.layer)
          
        }
        
      }else{print(paste0(Sys.time(),": SNAP names & nlayers in Irish & UK stacks for ", species," ",year," are NOT the same. Check.")) 
        break}
      
      ## FULL STACK of SNAP data for UK & EIRE, in tonnes cell-1
      
      
      ### GNFR STACK ###
      gnfr_order <- GNFR.to.SNAP[,GNFR]
      # pad out gnfr stacks with any missing layers, and then put in correct order of A_PublicPower to P_IntShipping
      # UK
      if(length(GNFR.to.SNAP[,GNFR][!(GNFR.to.SNAP[,GNFR] %in% names(uk.diffuse.gnfr))]) > 0){
        
        for(l in GNFR.to.SNAP[,GNFR][!(GNFR.to.SNAP[,GNFR] %in% names(uk.diffuse.gnfr))]){
          
          temp <- uk.domain.1km
          names(temp) <- l
          uk.diffuse.gnfr <- stack(uk.diffuse.gnfr,temp)
          
        }  
        
      }else{}
      # re-order
      uk.diffuse.gnfr <- uk.diffuse.gnfr[[gnfr_order]]
      
      # Eire
      if(length(GNFR.to.SNAP[,GNFR][!(GNFR.to.SNAP[,GNFR] %in% names(iri.diffuse.gnfr))]) > 0){
        
        for(l in GNFR.to.SNAP[,GNFR][!(GNFR.to.SNAP[,GNFR] %in% names(iri.diffuse.gnfr))]){
          
          temp <- uk.domain.1km
          names(temp) <- l
          iri.diffuse.gnfr <- stack(iri.diffuse.gnfr,temp)
          
        }  
        
      }else{}
      # re-order
      iri.diffuse.gnfr <- iri.diffuse.gnfr[[gnfr_order]]
      
      ## add the two sets of raster stacks together into 1
      
      if(identical(names(uk.diffuse.gnfr),names(iri.diffuse.gnfr))){
        
        full.gnfr.st <- stack()
        
        for(l in GNFR.to.SNAP[,GNFR]){
          
          full.layer <- calc(stack(uk.diffuse.gnfr[[l]],iri.diffuse.gnfr[[l]]), sum, na.rm=T)
          names(full.layer) <- l
          full.gnfr.st <- stack(full.gnfr.st,full.layer)
          
        }
        
      }else{print(paste0(Sys.time(),": GNFR names & nlayers in Irish & UK stacks for ", species," ",year," are NOT the same. Check.")) 
        break}
      
      ## FULL STACK of GNFR data for UK & EIRE, in tonnes cell-1
      
      
  ## summarise the combined stacks
  
  # summary data list for end combined stacks
  ukeire.combi.list <- list("combined.snap" = full.snap.st, "combined.gnfr" = full.gnfr.st)
  
  # summarise the combined stacks
  combined.stats.final <- diffuse.summary.combined(combined.data = ukeire.combi.list, species = species, year = year)
  
  
  #################
  ## return data ##
  
  diff.data <- list("uk.diffuse.snap" = uk.diffuse.snap, "uk.diffuse.gnfr" = uk.diffuse.gnfr,
                    "eire.diffuse.snap" = iri.diffuse.snap, "eire.diffuse.gnfr" = iri.diffuse.gnfr,
                    "ukeire.diffuse.snap" = full.snap.st, "ukeire.diffuse.gnfr" = full.gnfr.st)
  
  diff.summary <- list("uk" = uk.stats.midway, "eire" = eire.stats.midway, "ukeire" = combined.stats.final)
  
  diff.list <- list("data" = diff.data, "summary" = diff.summary)
  
  return(diff.list)
  
  print(paste0(Sys.time(),":       COMPLETE."))
  
} # end of diffuse emissions function

############################################################################################

create.m2.cell.area.LL <- function(uk.latlon.grid, reqres){
  
  ####################################################################
  ####   Create a surface that represents LL grid as area in m2   ####
  ####################################################################
  
  all.xy <- as.data.table(as.data.frame(uk.latlon.grid, xy=T))
  
  returnLLarea <- function(x,y){
    coords <- rbind(c(x - reqres/2, y - reqres/2),c(x - reqres/2, y + reqres/2),c(x + reqres/2, y + reqres/2),c(x + reqres/2, y - reqres/2),c(x - reqres/2, y - 0.005))
    
    polyArea <- geosphere::areaPolygon(coords)
    
    return(polyArea)
  }
  
  all.xy[,area_m2 := mapply(returnLLarea, x, y)]
  
  LLareas <- rast(as.matrix(all.xy[,c(1,2,4)]), type = "xyz")
  crs(LLareas) <- "epsg:4326"
  #LLareas <- rasterFromXYZ(all.xy[,c(1,2,4)])
  
  return(LLareas)
  
  
} # end of function


############################################################################################


reproject.point.source.inputs <- function(year, species){
  
  ################################################################
  ####   Reproject pre-made BNG point data to LL projection   ####
  ################################################################
 
  # create directory
  dir.create(file.path(paste0("./Emissions_grids_plain/LL/",species,"/point/",year)), showWarnings = FALSE, recursive = T)
  
  # do it for uk, eire, uk&eire for GNFR and SNAP
  
  reprojected.pts <- list()
  
  for(country in c("uk","eire","ukeire")){
    
    print(paste0(Sys.time(),":           ",toupper(country),"..."))
    
    for(class in c("SNAP","GNFR")){
    
    pt.data <- fread(paste0("./Emissions_grids_plain/BNG/",species,"/point/",year,"/",species,"_pt_",year,"_",country,"_",class,"_t_BNG.csv"))
    
    if(nrow(pt.data) == 0){
      
      data.converted.dt <- data.table(Lon = numeric(), Lat = numeric(), Emission = numeric(), class = character(), AREA = character(), pow.flag = integer())
      setnames(data.converted.dt,"class",paste0(class))
      
      reprojected.pts[[paste0(country,".",class)]] <- data.converted.dt
      
      print(paste0(Sys.time(),":       No ",class," point data for ",species," in ",year," in ",country,"; skipping..."))
      
    }else{
    
      
    # promote to sf
    #data.to.convert <- pt.data
    #coordinates(data.to.convert) <- ~Easting + Northing
    #crs(data.to.convert) <- BNG
      
    data.to.convert <- st_as_sf(pt.data, coords = c('Easting', 'Northing'), crs = st_crs(27700))
    
    # transform points to LL
    #data.converted <- spTransform(data.to.convert, LL)
    data.converted <- st_transform(data.to.convert, st_crs(4326))
    
    # convert back to data frame
    #data.converted.dt <- as.data.table(as.data.frame(data.converted, xy=T))
    #setnames(data.converted.dt, c("Easting","Northing"), c("Lon","Lat"))
    data.converted.dt <- cbind(st_coordinates(data.converted),st_drop_geometry(data.converted))
    setnames(data.converted.dt, c("X","Y"), c("Lon","Lat"))
    
    # choose columns and return
    cols.keep <- c("Lon","Lat","Emission",class, "AREA","pow.flag")
    data.converted.dt <- data.converted.dt[, ..cols.keep]
    
    reprojected.pts[[paste0(country,".",class)]] <- data.converted.dt
    
    if(identical(sum(reprojected.pts[[paste0(country,".",class)]][,Emission])/1000,sum(pt.data[,Emission])/1000)){
      }else{stop(print("POINT REPROJECTION HAS CHANGED TOTALS - CHECK"))}
    
      } # ifelse for zero point data
    
    } # end of class loop
    
    
  } # end of country loop
  
  return(reprojected.pts)
  
} # end of point reprojection function


############################################################################################


reproject.diffuse.source.emissions <- function(year, species, latest.avail, latlon.in.m2, reqres){
  
  #################################################################
  ####   Reproject pre-made BNG diffuse data to pre-made LL    ####
  ####   grid (using terra::resample for cubic function)       ####
  #################################################################
  
  ## gdalwarp not being used (oct 2020)
  
  master.return <- list()
  
  
  for(country in c("uk","eire","ukeire")){
    
    for(class in c("SNAP","GNFR")){
      
      # create directory
      dir.create(file.path(paste0("./Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_",class)), showWarnings = FALSE, recursive = T)
      
      ############
      
      print(paste0(Sys.time(),":           ",toupper(country)," ",class," sectors..."))
      
      rel.rasters <- list.files(paste0("./Emissions_grids_plain/BNG/",species,"/diffuse/",year,"/rasters_",class), pattern = paste0(species,"_diff_",year,"_",country,"_",class,".*",latest.avail,"NAEImap.tif$"), full.names = T)
      
      ### GDAL REMOVED due to problems with PROJ6 updates
      ### 29/10/20: using terra::resample now
      # this is the lat long grid ready for terra, using WKT CRS
      uk.latlon.grid.wkt <- rast(xmin = -13.8, xmax = 4.6, ymin = 49, ymax = 61.5, resolution = 0.01, crs = crs(LL), vals = NA)
      
      # take all sector names from raster file names
      if(class=="SNAP"){
        sec.codes <- unlist(lapply(X=1:length(rel.rasters), function(x) str_split(rel.rasters[x],"_")[[1]][9]))
      }else{
        sec.codes <- paste0(unlist(lapply(X=1:length(rel.rasters), function(x) str_split(rel.rasters[x],"_")[[1]][9])),"_",unlist(lapply(X=1:length(rel.rasters), function(x) str_split(rel.rasters[x],"_")[[1]][10])))
      }
      
     
      
      # terra can work in a stack: read in and get totals
      rs <- rast(rel.rasters)
      crs(rs) <- "epsg:27700"
      BNG.sector.tot <- as.numeric((global(rs, sum, na.rm=T)/1000)[,1])
      
            # resample/reproject entire stack. Set <0 to 0 and get totals
      ## !! ISSUE: terra::resample setting NA to -3.4e+38 !!##
      
      # divide down to m2 for transformation
      rs.m2 <- rs / 1000000
      
      r.resam <- terra::resample(rs.m2, uk.latlon.grid.wkt, method="bilinear")
      r.resam[r.resam < 0] <- 0
      
      # bring back up to actual area
      r.resam <- r.resam * latlon.in.m2
      
      LL.sector.tot <- as.numeric((global(r.resam, sum, na.rm=T)/1000)[,1])
      
      # scale the new LL rasters to BNG totals to conserve totals
      r.rescaled <- (r.resam / LL.sector.tot) * BNG.sector.tot
      
      LL.rescaled.tot <- as.numeric((global(r.rescaled, sum, na.rm=T)/1000)[,1])
      
      # write them out as a stack all at once
      writeRaster(r.rescaled, filename=paste0("./Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_",class,"/",species,"_diff_",year,"_",country,"_",class,"_",sec.codes,"_t_",reqres,"_LL_",latest.avail,"NAEImap.tif"), overwrite=T, wopt=list(gdal=c("COMPRESS=LZW", "TFW=NO")))
     
      
      # totals summary
      dtrow <- data.table(Species = rep(species,length(sec.codes)), Year = rep(year,length(sec.codes)), Sector = sec.codes, BNG.sector.tot=BNG.sector.tot, LL.warped.noneg.tot=LL.sector.tot, LL.rescaled.tot = LL.rescaled.tot)
      
      
      
      #LL.st <- stack()
      #totals.l <- list()
      #m2.st <- stack()
      
      #for(sector in rel.rasters){
      #  
      #  sec.code <- ifelse(class=="SNAP", str_split(sector,"_")[[1]][9], 
      #                     paste0(str_split(sector,"_")[[1]][9],"_",str_split(sector,"_")[[1]][10]))
      # 
      #  
      #  r <- raster(sector)
      #  crs(r) <- BNG
      #  BNG.sector.tot <- cellStats(r, sum)/1000
      #  
      #  # convert down to per metre square, as a 'rate'
      #  r.m2 <- r / 1000000
      #  names(r.m2) <- sec.code
      #  
      #  # and temp converted file needs writing for gdal
      #  #writeRaster(r.m2, paste0("./Emissions_grids_plain/BNG/",species,"/diffuse/",year,"/rasters_",class,"/tempBNG_as_m2_data.tif"), overwrite=T)
      #  writeRaster(r.m2, paste0("E:/samT/dump/tempBNG_as_m2_data.tif"), overwrite=T)
      #  
        #temp <- projectRaster(r.m2,uk.latlon.grid,"bilinear")
        #temp2 <- temp * latlon.in.m2
        #cellStats(temp2,sum)/1000
        # reproject and write in LL
        #suppressMessages(gdalwarp(paste0("./Emissions_grids_plain/BNG/",species,"/diffuse/",year,"/rasters_",class,"/tempBNG_as_m2_data.tif"), dstfile = paste0("./Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_",class,"/tempLL_needs_area_data.tif"), s_srs = BNG, t_srs = LL, r = "cubic", tr = c(reqres, reqres), te=c(-13.8, 49, 4.6, 61.5), output_Raster=TRUE, overwrite=TRUE,verbose=TRUE))
      #  suppressMessages(gdalwarp(paste0("E:/samT/dump/tempBNG_as_m2_data.tif"), dstfile = paste0("E:/samT/dump/tempLL_needs_area_data.tif"), s_srs = BNG, t_srs = LL, r = "cubic", tr = c(reqres, reqres), te=c(-13.8, 49, 4.6, 61.5), output_Raster=TRUE, overwrite=TRUE,verbose=TRUE))
        
        # read the warped raster back in and convert the m2 rates back to total emissions per cell, using the area raster
      #  LL.ems.cell <- raster(paste0("E:/samT/dump/tempLL_needs_area_data.tif")) * latlon.in.m2
      #  LL.warped.tot <- cellStats(LL.ems.cell, sum)/1000
      #  
      #  # write this out with proper name for posterity
      #  #### HOWEVER - the cubic warp produces some small negatives, replace with zero and write
      #  LL.ems.cell[LL.ems.cell < 0] <- NA
      #  writeRaster(LL.ems.cell, paste0("./Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_",class,"/",species,"_diff_",year,"_",country,"_",class,"_",sec.code,"_t_",reqres,"_LL_",latest.avail,"NAEImap.tif"), overwrite=T)
      #  
      #  LL.no.negs.tot <- cellStats(LL.ems.cell, sum)/1000
      #  names(LL.ems.cell) <- paste0(country,"_",species,"_",class,"_",sec.code,"_",year)
      #  
      #  dtrow <- data.table(Species = rep(species,3), Year = rep(year,3), Sector = sec.code, Stage = c("BNG_1km_total",paste0("LL_",reqres,"_total"),paste0("LL_",reqres,"_no.neg_total")), Emis_kt = c(BNG.sector.tot, LL.warped.tot, LL.no.negs.tot))
      #  
      #  totals.l[[paste0(species,"_",sec.code,"_",year)]] <- dtrow
      #  
      #  LL.st <- stack(LL.st, LL.ems.cell)
    #
  #}
  
  # make the stack into a data table again
  #LL.as.dt <- as.data.table(as.data.frame(LL.st, xy=T))
  LL.as.dt <- as.data.table(as.data.frame(r.rescaled, xy=T))
  names(LL.as.dt) <- c("x","y",sec.codes)
  
  # tableise the totals and summary
  #ll.tots.as.dt <- rbindlist(totals.l)
  
  #totals.tab <- data.table(Species = rep(species,3), Year = rep(year,3), Sector = rep("Diff_Total",3), Stage = c("BNG_1km_total",paste0("LL_",reqres,"_total"),paste0("LL_",reqres,"_no.neg_total")), Emis_kt = c(sum(ll.tots.as.dt[Stage == "BNG_1km_total",Emis_kt]), sum(ll.tots.as.dt[Stage == paste0("LL_",reqres,"_total"),Emis_kt]), sum(ll.tots.as.dt[Stage == paste0("LL_",reqres,"_no.neg_total"),Emis_kt])))
  
  #ll.tots.as.dt <- rbind(ll.tots.as.dt, totals.tab)
  
  master.return[[paste0(country,"_",class,"_LL_data")]] <- LL.as.dt
  master.return[[paste0(country,"_",class,"_LL_table")]] <- dtrow
  
    } # end of class
    
  } # end of country
  
  return(master.return)
  
} # end of diffuse emissions function

#####################################################################################

create.pmco.surfaces.BNG <- function(){
  
  #################################################################
  ####   An extra function to create PM coarse layers, once    ####
  ####          PM10 and PM2.5 have been processed             ####
  #################################################################
  
  ## PMCO is simply PM10 - PM2.5
  
  for(year in years){
    
    print(paste0(Sys.time(),": Generating PMco in ",year,"~~~~"))
    
    
    pm.stats <- list()
    pm.pt.stats <- list()
    
    for(class in c("SNAP","GNFR")){
      
      print(paste0(Sys.time(),":     DIFFUSE ",class," sectors..."))
      
      
      # create new pmco directory
      dir.create(file.path(paste0("//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/Emissions_grids_plain/BNG/pmco/diffuse/",year,"/rasters_", class)), showWarnings = FALSE, recursive = T)
      
      all.pm10.files <- list.files(paste0("//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/Emissions_grids_plain/BNG/pm10/diffuse/",year,"/rasters_", class), pattern=".tif$", full.names = T)
      
      all.pm25.files <- list.files(paste0("//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/Emissions_grids_plain/BNG/pm2.5/diffuse/",year,"/rasters_", class), pattern=".tif$", full.names = T)
      
      for(pm10file in all.pm10.files){
        
        if(class=="SNAP"){
          sec <- strsplit(pm10file,"_")[[1]][16]
          country <- strsplit(pm10file,"_")[[1]][14]
        }else{
          sec <- paste0(strsplit(pm10file,"_")[[1]][16],"_",strsplit(pm10file,"_")[[1]][17])
          country <- strsplit(pm10file,"_")[[1]][14]
        }
        
        r.pm10 <- raster(pm10file)
        r.pm10[is.na(r.pm10)] <- 0
        pm10.total <- cellStats(r.pm10, sum)/1000
        
        r.pm25 <- raster(all.pm25.files[grep(paste0("_diff_",year,"_",country,"_",class,"_",sec,"_t_1km_BNG_2018NAEImap"), all.pm25.files)])
        
        if(nlayers(r.pm25) != 1){
          stop(paste0("ERROR: too many pm2.5 layers read in!!"))
        }
        
        r.pm25[is.na(r.pm25)] <- 0
        pm25.total <- cellStats(r.pm25, sum)/1000
        
        r.pmco <- r.pm10 - r.pm25
        pmco.total <- cellStats(r.pmco, sum)/1000
        
        r.pmco.noneg <- r.pmco
        r.pmco.noneg[r.pmco.noneg<0] <- 0
        pmco.noneg.total <- cellStats(r.pmco.noneg, sum)/1000
        
        negs <- ifelse(cellStats(r.pmco,min)<0,"YES","NO")
        
        writeRaster(r.pmco.noneg, paste0("./Emissions_grids_plain/BNG/pmco/diffuse/",year,"/rasters_", class,"/pmco_diff_",year,"_",country,"_",class,"_",sec,"_t_1km_BNG_2018NAEImap.tif"), overwrite=T)
        
        
        dt <- data.table(AREA = country,Year = year, Sector = sec, pm10.tot = pm10.total, pm25.tot = pm25.total, pmco.tot = pmco.total,NEGS = negs, pmco.tot.NOnegs = pmco.noneg.total)
        
        pm.stats[[paste0("pmco_",sec,"_",country,"_",class,"_",year)]] <- dt
        
      } # file end
      
     
      
      dir.create(file.path(paste0("//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/Emissions_grids_plain/BNG/pmco/point/",year)), showWarnings = FALSE, recursive = T)
      
      
      for(country in c("uk","eire","ukeire")){
        print(paste0(Sys.time(),":     POINT ",country))
        
        # as Eire no longer uses point data, create blank table
        if(country == "eire"){
          pmco.pts <- data.table(Easting=integer(), Northing = integer(), pmco = as.numeric(), SNAP = as.integer(), AREA = as.character(), pow.flag = as.integer())
          write.csv(pmco.pts, paste0("./Emissions_grids_plain/BNG/pmco/point/",year,"/pmco_pt_",year,"_",country,"_",class,"_t_BNG.csv"), row.names = F)
          next
        }else{
            
          }
        
        
        pm10.pts <- fread(paste0("./Emissions_grids_plain/BNG/pm10/point/",year,"/pm10_pt_",year,"_",country,"_",class,"_t_BNG.csv"))
        setnames(pm10.pts,"Emission","PM10")
        pm25.pts <- fread(paste0("./Emissions_grids_plain/BNG/pm2.5/point/",year,"/pm2.5_pt_",year,"_",country,"_",class,"_t_BNG.csv"))
        setnames(pm25.pts,"Emission","PM25")
        
        allpm <- pm25.pts[pm10.pts, on = c("Easting","Northing")]
        allpm <- allpm[!is.na(PM10)]
        allpm <- allpm[!is.na(PM25)]
        
        allpm[,pmco := PM10 - PM25]
        
        allpm <- allpm[pmco > 0]
        
        cols.keep <- c("Easting","Northing","pmco",class,"AREA","pow.flag")
        
        pmco.pts <- allpm[,..cols.keep]
        setnames(pmco.pts,"pmco","Emission")
        
        write.csv(pmco.pts, paste0("./Emissions_grids_plain/BNG/pmco/point/",year,"/pmco_pt_",year,"_",country,"_",class,"_t_BNG.csv"), row.names = F)
        
        pt.summary.10 <- pm10.pts[,sum(PM10)/1000, by=.(AREA,get(class))]
        names(pt.summary.10) <- c("Area","Sector","emis.kt")
        pt.summary.10[,species := "PM10"]
        
        pt.summary.25 <- pm25.pts[,sum(PM25)/1000, by=.(AREA,get(class))]
        names(pt.summary.25) <- c("Area","Sector","emis.kt")
        pt.summary.25[,species := "PM25"]
        
        pt.summary.co <- pmco.pts[,sum(Emission)/1000, by=.(AREA,get(class))]
        names(pt.summary.co) <- c("Area","Sector","emis.kt")
        pt.summary.co[,species := "PMCO.noneg"]
        
        pt.summary.all <- rbind(pt.summary.10,pt.summary.25,pt.summary.co)
        
        
        pt.summary.all <- dcast(pt.summary.all, ...~species, value.var = "emis.kt")
        
        
        pm.pt.stats[[paste0("PMCO_pts_",country,"_",class,"_",year)]] <- pt.summary.all
        
      }
      
      
    } # class loop end
    
    stats.to.write <- rbindlist(pm.stats)
    write.csv(stats.to.write, paste0("./Emissions_grids_plain/BNG/pmco/diffuse/",year,"/pmco_diff_",year,"_ukeire_BNG_SUMMARY.csv"), row.names = F)  
    
    pt.stats <- rbindlist(pm.pt.stats)
    write.csv(pt.stats, paste0("./Emissions_grids_plain/BNG/pmco/point/",year,"/pmco_pt_",year,"_ALL_SNAPGNFR_kt_BNG_SUMMARY.csv"), row.names = F)  
    
    
  } # end of year
  
  print(paste0(Sys.time(),": COMPLETE."))
  
} # end of function



