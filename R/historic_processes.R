packs <- c("sp","raster","stringr","gdalUtils","rgeos","rgdal","grid","plyr","car","reshape2","ggplot2","ggrepel","data.table","stats","readr","ggplot2","sf","rasterVis","ncdf4","readxl")
lapply(packs, require, character.only = TRUE)


###########################################################
#### Time Series of point source emissions for EMEP4UK ####
###########################################################
##  Need as much point info for UK as poss, to go with   ##
##  scaled diffuse surfaces. Only SNAP1 are injected     ##
##  SPECIFIC TO EMEP4UK (i.e. no stack heights etc)


## Read in all data and work to standard format

setwd("//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS")

pts.sec.to.snap <- fread("./lookups/points_sectors_to_SNAP.csv")


dt_pts_2000_18_all <- setDT(read_excel("./point/raw_data/NAEIPointsSources_1990-2018_v2_full.xlsx", sheet = "Data"))
dt_pts_2000_18_all[,c("PollCode","PlantID","Operator","SectorName","LocusID","Region","Unit","Datatype",as.character(1990:1999)) := NULL]
setnames(dt_pts_2000_18_all, c("Easting","Northing","Pollutant Name"), c("x","y","Pollutant"))
dt_pts_2000_18_all[Pollutant == "VOC", Pollutant := "NMVOC"]


for(y in years){
  
  for(species in pollutants){
    
    # subset the data
    
    cols.keep <- c("Pollutant","Site","SectorID","x","y",as.character(y))
    
    dt_pts_y_poll <- dt_pts_2000_18_all[Pollutant=="NOx", ..cols.keep  ]
    
    setnames(dt_pts_y_poll, as.character(y), "Emis_t")
    
    dt_pts_y_poll <- dt_pts_y_poll[!is.na(Emis_t)]
    
    # remove points with 0,0 location
    dt_pts_y_poll <- dt_pts_y_poll[!is.na(x)]
    dt_pts_y_poll <- dt_pts_y_poll[!is.na(y)]
    
    dt_pts_y_poll_collapse <- dt_pts_y_poll[, lapply(.SD, sum, na.rm=TRUE), by=.(Pollutant,Site,SectorID,x,y), .SDcols="Emis_t"]
    
    pts.sec.to.snap.sub <- pts.sec.to.snap[Pollutant == species]
    pts.sec.to.snap.sub[,c("Pollutant","Sector","Group","NFR") := NULL]
    
    NAEI.SNAP <- pts.sec.to.snap.sub[dt_pts_y_poll_collapse, on = "SectorID"]
    
    
  }
  
}

## no irish points




dt_pts_2000_18_nox <- dt_pts_2000_18_all[Pollutant=="NOx"]

dt_pts_2000_18_collapse <- dt_pts_2000_18_nox[, lapply(.SD, sum, na.rm=TRUE), by=.(Pollutant,Site), .SDcols=c(as.character(2000:2018))]
dt_pts_2000_18_collapse <- melt(dt_pts_2000_18_collapse, id.vars = c("Pollutant","Site"), variable.name = "Year", value.name = "Emission")

dt_pts_2000_18_collapse <- dt_pts_2000_18_collapse[order(-Emission), .SD, by=Year]

dt_pts_2000_18_collapse[,cum.tonne := cumsum(Emission), by=Year]
dt_pts_2000_18_collapse[,cum.pc := cum.tonne/sum(Emission, na.rm=T), by=Year]

dt_pts_2000_18_collapse[,Rank := 1:.N, by=Year]

ggplot(data=dt_pts_2000_18_collapse,aes(x=Year,y=Emission,group=Site))+
  geom_point(size=1)+
  geom_line()+
  #geom_text_repel(data=all[Rank<21],aes(label = Site),direction = "x", hjust=1, nudge_x = 1, size=2, colour="black" , segment.color = "grey70")+
  labs(x="Year",y="Emission (t)")





