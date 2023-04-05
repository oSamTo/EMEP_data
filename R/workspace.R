##############################################################################################################
list.of.packages <- c("sf","terra","stringr","dplyr","ggplot2","data.table","stats","readxl")
lapply(list.of.packages, require, character.only = TRUE)
##############################################################################################################

data_dir <<- ("//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS")

#########################################################################
#### SETTING UP WORKSPACE FOR PROCESSING UK AND IRISH EMISSIONS DATA ####
#########################################################################

# new extended domain to include both UK & Eire
r_naei_1km_BNG <<- rast(xmin = -50000, xmax = 800000, ymin = -50000, ymax = 1350000,
                        res = 1000, crs = "epsg:27700", vals = NA)
# This is the lat long equivalent raster of the UK domain at 1km in BNG
r_naei_0.01_LL <<- rast(xmin = -10.6, xmax = 5.7, ymin = 49.2, ymax = 62.1,
                        res = 0.01, crs = "epsg:4326", vals = NA)

r_eire_1km_BNG <<- rast(xmin = -230000, xmax = 300000, ymin = -50000, ymax = 800000,
                        res = 1000, crs = "epsg:27700", vals = NA)

r_eire_0.01_LL <<- rast(xmin = -12.5, xmax = -3.3, ymin = 49, ymax = 57.2,
                        res = 0.01, crs = "epsg:4326", vals = NA)


# sector and sector names
dt_SNAP <<- data.table(sector_num = 1:11,
                       SNAP = 1:11,
                       sector_name = c("energyprod", "domcom", "indcom", "indproc", "offshore",
                              "solvents", "roadtrans", "othertrans", "waste", "agric", "nature"))

dt_GNFR <<- data.table(sector_num = 1:16,
                       GNFR = c(LETTERS[1:12],LETTERS[14:16],"q"),
                       sector_name = c("A_PublicPower","B_Industry","C_OtherStatComb","D_Fugitive","E_Solvents","F_RoadTransport","G_Shipping","H_Aviation","I_Offroad","J_Waste","K_AgriLivestock","L_AgriOther","N_Natural","O_AviCruise","P_IntShipping","q_LULUCF"))

# generate rough SNAP to GNFR (and vice versa) lookup tables
dt_SNAP_to_GNFR <<- data.table(SNAP = c(1,3,4,2,5,6,7,NA,8,NA,9,10,NA,11,NA,NA,NA), 
                            GNFR = c("A_PublicPower","B_Industry","B_Industry","C_OtherStatComb","D_Fugitive","E_Solvents","F_RoadTransport","G_Shipping","H_Aviation","I_Offroad","J_Waste","K_AgriLivestock","L_AgriOther","N_Natural","O_AviCruise","P_IntShipping","q_LULUCF"))

dt_GNFR_to_SNAP <<- data.table(GNFR = c("A_PublicPower","B_Industry","C_OtherStatComb","D_Fugitive","E_Solvents","F_RoadTransport","G_Shipping","H_Aviation","I_Offroad","J_Waste","K_AgriLivestock","L_AgriOther","N_Natural","O_AviCruise","P_IntShipping","q_LULUCF"),
                            SNAP = c(1,3,2,5,6,7,8,8,8,9,10,10,11,NA,8,11))


# other look-up tables:
dt_CRF <<- fread(paste0(data_dir, "/lookups/CRF_to_GNFR.csv")) # NFR19 codes, with GNFR and SNAP

dt_NFR19 <<- fread(paste0(data_dir, "/lookups/NFR19_SNAP_lookup.csv")) # NFR19 codes, with GNFR and SNAP
dt_NFR19[NFR19 == "4.00E+01", NFR19 := "4E1"] # how to fix this?? options(scipen=999) not working
dt_NFR19[NFR19 == "4.00E+02", NFR19 := "4E2"] # how to fix this?? options(scipen=999) not working
dt_NFR19[NFR19 == "40", NFR19 := "4E1"] # how to fix this?? options(scipen=999) not working
dt_NFR19[NFR19 == "400", NFR19 := "4E2"] # how to fix this?? options(scipen=999) not working

dt_ptsec <<- fread(paste0(data_dir, "/lookups/points_sectors_to_SNAP.csv")) # NAEI point sector codes, with SNAP
dt_ptsec <<- dt_SNAP_to_GNFR[dt_ptsec, on = "SNAP"]

dt_poll_IDs <<- fread(paste0(data_dir, "/lookups/NAEI_pollutants.csv")) # NAEI and UKCEH pollutant names/IDs

