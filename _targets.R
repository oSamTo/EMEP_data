
#####################
#### Set wd with _Targets file & here package
#####################

library(here) # construct file paths relative to project root
here::i_am("./_targets.R")

#####################
#### Load package dependencies 
#####################

library(targets)
library(tarchetypes) # for tar_knitr_deps_expr()
library(fs) # file system operations
list.of.packages <- c("sf","terra","stringr","dplyr","ggplot2","data.table","stats","readxl")
lapply(list.of.packages, require, character.only = TRUE)
#tar_option_set(packages = c("sf","terra","stringr","dplyr","ggplot2","data.table","stats","readxl"))

# the above loads these packages at a global level for all targets. You can also choose to load them separately 

#####################
#### Some generic options
#####################
#map_yr_uk
#map_yr_ie
#map_yr_eu
start_year <- 2020
end_year   <- 2020
v_sector_maps <- c("SNAP","GNFR")

#####################
#### Load the functions to be used in targets
#####################

# Functions used by the targets.
source(here::here("R", "process_functions.R"))


#####################
#### Define pipeline
#####################

list(
  
  # Domains
  tar_target(r_dom_naei_BNG, setDomain(area = "UK", crs = "BNG") ), # set UK NAEI domain in BNG
  tar_target(r_dom_naei_LL, setDomain(area = "UK", crs = "BNG") ), # set UK NAEI domain in LL
  
  tar_target(r_dom_eire_BNG, setDomain(area = "UK", crs = "BNG") ), # set EIRE domain in BNG
  tar_target(r_dom_eire_LL, setDomain(area = "UK", crs = "BNG") ), # set EIRE domain in LL
  
  tar_target(r_dom_emep_LL, setDomain(area = "UK", crs = "BNG") ), # set EU-EMEP domain in LL
  
  # Look-up tables
  tar_target(lookup_NFR,  "//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/lookups/NFR19_SNAP_lookup.csv", format = "file"),
  tar_target(lookup_SIC,  "//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/lookups/points_sectors_to_SNAP.csv", format = "file"),
  tar_target(lookup_CRF,  "//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/lookups/CRF_to_SNAP.csv", format = "file"),
  tar_target(lookup_PID,  "//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/lookups/NAEI_pollutants.csv", format = "file"),
  tar_target(dt_SNAPGNFR,  SNAPtoGNFR()),
  tar_target(dt_GNFRSNAP,  GNFRtoSNAP()),
  
  # create a target of aggregation types (required for dynamic targets later)
  # https://books.ropensci.org/targets/dynamic.html
  #tar_target(v_pollutants, vectorPolls(v_poll_select)),
  tar_target(v_aggregations, vectorSecs(v_sector_maps)),
  tar_target(vfname_aggregations, vectorSecs_fnames(v_sector_maps), format = "file"),
  
  # produce ONE table of point data in the UK
  tar_target(fname_pointData_BNG, pointsUKformat(end_year, lookup_NFR, lookup_SIC, lookup_PID, dt_SNAPGNFR), format = "file"),
  tar_target(fname_pointData_LL,  transformUKpts(fname = fname_pointData_BNG), format = "file"),
  
  # produce ONE table of emissions inventory totals at NFR19 (all pollutants/years)
  tar_target(fname_UKtotalsNFR,  totalsUKformat(end_year, lookup_NFR, lookup_SIC, lookup_PID), format = "file"),
  
  # produce new tables of aggregated sector data for every nominated sector mapping in v_aggregations
  tar_target(UKtotalsAgg,  totalsUKagg(fname = fname_UKtotalsNFR, classification = v_aggregations, end_year, lookup_NFR), pattern = map(v_aggregations))
  
  
)