
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
#### Load the functions to be used in targets
#####################

# Functions used by the targets.
source(here::here("R", "process_functions.R"))
# Parameters used by the targets.
source(here::here(".", "PARAMETERS.R"))

#####################
#### Define pipeline
#####################

list(
  
  #############
  ## Domains ##
  tar_target(r_dom_naei_BNG, setDomain(area = "UK", crs = "BNG") ), # set UK NAEI domain in BNG
  tar_target(r_dom_naei_LL, setDomain(area = "UK", crs = "BNG") ), # set UK NAEI domain in LL
  
  tar_target(r_dom_eire_BNG, setDomain(area = "UK", crs = "BNG") ), # set EIRE domain in BNG
  tar_target(r_dom_eire_LL, setDomain(area = "UK", crs = "BNG") ), # set EIRE domain in LL
  
  tar_target(r_dom_emep_LL, setDomain(area = "UK", crs = "BNG") ), # set EU-EMEP domain in LL
  
  ####################
  ## Look-up tables ##
  tar_target(lookup_NFR,  "//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/lookups/NFR19_SNAP_lookup.csv", format = "file"),
  tar_target(lookup_SIC,  "//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/lookups/points_sectors_to_SNAP.csv", format = "file"),
  tar_target(lookup_CRF,  "//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/lookups/CRF_to_SNAP.csv", format = "file"),
  tar_target(lookup_PID,  "//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/lookups/NAEI_pollutants.csv", format = "file"),
  tar_target(lookup_ISO,  "C:/FastProcessingSam/Git_repos/EMEP_inputs/data/lookups/EMEP_territories.csv", format = "file"),
  
  tar_target(dt_NFR,  fread(lookup_NFR)),
  tar_target(dt_SIC,  fread(lookup_SIC)),
  tar_target(dt_CRF,  fread(lookup_CRF)),
  tar_target(dt_PID,  fread(lookup_PID)),
  tar_target(dt_ISO,  fread(lookup_ISO)),
  
  tar_target(dt_SNAPGNFR,  SNAPtoGNFR()),
  tar_target(dt_GNFRSNAP,  GNFRtoSNAP()),
  
  #################################
  ## vectors for dynamic targets ##
  # create a target of aggregation types (required for dynamic targets later)
  # https://books.ropensci.org/targets/dynamic.html
  tar_target(v_poll_ceh, vectorPolls(dt_PID, class = "ceh")),
  tar_target(v_poll_naei, vectorPolls(dt_PID, class = "naei")),
  tar_target(v_poll_emep, vectorPolls(dt_PID, class = "emep")),
  tar_target(v_poll_eire, vectorPolls(dt_PID, class = "eire")),
  tar_target(v_aggregations, vectorSecs(v_sector_maps)),
  tar_target(vfname_aggregations, vectorSecs_fnames(v_sector_maps), format = "file"),
  
  ######################
  ## Downloading data ##
  # EMEP EU totals (.csv format)
  # the result is a single filename for every pollutant, on a branched dynamic target (per pollutant)
  tar_target(dy_fname_EMEPtotals, downloadEMEPtotals(species = v_poll_emep, end_year, invYear_EMEP, dt_PID, dt_ISO), pattern = map(v_poll_emep)),
  
  # EMEP EU gridded data (.txt format)
  # the result is a vector of filenames for every sector, on a branched dynamic target (per pollutant)
  tar_target(dy_vfname_EMEPmaps, downloadEMEPmaps(species = v_poll_emep, end_year, invYear_EMEP, dt_PID), pattern = map(v_poll_emep)),
  
  # NAEI UK totals (.csv format)
  # the result is a 
  tar_target(vfname_NAEItotals, downloadNAEItotals()), # AWAITING TO DECIDE WHAT TO DO - MAKE vector OF FILENAMES? yes same as Eire maps
    
  # NAEI UK points data (.xlsx format)
  # the result is a 
  tar_target(vfname_NAEIpoints, downloadNAEIpoints(ptsYear_UK, invYear_NAEI)),
  
  # NAEI UK gridded data (.asc format)
  # the result is a 
  tar_target(vfname_NAEImaps, downloadNAEImaps(species = v_poll_naei, end_year, invYear_NAEI, dt_PID), pattern = map(v_poll_naei)),
  
  # EIRE gridded data (.shp format)
  # the result is a 
  tar_target(vfname_EIREmaps, downloadEIREmaps(mapYear_IE)),
  
  ##############################
  ## Formatting download data ##
  # EMEP EU totals - dynamic target over pollutants from pollutants.csv
  # Into native sectors, plus aggregations via sector mapping vector (above)
  tar_target(dy_vfname_EMEPtotalsForm, formatEUtotals(fname = dy_fname_EMEPtotals, endYear, invYear_EMEP, dt_PID), pattern = map(dy_fname_EMEPtotals)),
  
  tar_target(, aggEUtotals(dy_vfname_EMEPtotalsForm, ), pattern = map(dy_vfname_EMEPtotalsForm) ),
  
  # EMEP EU gridded data
  formatEUmaps
  vfname_EMEPmaps
  
  
  # NAEI UK totals
  # Into native sectors, plus aggregations via sector mapping vector (above)
  
  # NAEI UK points data, including LL conversion
  
  # NAEI UK gridded data, including LL conversion
  
  # EIRE gridded data, including BNG conversion
  
  tar_target(fname_pointData_BNG, pointsUKformat(end_year, lookup_NFR, lookup_SIC, lookup_PID, dt_SNAPGNFR), format = "file"),
  tar_target(fname_pointData_LL,  transformUKpts(fname = fname_pointData_BNG), format = "file"),
  
  # UK: produce ONE table of emissions inventory totals at NFR19 (all pollutants/years)
  tar_target(fname_UKtotalsNFR,  totalsUKformatted(end_year, lookup_NFR, lookup_SIC, lookup_PID), format = "file"),
  # IE: produce ONE table of emissions inventory totals at GNFR (all pollutants/years)
  #tar_target(fname_IEtotalsGNFR,  totalsUKformat(end_year, lookup_NFR, lookup_SIC, lookup_PID), format = "file"),
  
  # UK: produce new tables of aggregated sector data for every nominated sector mapping in v_aggregations
  # this is fine, but i do not know what the target produced is? I assume it's as long as the v_aggregations?
  # should i return filenames instead?
  tar_target(UKtotalsAgg,  totalsUKagg(fname = fname_UKtotalsNFR, classification = v_aggregations, end_year, lookup_NFR), pattern = map(v_aggregations))
  
  
  # would it be better to run a pollutants vector through pattern = map()?
  # this way when adding a pollutant to pollutants.csv, it should only run for new ones?
  # definitely worth it for maps, maybe simpler to just bulk process points and totals
  
  ##########################
  ## Time Series Creation ##
  
  # more targets to create complete time series of maps?
  
  
  
)