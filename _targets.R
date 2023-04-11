
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
v_poll_select <- c("nox","nh3","sox")

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
  tar_target(lookup_PID,  "C:/FastProcessingSam/Git_repos/EMEP_inputs/data/lookups/pollutants.csv", format = "file"),
  tar_target(dt_SNAPGNFR,  SNAPtoGNFR()),
  tar_target(dt_GNFRSNAP,  GNFRtoSNAP()),
  
  # create a target of pollutant selection (required for dynamic targets later)
  # https://books.ropensci.org/targets/dynamic.html
  tar_target(v_pollutants, vectorPolls(v_poll_select)),
  
  # produce tables of point data in the UK
  # dont need dynamic target yet, just produce one large table
  tar_target(l_pointData, pointsUKformat(species = v_pollutants, start_year, end_year, lookup_NFR, lookup_SIC, lookup_PID, dt_SNAPGNFR), pattern = map(v_pollutants))
  
  
  
  
  
  
  
)