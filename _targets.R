
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
  tar_target(lookup_SNAPGNFR,  SNAPtoGNFR()),
  tar_target(lookup_GNFRSNAP,  GNFRtoSNAP())
  
  
)