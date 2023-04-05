
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

tar_option_set(packages = c("sf","terra","stringr","dplyr","ggplot2","data.table","stats","readxl"))

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
  
  tar_target(r_dom_emep_LL, setDomain(area = "UK", crs = "BNG") ) # set EU-EMEP domain in LL
  
)