library(targets)
#####################
#### Inspect the pipeline
#####################

tar_manifest(fields = all_of("command"))

tar_glimpse()

tar_visnetwork()

tar_outdated() # what is out of date?

# serial
system.time(tar_make())
# parallel
#system.time(tar_make_future(workers = 6L))

tar_meta(fields = warnings)


dir_data_raw <<- ("/gws/nopw/j04/ceh_generic/samtom/emissions_data/data_raw")
dir_data <<- ("/gws/nopw/j04/ceh_generic/samtom/emissions_data/data")

lookup_NFR <- "//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/lookups/NFR19_SNAP_lookup.csv"
lookup_SIC <- "//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/lookups/points_sectors_to_SNAP.csv"
lookup_CRF <- "//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/lookups/CRF_to_SNAP.csv"
lookup_PID <- "//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/lookups/NAEI_pollutants.csv"



