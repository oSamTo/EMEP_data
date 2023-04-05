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