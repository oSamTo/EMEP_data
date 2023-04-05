#library(targets)
list.of.packages <- c("sf","terra","stringr","dplyr","ggplot2","data.table","stats","readxl")
lapply(list.of.packages, require, character.only = TRUE)

source("./_targets.R")


#####################
#### Inspect the pipeline
#####################

tar_manifest(fields = all_of("command"))

tar_glimpse()

tar_visnetwork()

tar_outdated() # what is out of date?

tar_manifest(fields = "command") # list the targets
# visualise the network of targets / pipeline workflow
g <- tar_glimpse()
g$height <- 1000; g$width <- "100%"
g
v <- tar_visnetwork(targets_only = TRUE)
v$height <- 1000; v$width <- "100%"
v
tar_visnetwork()

# serial
system.time(tar_make())
# parallel
system.time(tar_make_future(workers = 6L))

tar_meta(fields = warnings)

require(terra)
v_files <- list.files("C:/FastProcessingSam/EMEP_new_grid/naei/nox/2020", pattern = ".tif$", full.names = T)
for(i in v_files){
  r <- rast(i)
  print(ext(r))
}
