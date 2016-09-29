##R --vanilla
## Script ''GBIFgaps''
##    for assessing data and knowledge gaps in GBIF distribution data
if (require(raster) &
    require(rgbif) &
    require(alphahull) &
    require(spdep) &
    require(RColorBrewer)) {
    cat("All packages loaded\n")
} else {
    stop("Some package(s) missing! you might need to install some dependencies too.")
}

## run this script to load some helper functions
source("R/functions.R")

## World borders from thematicmapping.org:
## see Readme for detailed sources and recommendations
wrld <- shapefile("data/TMWB/TM_WORLD_BORDERS-0.3.shp")
wlim <- poly2nb(wrld)

## example range maps from Socioeconomic Data and Applications Center (SEDAC). http://sedac.ciesin.columbia.edu/data/set/species-v1-americas-bird-presence
## see Readme for detailed sources and recommendations
lst <- dir("data/","shp$")

##script to run example code for Amazona amazonica 
source("R/code1_example_Amazona_amazonica.R")

##script to run example code for eight Amazona spp 
source("R/code2_example_eight_Amazona_spp.R")


