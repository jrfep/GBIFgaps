##R --vanilla
## Script ''GBIFgaps''
##    for assessing data and knowledge gaps in GBIF distribution data
if (require(raster) &
    require(rgbif) &
    require(alphahull) &
    require(spdep) &
    require(plotrix) &
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

## prepare some R objects
##nmbrs <- c("outside.range","G.notE","E.notG","EG.intersect")
nmbrs <- c("outside.range","KNOWLEDGE.GAP","DATA.GAP","OVERLAP")
summaries.spp <- data.frame()

##script to run example code for one species with given alpha value
k <- 1
source("R/code1_single_species.R")
## We test alpha = 6 (in coordinate system units) 
mi.alpha <- 6
source("R/code2_single_alpha.R")

summaries.spp <-
    rbind(summaries.spp,
          data.frame(spp,
                     alpha=mi.alpha,
                     G.=ifelse (any(tt$sets %in% 1),
                         subset(tt,sets %in% 1)$area.km2,0),
                         .E=ifelse (any(tt$sets %in% 2),
                             subset(tt,sets %in% 2)$area.km2,0),
                     GE=ifelse (any(tt$sets %in% 3),
                         subset(tt,sets %in% 3)$area.km2,0)))



##script to run example code for eight Amazona spp
## and different values of alpha
for (k in 1:length(lst)) {
    source("R/code1_single_species.R")
    for (mi.alpha in seq(2,16)) {
        if (!any(summaries.spp$spp %in% spp &
                     summaries.spp$alpha %in% mi.alpha)) {
            source("R/code2_single_alpha.R")
            summaries.spp <-
                rbind(summaries.spp,
                      data.frame(spp,
                                 alpha=mi.alpha,
                                 G.=ifelse (any(tt$sets %in% 1),
                                     subset(tt,sets %in% 1)$area.km2,0),
                                 .E=ifelse (any(tt$sets %in% 2),
                                     subset(tt,sets %in% 2)$area.km2,0),
                                 GE=ifelse (any(tt$sets %in% 3),
                                     subset(tt,sets %in% 3)$area.km2,0)))
            
        }
    }
}



## script to create some images for the presentation
##   at https://youtu.be/sCSlzzgab8Q
##source("R/code3_figures.R")


