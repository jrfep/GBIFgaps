## First read shapefile of the selected species (range map)
## according to sedac/ciesin database
shp <- shapefile(sprintf("%s/%s",mi.lib,lst[k]))
if ("SCI_NAME" %in% colnames(shp@data)) {
    spp <- unique(shp@data$SCI_NAME)
} else {
    spp <- unique(shp@data$SCINAME)
}
## spatial overlay of range map with world map of countries 
qst <- over(shp,wrld,returnList=T)
qst <- unlist(lapply(qst,function(x) unlist(x$ISO2)))

## we select a list of countries that overlap with the range map
## we will use this to avoid some outliers in GBIF data that might be
## due to wrong georeferencing 
## however, keep in mind that this might be incorrect in some instances
slc <- unique(c(qst,wrld@data$ISO2[unique(unlist(wlim[wrld@data$ISO2 %in% qst]))]))

## use 'occ_search' function from package 'rgbif'
## query results are stored in a Rdata file, we will use this file
## if it is available, otherwise download the data
GBIF.rslt <- sprintf("%s/Gbif.%s.rda",gbif.dir,gsub(" ","_",spp))
if (!file.exists(GBIF.rslt)) {
    prb <- occ_search(scientificName=spp,limit=200000)
    save(file=GBIF.rslt,prb)
} else {
    load(GBIF.rslt)
}

## extract only spatial data from results
xys <- data.frame(x=prb$data$decimalLongitude,
                  y=prb$data$decimalLatitude)

## this is a filter for selecting records for the analysis
ss <- rowSums(is.na(xys))==0 & ## filter out incomplete coordinates
    prb$data$countryCode %in% slc & ## only countries selected above
        !duplicated(xys) & ## eliminate exact duplicates
            !(xys$x==0 & xys$y==0) ## filter out coordinates equal to zero

## if more than 500 unique spatial records, take a subset to speed up
## computation time, keep in mind possible underestimation of range
if (sum(ss)>500) { 
    s1 <- sample(seq(along=ss)[ss],500)
} else {
    s1 <- seq(along=ss)[ss]
}

## extract selected records into a new spatial object
xy1 <- xys[s1,]
coordinates(xy1) <- 1:2
proj4string(xy1) <- shp@proj4string

