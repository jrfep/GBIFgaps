##R --vanilla
require(raster)
require(rgbif)
require(alphahull)
require(spdep)
source("R/functions.R")

## World borders from thematicmapping.org:
## see Readme for detailed sources and recommendations
wrld <- shapefile("data/TMWB/TM_WORLD_BORDERS-0.3.shp")
wlim <- poly2nb(wrld)

## example range maps from Socioeconomic Data and Applications Center (SEDAC). http://sedac.ciesin.columbia.edu/data/set/species-v1-americas-bird-presence
## see Readme for detailed sources and recommendations
lst <- dir("data/","shp$")

## We will summarize data for each species
summaries.spp <- data.frame()


for (k in 1:length(lst)) {
    shp <- shapefile(sprintf("data/%s",lst[k]))
    spp <- unique(shp@data$SCI_NAME)
    cat(sprintf("%03d :: %s \n\t:",k, spp))
    
    if (!spp %in% summaries.spp$spp) {
        
        qst <- over(shp,wrld,returnList=T)
        qst <- unlist(lapply(qst,function(x) unlist(x$ISO2)))

        slc <- unique(c(qst,wrld@data$ISO2[unique(unlist(wlim[wrld@data$ISO2 %in% qst]))]))
        cat(sprintf(": %s paises :",length(slc)))
        
        GBIF.rslt <- sprintf("Rdata/Gbif.%s.rda",gsub(" ","_",spp))
        if (!file.exists(GBIF.rslt)) {
            prb <- occ_search(scientificName=spp,limit=200000)
            save(file=GBIF.rslt,prb)
        } else {
            load(GBIF.rslt)
        }
        
        if (prb$meta$count<3) {
            cat(sprintf(": < 3 registros en GBIF, ¡siguiente...!\n"))
            
            
        } else {
            if (!"decimalLongitude" %in% colnames(prb$data)) {
                cat(sprintf(": registros en GBIF sin coordenadas, ¡siguiente...!\n"))
            } else {
                
                xys <- data.frame(x=prb$data$decimalLongitude,
                                  y=prb$data$decimalLatitude)
                ss <- rowSums(is.na(xys))==0 & prb$data$countryCode %in% slc & !duplicated(xys) & !(xys$x==0 & xys$y==0)
                cat(sprintf(": %s datos de GBIF, %s puntos utilizados \n\t:",
                            prb$meta$count,sum(ss)))
                if (sum(ss)<3) {
                    cat(sprintf(": no hay suficientes puntos \n"))
                    
                } else {
                    if (sum(ss)>500) {
                        s1 <- sample(seq(along=ss)[ss],500)
                    } else {
                        s1 <- seq(along=ss)[ss]
                    }
                    mi.alpha <- median(dist(xys[s1,]))
                    
                    cat(sprintf(": alpha = %0.2f :",mi.alpha))
                    
                    aH <- ahull(xys[s1,"x"],xys[s1,"y"],alpha=mi.alpha)
                    res <- ah2sp(aH)
                    if (is.null(res)) {
                        cat(sprintf(": null hull \n",mi.alpha))
                    } else {
                        cat(sprintf(": hull :",mi.alpha))
                        xys <- xys[ss,]
                        coordinates(xys) <- 1:2
                        proj4string(xys) <- shp@proj4string
                        e <- extent(min(sapply(list(extent(xys),extent(res),extent(shp)),
                                               function(x) x@xmin)),
                                    max(sapply(list(extent(xys),extent(res),extent(shp)),
                                               function(x) x@xmax)),
                                    min(sapply(list(extent(xys),extent(res),extent(shp)),
                                               function(x) x@ymin)),
                                    max(sapply(list(extent(xys),extent(res),extent(shp)),
                                               function(x) x@ymax)))
                        
                        plot(e,main=spp)
                        plot(shp,col=rgb(.8,.1,0,.5),add=T)
                        plot(res,add=T,col=rgb(.0,.1,.8,.5))
                        points(decimalLatitude~decimalLongitude,prb$data,col=rgb(.05,.8,.05,.5),pch=19,cex=1.6)
                        
                        cat(sprintf(": plot :",mi.alpha))
                        
                        r0 <- raster(e,nrow=1000,ncol=1000,crs=shp@proj4string)
                        r1 <- rasterize(shp,r0)
                        r2 <- rasterize(res,r0)
                        r3 <- rasterize(xys,r0)
                        values(r3)[cellFromXY(r0,xys)] <- 1
                        b3 <- buffer(r3, width=10000)
                        
                        r3 <- b3 | r2
                
                        overlap <- (!is.na(r3)) + (!is.na(r1))*2
                        plot(overlap)
                        
                        cat(sprintf(": overlap :",mi.alpha))
                        
                        tt <- aggregate(values(area(overlap)),
                                        list(values(overlap)),sum,na.rm=T)
                        cat(sprintf(": resumen :",mi.alpha))
                        
                        summaries.spp <-
                            rbind(summaries.spp,
                                  data.frame(spp,
                                             alpha=mi.alpha,
                                             G.=ifelse (any(tt$Group.1 %in% 1),subset(tt,Group.1 %in% 1)$x,0),
                                             .E=ifelse (any(tt$Group.1 %in% 2),subset(tt,Group.1 %in% 2)$x,0),
                                             GE=ifelse (any(tt$Group.1 %in% 3),subset(tt,Group.1 %in% 3)$x,0)))
                        
                        cat(sprintf(": ¡Listo!\n",k))
                    }
                }
            }
        }
    }
}


summaries.spp[,3:5]*100/rowSums(summaries.spp[,3:5])
soil.texture(summaries.spp[,3:5]*100/rowSums(summaries.spp[,3:5]))

overlap.plot(summaries.spp[,3:5]*100/rowSums(summaries.spp[,3:5]),
             col.symbols="slateblue4",col.labels="grey77")


for (k in 1:length(lst)) {
    shp <- shapefile(sprintf("data/%s",lst[k]))
    spp <- unique(shp@data$SCI_NAME)
    cat(sprintf("%03d :: %s \n\t:",k, spp))
    
    qst <- over(shp,wrld,returnList=T)
    qst <- unlist(lapply(qst,function(x) unlist(x$ISO2)))
    
    slc <- unique(c(qst,wrld@data$ISO2[unique(unlist(wlim[wrld@data$ISO2 %in% qst]))]))
    cat(sprintf(": %s paises :",length(slc)))
    
    GBIF.rslt <- sprintf("Rdata/Gbif.%s.rda",gsub(" ","_",spp))
    if (!file.exists(GBIF.rslt)) {
        prb <- occ_search(scientificName=spp,limit=200000)
        save(file=GBIF.rslt,prb)
    } else {
        load(GBIF.rslt)
    }
    
    if (prb$meta$count<3) {
        cat(sprintf(": < 3 registros en GBIF, ¡siguiente...!\n"))
        
        
    } else {
        if (!"decimalLongitude" %in% colnames(prb$data)) {
            cat(sprintf(": registros en GBIF sin coordenadas, ¡siguiente...!\n"))
        } else {
            
            xys <- data.frame(x=prb$data$decimalLongitude,
                              y=prb$data$decimalLatitude)
            ss <- rowSums(is.na(xys))==0 & prb$data$countryCode %in% slc & !duplicated(xys) & !(xys$x==0 & xys$y==0)
            cat(sprintf(": %s datos de GBIF, %s puntos utilizados \n\t:",
                        prb$meta$count,sum(ss)))
            if (sum(ss)<3) {
                cat(sprintf(": no hay suficientes puntos \n"))
                
            } else {
                if (sum(ss)>500) {
                    s1 <- sample(seq(along=ss)[ss],500)
                } else {
                    s1 <- seq(along=ss)[ss]
                }
                
                for (mi.alpha in seq(2,16)) {
                    cat(sprintf(": alpha = %0.2f :",mi.alpha))
                    
                    aH <- ahull(xys[s1,"x"],xys[s1,"y"],alpha=mi.alpha)
                    res <- ah2sp(aH)
                    if (is.null(res)) {
                        cat(sprintf(": null hull \n",mi.alpha))
                    } else {
                        cat(sprintf(": hull :",mi.alpha))
                        xy1 <- xys[ss,]
                        coordinates(xy1) <- 1:2
                        proj4string(xy1) <- shp@proj4string
                        e <- extent(min(sapply(list(extent(xy1),extent(res),extent(shp)),
                                               function(x) x@xmin)),
                                    max(sapply(list(extent(xy1),extent(res),extent(shp)),
                                               function(x) x@xmax)),
                                    min(sapply(list(extent(xy1),extent(res),extent(shp)),
                                               function(x) x@ymin)),
                                    max(sapply(list(extent(xy1),extent(res),extent(shp)),
                                               function(x) x@ymax)))
                        
                        plot(e,main=spp)
                        plot(shp,col=rgb(.8,.1,0,.5),add=T)
                        plot(res,add=T,col=rgb(.0,.1,.8,.5))
                        points(decimalLatitude~decimalLongitude,prb$data,col=rgb(.05,.8,.05,.5),pch=19,cex=1.6)
                        
                        cat(sprintf(": plot :",mi.alpha))
                        
                        r0 <- raster(e,nrow=1000,ncol=1000,crs=shp@proj4string)
                        r1 <- rasterize(shp,r0)
                        r2 <- rasterize(res,r0)
                        r3 <- rasterize(xys,r0)
                        values(r3)[cellFromXY(r0,xy1)] <- 1
                        b3 <- buffer(r3, width=10000)
                        
                        r3 <- b3 | r2
                
                        overlap <- (!is.na(r3)) + (!is.na(r1))*2
                        plot(overlap)
                        
                        cat(sprintf(": overlap :",mi.alpha))
                        
                        tt <- aggregate(values(area(overlap)),
                                        list(values(overlap)),sum,na.rm=T)
                        cat(sprintf(": resumen :",mi.alpha))
                        
                        summaries.spp <-
                            rbind(summaries.spp,
                                  data.frame(spp,
                                             alpha=mi.alpha,
                                             G.=ifelse (any(tt$Group.1 %in% 1),subset(tt,Group.1 %in% 1)$x,0),
                                             .E=ifelse (any(tt$Group.1 %in% 2),subset(tt,Group.1 %in% 2)$x,0),
                                             GE=ifelse (any(tt$Group.1 %in% 3),subset(tt,Group.1 %in% 3)$x,0)))
                        
                        cat(sprintf(": ¡Listo!\n",k))
                    }
                }
            }
        }
    }
}

overlap.plot(summaries.spp[,3:5]*100/rowSums(summaries.spp[,3:5]),
             col.labels="grey77",col.symbols=summaries.spp$spp,cex=summaries.spp$alpha/5)
triax.abline(r=.5,b=.5,l=.5,lty=2,col=2)



all.rslt <- sprintf("Rdata/GBIFgaps.rda")
save(file=GBIF.rslt,summaries.spp)

## eval GBIF against expert map
summaries.spp$GE/(summaries.spp$GE+summaries.spp$.E)
##possible contribution from GBIF to extend known range
summaries.spp$G./(summaries.spp$GE+summaries.spp$.E)


##to do:
##arreglar el extent para incluir todos los polígonos
##optimizing over a range of alpha values to achieve best correspondence
##rank species in terms of agreement
##compare agreement of different maps for the same species
## temporal change in completeness
## compare contributions from different sources
## how much of GBIF observations or GBIF-hull falls within a buffer of the known distribution?
## how well does a SDM fills the gap between GBIF obs and experts distribution

## check overlaping polygons in BirdLife, different meanings extinct, extant...?

summaries.spp$GE/apply(summaries.spp[,-1],1,sum)

wlim[wrld@data$ISO2 %in% prb$data$countryCode]
