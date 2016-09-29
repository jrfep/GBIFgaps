## auxiliary function 'ah2sp' from:
##https://stat.ethz.ch/pipermail/r-sig-geo/2012-March/014409.html
ah2sp <- function(x, increment=360, rnd=10, proj4string=CRS(as.character(NA))){
  require(alphahull)
  require(maptools)
  if (class(x) != "ahull"){
    stop("x needs to be an ahull class object")
  }
  # Extract the edges from the ahull object as a dataframe
  xdf <- as.data.frame(x$arcs)
  # Remove all cases where the coordinates are all the same      
  xdf <- subset(xdf,xdf$r > 0)
  res <- NULL
  if (nrow(xdf) > 0){
    # Convert each arc to a line segment
    linesj <- list()
    prevx<-NULL
    prevy<-NULL
    j<-1
    for(i in 1:nrow(xdf)){
      rowi <- xdf[i,]
      v <- c(rowi$v.x, rowi$v.y)
      theta <- rowi$theta
      r <- rowi$r
      cc <- c(rowi$c1, rowi$c2)
      # Arcs need to be redefined as strings of points. Work out the number of points to allocate in this arc segment.
      ipoints <- 2 + round(increment * (rowi$theta / 2),0)
      # Calculate coordinates from arc() description for ipoints along the arc.
      angles <- anglesArc(v, theta)
      seqang <- seq(angles[1], angles[2], length = ipoints)
      x <- round(cc[1] + r * cos(seqang),rnd)
      y <- round(cc[2] + r * sin(seqang),rnd)
      # Check for line segments that should be joined up and combine their coordinates
      if (is.null(prevx)){
        prevx<-x
        prevy<-y
      } else if (x[1] == round(prevx[length(prevx)],rnd) && y[1] == round(prevy[length(prevy)],rnd)){
          if (i == nrow(xdf)){
          #We have got to the end of the dataset
            prevx<-append(prevx,x[2:ipoints])
            prevy<-append(prevy,y[2:ipoints])
            prevx[length(prevx)]<-prevx[1]
            prevy[length(prevy)]<-prevy[1]
            coordsj<-cbind(prevx,prevy)
            colnames(coordsj)<-NULL
            # Build as Line and then Lines class
            linej <- Line(coordsj)
            linesj[[j]] <- Lines(linej, ID = as.character(j))
          } else {
            prevx<-append(prevx,x[2:ipoints])
            prevy<-append(prevy,y[2:ipoints])
          }
        } else {
      # We have got to the end of a set of lines, and there are several such sets, so convert the whole of this one to a line segment and reset.
          prevx[length(prevx)]<-prevx[1]
          prevy[length(prevy)]<-prevy[1]
          coordsj<-cbind(prevx,prevy)
          colnames(coordsj)<-NULL
      # Build as Line and then Lines class
          linej <- Line(coordsj)
          linesj[[j]] <- Lines(linej, ID = as.character(j))
          j<-j+1
          prevx<-NULL
          prevy<-NULL
        }
    }
    # Promote to SpatialLines
    lspl <- SpatialLines(linesj)
    # Convert lines to polygons
    # Pull out Lines slot and check which lines have start and end points that are the same
    lns <- slot(lspl, "lines")
    polys <- sapply(lns, function(x) { 
      crds <- slot(slot(x, "Lines")[[1]], "coords")
      identical(crds[1, ], crds[nrow(crds), ])
    }) 
    # Select those that do and convert to SpatialPolygons
    polyssl <- lspl[polys]
    list_of_Lines <- slot(polyssl, "lines")
    sppolys <- SpatialPolygons(list(Polygons(lapply(list_of_Lines, function(x) { Polygon(slot(slot(x, "Lines")[[1]], "coords")) }), ID = "1")), proj4string=proj4string)
                                        # Create a set of ids in a dataframe, then promote to SpatialPolygonsDataFrame
    
    hid <- sapply(slot(sppolys, "polygons"), function(x) slot(x, "ID"))
    areas <- sapply(slot(sppolys, "polygons"), function(x) slot(x, "area"))
    df <- data.frame(hid,areas)
    names(df) <- c("HID","Area")
    rownames(df) <- df$HID
    res <- SpatialPolygonsDataFrame(sppolys, data=df)
    res <- res[which(res@data$Area > 0),]
  }  
  return(res)
}

## auxiliary function 'overlap.plot' modified from
## 'soil.texture' function in plotrix-package

overlap.plot <- function (soiltexture = NULL, main = "", at = seq(0.1, 0.9, by = 0.1), col.labels,
                          axis.labels = c(
                              expression(G - E),
                              expression(E - G),
                              expression(G *intersect(E))
                          ), 
    tick.labels = list(l = seq(10, 90, by = 10), r = seq(10, 
        90, by = 10), b = seq(10, 90, by = 10)), show.names = TRUE, 
    show.lines = TRUE, col.names = "gray", bg.names = par("bg"), 
    show.grid = FALSE, col.axis = "black", col.lines = "gray", 
    col.grid = "gray", lty.grid = 3, show.legend = FALSE, label.points = FALSE, 
    point.labels = NULL, col.symbols = "black", pch = par("pch"), 
    ...) 
{
    par(xpd = TRUE)
    plot(0.5, type = "n", axes = FALSE, xlim = c(0, 1), ylim = c(0, 
        1), main = NA, xlab = NA, ylab = NA)
    triax.plot(x = NULL, main = main, at = at, axis.labels = axis.labels, 
        tick.labels = tick.labels, col.axis = col.axis, show.grid = show.grid, 
        col.grid = col.grid, lty.grid = lty.grid)
    arrows(0.12, 0.41, 0.22, 0.57, length = 0.15)
    arrows(0.78, 0.57, 0.88, 0.41, length = 0.15)
    arrows(0.6, -0.1, 0.38, -0.1, length = 0.15)
    
    if (show.names) {
        xpos <- c(0.5, 0.25, ##0.25,
                  0.75,##0.75,
                  .5,.5,.5)
        ypos <- c(0.6, 0.06,##0.13,
                  0.06,##0.13,
                  .45,.41,.36) * sin(pi/3)
        snames <- c("mostly overlapping",
                    ##"range nested in GBIF hull",
                    expression(E %subset% G),
                    ##"GBIF data nested in range",
                    expression(G %subset% E),
                    ##"lack of data in GBIF",
                    "complementarity","between","sources"
                    )
        boxed.labels(xpos, ypos, snames, border = FALSE, xpad = 0.5,
                     col=col.labels)
    }
    par(xpd = FALSE)
    if (is.null(soiltexture)) 
        return(NULL)
    soilpoints <- triax.points(soiltexture, show.legend = show.legend, 
        label.points = label.points, point.labels = point.labels, 
        col.symbols = col.symbols, pch = pch, ...)
    invisible(soilpoints)
}

## Function overlap.Ranges to create a raster representaiton of the
## spatial overlap of expert's range and the alpha-hull around GBIF data
overlap.Ranges <- function(bounding.box,expert.range,GBIF.hull,GBIF.data) {
    r0 <- raster(bounding.box,nrow=1000,ncol=1000,crs=expert.range@proj4string)
    r1 <- rasterize(expert.range,r0)
    r2 <- rasterize(GBIF.hull,r0)
    r3 <- rasterize(GBIF.data,r0)
    values(r3)[cellFromXY(r0,GBIF.data)] <- 1
    b3 <- buffer(r3, width=10000)
    r3 <- b3 | r2
    return((!is.na(r3)) + (!is.na(r1))*2)
}
