## we use function 'ahull' from package 'alphahull'
## alpha-hull will depend on parameter alpha, given here by mi.alpha

aH <- ahull(xys[s1,"x"],xys[s1,"y"],alpha=mi.alpha)

## convert to spatial object with helper functions

res <- ah2sp(aH)

## calculate spatial bounding box for points, ahull and original range map
e <- extent(min(sapply(list(extent(xy1),extent(res),extent(shp)),
                       function(x) x@xmin)),
            max(sapply(list(extent(xy1),extent(res),extent(shp)),
                       function(x) x@xmax)),
            min(sapply(list(extent(xy1),extent(res),extent(shp)),
                       function(x) x@ymin)),
            max(sapply(list(extent(xy1),extent(res),extent(shp)),
                       function(x) x@ymax)))

##apply the helper function 'overlap.Ranges'
overlap <- overlap.Ranges(bounding.box=e,
                          expert.range=shp,
                          GBIF.hull=res,
                          GBIF.data=xy1)
tt <- aggregate(data.frame(area.km2=values(area(overlap))),
                list(sets=values(overlap)),sum,na.rm=T)
tt$names <- nmbrs[tt$sets+1]
