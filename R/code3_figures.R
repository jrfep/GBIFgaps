
system(sprintf("mkdir -p img/%s/",gsub(" ","_",spp)))
tst <- summaries.spp[summaries.spp$spp %in% spp,,drop=F]

png(file=sprintf("img/%s/Fig_triag_compare_alphas.png",gsub(" ","_",spp)),
    pointsize=20,width=700,height=700)
par(mar=c(0,0,0,0))
overlap.plot(tst[,3:5]*100/rowSums(tst[,3:5]),
             col.symbols="slateblue",col.labels="grey77",
             pch=1,cex=log(tst$alpha))
triax.abline(r=.5,b=.5,l=.5,lty=2,col=2)
dev.off()

##output maps and figures for presentation

png(file=sprintf("img/%s/Fig_RangeMap.png",gsub(" ","_",spp)),
    width=500,height=500)
plot(e,sub=spp,font.sub=4,lty=0,xlab="Longitude",ylab="Latitude",
     main="Range Map according to SEDAC",font.main=2)
plot(wrld,add=T,border="seagreen4")
text(subset(wrld,NAME %in% c("Ecuador","Venezuela","Brazil","Colombia","Bolivia","Paraguay","Peru","French Guiana","Cuba")),"NAME")
plot(shp,col=rgb(.8,.1,0,.5),add=T)
dev.off()

png(file=sprintf("img/%s/Fig_GBIFdata.png",gsub(" ","_",spp)),
    width=500,height=500)
plot(e,sub=spp,font.sub=4,lty=0,xlab="Longitude",ylab="Latitude",
     main="Filtered distribution records from GBIF",font.main=2)
plot(wrld,add=T,border="seagreen4")
text(subset(wrld,NAME %in% c("Ecuador","Venezuela","Brazil","Colombia","Bolivia","Paraguay","Peru","French Guiana","Cuba")),"NAME")
points(xys,pch=3,col="slateblue4",cex=0.7)
dev.off()

png(file=sprintf("img/%s/Fig_bothSources.png",gsub(" ","_",spp)),
    width=500,height=500)
plot(e,sub=spp,font.sub=4,lty=0,xlab="Longitude",ylab="Latitude",
     main="",font.main=2)
plot(wrld,add=T,border="seagreen4")
text(subset(wrld,NAME %in% c("Ecuador","Venezuela","Brazil","Colombia","Bolivia","Paraguay","Peru","French Guiana","Cuba")),"NAME")
plot(shp,col=rgb(.8,.1,0,.5),add=T)
points(xys,pch=3,col="slateblue4",cex=0.7)
dev.off()

png(file=sprintf("img/%s/Fig_AlphaHull_%s.png",gsub(" ","_",spp),mi.alpha),
    width=500,height=500)
plot(e,sub=spp,font.sub=4,lty=0,xlab="Longitude",ylab="Latitude",
     main=expression(alpha-plain(hull)),font.main=2)
plot(wrld,add=T,border="seagreen4")
text(subset(wrld,NAME %in% c("Ecuador","Venezuela","Brazil","Colombia","Bolivia","Paraguay","Peru","French Guiana","Cuba")),"NAME")
plot(res,add=T,col=rgb(.0,.1,.8,.15))
##plot(res.06,add=T,col=rgb(.0,.1,.8,.15))
##plot(res.02,add=T,col=rgb(.0,.1,.8,.15))
points(xy1,pch=3,col="slateblue4",cex=0.7)
##text(-43,-10,expression(alpha==12.0),cex=1.3)
##text(-65,-7,expression(alpha==6.0),cex=1.3)
##text(-66,9,expression(alpha==2.0),cex=1.3)
##points(xy1)
##points(decimalLatitude~decimalLongitude,prb$data,col=rgb(.05,.8,.05,.5),pch=19,cex=1.6)
dev.off()


png(file=sprintf("img/%s/Fig_Overlap_%s.png",gsub(" ","_",spp),mi.alpha),
    width=500,height=500)
plot(e,sub=spp,font.sub=4,lty=0,xlab="Longitude",ylab="Latitude",
     main=expression(alpha-plain(hull) + plain(range-map)),font.main=2)
plot(wrld,add=T,border="seagreen4")
text(subset(wrld,NAME %in% c("Ecuador","Venezuela","Brazil","Colombia","Bolivia","Paraguay","Peru","French Guiana","Cuba")),"NAME")
plot(shp,col=rgb(.8,.1,0,.5),add=T)
plot(res,add=T,col=rgb(.0,.1,.8,.15))
text(-45,15,expression(sprintf("alpha==%s",mi.alpha)),cex=1.3)
dev.off()



tst <- summaries.spp[summaries.spp$spp %in% spp &
                         summaries.spp$alpha %in% mi.alpha,,drop=F]

png(file=sprintf("img/%s/Fig_Triplot_%s.png",gsub(" ","_",spp),mi.alpha),
    pointsize=20,width=700,height=700)
par(mar=c(0,0,0,0))
overlap.plot(tst[,3:5]*100/rowSums(tst[,3:5]),
             col.symbols="slateblue",col.labels="grey77",pch=19,cex=2)
triax.abline(r=.5,b=.5,l=.5,lty=2,col=2)
dev.off()



