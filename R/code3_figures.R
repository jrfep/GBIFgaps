
##output maps and figures for presentation

png(file="img/FigE.png",width=500,height=500)
plot(e,sub=spp,font.sub=4,lty=0,xlab="Longitude",ylab="Latitude",
     main="Range Map according to SEDAC",font.main=2)
plot(wrld,add=T,border="seagreen4")
text(subset(wrld,NAME %in% c("Ecuador","Venezuela","Brazil","Colombia","Bolivia","Paraguay","Peru","French Guiana","Cuba")),"NAME")
plot(shp,col=rgb(.8,.1,0,.5),add=T)
dev.off()

png(file="img/FigGBIF.png",width=500,height=500)
plot(e,sub=spp,font.sub=4,lty=0,xlab="Longitude",ylab="Latitude",
     main="Filtered distribution records from GBIF",font.main=2)
plot(wrld,add=T,border="seagreen4")
text(subset(wrld,NAME %in% c("Ecuador","Venezuela","Brazil","Colombia","Bolivia","Paraguay","Peru","French Guiana","Cuba")),"NAME")
points(xys,pch=3,col="slateblue4",cex=0.7)
dev.off()

png(file="img/FigAsk.png",width=500,height=500)
plot(e,sub=spp,font.sub=4,lty=0,xlab="Longitude",ylab="Latitude",
     main="",font.main=2)
plot(wrld,add=T,border="seagreen4")
text(subset(wrld,NAME %in% c("Ecuador","Venezuela","Brazil","Colombia","Bolivia","Paraguay","Peru","French Guiana","Cuba")),"NAME")
plot(shp,col=rgb(.8,.1,0,.5),add=T)
points(xys,pch=3,col="slateblue4",cex=0.7)
dev.off()

png(file="img/FigAlpha.png",width=500,height=500)
plot(e,sub=spp,font.sub=4,lty=0,xlab="Longitude",ylab="Latitude",
     main=expression(alpha-plain(hull)),font.main=2)
plot(wrld,add=T,border="seagreen4")
text(subset(wrld,NAME %in% c("Ecuador","Venezuela","Brazil","Colombia","Bolivia","Paraguay","Peru","French Guiana","Cuba")),"NAME")
plot(res.12,add=T,col=rgb(.0,.1,.8,.15))
plot(res.06,add=T,col=rgb(.0,.1,.8,.15))
plot(res.02,add=T,col=rgb(.0,.1,.8,.15))
points(xy1,pch=3,col="slateblue4",cex=0.7)
text(-43,-10,expression(alpha==12.0),cex=1.3)
text(-65,-7,expression(alpha==6.0),cex=1.3)
text(-66,9,expression(alpha==2.0),cex=1.3)
##points(xy1)
##points(decimalLatitude~decimalLongitude,prb$data,col=rgb(.05,.8,.05,.5),pch=19,cex=1.6)
dev.off()


png(file="img/FigOverlap.png",width=500,height=500)
plot(e,sub=spp,font.sub=4,lty=0,xlab="Longitude",ylab="Latitude",
     main=expression(alpha-plain(hull) + plain(range-map)),font.main=2)
plot(wrld,add=T,border="seagreen4")
text(subset(wrld,NAME %in% c("Ecuador","Venezuela","Brazil","Colombia","Bolivia","Paraguay","Peru","French Guiana","Cuba")),"NAME")
plot(shp,col=rgb(.8,.1,0,.5),add=T)
plot(res.06,add=T,col=rgb(.0,.1,.8,.15))
text(-45,15,expression(alpha==6.0),cex=1.3)
dev.off()


png(file="img/FigVenn.png",
    pointsize=20,width=500,height=500)
par(mar=c(0,0,0,0))
plot(1,1,pch=NA,xlim=c(0.7,1.3),ylim=c(.8,1.2),axes=F,xlab="",ylab="")

symbols(c(.95,1.05),c(1,1),circles=c(.15,.15),inches=F,add=T,bg=rgb(c(.8,.0),c(.1,.1),c(0,.8),c(.5,.15)))
text(c(.85,1.15),c(.87,.87),c("E","G"),font=2,cex=2)
text(c(.85,1,1.15),c(1,1,1),c(expression(E-G),expression(E*intersect(G)),expression(G-E)))
dev.off()


png(file="img/FigVennEjemplos.png",
    pointsize=20,width=500,height=500)
layout(matrix(c(5,1,6,2,3,4),ncol=3,byrow=T))
par(mar=c(0,0,0,0))
plot(1,1,pch=NA,xlim=c(0.7,1.3),ylim=c(.8,1.2),axes=F,xlab="",ylab="")
symbols(c(.95,1.05),c(1,1),circles=c(.21,.23),inches=F,add=T,bg=rgb(c(.8,.0),c(.1,.1),c(0,.8),c(.5,.15)))

plot(1,1,pch=NA,xlim=c(0.7,1.3),ylim=c(.8,1.2),axes=F,xlab="",ylab="")
symbols(c(.95,1.05),c(1,1),circles=c(.08,.15),inches=F,add=T,bg=rgb(c(.8,.0),c(.1,.1),c(0,.8),c(.5,.15)))

plot(1,1,pch=NA,xlim=c(0.7,1.3),ylim=c(.8,1.2),axes=F,xlab="",ylab="")
symbols(c(.87,1.13),c(1,1),circles=c(.17,.17),inches=F,add=T,bg=rgb(c(.8,.0),c(.1,.1),c(0,.8),c(.5,.15)))

plot(1,1,pch=NA,xlim=c(0.7,1.3),ylim=c(.8,1.2),axes=F,xlab="",ylab="")
symbols(c(.95,1.05),c(1,1),circles=c(.15,.08),inches=F,add=T,bg=rgb(c(.8,.0),c(.1,.1),c(0,.8),c(.5,.15)))


dev.off()


png(file="img/FigVennTriag.png",
    pointsize=20,width=700,height=700)
par(mar=c(0,0,0,0))
overlap.plot(summaries.spp[1,3:5]*100/rowSums(summaries.spp[1,3:5]),
             col.symbols=NA,col.labels="grey77")

triax.abline(r=.5,b=.5,l=.5,lty=2,col=2)
symbols(c(.73,.77),c(.2,.2),circle=c(0.07,.04),inches=F,
bg=rgb(c(.8,.0),c(.1,.1),c(0,.8),c(.5,.15)),add=T)

symbols(c(.23,.27),c(.2,.2),circle=c(0.04,.07),inches=F,
bg=rgb(c(.8,.0),c(.1,.1),c(0,.8),c(.5,.15)),add=T)

symbols(c(.465,.535),c(.24,.24),circle=c(0.055,.055),inches=F,
bg=rgb(c(.8,.0),c(.1,.1),c(0,.8),c(.5,.15)),add=T)

symbols(c(.48,.52),c(.67,.67),circle=c(0.06,.06),inches=F,
bg=rgb(c(.8,.0),c(.1,.1),c(0,.8),c(.5,.15)),add=T)
##symbols(.25,.2,circle=.05,inches=F,col=2,add=T)
dev.off()




tst <- subset(summaries.spp,spp %in% "Amazona amazonica" & alpha %in% 6)

png(file="~/Imágenes/presentacionGBIF/FigVennAmaz.png",
    pointsize=20,width=700,height=700)
par(mar=c(0,0,0,0))
overlap.plot(tst[,3:5]*100/rowSums(tst[,3:5]),
             col.symbols="slateblue",col.labels="grey77",pch=19,cex=2)
triax.abline(r=.5,b=.5,l=.5,lty=2,col=2)
dev.off()

require(RColorBrewer)

tst <- subset(summaries.spp,alpha %in% 6)
tst$clr <- brewer.pal(8,"Set3")

png(file="~/Imágenes/presentacionGBIF/FigVennSeveral.png",
    pointsize=20,width=700,height=700)
par(mar=c(0,0,0,0))
overlap.plot(tst[,3:5]*100/rowSums(tst[,3:5]),
             col.symbols=tst$clr,col.labels="grey77",pch=19,cex=2)
triax.abline(r=.5,b=.5,l=.5,lty=2,col=2)
legend("topleft",pch=19,col=tst$clr,legend=tst$spp,text.font=3,bty="n",cex=.9)

dev.off()


tst <- subset(summaries.spp,spp %in% "Amazona amazonica")

png(file="~/Imágenes/presentacionGBIF/FigVennAlphas.png",
    pointsize=20,width=700,height=700)
par(mar=c(0,0,0,0))
overlap.plot(tst[,3:5]*100/rowSums(tst[,3:5]),
             col.symbols="slateblue",col.labels="grey77",
             pch=1,cex=log(tst$alpha))
triax.abline(r=.5,b=.5,l=.5,lty=2,col=2)
dev.off()

