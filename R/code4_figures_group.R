tst <- subset(summaries.spp,alpha %in% 6)
tst$clr <- brewer.pal(8,"Set3")

png(file="img/FigVennSeveral.png",
    pointsize=20,width=700,height=700)
par(mar=c(0,0,0,0))
overlap.plot(tst[,3:5]*100/rowSums(tst[,3:5]),
             col.symbols=tst$clr,col.labels="grey77",pch=19,cex=2)
triax.abline(r=.5,b=.5,l=.5,lty=2,col=2)
legend("topleft",pch=19,col=tst$clr,legend=tst$spp,text.font=3,bty="n",cex=.9)

dev.off()
