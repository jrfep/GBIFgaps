
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
