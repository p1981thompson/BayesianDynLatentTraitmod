
scat.hist.plot=function(x=model2.dunson$mean$omega,y=model.dunson$mean$theta1,ind=snames)
{

x1 <- x[control==0]
y1 <-  y[control==0]

x2 <- x[control==1]
y2 <-  y[control==1]

xhist <- hist(x, plot=F)
yhist <- hist(y, plot=F)

counttopx<-max(xhist$count*1.1)
counttopy<-max(yhist$count*1.1)

xtop <- max(xhist$breaks)
xbot<-  min(xhist$breaks)

ytop <- max(yhist$breaks)
ybot<-  min(yhist$breaks)

xhist<-hist(x, breaks=seq(xbot,xtop,0.1), plot=F)
yhist<-hist(y, breaks=seq(ybot,ytop,0.1), plot=F)

xrange <- c(xbot,xtop)
yrange <- c(ybot,ytop)
nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE)
#layout.show(nf)

par(mar=c(4.5,4.5,1,1))
plot(x, y, xlim=xrange, ylim=yrange, xlab=expression(omega), ylab=expression(theta),type="n")
points(x2,y2)
points(x1,y1,pch=4)
leg.txt<-c("control","PD")
legend("topleft",leg.txt, pch=c(1,4))
#abline(v=c(0,-2),col="blue")
#abline(h=c(0,-2),col="blue")
par(mar=c(0,4.5,1,1))
#barplot(xhist$counts, axes=T, ylim=c(0, top), space=0)
hist(x, breaks=seq(xbot,xtop,0.1), plot=T,axes=F,ylim=c(0,counttopx),main="",ylab="",col="grey")

lines(density(x1)$x,density(x1)$y*length(x1)*0.1,lwd=2,lty=2)
lines(density(x2)$x,density(x2)$y*length(x2)*0.1,lwd=2)

par(mar=c(4.5,0,1,1))

b1<-barplot(yhist$counts, axes=F, xlim=c(0, counttopy), space=0, horiz=TRUE)

lines(density(y1,from=ybot,to=ytop)$y*length(y1)*0.1,seq(0,length(b1),length.out=length(density(y1)$x)),lwd=2,lty=2)
lines(density(y2,from=ybot,to=ytop)$y*length(y2)*0.1,seq(0,length(b1),length.out=length(density(y2)$x)),lwd=2)
}

windows(record=T)
for(i in 2:3)
{
scat.hist.plot(x=model.dunson$mean$omega,y=model.dunson$mean[[i]],ind=i)
}

for(i in 2:3)
{
scat.hist.plot(x=model.dw2$mean$omega,y=model.dw2$mean[[i]],ind=i)
}
for(i in 2:3)
{
scat.hist.plot(x=model.cat$mean$omega,y=model.cat$mean[[i]],ind=i)
}
for(i in 2:3)
{
scat.hist.plot(x=model.beta$mean$omega,y=model.beta$mean[[i]],ind=i)
}
for(i in 2:3)
{
scat.hist.plot(x=model.beta.pine$mean$omega,y=model.beta.pine$mean[[i]],ind=i)
}

# Full simulated pine data #

windows(record=T)
png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Simulated/scatter%d.png",height=5,width=5,units="in",res=300)
for(i in 2:6)
{
scat.hist.plot(x=model.beta.pine3$mean$omega,y=model.beta.pine3$mean[[i]],ind=i)
}
dev.off()

# Full actual pine data #

png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/scatter%d.png",height=5,width=5,units="in",res=300)
for(i in 49:53)
{
scat.hist.plot(x=pine.par$omega,y=pine.par[[i]],ind=i)
}
dev.off()

windows(record=T)

for(i in 2:5)
{
scat.hist.plot(x=model.beta.pine.mood$mean$omega,y=model.beta.pine.mood$mean[[i]],ind=i)
}


scat.hist.plot(x=post.mean.bug2[474:973],y=post.mean.bug2[976:1475],ind=1)
scat.hist.plot(x=post.mean.bug2[474:973],y=post.mean.bug2[1476:1975],ind=2)

png(file="d:/Local Data/p1thompson/CHES/NIHR GRANT/Programs and software documentation/Tinn-R program scripts/PINE/model 9trt dw/real plot/scatter%d.png",height=5,width=5,units="in",res=300)
#windows(record=T)
for(i in 1:9)
{
scat.hist.plot(x=real.dat$mean.omega[1:483],y=theta.mat[,i],ind=i)
}
dev.off()

png(file="d:/Local Data/p1thompson/CHES/NIHR GRANT/Programs and software documentation/Tinn-R program scripts/PINE/model 9trt dw/sim plot/scatter%d.png",height=5,width=5,units="in",res=300)
#windows(record=T)
for(i in 1:9)
{
scat.hist.plot(x=sim.dat$mean.omega[1:500],y=theta.mat4[,i],ind=i)
}
dev.off()

windows(record=T)
for(i in 1:9)
{
scat.hist.plot(x=sim.dat$mean.omega[1:500],y=theta.mat3[,i],ind=i)
}

windows(record=T)
for(i in 1:2)
{
scat.hist.plot(x=sim.model.pine.2trt$mean$omega,y=sim.model.pine.2trt$mean[[i]],control=simmix.2trt$control,ind=i)
}

windows(record=T)
for(i in 1:2)
{
scat.hist.plot(x=sim.model.pine.2atrt$mean$omega,y=sim.model.pine.2atrt$mean[[i]],control=simmix.2atrt$control,ind=i)
}
scat.hist.plot(x=sim.model.pine.2atrt$mean[[2]],y=sim.model.pine.2atrt$mean[[1]],control=simmix.2atrt$control,ind=1)

scat.hist.plot(x=sim.dat[111:610,2],y=sim.dat[614:1113,2],control=control1,ind=1)
scat.hist.plot(x=sim.dat[111:610,2],y=sim.dat[1114:1613,2],control=control1,ind=2)
scat.hist.plot(x=sim.dat[111:610,2],y=sim.dat[1614:2113,2],control=control1,ind=3)


windows(record=T)
for(i in 1:3)
{
scat.hist.plot(x=model.pine.3atrt$mean$omega,y=model.pine.3atrt$mean[[i]],control=control,ind=i)
}


png(file="d:/Local Data/p1thompson/CHES/NIHR GRANT/Programs and software documentation/Tinn-R program scripts/PINE/model 9trt dw/real plot mixed subtrait no beta/scatter%d.png",height=5,width=5,units="in",res=300)
#windows(record=T)
for(i in 1:9)
{
scat.hist.plot(x=pine.par$omega,y=theta.mat[,i],ind=i)
}
dev.off()

png(file="d:/Local Data/p1thompson/CHES/NIHR GRANT/Programs and software documentation/Tinn-R program scripts/PINE/model 9trt dw/real plot all mixed subtrait no beta/scatter%d.png",height=5,width=5,units="in",res=300)
#windows(record=T)
for(i in 1:9)
{
scat.hist.plot(x=omega,y=theta.mat[,i],ind=i)
}
dev.off()

png(file="d:/Local Data/p1thompson/CHES/NIHR GRANT/Programs and software documentation/Tinn-R program scripts/PINE/model 9trt dw/real plot all mixed subtrait no beta 7trt/scatter%d.png",height=5,width=5,units="in",res=300)
#windows(record=T)
for(i in 1:7)
{
scat.hist.plot(x=pine.par$omega,y=theta.mat[,i],ind=i)
}
dev.off()