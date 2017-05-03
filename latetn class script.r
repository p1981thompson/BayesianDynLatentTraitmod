############################################
#                                          #
#     Script to load data and run Winbugs  #
#                                          #
############################################

# 17-04-2012 #

## load R2WinBUGS library to link R to WinBUGS ##
library(R2WinBUGS)
library(coda)
## write R data into winbugs format ##

#load simulated data into R
#
resp1a<-as.matrix(sim.cog.dyn$resp1a)
resp1b<-as.matrix(sim.cog.dyn$resp1b)
resp1c<-as.matrix(sim.cog.dyn$resp1c)
resp1d<-as.matrix(sim.cog.dyn$resp1d)
resp1f<-as.matrix(sim.cog.dyn$resp1f)
resp1g<-as.matrix(sim.cog.dyn$resp1g)
resp1h<-as.matrix(sim.cog.dyn$resp1h)
#
resp2a<-as.matrix(sim.cog.dyn$resp2a)
resp2b<-as.matrix(sim.cog.dyn$resp2b)
resp2c<-as.matrix(sim.cog.dyn$resp2c)
resp2d<-as.matrix(sim.cog.dyn$resp2d)
resp2f<-as.matrix(sim.cog.dyn$resp2f)
resp2g<-as.matrix(sim.cog.dyn$resp2g)
resp2h<-as.matrix(sim.cog.dyn$resp2h)
#
resp3a<-as.matrix(sim.cog.dyn$resp3a)
resp3b<-as.matrix(sim.cog.dyn$resp3b)
resp3c<-as.matrix(sim.cog.dyn$resp3c)
resp3d<-as.matrix(sim.cog.dyn$resp3d)
resp3f<-as.matrix(sim.cog.dyn$resp3f)
resp3g<-as.matrix(sim.cog.dyn$resp3g)
resp3h<-as.matrix(sim.cog.dyn$resp3h)
#
resp4a<-as.matrix(sim.cog.dyn$resp4a)
resp4b<-as.matrix(sim.cog.dyn$resp4b)
resp4c<-as.matrix(sim.cog.dyn$resp4c)
resp4d<-as.matrix(sim.cog.dyn$resp4d)
resp4f<-as.matrix(sim.cog.dyn$resp4f)
resp4g<-as.matrix(sim.cog.dyn$resp4g)
resp4h<-as.matrix(sim.cog.dyn$resp4h)
#
resp5a<-as.matrix(sim.cog.dyn$resp5a)
resp5b<-as.matrix(sim.cog.dyn$resp5b)
resp5c<-as.matrix(sim.cog.dyn$resp5c)
resp5d<-as.matrix(sim.cog.dyn$resp5d)
resp5f<-as.matrix(sim.cog.dyn$resp5f)
resp5g<-as.matrix(sim.cog.dyn$resp5g)
resp5h<-as.matrix(sim.cog.dyn$resp5h)
#
I = length(sim.cog.dyn$resp1a[,1])
prior = sim.cog.dyn$prior
Ncut=3
#
bugs.data(list("Ncut","I","resp1a","resp1b","resp1c","resp1d","resp1f","resp2a","resp2b","resp2c","resp2d","resp2f","resp2g","resp2h","resp3a","resp3b","resp3c","resp3d","resp3f","resp3g","resp3h","resp4a","resp4b","resp4c","resp4d","resp4f","resp4g","resp4h","resp5a","resp5b","resp5c","resp5d","resp5f","resp5g","resp5h"),dir="A:/latent class model/", data.file = "dynclasscogsim.txt",digits=3)
#
comb.sim.inits <- list(
c0 = rep(1,10),
c1 = rep(1,10),
c0a = rep(1,12),
c1a = rep(1,12),
c0b = rep(1,6),
c1b = rep(0.5,6),
c0c = rep(1,2),
c1c = rep(0.5,2),
c0e = rep(1,6),
c1e = rep(1,6),
c0f = rep(1,30),
c1f = rep(1,30),
c0g = as.matrix(t(matrix(rep(c(1,2,3),7), 3, 7))),
c1g = rep(1,7),
delta1=rep(-1,3),
delta2=rep(-1,3),
delta3=rep(-1,3),
delta4=rep(-1,3),
delta5=rep(-1,3),
p1=c(0.50,0.25,0.25),
p2=c(0.50,0.25,0.25),
p3=c(0.50,0.25,0.25),
p4=c(0.50,0.25,0.25),
p5=c(0.50,0.25,0.25)
)

##Run winBUGS model##
model.class.dyn.cog.sim<-bugs(data="A:/latent class model/dynclasscogsim.txt",inits = list(comb.sim.inits), model.file="A:/latent class model/latent class model.bug",
parameters=c("theta1","theta2","theta3","theta4","theta5","p1","p2","p3","p4","p5","c0","c1","c0a","c1a","c0b","c1b","c0c","c1c","c0e","c1e","c0f","c1f","c0g","c1g","delta1","delta2","delta3","delta4","delta5","a"),n.chains=1, n.iter=1500,n.thin=1,bugs.directory="c:/Program Files/WinBUGS14",debug=T,n.burnin=1000,save.history=T)

#par(mfrow=c(3,1))
#hist(model.adni.cog$mean$theta1[ind1==0&ind2==0],xlim=c(-6,4),main="Cognition trait (ADNI data): Control",xlab=expression(theta))
#hist(model.adni.cog$mean$theta1[ind1==1&ind2==0],xlim=c(-6,4),main="Cognition trait (ADNI data): MCI",xlab=expression(theta))
#hist(model.adni.cog$mean$theta1[ind1==0&ind2==1],xlim=c(-6,4),main="Cognition trait (ADNI data): AD",xlab=expression(theta))
