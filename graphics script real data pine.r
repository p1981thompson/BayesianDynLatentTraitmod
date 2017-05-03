#############################################################
#  Diagnostic plots for MCMc output- pine data (Real)  #
#############################################################

#20.01.2011#

# This script produces trace plots, Item characteristic curves
# and item information curves. It also dumps parameter estimates.
#
#

####################
#  Mobility plots  #
####################

 s<-pine.par$s

win.sim.mobility<-pine.par
##Trace plots##
png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Mobility/trace plots/item%04d.png",height=5,width=5,units="in",res=300)
par(mfrow=c(2,1))
for(i in 1:9)
{
for(j in 1:2)
{
ts.plot(win.sim.mobility$m0[i,j],ylab=expression(paste(m,"0")))
}
ts.plot(win.sim.mobility$m1[i],ylab=expression(paste(m,"1")))
}
for(i in 1:3)
{
ts.plot(win.sim.mobility$m0a[i],ylab=expression(paste(m,"0a")))
ts.plot(win.sim.mobility$m1a[i],ylab=expression(paste(m,"1a")))
}
for(i in 1:10)
{
for(j in 1:4)
{
ts.plot(win.sim.mobility$m0b[i,j],ylab=expression(paste(m,"0b")))
}
ts.plot(win.sim.mobility$m1b[i],ylab=expression(paste(m,"1b")))
}
for(i in 1:20)
{
for(j in 1:3)
{
ts.plot(win.sim.mobility$m0c[i,j],ylab=expression(paste(m,"0c")))
}
ts.plot(win.sim.mobility$m1c[i],ylab=expression(paste(m,"1c")))
}
dev.off()

 ##Save estimated model parameters##
 m0.mob<-win.sim.mobility$m0
 m1.mob<-win.sim.mobility$m1
 #
 m0a.mob<-win.sim.mobility$m0a
 m1a.mob<-win.sim.mobility$m1a
 #
 m0b.mob<-win.sim.mobility$m0b
 m1b.mob<-win.sim.mobility$m1b
 #
 m0c.mob<-win.sim.mobility$m0c
 m1c.mob<-win.sim.mobility$m1c
 #


dump(c("m0.mob","m1.mob","m0a.mob","m1a.mob","m0b.mob","m1b.mob","m0c.mob","m1c.mob"),file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/model parameters/betapinesim_mob.txt")

##Plot output plots from models##
#windows(record=T)
comb.mob.sim<-list(m0=m0.mob,m1=m1.mob,m0a=m0a.mob,m1a=m1a.mob,m0b=m0b.mob,m1b=m1b.mob,m0c=m0c.mob,m1c=m1c.mob,s=s)


plot.mob.comb<-function(dat = snames)
{
#par(mfrow = c(1, 1))
 omega <- seq(-5, 5, 0.1)
 nu1 <- rnorm(1,0,1)
 theta1 <-  comb.mob.sim$s[1]*omega + sqrt(1-(comb.mob.sim$s[1]*comb.mob.sim$s[1]))*nu1
	p.new <- runif(length(theta1), 0, 1)
	#


   png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Mobility/ICC/item%d.png",height=5,width=5,units="in",res=300)
  for(i in 1:9)
  {
		plot(theta1, p.new, ylim = c(0, 1), main = " ", type = "n", ylab = "Probability", xlab = expression(paste(theta[1]," (Mobility)")))
		#
		dat$m1 <- unlist(dat$m1)
		#
    b0.n1 <- dat$m0[i,1]
		b1.n1 <- dat$m1[i]
		b0.n2 <- dat$m0[i,2]
		b1.n2 <- dat$m1[i]
		#
		cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta1)))
		cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta1)))
    #
		prob1 <- cprob1
		prob2 <- cprob2 - cprob1
		prob3 <- 1 - cprob2
		#
		lines(theta1, prob1, col = 2, lwd = 2)
		lines(theta1, prob2, col = 3, lwd = 2)
		lines(theta1, prob3, col = 4, lwd = 2)
    #
    #
    leg.txt <- c("0", "1", "2")
		legend("topright",0.6, leg.txt, col = c(2, 3, 4), lwd = c(2, 2, 2), cex = 0.8)
  }
    #


	 dev.off()

png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Mobility/ICC/item1a.png",height=5,width=5,units="in",res=300)
  for(i in 1:2)
  {
		plot(theta1, p.new, ylim = c(0, 1), main = " ", type = "n", ylab = "Probability", xlab = expression(paste(theta[1]," (Mobility)")))
		#
		dat$m1a <- unlist(dat$m1a)
		#
		b0a.n1 <- dat$m0a[i]
		b1a.n1 <- dat$m1a[i]
		#
		cprob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta1)))
		#
		lines(theta1, cprob1, col = 2, lwd = 2)
    #
  }
	dev.off()


   png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Mobility/ICC/item%db.png",height=5,width=5,units="in",res=300)

   	for(i in 1:10){
		plot(theta1, p.new, ylim = c(0, 1), main = " ", type = "n", ylab = "Probability", xlab = expression(paste(theta[1]," (Mobility)")))
		#
		dat$m1b <- unlist(dat$m1b)
		#
		b0b.n1 <- dat$m0b[i, 1]
		b1b.n1 <- dat$m1b[i]
		b0b.n2 <- dat$m0b[i, 2]
		b1b.n2 <- dat$m1b[i]
		b0b.n3 <- dat$m0b[i, 3]
		b1b.n3 <- dat$m1b[i]
		b0b.n4 <- dat$m0b[i, 4]
		b1b.n4 <- dat$m1b[i]
		#
		cprob1 <- 1/(1 + exp( - (b0b.n1 - b1b.n1 * theta1)))
		cprob2 <- 1/(1 + exp( - (b0b.n2 - b1b.n2 * theta1)))
		cprob3 <- 1/(1 + exp( - (b0b.n3 - b1b.n3 * theta1)))
		cprob4 <- 1/(1 + exp( - (b0b.n4 - b1b.n4 * theta1)))
    #
		prob1 <- cprob1
		prob2 <- cprob2 - cprob1
		prob3 <- cprob3 - cprob2
		prob4 <- cprob4 - cprob3
		prob5 <- 1 - cprob4
		#
		lines(theta1, prob1, col = 2, lwd = 2)
		lines(theta1, prob2, col = 3, lwd = 2)
		lines(theta1, prob3, col = 4, lwd = 2)
		lines(theta1, prob4, col = 5, lwd = 2)
		lines(theta1, prob5, col = 6, lwd = 2)
		#

		leg.txt <- c("0", "1", "2", "3", "4")
		legend("topright", 0.6, leg.txt, col = c(2, 3, 4, 5, 6), lwd = c(2, 2, 2, 2, 2), cex = 0.8)
	}
   dev.off()

   png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Mobility/ICC/item%dc.png",height=5,width=5,units="in",res=300)

  for(i in 1:20) {
		plot(theta1, p.new, ylim = c(0, 1), main = " ", type = "n", ylab = "Probability", xlab = expression(paste(theta[1]," (Mobility)")))
		#
		dat$m1c <- unlist(dat$m1c)
		#
		b0c.n1 <- dat$m0c[i, 1]
		b1c.n1 <- dat$m1c[i]
		b0c.n2 <- dat$m0c[i, 2]
		b1c.n2 <- dat$m1c[i]
		b0c.n3 <- dat$m0c[i, 3]
		b1c.n3 <- dat$m1c[i]
    #
		cprob1 <- 1/(1 + exp( - (b0c.n1 - b1c.n1 * theta1)))
		cprob2 <- 1/(1 + exp( - (b0c.n2 - b1c.n2 * theta1)))
		cprob3 <- 1/(1 + exp( - (b0c.n3 - b1c.n3 * theta1)))
   	#
		prob1 <- cprob1
		prob2 <- cprob2 - cprob1
		prob3 <- cprob3 - cprob2
		prob4 <- 1 - cprob3
		#
		lines(theta1, prob1, col = 2, lwd = 2)
		lines(theta1, prob2, col = 3, lwd = 2)
		lines(theta1, prob3, col = 4, lwd = 2)
		lines(theta1, prob4, col = 5, lwd = 2)
    #
		#
		leg.txt <- c("0", "1", "2", "3")
		legend("topright", 0.6, leg.txt, col = c(2, 3, 4, 5), lwd = c(2, 2, 2, 2), cex = 0.8)
	}
	 dev.off()

 }
  plot.mob.comb(dat=comb.mob.sim)

 rm(win.sim.mobility)
###############
#  ADL plots  #
###############
win.sim.adl<-pine.par
##Trace plots##
png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/ADL/trace plots//item%02d.png",height=5,width=5,units="in",res=300)
par(mfrow=c(2,1))
for(i in 1:9)
{
for(j in 1:2)
{
ts.plot(win.sim.adl$a0[i,j],ylab=expression(paste(a,"0")))
}
ts.plot(win.sim.adl$a1[i],ylab=expression(paste(a,"1")))
}
for(i in 1:5)
{
ts.plot(win.sim.adl$a0a[i],ylab=expression(paste(a,"0a")))
ts.plot(win.sim.adl$a1a[i],ylab=expression(paste(a,"1a")))
}
for(i in 1:11)
{
for(j in 1:3)
{
ts.plot(win.sim.adl$a0b[i,j],ylab=expression(paste(a,"0b")))
}
ts.plot(win.sim.adl$a1b[i],ylab=expression(paste(a,"1b")))
}
for(i in 1:4)
{
for(j in 1:4)
{
ts.plot(win.sim.adl$a0c[i,j],ylab=expression(paste(a,"0c")))
}
ts.plot(win.sim.adl$a1c[i],ylab=expression(paste(a,"1c")))
}

ts.plot(win.sim.adl$a0d,ylab=expression(paste(a,"0d")))
ts.plot(win.sim.adl$a1d,ylab=expression(paste(a,"1d")))

dev.off()

 ##Save estimated model parameters##
 a0.adl<-win.sim.adl$a0
 a1.adl<-win.sim.adl$a1
 #
 a0a.adl<-win.sim.adl$a0a
 a1a.adl<-win.sim.adl$a1a
 #
 a0b.adl<-win.sim.adl$a0b
 a1b.adl<-win.sim.adl$a1b
 #
 a0c.adl<-win.sim.adl$a0c
 a1c.adl<-win.sim.adl$a1c
 #
 a0d.adl<-win.sim.adl$a0d
 a1d.adl<-win.sim.adl$a1d
 #

dump(c("a0.adl","a1.adl","a0a.adl","a1a.adl","a0b.adl","a1b.adl","a0c.adl","a1c.adl","a0d.adl","a1d.adl"),file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/model parameters/betapine3_adl.txt")

##Plot output plots from models##
#windows(record=T)
comb.adl.sim<-list(a0=a0.adl,a1=a1.adl,a0a=a0a.adl,a1a=a1a.adl,a0b=a0b.adl,a1b=a1b.adl,a0c=a0c.adl,a1c=a1c.adl,a0d=a0d.adl,a1d=a1d.adl,s=s)



plot.adl.comb<-function(dat = comb.adl.sim)
{
par(mfrow = c(1, 1))
 omega <- seq(-4, 4, 0.1)
 nu2 <- rnorm(1,0,1)
 theta2 <-  comb.adl.sim$s[2]*omega + sqrt(1-(comb.adl.sim$s[2]*comb.adl.sim$s[2]))*nu2
	p.new <- runif(length(theta2), 0, 1)
	#
png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/ADL/ICC/item%d.png",height=5,width=5,units="in",res=300)

for(i in 1:9) {
		plot(theta2, p.new, ylim = c(0, 1), main = paste("Item ",i), type = "n", ylab = "Probability", xlab = expression(paste(theta[2]," (ADL)")))
		#
		dat$a1 <- unlist(dat$a1)
		#
		b0.n1 <- dat$a0[i,1]
		b1.n1 <- dat$a1[i]
		b0.n2 <- dat$a0[i,2]
		b1.n2 <- dat$a1[i]
		#
		cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta2)))
		cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta2)))

		prob1<-cprob1
		prob2<-cprob2-cprob1
		prob3<-1-cprob2
  #
  lines(theta2, prob1, col = 2, lwd = 2)
  lines(theta2, prob2, col = 3, lwd = 2)
  lines(theta2, prob3, col = 4, lwd = 2)
   #
		leg.txt <- c("0", "1", "2")
		legend("topright", leg.txt, col = c(2, 3, 4), lwd = c(2, 2, 2), cex = 0.8)
	}


	 dev.off()

	 png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/ADL/ICC/item%da.png",height=5,width=5,units="in",res=300)

   	for(i in 1:5) {
		plot(theta2, p.new, ylim = c(0, 1), main = " ", type = "n", ylab = "Probability", xlab = expression(paste(theta[2]," (ADL)")))
		#
		dat$a1a <- unlist(dat$a1a)
		#
		b0a.n1 <- dat$a0a[i]
		b1a.n1 <- dat$a1a[i]
		#
		cprob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta2)))
    #
		lines(theta2, cprob1, col = 2, lwd = 2)
	}
	dev.off()


   png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/ADL/ICC/item%db.png",height=5,width=5,units="in",res=300)

   	for(i in 1:11) {
		plot(theta2, p.new, ylim = c(0, 1), main = " ", type = "n", ylab = "Probability", xlab = expression(paste(theta[2]," (ADL)")))
		#
		dat$a1b <- unlist(dat$a1b)
		#
		b0b.n1 <- dat$a0b[i, 1]
		b1b.n1 <- dat$a1b[i]
		b0b.n2 <- dat$a0b[i, 2]
		b1b.n2 <- dat$a1b[i]
		b0b.n3 <- dat$a0b[i, 3]
		b1b.n3 <- dat$a1b[i]
		#
		cprob1 <- 1/(1 + exp( - (b0b.n1 - b1b.n1 * theta2)))
		cprob2 <- 1/(1 + exp( - (b0b.n2 - b1b.n2 * theta2)))
		cprob3 <- 1/(1 + exp( - (b0b.n3 - b1b.n3 * theta2)))
		#
		prob1 <- cprob1
		prob2 <- cprob2 - cprob1
		prob3 <- cprob3 - cprob2
		prob4 <- 1 - cprob3
		#
		lines(theta2, prob1, col = 2, lwd = 2)
		lines(theta2, prob2, col = 3, lwd = 2)
		lines(theta2, prob3, col = 4, lwd = 2)
		lines(theta2, prob4, col = 5, lwd = 2)
  #
		leg.txt <- c("0", "1", "2", "3")
		legend("topright", leg.txt, col = c(2, 3, 4, 5), lwd = c(2, 2, 2, 2), cex = 0.8)
	}
   dev.off()

   png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/ADL/ICC/item%dc.png",height=5,width=5,units="in",res=300)

  for(i in 1:4)
  {
		plot(theta2, p.new, ylim = c(0, 1), main = " ", type = "n", ylab = "Probability", xlab = expression(paste(theta[2]," (ADL)")))
		#
		dat$a1c <- unlist(dat$a1c)
		#
		b0c.n1 <- dat$a0c[i,1]
		b1c.n1 <- dat$a1c[i]
		b0c.n2 <- dat$a0c[i,2]
		b1c.n2 <- dat$a1c[i]
		b0c.n3 <- dat$a0c[i,3]
		b1c.n3 <- dat$a1c[i]
		b0c.n4 <- dat$a0c[i,4]
		b1c.n4 <- dat$a1c[i]
		#
		cprob1 <- 1/(1 + exp( - (b0c.n1 - b1c.n1 * theta2)))
		cprob2 <- 1/(1 + exp( - (b0c.n2 - b1c.n2 * theta2)))
		cprob3 <- 1/(1 + exp( - (b0c.n3 - b1c.n3 * theta2)))
    cprob4 <- 1/(1 + exp( - (b0c.n4 - b1c.n4 * theta2)))
    #
		prob1 <- cprob1
		prob2 <- cprob2 - cprob1
		prob3 <- cprob3 - cprob2
		prob4 <- cprob4 - cprob3
		prob5 <- 1 - cprob4
		#
		lines(theta2, prob1, col = 2, lwd = 2)
		lines(theta2, prob2, col = 3, lwd = 2)
		lines(theta2, prob3, col = 4, lwd = 2)
		lines(theta2, prob4, col = 5, lwd = 2)
		lines(theta2, prob5, col = 6, lwd = 2)
		#
		#
		leg.txt <- c("0", "1", "2", "3", "4")
		legend("topright", leg.txt, col = c(2, 3, 4, 5, 6), lwd = c(2, 2, 2, 2, 2), cex = 0.8)
  }
	 dev.off()
	  png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/ADL/ICC/item%01dd.png",height=5,width=5,units="in",res=300)
		#
		dat$a1d <- unlist(dat$a1d)
		#
		b0d.n1 <- dat$a0d
		b1d.n1 <- dat$a1d
    #
		mean <- b0d.n1 + b1d.n1 * theta2
		#
		plot(theta2, mean, main = " ",type="l", ylab = "Mean", xlab = expression(paste(theta[2]," (ADL)")))

	 dev.off()
 }
  plot.adl.comb(dat=comb.adl.sim)

rm(win.sim.adl)
######################
#  Depression plots  #
######################
win.sim.depression<-pine.par
##Trace plots##
png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Depression/trace plots/item%02d.png",height=5,width=5,units="in",res=300)
par(mfrow=c(2,1))

for(j in 1:27)
{
ts.plot(win.sim.depression$d0[i],ylab=expression(paste(d,"0")))
ts.plot(win.sim.depression$d1[i],ylab=expression(paste(d,"1")))
}

for(i in 1:6)
{
for(j in 1:3)
{
ts.plot(win.sim.depression$d0a[i,j],ylab=expression(paste(d,"0a")))
}
ts.plot(win.sim.depression$d1a[i],ylab=expression(paste(d,"1a")))
}
for(i in 1:4)
{
ts.plot(win.sim.depression$d0b[i],ylab=expression(paste(d,"0b")))
}
ts.plot(win.sim.depression$d1b,ylab=expression(paste(d,"1b")))

for(i in 1:9)
{
for(j in 1:2)
{
ts.plot(win.sim.depression$d0c[i,j],ylab=expression(paste(d,"0c")))
}
ts.plot(win.sim.depression$d1c[i],ylab=expression(paste(d,"1c")))
}
dev.off()

 ##Save estimated model parameters##
 d0.depression<-win.sim.depression$d0
 d1.depression<-win.sim.depression$d1
 #
 d0a.depression<-win.sim.depression$d0a
 d1a.depression<-win.sim.depression$d1a
 #
 d0b.depression<-win.sim.depression$d0b
 d1b.depression<-win.sim.depression$d1b
 #
 d0c.depression<-win.sim.depression$d0c
 d1c.depression<-win.sim.depression$d1c
 #

dump(c("d0.depression","d1.depression","d0a.depression","d1a.depression","d0b.depression","d1b.depression","d0c.depression","d1c.depression"),file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/model parameters/betapine3_depression.txt")

##Plot output plots from models##
#windows(record=T)
dat<-comb.depression.sim<-list(d0=d0.depression,d1=d1.depression,d0a=d0a.depression,d1a=d1a.depression,d0b=d0b.depression,d1b=d1b.depression,d0c=d0c.depression,d1c=d1c.depression,s=s)



plot.depression.comb<-function(dat = dat)
{
par(mfrow = c(1, 1))
omega <- seq(-4, 4, 0.1)
 nu3 <- rnorm(1,0,1)
 theta3 <-  dat$s[3]*omega + sqrt(1-(dat$s[3]*dat$s[3]))*nu3
	p.new <- runif(length(theta3), 0, 1)
	#
png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Depression/ICC/item%d.png",height=5,width=5,units="in",res=300)
for(i in 1:26)
{
		plot(theta3, p.new, ylim = c(0, 1), main = " ", type = "n", ylab = "Probability", xlab = expression(paste(theta[3]," (Depression)")))
		#
		dat$d1 <- unlist(dat$d1)
		#
		b0.n1 <- dat$d0[i]
		b1.n1 <- dat$d1[i]
		#
		cprob1 <- 1/(1 + exp( - (b0.n1 + b1.n1 * theta3)))
    #
		lines(theta3, cprob1, col = 2, lwd = 2)

}
	dev.off()

	 png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Depression/ICC/item%da.png",height=5,width=5,units="in",res=300)

    for(i in 1:6)
    {
		plot(theta3, p.new, ylim=c(0,1), main =" ", type = "n", ylab = "Probability", xlab = expression(paste(theta[3]," (Depression)")))
		#
		dat$d1a <- unlist(dat$d1a)
		#
		b0a.n1 <- dat$d0a[i,1]
		b1a.n1 <- dat$d1a[i]
		b0a.n2 <- dat$d0a[i,2]
		b1a.n2 <- dat$d1a[i]
		b0a.n3 <- dat$d0a[i,3]
		b1a.n3 <- dat$d1a[i]
		#
		cprob1 <- 1/(1 + exp( - (b0a.n1 - b1a.n1 * theta3)))
		cprob2 <- 1/(1 + exp( - (b0a.n2 - b1a.n2 * theta3)))
		cprob3 <- 1/(1 + exp( - (b0a.n3 - b1a.n3 * theta3)))
		#
		prob1<-cprob1
		prob2<-cprob2-cprob1
		prob3<-cprob3-cprob2
		prob4<-1-cprob3
    #
		lines(theta3, prob1, col = 2, lwd = 2)
		lines(theta3, prob2, col = 3, lwd = 2)
		lines(theta3, prob3, col = 4, lwd = 2)
		lines(theta3, prob4, col = 5, lwd = 2)
		#
		#
		leg.txt <- c("0", "1", "2","3")
		legend("topright", leg.txt, col = c(2, 3, 4, 5), lwd = c(2, 2, 2, 2), cex = 0.8)
    }

	 dev.off()

  png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Depression/ICC/item1b.png",height=5,width=5,units="in",res=300)
 plot(theta3, p.new, ylim=c(0,1), main =" ", type = "n", ylab = "Probability", xlab = expression(paste(theta[3]," (Depression)")))
		#
		dat$d1b <- unlist(dat$d1b)
		#
		b0b.n1 <- dat$d0b[1]
		b1b.n1 <- dat$d1b
		b0b.n2 <- dat$d0b[2]
		b1b.n2 <- dat$d1b
		b0b.n3 <- dat$d0b[3]
		b1b.n3 <- dat$d1b
		b0b.n4 <- dat$d0b[4]
		b1b.n4 <- dat$d1b
		#
		cprob1 <- 1/(1 + exp( - (b0b.n1 - b1b.n1 * theta3)))
		cprob2 <- 1/(1 + exp( - (b0b.n2 - b1b.n2 * theta3)))
		cprob3 <- 1/(1 + exp( - (b0b.n3 - b1b.n3 * theta3)))
		cprob4 <- 1/(1 + exp( - (b0b.n4 - b1b.n4 * theta3)))
		#
		prob1<-cprob1
		prob2<-cprob2-cprob1
		prob3<-cprob3-cprob2
		prob4<-cprob4-cprob3
		prob5<-1-cprob4
    #
		lines(theta3, prob1, col = 2, lwd = 2)
		lines(theta3, prob2, col = 3, lwd = 2)
		lines(theta3, prob3, col = 4, lwd = 2)
		lines(theta3, prob4, col = 5, lwd = 2)
		lines(theta3, prob5, col = 6, lwd = 2)
		#
    leg.txt <- c("0", "1", "2","3")
		legend("topright", leg.txt, col = c(2, 3, 4, 5, 6), lwd = c(2, 2, 2, 2, 2), cex = 0.8)
	 dev.off()

 	 png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Depression/ICC/item%dc.png",height=5,width=5,units="in",res=300)

    for(i in 1:9)
    {
		plot(theta3, p.new, ylim=c(0,1), main =" ", type = "n", ylab = "Probability", xlab = expression(paste(theta[3]," (Depression)")))
		#
		dat$d1c <- unlist(dat$d1c)
		#
		b0c.n1 <- dat$d0c[i,1]
		b1c.n1 <- dat$d1c[i]
		b0c.n2 <- dat$d0c[i,2]
    b1c.n2 <- dat$d1c[i]
		#
		cprob1 <- 1/(1 + exp( - (b0c.n1 - b1c.n1 * theta3)))
		cprob2 <- 1/(1 + exp( - (b0c.n2 - b1c.n2 * theta3)))
		#
		prob1<-cprob1
		prob2<-cprob2-cprob1
		prob3<-1-cprob2
    #
		lines(theta3, prob1, col = 2, lwd = 2)
		lines(theta3, prob2, col = 3, lwd = 2)
		lines(theta3, prob3, col = 4, lwd = 2)
		#
		leg.txt <- c("0", "1", "2")
		legend("topright", leg.txt, col = c(2, 3, 4), lwd = c(2, 2, 2), cex = 0.8)
    }


	 dev.off()
}
  plot.depression.comb(dat=comb.depression.sim)

rm(win.sim.depression)
################
#  Pain plots  #
################

win.sim.pain<-pine.par
##Trace plots##
png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Pain/trace plots/item%02d.png",height=5,width=5,units="in",res=300)
par(mfrow=c(2,1))

for(i in 1:2)
{
ts.plot(win.sim.pain$p0[i],ylab=expression(paste(p,"0")))
}
ts.plot(win.sim.pain$p1,ylab=expression(paste(p,"1")))

for(i in 1:2)
{
for(j in 1:3)
{
ts.plot(win.sim.pain$p0a[i,j],ylab=expression(paste(p,"0a")))
}
ts.plot(win.sim.pain$p1a[i],ylab=expression(paste(p,"1a")))
}

for(j in 1:4)
{
ts.plot(win.sim.pain$p0b[j],ylab=expression(paste(p,"0b")))
}
ts.plot(win.sim.pain$p1b,ylab=expression(paste(p,"1b")))

ts.plot(win.sim.pain$p0c,ylab=expression(paste(p,"0c")))
ts.plot(win.sim.pain$p1c,ylab=expression(paste(p,"1c")))

for(j in 1:6)
{
ts.plot(win.sim.pain$p0d[j],ylab=expression(paste(p,"0d")))
}
ts.plot(win.sim.pain$p1d,ylab=expression(paste(p,"1d")))

ts.plot(win.sim.pain$p0f,ylab=expression(paste(p,"0f")))
ts.plot(win.sim.pain$p1f,ylab=expression(paste(p,"1f")))

dev.off()

 ##Save estimated model parameters##
 p0.pain<-win.sim.pain$p0
 p1.pain<-win.sim.pain$p1
 #
 p0a.pain<-win.sim.pain$p0a
 p1a.pain<-win.sim.pain$p1a
 #
 p0b.pain<-win.sim.pain$p0b
 p1b.pain<-win.sim.pain$p1b
 #
 p0c.pain<-win.sim.pain$p0c
 p1c.pain<-win.sim.pain$p1c
 #
 p0d.pain<-win.sim.pain$p0d
 p1d.pain<-win.sim.pain$p1d
 #
 p0f.pain<-win.sim.pain$p0f
 p1f.pain<-win.sim.pain$p1f
 #

dump(c("p0.pain","p1.pain","p0a.pain","p1a.pain","p0b.pain","p1b.pain","p0c.pain","p1c.pain","p0d.pain","p1d.pain","p0f.pain","p1f.pain"),file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/model parameters/betapine3_pain.txt")

##Plot output plots from models##
#windows(record=T)
comb.pain.sim<-list(p0=p0.pain,p1=p1.pain,p0a=p0a.pain,p1a=p1a.pain,p0b=p0b.pain,p1b=p1b.pain,p0c=p0c.pain,p1c=p1c.pain,p0d=p0d.pain,p1d=p1d.pain,p0f=p0f.pain,p1f=p1f.pain,s=s)


plot.pain.comb<-function(dat = comb.pain.sim)
{
par(mfrow = c(1, 1))
  omega <- seq(-5, 5, 0.1)
 nu4 <- rnorm(1,0,1)
 theta4 <-  comb.pain.sim$s[4]*omega + sqrt(1-(comb.pain.sim$s[4]*comb.pain.sim$s[4]))*nu4
	p.new <- runif(length(theta4), 0, 1)
	#

png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Pain/ICC/item%d.png",height=5,width=5,units="in",res=300)

		plot(theta4, p.new, ylim = c(0, 1), main = " ", type = "n", ylab = "Probability", xlab = expression(paste(theta[4]," (Pain)")))
		#
		dat$p1 <- unlist(dat$p1)
		#
		b0.n1 <- dat$p0[1]
		b1.n1 <- dat$p1
		b0.n2 <- dat$p0[2]
		b1.n2 <- dat$p1
		#
		cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta4)))
		cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta4)))
    #
		prob1 <- cprob1
		prob2 <- cprob2 - cprob1
		prob3 <- 1 - cprob2
		#
		lines(theta4, prob1, col = 2, lwd = 2)
		lines(theta4, prob2, col = 3, lwd = 2)
		lines(theta4, prob3, col = 4, lwd = 2)
		#
		leg.txt <- c("0", "1", "2")
		legend("topright", leg.txt, col = c(2, 3, 4), lwd = c(2, 2, 2), cex = 0.8)

	dev.off()

  png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Pain/ICC/item%da.png",height=5,width=5,units="in",res=300)
  for(i in 1:2)
  {
		plot(theta4, p.new, ylim = c(0, 1), main = " ", type = "n", ylab = "Probability", xlab =expression(paste(theta[4]," (Pain)")))
		#
		dat$p1a <- unlist(dat$p1a)
		#
		b0a.n1 <- dat$p0a[i,1]
		b1a.n1 <- dat$p1a[i]
		b0a.n2 <- dat$p0a[i,2]
		b1a.n2 <- dat$p1a[i]
		b0a.n3 <- dat$p0a[i,3]
		b1a.n3 <- dat$p1a[i]
		#
		cprob1 <- 1/(1 + exp( - (b0a.n1 - b1a.n1 * theta4)))
		cprob2 <- 1/(1 + exp( - (b0a.n2 - b1a.n2 * theta4)))
		cprob3 <- 1/(1 + exp( - (b0a.n3 - b1a.n3 * theta4)))
    #
		prob1 <- cprob1
		prob2 <- cprob2 - cprob1
		prob3 <- cprob3 - cprob2
		prob4 <- 1 - cprob3
		#
		lines(theta4, prob1, col = 2, lwd = 2)
		lines(theta4, prob2, col = 3, lwd = 2)
		lines(theta4, prob3, col = 4, lwd = 2)
		lines(theta4, prob4, col = 5, lwd = 2)
		#
		leg.txt <- c("0", "1", "2")
		legend("topright", leg.txt, col = c(2, 3, 4, 5), lwd = c(2, 2, 2, 2), cex = 0.8)
  }
	dev.off()

   png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Pain/ICC/item1b.png",height=5,width=5,units="in",res=300)

		plot(theta4, p.new, ylim = c(0, 1), main = " ", type = "n", ylab = "Probability", xlab = expression(paste(theta[4]," (Pain)")))
		#
		dat$p1b <- unlist(dat$p1b)
		#
		b0b.n1 <- dat$p0b[1]
		b1b.n1 <- dat$p1b
		b0b.n2 <- dat$p0b[2]
		b1b.n2 <- dat$p1b
		b0b.n3 <- dat$p0b[3]
		b1b.n3 <- dat$p1b
		b0b.n4 <- dat$p0b[4]
		b1b.n4 <- dat$p1b
		#
		cprob1 <- 1/(1 + exp( - (b0b.n1 - b1b.n1 * theta4)))
		cprob2 <- 1/(1 + exp( - (b0b.n2 - b1b.n2 * theta4)))
		cprob3 <- 1/(1 + exp( - (b0b.n3 - b1b.n3 * theta4)))
		cprob4 <- 1/(1 + exp( - (b0b.n4 - b1b.n4 * theta4)))
		#
		prob1 <- cprob1
		prob2 <- cprob2 - cprob1
		prob3 <- cprob3 - cprob2
		prob4 <- cprob4 - cprob3
		prob5 <- 1 - cprob4
		#
		lines(theta4, prob1, col = 2, lwd = 2)
		lines(theta4, prob2, col = 3, lwd = 2)
		lines(theta4, prob3, col = 4, lwd = 2)
		lines(theta4, prob4, col = 5, lwd = 2)
		lines(theta4, prob5, col = 6, lwd = 2)
    #
    #
		leg.txt <- c("0", "1", "2", "3", "4")
		legend("topright", leg.txt, col = c(2, 3, 4, 5, 6), lwd = c(2, 2, 2, 2, 2), cex = 0.8)

   dev.off()

   png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Pain/ICC/item1c.png",height=5,width=5,units="in",res=300)

		plot(theta4, p.new, ylim = c(0, 1), main = " ", type = "n", ylab = "Probability", xlab = expression(paste(theta[4]," (Pain)")))
		#
		dat$d1c <- unlist(dat$d1c)
		#
		b0c.n1 <- dat$p0c
		b1c.n1 <- dat$p1c
		#
		cprob1 <- 1/(1 + exp( - (b0c.n1 + b1c.n1 * theta4)))
    #
		lines(theta4, cprob1, col = 2, lwd = 2)
		#

	dev.off()

     png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Pain/ICC/item1d.png",height=5,width=5,units="in",res=300)

		plot(theta4, p.new, ylim = c(0, 1), main = " ", type = "n", ylab = "Probability", xlab = expression(paste(theta[4]," (Pain)")))
		#
		dat$p1d <- unlist(dat$p1d)
		#
		b0d.n1 <- dat$p0d[1]
		b1d.n1 <- dat$p1d
		b0d.n2 <- dat$p0d[2]
		b1d.n2 <- dat$p1d
		b0d.n3 <- dat$p0d[3]
		b1d.n3 <- dat$p1d
		b0d.n4 <- dat$p0d[4]
		b1d.n4 <- dat$p1d
		b0d.n5 <- dat$p0d[5]
		b1d.n5 <- dat$p1d
		b0d.n6 <- dat$p0d[6]
		b1d.n6 <- dat$p1d
		#
		cprob1 <- 1/(1 + exp( - (b0d.n1 - b1d.n1 * theta4)))
		cprob2 <- 1/(1 + exp( - (b0d.n2 - b1d.n2 * theta4)))
		cprob3 <- 1/(1 + exp( - (b0d.n3 - b1d.n3 * theta4)))
		cprob4 <- 1/(1 + exp( - (b0d.n4 - b1d.n4 * theta4)))
		cprob5 <- 1/(1 + exp( - (b0d.n5 - b1d.n5 * theta4)))
		cprob6 <- 1/(1 + exp( - (b0d.n6 - b1d.n6 * theta4)))
		#
		prob1 <- cprob1
		prob2 <- cprob2 - cprob1
		prob3 <- cprob3 - cprob2
		prob4 <- cprob4 - cprob3
		prob5 <- cprob5 - cprob4
		prob6 <- cprob6 - cprob5
		prob7 <- 1 - cprob6
		#
		lines(theta4, prob1, col = 2, lwd = 2)
		lines(theta4, prob2, col = 3, lwd = 2)
		lines(theta4, prob3, col = 4, lwd = 2)
		lines(theta4, prob4, col = 5, lwd = 2)
		lines(theta4, prob5, col = 6, lwd = 2)
		lines(theta4, prob6, col = 7, lwd = 2)
		lines(theta4, prob7, col = 8, lwd = 2)
    #
    #
		leg.txt <- c("1", "2", "3", "4", "5", "6", "7")
		legend("topright", leg.txt, col = c(2, 3, 4, 5, 6, 7, 8), lwd = c(2, 2, 2, 2, 2, 2, 2), cex = 0.8)

   dev.off()

#    png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Pain/ICC/item1f.png",height=5,width=5,units="in",res=300)
#    #
#		#
#		dat$p1f <- unlist(dat$p1f)
#		#
#		b0f.n1 <- dat$p0f
#		b1f.n1 <- dat$p1f
#		#
#		mean<-b0f.n1 + b1f.n1 * theta4
#    #
#    plot(theta4, mean, main = " ",type="l", ylab = "Probability", xlab = "Theta")
#    #
#    #
#	  dev.off()
#	  #

 }
  plot.pain.comb(dat=comb.pain.sim)

rm(win.sim.pain)
#####################
#  Cognition plots  #
#####################
win.sim.cognition1<-pine.par
##Trace plots##
png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Cognition/trace plots//item%02d.png",height=5,width=5,units="in",res=300)
par(mfrow=c(2,1))

for(j in 1:24)
{
ts.plot(win.sim.cognition1$c0[j],ylab=expression(paste(c,"0")))
ts.plot(win.sim.cognition1$c1[j],ylab=expression(paste(c,"1")))
}
for(i in 1:8)
{
for(j in 1:3)
{
ts.plot(win.sim.cognition1$c0a[i,j],ylab=expression(paste(c,"0a")))
}
ts.plot(win.sim.cognition1$c1a[i],ylab=expression(paste(c,"1a")))
}

for(i in 1:2)
{
for(j in 1:2)
{
ts.plot(win.sim.cognition1$c0b[i,j],ylab=expression(paste(c,"0b")))
}
ts.plot(win.sim.cognition1$c1b[i],ylab=expression(paste(c,"1b")))
}

dev.off()

 ##Save estimated model parameters##
 c0.cognition<-win.sim.cognition1$c0
 c1.cognition<-win.sim.cognition1$c1
 #
 c0a.cognition<-win.sim.cognition1$c0a
 c1a.cognition<-win.sim.cognition1$c1a
 #
 c0b.cognition<-win.sim.cognition1$c0b
 c1b.cognition<-win.sim.cognition1$c1b
 #

dump(c("c0.cognition","c1.cognition","c0a.cognition","c1a.cognition","c0b.cognition","c1b.cognition"),file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/model parameters/betapine3_cognition.txt")


##Plot output plots from models##
#windows(record=T)
comb.cognition.sim<-list(c0=c0.cognition,c1=c1.cognition,c0a=c0a.cognition,c1a=c1a.cognition,c0b=c0b.cognition,c1b=c1b.cognition,s=s)


plot.cognition.comb<-function(dat = snames)
{
par(mfrow = c(1, 1))
omega <- seq(-4, 4, 0.1)
 nu5 <- rnorm(1,0,1)
 theta5 <-  comb.cognition.sim$s[5]*omega + sqrt(1-(comb.cognition.sim$s[5]*comb.cognition.sim$s[5]))*nu5
	p.new <- runif(length(theta5), 0, 1)
	#

	 png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Cognition/ICC/item%d.png",height=5,width=5,units="in",res=300)

    for(i in 1:24)
    {
		plot(theta5, p.new, ylim=c(0,1), main =" ", type = "n", ylab = "Probability", xlab = expression(paste(theta[5]," (Cognition)")))
		#
		dat$c1 <- unlist(dat$c1)
		#
		b0.n1 <- dat$c0[i]
		b1.n1 <- dat$c1[i]
		#
		prob1 <- 1/(1 + exp( - (b0.n1 + b1.n1 * theta5)))
		lines(theta5, prob1, col = 2, lwd = 2)
    }
	 dev.off()

   png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Cognition/ICC/item1a.png",height=5,width=5,units="in",res=300)
  for(i in 1:8)
  {
		plot(theta5, p.new, ylim = c(0, 1), main = " ", type = "n", ylab = "Probability", xlab = expression(paste(theta[5]," (Cognition)")))
		#
		dat$c1a <- unlist(dat$c1a)
		#
		b0a.n1 <- dat$c0a[i,1]
		b1a.n1 <- dat$c1a[i]
		b0a.n2 <- dat$c0a[i,2]
		b1a.n2 <- dat$c1a[i]
		b0a.n3 <- dat$c0a[i,3]
		b1a.n3 <- dat$c1a[i]
		#
		cprob1 <- 1/(1 + exp( - (b0a.n1 - b1a.n1 * theta5)))
		cprob2 <- 1/(1 + exp( - (b0a.n2 - b1a.n2 * theta5)))
		cprob3 <- 1/(1 + exp( - (b0a.n3 - b1a.n3 * theta5)))
    #
		prob1 <- cprob1
		prob2 <- cprob2 - cprob1
		prob3 <- cprob3 - cprob2
		prob4 <- 1 - cprob3
		#
		lines(theta5, prob1, col = 2, lwd = 2)
		lines(theta5, prob2, col = 3, lwd = 2)
		lines(theta5, prob3, col = 4, lwd = 2)
		lines(theta5, prob4, col = 5, lwd = 2)
    #
		leg.txt <- c("0", "1", "2","3")
		legend("topright", leg.txt, col = c(2, 3, 4, 5), lwd = c(2, 2, 2, 2), cex = 0.8)
  }
	dev.off()

   png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Cognition/ICC/item%db.png",height=5,width=5,units="in",res=300)

   	for(i in 1:2) {
		plot(theta5, p.new, ylim = c(0, 1), main = " ", type = "n", ylab = "Probability", xlab = expression(paste(theta[5]," (Cognition)")))
		#
		dat$c1b <- unlist(dat$c1b)
		#
		b0b.n1 <- dat$c0b[i, 1]
		b1b.n1 <- dat$c1b[i]
		b0b.n2 <- dat$c0b[i, 2]
		b1b.n2 <- dat$c1b[i]
		#
		cprob1 <- 1/(1 + exp( - (b0b.n1 - b1b.n1 * theta5)))
		cprob2 <- 1/(1 + exp( - (b0b.n2 - b1b.n2 * theta5)))
    #
		prob1 <- cprob1
		prob2 <- cprob2 - cprob1
		prob3 <- 1 - cprob2
    #
		lines(theta5, prob1, col = 2, lwd = 2)
		lines(theta5, prob2, col = 3, lwd = 2)
		lines(theta5, prob3, col = 4, lwd = 2)
		#
		#
		leg.txt <- c("0", "1", "2")
		legend("topright", leg.txt, col = c(2, 3, 4), lwd = c(2, 2, 2), cex = 0.8)
	}
   dev.off()

 }
  plot.cognition.comb(dat=comb.cognition.sim)

## trace plots for omega and theta traits ##
par(mfrow=c(2,1))
for(j in 1:483)
{
ts.plot(win.sim.cognition1$omega[j],ylab=expression(paste(omega," ",i)))
}


################################################################################
#                                                                              #
#     ITEM INFORMATION CURVES: MOBILITY, ADL, PAIN, DEPRESSION & COGNITION     #
#                                                                              #
################################################################################


####################
#  Mobility plots  #
####################

#win.sim.mobility<-comb.sep2010.theta.mix4a

#comb.mob.sim<-list(m0=m0.mob,m1=m1.mob,m0a=m0a.mob,m1a=m1a.mob,m0b=m0b.mob,m1b=m1b.mob,m0c=m0c.mob,m1c=m1c.mob,s=s)

plot.mob.iic<-function(dat = snames)
{
#par(mfrow = c(1, 1))
 omega <- seq(-4, 4, 0.1)
 nu1 <- rnorm(1,0,1)
 theta1 <-  comb.mob.sim$s[1]*omega + sqrt(1-(comb.mob.sim$s[1]*comb.mob.sim$s[1]))*nu1

	#
   png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Mobility/IIC/item%d.png",height=5,width=5,units="in",res=300)
  for(i in 1:8)
  {
		#
		dat$m1 <- unlist(dat$m1)
		#
    b0.n1 <- dat$m0[i,1]
		b1.n1 <- dat$m1[i]
		b0.n2 <- dat$m0[i,2]
		b1.n2 <- dat$m1[i]
		#
		p1<-cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta1)))
		p2<-cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta1)))
    #
    	resp1<-resp2<-resp3<-1
		#
 info1<- -(-resp1*b1.n1^2*p1*(1-p1))*p1
		#
 info2<- ((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1.n2^2*((1-p2)*p2)+p1*(1-p1)*b1.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1.n2-p1*(1-p1)*b1.n1)*(p2*(1-p2)*b1.n2-p1*(1-p1)*b1.n1))*(p2-p1)
		#
 info3<- -(-resp3*b1.n2^2*p2*(1-p2))*(1-p2)
		#
  info.tot<- -(-resp1*b1.n1^2*p1*(1-p1))*p1+
((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1.n2^2*((1-p2)*p2)+p1*(1-p1)*b1.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1.n2-p1*(1-p1)*b1.n1)*(p2*(1-p2)*b1.n2-p1*(1-p1)*b1.n1))*(p2-p1)
-(-resp3*b1.n2^2*p2*(1-p2))*(1-p2)

		#
		plot(theta1, info.tot, main = " ", type = "n", ylab = "Information", xlab = expression(paste(theta[1]," (Mobility)")),ylim=c(0,max(info.tot)))
     print("!")
		lines(theta1, info1, col = 2, lwd = 2,lty=2)
		lines(theta1, info2, col = 3, lwd = 2,lty=2)
		lines(theta1, info3, col = 4, lwd = 2,lty=2)
		lines(theta1, info.tot, col = 1, lwd=2)
		#
		leg.txt <- c("0", "1", "2", "total")
		legend("topleft", leg.txt, col = c(2, 3, 4, 1), lwd = c(2, 2, 2, 2), lty=c(2,2,2,1), cex = 0.8)
  }


	 dev.off()

png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Mobility/IIC/item1a.png",height=5,width=5,units="in",res=300)

  for(i in 1:2)
  {
		#
		dat$m1a <- unlist(dat$m1a)
		#
		b0a.n1 <- dat$m0a[i]
		b1a.n1 <- dat$m1a[i]
		#
		prob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta1)))
		info<- prob1*(1-prob1)*b1a.n1^2
		#
		plot(theta1, info, main = " ", type = "n", ylab = "Information", xlab = expression(paste(theta[1]," (Mobility)")),ylim=c(0,max(info)))
		lines(theta1, info, col = 1, lwd = 2)
    #
   }
	dev.off()


   png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Mobility/IIC/item%db.png",height=5,width=5,units="in",res=300)

   	for(i in 1:10) {

		#
		dat$m1b <- unlist(dat$m1b)
		#
		b0b.n1 <- dat$m0b[i, 1]
		b1b.n1 <- dat$m1b[i]
		b0b.n2 <- dat$m0b[i, 2]
		b1b.n2 <- dat$m1b[i]
		b0b.n3 <- dat$m0b[i, 3]
		b1b.n3 <- dat$m1b[i]
		b0b.n4 <- dat$m0b[i, 4]
		b1b.n4 <- dat$m1b[i]
		#
		p1<-cprob1 <- 1/(1 + exp( - (b0b.n1 - b1b.n1 * theta1)))
		p2<-cprob2 <- 1/(1 + exp( - (b0b.n2 - b1b.n2 * theta1)))
		p3<-cprob3 <- 1/(1 + exp( - (b0b.n3 - b1b.n3 * theta1)))
		p4<-cprob4 <- 1/(1 + exp( - (b0b.n4 - b1b.n4 * theta1)))
    #
 	resp1<-resp2<-resp3<-resp4<-resp5<-1
		#
 info1<- -(-resp1*b1b.n1^2*p1*(1-p1))*p1
		#
 info2<- ((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1b.n2^2*((1-p2)*p2)+p1*(1-p1)*b1b.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1))*(p2-p1)
		#
 info3<- ((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1b.n3^2*((1-p3)*p3)+p2*(1-p2)*b1b.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1b.n3-p2*(1-p2)*b1b.n2)*(p3*(1-p3)*b1b.n3-p2*(1-p2)*b1b.n2))*(p3-p2)
		#
 info4<- ((1/(p4-p3)^2)*resp4*((p4-p3)*(p4*(1-p4)*b1b.n4^2*((1-p4)*p4)+p3*(1-p3)*b1b.n3^2*(p3+(1-p3))))+(1/(p4-p3)^2)*(p4*(1-p4)*b1b.n4-p3*(1-p3)*b1b.n3)*(p4*(1-p4)*b1b.n4-p3*(1-p3)*b1b.n3))*(p4-p3)
		#
 info5<- -(-resp5*b1b.n4^2*p4*(1-p4))*(1-p4)
		#
		info.tot<- -(-resp1*b1b.n1^2*p1*(1-p1))*p1+
((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1b.n2^2*((1-p2)*p2)+p1*(1-p1)*b1b.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1))*(p2-p1)+
((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1b.n3^2*((1-p3)*p3)+p2*(1-p2)*b1b.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1b.n3-p2*(1-p2)*b1b.n2)*(p3*(1-p3)*b1b.n3-p2*(1-p2)*b1b.n2))*(p3-p2)+
((1/(p4-p3)^2)*resp4*((p4-p3)*(p4*(1-p4)*b1b.n4^2*((1-p4)*p4)+p3*(1-p3)*b1b.n3^2*(p3+(1-p3))))+(1/(p4-p3)^2)*(p4*(1-p4)*b1b.n4-p3*(1-p3)*b1b.n3)*(p4*(1-p4)*b1b.n4-p3*(1-p3)*b1b.n3))*(p4-p3)
-(-resp5*b1b.n4^2*p4*(1-p4))*(1-p4)
    #
		plot(theta1, info.tot, main = " ", type = "n", ylab = "Information", xlab = expression(paste(theta[1]," (Mobility)")),ylim=c(0,max(info.tot)))

		lines(theta1, info1, col = 2, lwd = 2,lty=2)
		lines(theta1, info2, col = 3, lwd = 2,lty=2)
		lines(theta1, info3, col = 4, lwd = 2,lty=2)
		lines(theta1, info4, col = 5, lwd = 2,lty=2)
		lines(theta1, info5, col = 6, lwd = 2,lty=2)
		lines(theta1, info.tot, col = 1, lwd = 2)
		#
		leg.txt <- c("0", "1", "2", "3", "4", "total")
		legend("topleft", leg.txt, col = c(2, 3, 4, 5, 6, 1), lwd = c(2, 2, 2, 2, 2, 2),lty = c(2, 2, 2, 2, 2, 1), cex = 0.8)
	}
   dev.off()

   png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Mobility/IIC/item%dc.png",height=5,width=5,units="in",res=300)

  for(i in 1:20) {

		#
		dat$m1c <- unlist(dat$m1c)
		#
		b0c.n1 <- dat$m0c[i, 1]
		b1c.n1 <- dat$m1c[i]
		b0c.n2 <- dat$m0c[i, 2]
		b1c.n2 <- dat$m1c[i]
		b0c.n3 <- dat$m0c[i, 3]
		b1c.n3 <- dat$m1c[i]
    #
		p1<-cprob1 <- 1/(1 + exp( - (b0c.n1 - b1c.n1 * theta1)))
		p2<-cprob2 <- 1/(1 + exp( - (b0c.n2 - b1c.n2 * theta1)))
		p3<-cprob3 <- 1/(1 + exp( - (b0c.n3 - b1c.n3 * theta1)))
   	#
    #
 	resp1<-resp2<-resp3<-resp4<-1
		#
 info1<- -(-resp1*b1c.n1^2*p1*(1-p1))*p1
		#
 info2<- ((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1c.n2^2*((1-p2)*p2)+p1*(1-p1)*b1c.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1c.n2-p1*(1-p1)*b1c.n1)*(p2*(1-p2)*b1c.n2-p1*(1-p1)*b1c.n1))*(p2-p1)
		#
 info3<- ((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1c.n3^2*((1-p3)*p3)+p2*(1-p2)*b1c.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1c.n3-p2*(1-p2)*b1c.n2)*(p3*(1-p3)*b1c.n3-p2*(1-p2)*b1c.n2))*(p3-p2)
		#
 info4<- -(-resp4*b1c.n3^2*p3*(1-p3))*(1-p3)
		#
		info.tot<- -(-resp1*b1c.n1^2*p1*(1-p1))*p1+
((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1c.n2^2*((1-p2)*p2)+p1*(1-p1)*b1c.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1c.n2-p1*(1-p1)*b1c.n1)*(p2*(1-p2)*b1c.n2-p1*(1-p1)*b1c.n1))*(p2-p1)+
((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1c.n3^2*((1-p3)*p3)+p2*(1-p2)*b1c.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1c.n3-p2*(1-p2)*b1c.n2)*(p3*(1-p3)*b1c.n3-p2*(1-p2)*b1c.n2))*(p3-p2)
-(-resp4*b1c.n3^2*p3*(1-p3))*(1-p3)
		#
		plot(theta1, info.tot, main = " ", type = "n", ylab = "Information", xlab = expression(paste(theta[1]," (Mobility)")),ylim=c(0,max(info.tot)))

		lines(theta1, info1, col = 2, lwd = 2,lty=2)
		lines(theta1, info2, col = 3, lwd = 2,lty=2)
		lines(theta1, info3, col = 4, lwd = 2,lty=2)
		lines(theta1, info4, col = 5, lwd = 2,lty=2)
		lines(theta1, info.tot, col = 1, lwd = 2)
    #
		leg.txt <- c("0", "1", "2", "3", "total")
		legend("topleft", leg.txt, col = c(2, 3, 4, 5, 1), lwd = c(2, 2, 2, 2, 2),lty = c(2, 2, 2, 2, 1), cex = 0.8)
	}
	 dev.off()

 }
  plot.mob.iic(dat=comb.mob.sim)


###############
#  ADL plots  #
###############

#win.sim.adl<-comb.sep2010.theta.mix4a

#comb.adl.sim<-list(a0=a0.adl,a1=a1.adl,a0a=a0a.adl,a1a=a1a.adl,a0b=a0b.adl,a1b=a1b.adl,a0c=a0c.adl,a1c=a1c.adl,a0d=a0d.adl,a1d=a1d.adl,s=s)

plot.adl.iic<-function(dat = comb.adl.sim)
{
par(mfrow = c(1, 1))
 omega <- seq(-4, 4, 0.1)
 nu2 <- rnorm(1,0,1)
 theta2 <-  comb.adl.sim$s[2]*omega + sqrt(1-(comb.adl.sim$s[2]*comb.adl.sim$s[2]))*nu2

	#
png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/ADL/IIC/item%d.png",height=5,width=5,units="in",res=300)

for(i in 1:9) {

		#
		dat$a1 <- unlist(dat$a1)
		#
		b0.n1 <- dat$a0[i,1]
		b1.n1 <- dat$a1[i]
		b0.n2 <- dat$a0[i,2]
		b1.n2 <- dat$a1[i]
		#
		p1<-cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta2)))
		p2<-cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta2)))
    #
     	resp1<-resp2<-resp3<-1
		#
 info1<- -(-resp1*b1.n1^2*p1*(1-p1))*p1
		#
 info2<- ((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1.n2^2*((1-p2)*p2)+p1*(1-p1)*b1.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1.n2-p1*(1-p1)*b1.n1)*(p2*(1-p2)*b1.n2-p1*(1-p1)*b1.n1))*(p2-p1)
		#
 info3<- -(-resp3*b1.n2^2*p2*(1-p2))*(1-p2)
		#
  info.tot<- -(-resp1*b1.n1^2*p1*(1-p1))*p1+
((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1.n2^2*((1-p2)*p2)+p1*(1-p1)*b1.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1.n2-p1*(1-p1)*b1.n1)*(p2*(1-p2)*b1.n2-p1*(1-p1)*b1.n1))*(p2-p1)
-(-resp3*b1.n2^2*p2*(1-p2))*(1-p2)

  #
  plot(theta2, info.tot, main = paste("Item ",i), type = "n", ylab = "Information", xlab = expression(paste(theta[2]," (ADL)")),ylim=c(0,max(info.tot)))

  lines(theta2, info1, col = 2, lwd = 2,lty=2)
  lines(theta2, info2, col = 3, lwd = 2,lty=2)
  lines(theta2, info3, col = 4, lwd = 2,lty=2)
  lines(theta2, info.tot, col = 1, lwd = 2)
  #
    leg.txt <- c("0", "1", "2", "total")
		legend("topleft", leg.txt, col = c(2, 3, 4, 1), lwd = c(2, 2, 2, 2),lty = c(2, 2, 2, 1), cex = 0.8)
	}

	 dev.off()

	 png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/ADL/IIC/item%da.png",height=5,width=5,units="in",res=300)

   	for(i in 1:5) {
		#
		dat$a1a <- unlist(dat$a1a)
		#
		b0a.n1 <- dat$a0a[i]
		b1a.n1 <- dat$a1a[i]
		#
		prob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta2)))
    #
    info<- prob1*(1-prob1)*b1a.n1^2
    #
    plot(theta2, info, main = " ", type = "n", ylab = "Information", xlab = expression(paste(theta[2]," (ADL)")),ylim=c(0,max(info)))
    lines(theta2, info, col = 1, lwd = 2)
    #
	}
	dev.off()


   png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/ADL/IIC/item%db.png",height=5,width=5,units="in",res=300)

   	for(i in 1:11) {

		#
		dat$a1b <- unlist(dat$a1b)
		#
		b0b.n1 <- dat$a0b[i, 1]
		b1b.n1 <- dat$a1b[i]
		b0b.n2 <- dat$a0b[i, 2]
		b1b.n2 <- dat$a1b[i]
		b0b.n3 <- dat$a0b[i, 3]
		b1b.n3 <- dat$a1b[i]
		#
		p1<-cprob1 <- 1/(1 + exp( - (b0b.n1 - b1b.n1 * theta2)))
		p2<-cprob2 <- 1/(1 + exp( - (b0b.n2 - b1b.n2 * theta2)))
		p3<-cprob3 <- 1/(1 + exp( - (b0b.n3 - b1b.n3 * theta2)))
		#
  	resp1<-resp2<-resp3<-resp4<-1
		#
 info1<- -(-resp1*b1b.n1^2*p1*(1-p1))*p1
		#
 info2<- ((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1b.n2^2*((1-p2)*p2)+p1*(1-p1)*b1b.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1))*(p2-p1)
		#
 info3<- ((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1b.n3^2*((1-p3)*p3)+p2*(1-p2)*b1b.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1b.n3-p2*(1-p2)*b1b.n2)*(p3*(1-p3)*b1b.n3-p2*(1-p2)*b1b.n2))*(p3-p2)
		#
 info4<- -(-resp4*b1b.n3^2*p3*(1-p3))*(1-p3)
		#
		info.tot<- -(-resp1*b1b.n1^2*p1*(1-p1))*p1+
((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1b.n2^2*((1-p2)*p2)+p1*(1-p1)*b1b.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1))*(p2-p1)+
((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1b.n3^2*((1-p3)*p3)+p2*(1-p2)*b1b.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1b.n3-p2*(1-p2)*b1b.n2)*(p3*(1-p3)*b1b.n3-p2*(1-p2)*b1b.n2))*(p3-p2)
-(-resp4*b1b.n3^2*p3*(1-p3))*(1-p3)
		#
		plot(theta2, info.tot, main = " ", type = "n", ylab = "Information", xlab = expression(paste(theta[2]," (ADL)")),ylim=c(0,max(info.tot)))
		#
		lines(theta2, info1, col = 2, lwd = 2,lty=2)
		lines(theta2, info2, col = 3, lwd = 2,lty=2)
		lines(theta2, info3, col = 4, lwd = 2,lty=2)
		lines(theta2, info4, col = 5, lwd = 2,lty=2)
		lines(theta2, info.tot, col = 1, lwd = 2)
    #
		leg.txt <- c("0", "1", "2", "3", "total")
		legend("topleft", leg.txt, col = c(2, 3, 4, 5, 1), lwd = c(2, 2, 2, 2, 2),lty = c(2, 2, 2, 2, 1), cex = 0.8)
	}
   dev.off()

   png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/ADL/IIC/item%dc.png",height=5,width=5,units="in",res=300)

  for(i in 1:4)
  {

		#
		dat$a1c <- unlist(dat$a1c)
		#
		b0c.n1 <- dat$a0c[i,1]
		b1c.n1 <- dat$a1c[i]
		b0c.n2 <- dat$a0c[i,2]
		b1c.n2 <- dat$a1c[i]
		b0c.n3 <- dat$a0c[i,3]
		b1c.n3 <- dat$a1c[i]
		b0c.n4 <- dat$a0c[i,4]
		b1c.n4 <- dat$a1c[i]
		#
		p1<-cprob1 <- 1/(1 + exp( - (b0c.n1 - b1c.n1 * theta2)))
		p2<-cprob2 <- 1/(1 + exp( - (b0c.n2 - b1c.n2 * theta2)))
		p3<-cprob3 <- 1/(1 + exp( - (b0c.n3 - b1c.n3 * theta2)))
    p4<-cprob4 <- 1/(1 + exp( - (b0c.n4 - b1c.n4 * theta2)))
    #
 resp1<-resp2<-resp3<-resp4<-resp5<-1
		#
 info1<- -(-resp1*b1c.n1^2*p1*(1-p1))*p1
		#
 info2<- ((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1c.n2^2*((1-p2)*p2)+p1*(1-p1)*b1c.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1c.n2-p1*(1-p1)*b1c.n1)*(p2*(1-p2)*b1c.n2-p1*(1-p1)*b1c.n1))*(p2-p1)
		#
 info3<- ((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1c.n3^2*((1-p3)*p3)+p2*(1-p2)*b1c.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1c.n3-p2*(1-p2)*b1c.n2)*(p3*(1-p3)*b1c.n3-p2*(1-p2)*b1c.n2))*(p3-p2)
		#
 info4<- ((1/(p4-p3)^2)*resp4*((p4-p3)*(p4*(1-p4)*b1c.n4^2*((1-p4)*p4)+p3*(1-p3)*b1c.n3^2*(p3+(1-p3))))+(1/(p4-p3)^2)*(p4*(1-p4)*b1c.n4-p3*(1-p3)*b1c.n3)*(p4*(1-p4)*b1c.n4-p3*(1-p3)*b1c.n3))*(p4-p3)
		#
 info5<- -(-resp5*b1c.n4^2*p4*(1-p4))*(1-p4)

  info.tot<- -(-resp1*b1c.n1^2*p1*(1-p1))*p1+
((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1c.n2^2*((1-p2)*p2)+p1*(1-p1)*b1c.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1c.n2-p1*(1-p1)*b1c.n1)*(p2*(1-p2)*b1c.n2-p1*(1-p1)*b1c.n1))*(p2-p1)+
((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1c.n3^2*((1-p3)*p3)+p2*(1-p2)*b1c.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1c.n3-p2*(1-p2)*b1c.n2)*(p3*(1-p3)*b1c.n3-p2*(1-p2)*b1c.n2))*(p3-p2)+
((1/(p4-p3)^2)*resp4*((p4-p3)*(p4*(1-p4)*b1c.n4^2*((1-p4)*p4)+p3*(1-p3)*b1c.n3^2*(p3+(1-p3))))+(1/(p4-p3)^2)*(p4*(1-p4)*b1c.n4-p3*(1-p3)*b1c.n3)*(p4*(1-p4)*b1c.n4-p3*(1-p3)*b1c.n3))*(p4-p3)
-(-resp5*b1c.n4^2*p4*(1-p4))*(1-p4)
		#
		plot(theta2, info.tot, main = " ", type = "n", ylab = "Information", xlab = expression(paste(theta[2]," (ADL)")),ylim=c(0,max(info.tot)))
		#
		lines(theta2, info1, col = 2, lwd = 2, lty=2)
		lines(theta2, info2, col = 3, lwd = 2, lty=2)
		lines(theta2, info3, col = 4, lwd = 2, lty=2)
		lines(theta2, info4, col = 5, lwd = 2, lty=2)
		lines(theta2, info5, col = 6, lwd = 2, lty=2)
		lines(theta2, info.tot, col = 1, lwd = 2)
		#
		leg.txt <- c("0", "1", "2", "3", "4", "total")
		legend("topleft", leg.txt, col = c(2, 3, 4, 5, 6, 1), lwd = c(2, 2, 2, 2, 2, 2),lty = c(2, 2, 2, 2, 2, 1), cex = 0.8)
  }
	 dev.off()
#	  png(file="d://Local Data//p1thompson//dump graphics baseline inc control//combined//ADL//mixture//item_information_curves//item%01dd.png",height=5,width=5,units="in",res=300)
#		#
#		dat$a1d <- unlist(dat$a1d)
#		#
#		b0d.n1 <- dat$a0d
#		b1d.n1 <- dat$a1d
#
#    #
#		mean <- b0d.n1 + b1d.n1 * theta2
#		#
#
#    resp1 <- 20
#    resp2 <- 50
#    resp3 <- 80
#	#
# 	info1 <- (2/sqrt(2*pi))*(b1f.n1^2)*(tau^2)*((resp1-mean)^2)*exp(-(0.5*tau*(resp1-mean)^2))
#	info2 <- (2/sqrt(2*pi))*(b1f.n1^2)*(tau^2)*((resp2-mean)^2)*exp(-(0.5*tau*(resp2-mean)^2))
#	info3 <- (2/sqrt(2*pi))*(b1f.n1^2)*(tau^2)*((resp3-mean)^2)*exp(-(0.5*tau*(resp3-mean)^2))

#   plot(theta2, info1, main = " ",type="n", ylab = "Information", xlab = "Theta")
#   lines(theta2,info1, lwd=2, col=2)
#   lines(theta2, info2, lwd=2, col=3)
#   lines(theta2, info3, lwd=2, col=5)

#   leg.txt <- c("20", "50", "80")
#   legend(-4, 0.6, leg.txt, col = c(2, 3, 5), lwd = c(2, 2, 2), cex = 0.8)

#	 dev.off()
 }
  plot.adl.iic(dat=comb.adl.sim)

######################
#  Depression plots  #
######################

#win.sim.depression<-comb.sep2010.theta.mix4a

#dat<-comb.depression.sim<-list(d0=d0.depression,d1=d1.depression,d0a=d0a.depression,d1a=d1a.depression,d0b=d0b.depression,d1b=d1b.depression,d0c=d0c.depression,d1c=d1c.depression,s=s)

plot.depression.iic<-function(dat = dat)
{
par(mfrow = c(1, 1))
omega <- seq(-4, 4, 0.1)
 nu3 <- rnorm(1,0,1)
 theta3 <-  dat$s[3]*omega + sqrt(1-(dat$s[3]*dat$s[3]))*nu3
	p.new <- runif(length(theta3), 0, 1)
	#
png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Depression/IIC/item%d.png",height=5,width=5,units="in",res=300)
for(i in 1:26)
{
		#
		dat$d1 <- unlist(dat$d1)
		#
		b0.n1 <- dat$d0[i]
		b1.n1 <- dat$d1[i]
		#
    prob1 <- 1/(1 + exp( - (b0.n1 + b1.n1 * theta3)))
    info<- prob1*(1-prob1)*b1.n1^2
    #
    plot(theta3, info, main = " ", type = "n", ylab = "Information", xlab = expression(paste(theta[3]," (Depression)")),ylim=c(0,max(info)))
		lines(theta3, info, col = 2, lwd = 2)
}
	dev.off()

	 png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Depression/IIC/item%da.png",height=5,width=5,units="in",res=300)

    for(i in 1:6)
    {
		#
		dat$d1a <- unlist(dat$d1a)
		#
		b0a.n1 <- dat$d0a[i,1]
		b1a.n1 <- dat$d1a[i]
		b0a.n2 <- dat$d0a[i,2]
		b1a.n2 <- dat$d1a[i]
		b0a.n3 <- dat$d0a[i,3]
		b1a.n3 <- dat$d1a[i]
		#
		p1<-cprob1 <- 1/(1 + exp( - (b0a.n1 - b1a.n1 * theta3)))
		p2<-cprob2 <- 1/(1 + exp( - (b0a.n2 - b1a.n2 * theta3)))
		p3<-cprob3 <- 1/(1 + exp( - (b0a.n3 - b1a.n3 * theta3)))
		#
 	resp1<-resp2<-resp3<-resp4<-1
		#
 info1<- -(-resp1*b1a.n1^2*p1*(1-p1))*p1
		#
 info2<- ((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1a.n2^2*((1-p2)*p2)+p1*(1-p1)*b1a.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1a.n2-p1*(1-p1)*b1a.n1)*(p2*(1-p2)*b1a.n2-p1*(1-p1)*b1a.n1))*(p2-p1)
		#
 info3<- ((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1a.n3^2*((1-p3)*p3)+p2*(1-p2)*b1a.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1a.n3-p2*(1-p2)*b1a.n2)*(p3*(1-p3)*b1a.n3-p2*(1-p2)*b1a.n2))*(p3-p2)
		#
 info4<- -(-resp4*b1a.n3^2*p3*(1-p3))*(1-p3)
		#
		info.tot<- -(-resp1*b1a.n1^2*p1*(1-p1))*p1+
((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1a.n2^2*((1-p2)*p2)+p1*(1-p1)*b1a.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1a.n2-p1*(1-p1)*b1a.n1)*(p2*(1-p2)*b1a.n2-p1*(1-p1)*b1a.n1))*(p2-p1)+
((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1a.n3^2*((1-p3)*p3)+p2*(1-p2)*b1a.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1a.n3-p2*(1-p2)*b1a.n2)*(p3*(1-p3)*b1a.n3-p2*(1-p2)*b1a.n2))*(p3-p2)
-(-resp4*b1a.n3^2*p3*(1-p3))*(1-p3)
    #
    plot(theta3, info.tot, main =" ", type = "n", ylab = "Information", xlab = expression(paste(theta[3]," (Depression)")),ylim=c(0,max(info.tot)))
    #
		lines(theta3, info1, col = 2, lwd = 2, lty=2)
		lines(theta3, info2, col = 3, lwd = 2, lty=2)
		lines(theta3, info3, col = 4, lwd = 2, lty=2)
		lines(theta3, info4, col = 5, lwd = 2, lty=2)
		lines(theta3, info.tot, col = 1, lwd = 2)
    #
    leg.txt <- c("0", "1", "2","3","total")
		legend("topleft", leg.txt, col = c(2, 3, 4, 5,1), lwd = c(2, 2, 2, 2,1),lty = c(2, 2, 2, 2, 1), cex = 0.8)
    }

	 dev.off()

  png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Depression/IIC/item1b.png",height=5,width=5,units="in",res=300)

		#
		dat$d1b <- unlist(dat$d1b)
		#
		b0b.n1 <- dat$d0b[1]
		b1b.n1 <- dat$d1b
		b0b.n2 <- dat$d0b[2]
		b1b.n2 <- dat$d1b
		b0b.n3 <- dat$d0b[3]
		b1b.n3 <- dat$d1b
		b0b.n4 <- dat$d0b[4]
		b1b.n4 <- dat$d1b
		#
		p1<-cprob1 <- 1/(1 + exp( - (b0b.n1 - b1b.n1 * theta3)))
		p2<-cprob2 <- 1/(1 + exp( - (b0b.n2 - b1b.n2 * theta3)))
		p3<-cprob3 <- 1/(1 + exp( - (b0b.n3 - b1b.n3 * theta3)))
		p4<-cprob4 <- 1/(1 + exp( - (b0b.n4 - b1b.n4 * theta3)))
		#
  resp1<-resp2<-resp3<-resp4<-resp5<-1
		#
 info1<- -(-resp1*b1b.n1^2*p1*(1-p1))*p1
		#
 info2<- ((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1b.n2^2*((1-p2)*p2)+p1*(1-p1)*b1b.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1))*(p2-p1)
		#
 info3<- ((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1b.n3^2*((1-p3)*p3)+p2*(1-p2)*b1b.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1b.n3-p2*(1-p2)*b1b.n2)*(p3*(1-p3)*b1b.n3-p2*(1-p2)*b1b.n2))*(p3-p2)
		#
 info4<- ((1/(p4-p3)^2)*resp4*((p4-p3)*(p4*(1-p4)*b1b.n4^2*((1-p4)*p4)+p3*(1-p3)*b1b.n3^2*(p3+(1-p3))))+(1/(p4-p3)^2)*(p4*(1-p4)*b1b.n4-p3*(1-p3)*b1b.n3)*(p4*(1-p4)*b1b.n4-p3*(1-p3)*b1b.n3))*(p4-p3)
		#
 info5<- -(-resp5*b1b.n4^2*p4*(1-p4))*(1-p4)

  info.tot<- -(-resp1*b1b.n1^2*p1*(1-p1))*p1+
((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1b.n2^2*((1-p2)*p2)+p1*(1-p1)*b1b.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1))*(p2-p1)+
((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1b.n3^2*((1-p3)*p3)+p2*(1-p2)*b1b.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1b.n3-p2*(1-p2)*b1b.n2)*(p3*(1-p3)*b1b.n3-p2*(1-p2)*b1b.n2))*(p3-p2)+
((1/(p4-p3)^2)*resp4*((p4-p3)*(p4*(1-p4)*b1b.n4^2*((1-p4)*p4)+p3*(1-p3)*b1b.n3^2*(p3+(1-p3))))+(1/(p4-p3)^2)*(p4*(1-p4)*b1b.n4-p3*(1-p3)*b1b.n3)*(p4*(1-p4)*b1b.n4-p3*(1-p3)*b1b.n3))*(p4-p3)
-(-resp5*b1b.n4^2*p4*(1-p4))*(1-p4)
    #
    plot(theta3, info.tot, main =" ", type = "n", ylab = "Information", xlab = expression(paste(theta[3]," (Depression)")),ylim=c(0,max(info.tot)))
    #
		lines(theta3, info1, col = 2, lwd = 2, lty = 2)
		lines(theta3, info2, col = 3, lwd = 2, lty = 2)
		lines(theta3, info3, col = 4, lwd = 2, lty = 2)
		lines(theta3, info4, col = 5, lwd = 2, lty = 2)
		lines(theta3, info5, col = 6, lwd = 2, lty = 2)
		lines(theta3, info.tot, col = 1, lwd = 2)
    #
    leg.txt <- c("1", "2","3", "4", "5", "total")
		legend("topleft", leg.txt, col = c(2, 3, 4, 5, 6, 1), lwd = c(2, 2, 2, 2, 2, 2), lty = c(2, 2, 2, 2, 2, 1), cex = 0.8)

	 dev.off()

 	 png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Depression/IIC/item%dc.png",height=5,width=5,units="in",res=300)

    for(i in 1:9)
    {

		#
		dat$d1c <- unlist(dat$d1c)
		#
		b0c.n1 <- dat$d0c[i,1]
		b1c.n1 <- dat$d1c[i]
		b0c.n2 <- dat$d0c[i,2]
    b1c.n2 <- dat$d1c[i]
		#
		p1<-cprob1 <- 1/(1 + exp( - (b0c.n1 - b1c.n1 * theta3)))
		p2<-cprob2 <- 1/(1 + exp( - (b0c.n2 - b1c.n2 * theta3)))
		#
 	resp1<-resp2<-resp3<-1
		#
 info1<- -(-resp1*b1c.n1^2*p1*(1-p1))*p1
		#
 info2<- ((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1c.n2^2*((1-p2)*p2)+p1*(1-p1)*b1c.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1c.n2-p1*(1-p1)*b1c.n1)*(p2*(1-p2)*b1c.n2-p1*(1-p1)*b1c.n1))*(p2-p1)
		#
 info3<- -(-resp3*b1c.n2^2*p2*(1-p2))*(1-p2)
		#
  info.tot<- -(-resp1*b1c.n1^2*p1*(1-p1))*p1+
((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1c.n2^2*((1-p2)*p2)+p1*(1-p1)*b1c.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1c.n2-p1*(1-p1)*b1c.n1)*(p2*(1-p2)*b1c.n2-p1*(1-p1)*b1c.n1))*(p2-p1)
-(-resp3*b1c.n2^2*p2*(1-p2))*(1-p2)

    #
    plot(theta3, info.tot, main =" ", type = "n", ylab = "Information", xlab = expression(paste(theta[3]," (Depression)")),ylim=c(0,max(info.tot)))
    #
		lines(theta3, info1, col = 2, lwd = 2, lty=2)
		lines(theta3, info2, col = 3, lwd = 2, lty=2)
		lines(theta3, info3, col = 4, lwd = 2, lty=2)
		lines(theta3, info.tot, col = 1, lwd = 2)
		#
		leg.txt <- c("0", "1", "2", "total")
		legend("topleft", leg.txt, col = c(2, 3, 4, 1), lwd = c(2, 2, 2, 1), cex = 0.8)
    }

	 dev.off()
}
  plot.depression.iic(dat=comb.depression.sim)


################
#  Pain plots  #
################

#win.sim.pain<-comb.sep2010.theta.mix4a

#comb.pain.sim<-list(p0=p0.pain,p1=p1.pain,p0a=p0a.pain,p1a=p1a.pain,p0b=p0b.pain,p1b=p1b.pain,p0c=p0c.pain,p1c=p1c.pain,p0d=p0d.pain,p1d=p1d.pain,p0f=p0f.pain,p1f=p1f.pain,s=s)

plot.pain.iic<-function(dat = comb.pain.sim)
{
par(mfrow = c(1, 1))
  omega <- seq(-4, 4, 0.1)
 nu4 <- rnorm(1,0,1)
 theta4 <-  comb.pain.sim$s[4]*omega + sqrt(1-(comb.pain.sim$s[4]*comb.pain.sim$s[4]))*nu4
	p.new <- runif(length(theta4), 0, 1)
	#


png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Pain/IIC/item%d.png",height=5,width=5,units="in",res=300)

		#
		dat$p1 <- unlist(dat$p1)
		#
		b0.n1 <- dat$p0[1]
		b1.n1 <- 1
		b0.n2 <- dat$p0[2]
		b1.n2 <- 1
		#
		p1<-cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta4)))
		p2<-cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta4)))
    #
 	resp1<-resp2<-resp3<-1
		#
 info1<- -(-resp1*b1.n1^2*p1*(1-p1))*p1
		#
 info2<- ((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1.n2^2*((1-p2)*p2)+p1*(1-p1)*b1.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1.n2-p1*(1-p1)*b1.n1)*(p2*(1-p2)*b1.n2-p1*(1-p1)*b1.n1))*(p2-p1)
		#
 info3<- -(-resp3*b1.n2^2*p2*(1-p2))*(1-p2)
		#
  info.tot<- -(-resp1*b1.n1^2*p1*(1-p1))*p1+
((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1.n2^2*((1-p2)*p2)+p1*(1-p1)*b1.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1.n2-p1*(1-p1)*b1.n1)*(p2*(1-p2)*b1.n2-p1*(1-p1)*b1.n1))*(p2-p1)
-(-resp3*b1.n2^2*p2*(1-p2))*(1-p2)
		#
		plot(theta4, info.tot, main = " ", type = "n", ylab = "Information", xlab = expression(paste(theta[4]," (Pain)")),ylim=c(0,max(info.tot)))
		#
		lines(theta4, info1, col = 2, lwd = 2, lty = 2)
		lines(theta4, info2, col = 3, lwd = 2, lty = 2)
		lines(theta4, info3, col = 4, lwd = 2, lty = 2)
		lines(theta4, info.tot, col = 1, lwd = 2, lty=1)
		#
		leg.txt <- c("0", "1", "2", "total")
		legend("topright", leg.txt, col = c(2, 3, 4, 1), lwd = c(2, 2, 2, 2), cex = 0.8)

	dev.off()

  png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Pain/IIC/item%da.png",height=5,width=5,units="in",res=300)
  for(i in 1:2)
  {
		#
		dat$p1a <- unlist(dat$p1a)
		#
		b0a.n1 <- dat$p0a[i,1]
		b1a.n1 <- dat$p1a[i]
		b0a.n2 <- dat$p0a[i,2]
		b1a.n2 <- dat$p1a[i]
		b0a.n3 <- dat$p0a[i,3]
		b1a.n3 <- dat$p1a[i]
		#
		p1<-cprob1 <- 1/(1 + exp( - (b0a.n1 - b1a.n1 * theta4)))
		p2<-cprob2 <- 1/(1 + exp( - (b0a.n2 - b1a.n2 * theta4)))
		p3<-cprob3 <- 1/(1 + exp( - (b0a.n3 - b1a.n3 * theta4)))
    #
 	resp1<-resp2<-resp3<-resp4<-1
		#
 info1<- -(-resp1*b1a.n1^2*p1*(1-p1))*p1
		#
 info2<- ((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1a.n2^2*((1-p2)*p2)+p1*(1-p1)*b1a.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1a.n2-p1*(1-p1)*b1a.n1)*(p2*(1-p2)*b1a.n2-p1*(1-p1)*b1a.n1))*(p2-p1)
		#
 info3<- ((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1a.n3^2*((1-p3)*p3)+p2*(1-p2)*b1a.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1a.n3-p2*(1-p2)*b1a.n2)*(p3*(1-p3)*b1a.n3-p2*(1-p2)*b1a.n2))*(p3-p2)
		#
 info4<- -(-resp4*b1a.n3^2*p3*(1-p3))*(1-p3)
		#
		info.tot<- -(-resp1*b1a.n1^2*p1*(1-p1))*p1+
((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1a.n2^2*((1-p2)*p2)+p1*(1-p1)*b1a.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1a.n2-p1*(1-p1)*b1a.n1)*(p2*(1-p2)*b1a.n2-p1*(1-p1)*b1a.n1))*(p2-p1)+
((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1a.n3^2*((1-p3)*p3)+p2*(1-p2)*b1a.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1a.n3-p2*(1-p2)*b1a.n2)*(p3*(1-p3)*b1a.n3-p2*(1-p2)*b1a.n2))*(p3-p2)
-(-resp4*b1a.n3^2*p3*(1-p3))*(1-p3)
    #
    plot(theta4, info.tot, main = " ", type = "n", ylab = "Information", xlab = expression(paste(theta[4]," (Pain)")),ylim=c(0,max(info.tot)))
		#
		lines(theta4, info1, col = 2, lwd = 2, lty = 2)
		lines(theta4, info2, col = 3, lwd = 2, lty = 2)
		lines(theta4, info3, col = 4, lwd = 2, lty = 2)
		lines(theta4, info4, col = 5, lwd = 2, lty = 2)
		lines(theta4, info.tot, col = 1, lwd = 2)
		#
		leg.txt <- c("0", "1", "2", "3", "total")
		legend("topright", leg.txt, col = c(2, 3, 4, 5, 1), lwd = c(2, 2, 2, 2, 2), lty = c(2, 2, 2, 2, 1), cex = 0.8)
  }
	dev.off()

   png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Pain/IIC/item1b.png",height=5,width=5,units="in",res=300)


		#
		#dat$p1b <- unlist(dat$p1b)
		#
		b0b.n1 <- dat$p0b[1]
		b1b.n1 <- dat$p1b
		b0b.n2 <- dat$p0b[2]
		b1b.n2 <- dat$p1b
		b0b.n3 <- dat$p0b[3]
		b1b.n3 <- dat$p1b
		b0b.n4 <- dat$p0b[4]
		b1b.n4 <- dat$p1b
		#
		p1<-cprob1 <- 1/(1 + exp( - (b0b.n1 - b1b.n1 * theta4)))
		p2<-cprob2 <- 1/(1 + exp( - (b0b.n2 - b1b.n2 * theta4)))
		p3<-cprob3 <- 1/(1 + exp( - (b0b.n3 - b1b.n3 * theta4)))
		p4<-cprob4 <- 1/(1 + exp( - (b0b.n4 - b1b.n4 * theta4)))
		#
  resp1<-resp2<-resp3<-resp4<-resp5<-1
		#
 info1<- -(-resp1*b1b.n1^2*p1*(1-p1))*p1
		#
 info2<- ((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1b.n2^2*((1-p2)*p2)+p1*(1-p1)*b1b.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1))*(p2-p1)
		#
 info3<- ((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1b.n3^2*((1-p3)*p3)+p2*(1-p2)*b1b.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1b.n3-p2*(1-p2)*b1b.n2)*(p3*(1-p3)*b1b.n3-p2*(1-p2)*b1b.n2))*(p3-p2)
		#
 info4<- ((1/(p4-p3)^2)*resp4*((p4-p3)*(p4*(1-p4)*b1b.n4^2*((1-p4)*p4)+p3*(1-p3)*b1b.n3^2*(p3+(1-p3))))+(1/(p4-p3)^2)*(p4*(1-p4)*b1b.n4-p3*(1-p3)*b1b.n3)*(p4*(1-p4)*b1b.n4-p3*(1-p3)*b1b.n3))*(p4-p3)
		#
 info5<- -(-resp5*b1b.n4^2*p4*(1-p4))*(1-p4)

  info.tot<- -(-resp1*b1b.n1^2*p1*(1-p1))*p1+
((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1b.n2^2*((1-p2)*p2)+p1*(1-p1)*b1b.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1))*(p2-p1)+
((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1b.n3^2*((1-p3)*p3)+p2*(1-p2)*b1b.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1b.n3-p2*(1-p2)*b1b.n2)*(p3*(1-p3)*b1b.n3-p2*(1-p2)*b1b.n2))*(p3-p2)+
((1/(p4-p3)^2)*resp4*((p4-p3)*(p4*(1-p4)*b1b.n4^2*((1-p4)*p4)+p3*(1-p3)*b1b.n3^2*(p3+(1-p3))))+(1/(p4-p3)^2)*(p4*(1-p4)*b1b.n4-p3*(1-p3)*b1b.n3)*(p4*(1-p4)*b1b.n4-p3*(1-p3)*b1b.n3))*(p4-p3)
-(-resp5*b1b.n4^2*p4*(1-p4))*(1-p4)
		#
		plot(theta4, info.tot, main = " ", type = "n", ylab = "Information", xlab = expression(paste(theta[4]," (Pain)")),ylim=c(0,max(info.tot)))
    #
		lines(theta4, info1, col = 2, lwd = 2, lty = 2)
		lines(theta4, info2, col = 3, lwd = 2, lty = 2)
		lines(theta4, info3, col = 4, lwd = 2, lty = 2)
		lines(theta4, info4, col = 5, lwd = 2, lty = 2)
		lines(theta4, info5, col = 6, lwd = 2, lty = 2)
		lines(theta4, info.tot, col = 1, lwd = 2)
    #
		leg.txt <- c("0", "1", "2", "3", "4", "total")
		legend("topright", leg.txt, col = c(2, 3, 4, 5, 6, 1), lwd = c(2, 2, 2, 2, 2, 2), lty = c(2, 2, 2, 2, 2, 1), cex = 0.8)

   dev.off()

   png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Pain/IIC/item1c.png",height=5,width=5,units="in",res=300)
		#
		#dat$d1c <- unlist(dat$d1c)
		#
		b0c.n1 <- dat$p0c
		b1c.n1 <- dat$p1c
		#
		prob1 <- 1/(1 + exp( - (b0c.n1 + b1c.n1 * theta4)))
		info<- prob1*(1-prob1)*b1c.n1^2
    #
    plot(theta4, info, main = " ", type = "n", ylab = "Probability", xlab = expression(paste(theta[4]," (Pain)")),ylim=c(0,max(info.tot)))
		lines(theta4, info, col = 1, lwd = 2)

	dev.off()

     png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Pain/IIC/item1d.png",height=5,width=5,units="in",res=300)
		#
		dat$p1d <- unlist(dat$p1d)
		#
		b0d.n1 <- dat$p0d[1]
		b1d.n1 <- dat$p1d
		b0d.n2 <- dat$p0d[2]
		b1d.n2 <- dat$p1d
		b0d.n3 <- dat$p0d[3]
		b1d.n3 <- dat$p1d
		b0d.n4 <- dat$p0d[4]
		b1d.n4 <- dat$p1d
		b0d.n5 <- dat$p0d[5]
		b1d.n5 <- dat$p1d
		b0d.n6 <- dat$p0d[6]
		b1d.n6 <- dat$p1d
		#
		p1<-cprob1 <- 1/(1 + exp( - (b0d.n1 - b1d.n1 * theta4)))
		p2<-cprob2 <- 1/(1 + exp( - (b0d.n2 - b1d.n2 * theta4)))
		p3<-cprob3 <- 1/(1 + exp( - (b0d.n3 - b1d.n3 * theta4)))
		p4<-cprob4 <- 1/(1 + exp( - (b0d.n4 - b1d.n4 * theta4)))
		p5<-cprob5 <- 1/(1 + exp( - (b0d.n5 - b1d.n5 * theta4)))
		p6<-cprob6 <- 1/(1 + exp( - (b0d.n6 - b1d.n6 * theta4)))
		#
  	resp1<-resp2<-resp3<-resp4<-resp5<-resp6<-resp7<-1
		#
 info1<- -(-resp1*b1d.n1^2*p1*(1-p1))*p1
		#
 info2<- ((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1d.n2^2*((1-p2)*p2)+p1*(1-p1)*b1d.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1d.n2-p1*(1-p1)*b1d.n1)*(p2*(1-p2)*b1d.n2-p1*(1-p1)*b1d.n1))*(p2-p1)
		#
 info3<- ((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1d.n3^2*((1-p3)*p3)+p2*(1-p2)*b1d.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1d.n3-p2*(1-p2)*b1d.n2)*(p3*(1-p3)*b1d.n3-p2*(1-p2)*b1d.n2))*(p3-p2)
		#
 info4<- ((1/(p4-p3)^2)*resp4*((p4-p3)*(p4*(1-p4)*b1d.n4^2*((1-p4)*p4)+p3*(1-p3)*b1d.n3^2*(p3+(1-p3))))+(1/(p4-p3)^2)*(p4*(1-p4)*b1d.n4-p3*(1-p3)*b1d.n3)*(p4*(1-p4)*b1d.n4-p3*(1-p3)*b1d.n3))*(p4-p3)
    #
 info5<- ((1/(p5-p4)^2)*resp5*((p5-p4)*(p5*(1-p5)*b1d.n5^2*((1-p5)*p5)+p4*(1-p4)*b1d.n4^2*(p4+(1-p4))))+(1/(p5-p4)^2)*(p5*(1-p5)*b1d.n5-p4*(1-p4)*b1d.n4)*(p5*(1-p5)*b1d.n5-p4*(1-p4)*b1d.n4))*(p5-p4)
		#
 info6<- ((1/(p6-p5)^2)*resp6*((p6-p5)*(p6*(1-p6)*b1d.n6^2*((1-p6)*p6)+p5*(1-p5)*b1d.n5^2*(p5+(1-p5))))+(1/(p6-p5)^2)*(p6*(1-p6)*b1d.n6-p5*(1-p5)*b1d.n5)*(p6*(1-p6)*b1d.n6-p5*(1-p5)*b1d.n5))*(p6-p5)
		#
 info7<- -(-resp7*b1d.n6^2*p6*(1-p6))*(1-p6)
    #
  info.tot<- -(-resp1*b1d.n1^2*p1*(1-p1))*p1 +
((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1d.n2^2*((1-p2)*p2)+p1*(1-p1)*b1d.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1d.n2-p1*(1-p1)*b1d.n1)*(p2*(1-p2)*b1d.n2-p1*(1-p1)*b1d.n1))*(p2-p1)+
((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1d.n3^2*((1-p3)*p3)+p2*(1-p2)*b1d.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1d.n3-p2*(1-p2)*b1d.n2)*(p3*(1-p3)*b1d.n3-p2*(1-p2)*b1d.n2))*(p3-p2)+
((1/(p4-p3)^2)*resp4*((p4-p3)*(p4*(1-p4)*b1d.n4^2*((1-p4)*p4)+p3*(1-p3)*b1d.n3^2*(p3+(1-p3))))+(1/(p4-p3)^2)*(p4*(1-p4)*b1d.n4-p3*(1-p3)*b1d.n3)*(p4*(1-p4)*b1d.n4-p3*(1-p3)*b1d.n3))*(p4-p3)+
((1/(p5-p4)^2)*resp5*((p5-p4)*(p5*(1-p5)*b1d.n5^2*((1-p5)*p5)+p4*(1-p4)*b1d.n4^2*(p4+(1-p4))))+(1/(p5-p4)^2)*(p5*(1-p5)*b1d.n5-p4*(1-p4)*b1d.n4)*(p5*(1-p5)*b1d.n5-p4*(1-p4)*b1d.n4))*(p5-p4)+
((1/(p6-p5)^2)*resp6*((p6-p5)*(p6*(1-p6)*b1d.n6^2*((1-p6)*p6)+p5*(1-p5)*b1d.n5^2*(p5+(1-p5))))+(1/(p6-p5)^2)*(p6*(1-p6)*b1d.n6-p5*(1-p5)*b1d.n5)*(p6*(1-p6)*b1d.n6-p5*(1-p5)*b1d.n5))*(p6-p5)
 -(-resp7*b1d.n6^2*p6*(1-p6))*(1-p6)

		#
		l.min<-min(c(info1,info2,info3,info4,info5,info6,info7))
		l.max<-max(c(info1,info2,info3,info4,info5,info6,info7,info.tot))
		lims<-c(l.min,l.max)
    #
   	plot(theta4, info.tot,ylim=lims, main = " ", type = "n", ylab = "Information", xlab = expression(paste(theta[4]," (Pain)")))
    #
		lines(theta4, info1, col = 2, lwd = 2, lty=2)
		lines(theta4, info2, col = 3, lwd = 2, lty=2)
		lines(theta4, info3, col = 4, lwd = 2, lty=2)
		lines(theta4, info4, col = 5, lwd = 2, lty=2)
		lines(theta4, info5, col = 6, lwd = 2, lty=2)
		lines(theta4, info6, col = 7, lwd = 2, lty=2)
		lines(theta4, info7, col = 8, lwd = 2, lty=2)
		lines(theta4, info.tot, col = 1, lwd = 2)
    #
		leg.txt <- c("1", "1.5", "2", "2.5", "3", "4", "5", "total")
		legend("topright", leg.txt, col = c(2, 3, 4, 5, 6, 7, 8, 1), lwd = c(2, 2, 2, 2, 2, 2, 2, 2), cex = 0.8)
   dev.off()

#    png(file="d://Local Data//p1thompson//dump graphics baseline inc control//combined//pain//mixture//item_information_curves//item1f.png",height=5,width=5,units="in",res=300)
#    #
#		#
#		dat$p1f <- unlist(dat$p1f)
#		#
#		b0f.n1 <- dat$p0f
#		b1f.n1 <- dat$p1f
#		#
#		mean<-b0f.n1 + b1f.n1 * theta4
#    #
#    plot(theta4, mean, main = " ",type="n", ylab = "Information", xlab = "Theta")
#    #
#    resp1 <- 20
#    resp2 <- 50
#    resp3 <- 80
	#
# 	info1 <- (2/sqrt(2*pi))*(b1f.n1^2)*(tau^2)*((resp1-mean)^2)*exp(-(0.5*tau*(resp1-mean)^2))
#	info2 <- (2/sqrt(2*pi))*(b1f.n1^2)*(tau^2)*((resp2-mean)^2)*exp(-(0.5*tau*(resp2-mean)^2))
#	info3 <- (2/sqrt(2*pi))*(b1f.n1^2)*(tau^2)*((resp3-mean)^2)*exp(-(0.5*tau*(resp3-mean)^2))

#  lines(theta4, info1, col = "red", lwd = 2, lty=2)
#  lines(theta4, info2, col = "blue", lwd = 2, lty=2)
#  lines(theta4, info3, col = "purple", lwd = 2, lty=2)
#	  dev.off()
	  #

 }
  plot.pain.iic(dat=comb.pain.sim)


#####################
#  Cognition plots  #
#####################
#win.sim.cognition1<-comb.sep2010.theta.mix4a

#comb.cognition.sim<-list(c0=c0.cognition,c1=c1.cognition,c0a=c0a.cognition,c1a=c1a.cognition,c0b=c0b.cognition,c1b=c1b.cognition,s=s)

plot.cognition.iic<-function(dat = snames)
{
par(mfrow = c(1, 1))
omega <- seq(-4, 4, 0.1)
 nu5 <- rnorm(1,0,1)
 theta5 <-  comb.cognition.sim$s[5]*omega + sqrt(1-(comb.cognition.sim$s[5]*comb.cognition.sim$s[5]))*nu5


	 png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Cognition/IIC/item%d.png",height=5,width=5,units="in",res=300)

    for(i in 1:23)
    {

		#
		dat$c1 <- unlist(dat$c1)
		#
		b0.n1 <- dat$c0[i]
		b1.n1 <- dat$c1[i]
		#
		prob1 <- 1/(1 + exp( - (b0.n1 + b1.n1 * theta5)))
		info<- prob1*(1-prob1)*b1.n1^2

		plot(theta5, info, main =" ", type = "n", ylab = "Information", xlab = expression(paste(theta[5]," (Cognition)")),ylim=c(0,max(info)))
		lines(theta5, info, col = 1, lwd = 2)
    }
	 dev.off()

   png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Cognition/IIC/item1a.png",height=5,width=5,units="in",res=300)
  for(i in 1:8)
  {
		#
		dat$c1a <- unlist(dat$c1a)
		#
		b0a.n1 <- dat$c0a[i,1]
		b1a.n1 <- dat$c1a[i]
		b0a.n2 <- dat$c0a[i,2]
		b1a.n2 <- dat$c1a[i]
		b0a.n3 <- dat$c0a[i,3]
		b1a.n3 <- dat$c1a[i]
		#
		p1<-cprob1 <- 1/(1 + exp( - (b0a.n1 - b1a.n1 * theta5)))
		p2<-cprob2 <- 1/(1 + exp( - (b0a.n2 - b1a.n2 * theta5)))
		p3<-cprob3 <- 1/(1 + exp( - (b0a.n3 - b1a.n3 * theta5)))
    #
  resp1<-resp2<-resp3<-resp4<-1
		#
 info1<- -(-resp1*b1a.n1^2*p1*(1-p1))*p1
		#
 info2<- ((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1a.n2^2*((1-p2)*p2)+p1*(1-p1)*b1a.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1a.n2-p1*(1-p1)*b1a.n1)*(p2*(1-p2)*b1a.n2-p1*(1-p1)*b1a.n1))*(p2-p1)
		#
 info3<- ((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1a.n3^2*((1-p3)*p3)+p2*(1-p2)*b1a.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1a.n3-p2*(1-p2)*b1a.n2)*(p3*(1-p3)*b1a.n3-p2*(1-p2)*b1a.n2))*(p3-p2)
		#
 info4<- -(-resp4*b1a.n3^2*p3*(1-p3))*(1-p3)
		#
		info.tot<- -(-resp1*b1a.n1^2*p1*(1-p1))*p1+
((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1a.n2^2*((1-p2)*p2)+p1*(1-p1)*b1a.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1a.n2-p1*(1-p1)*b1a.n1)*(p2*(1-p2)*b1a.n2-p1*(1-p1)*b1a.n1))*(p2-p1)+
((1/(p3-p2)^2)*resp3*((p3-p2)*(p3*(1-p3)*b1a.n3^2*((1-p3)*p3)+p2*(1-p2)*b1a.n2^2*(p2+(1-p2))))+(1/(p3-p2)^2)*(p3*(1-p3)*b1a.n3-p2*(1-p2)*b1a.n2)*(p3*(1-p3)*b1a.n3-p2*(1-p2)*b1a.n2))*(p3-p2)
-(-resp4*b1a.n3^2*p3*(1-p3))*(1-p3)
    #
		#
		plot(theta5, info.tot, main = " ", type = "n", ylab = "Information", xlab = expression(paste(theta[5]," (Cognition)")),ylim=c(0,max(info.tot)))
		#
		lines(theta5, info1, col = 2, lwd = 2, lty = 2)
		lines(theta5, info2, col = 3, lwd = 2, lty = 2)
		lines(theta5, info3, col = 4, lwd = 2, lty = 2)
		lines(theta5, info4, col = 5, lwd = 2, lty = 2)
		lines(theta5, info.tot, col = 1, lwd = 2)
    #
		leg.txt <- c("0", "1", "2", "3", "total")
		legend("topright", leg.txt, col = c(2, 3, 4, 5, 1), lwd = c(2, 2, 2, 2, 2), lty = c(2, 2, 2, 2, 1), cex = 0.8)
  }
	dev.off()

   png(file="c:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Real/Cognition/IIC/item%db.png",height=5,width=5,units="in",res=300)

   	for(i in 1:2) {

		#
		dat$c1b <- unlist(dat$c1b)
		#
		b0b.n1 <- dat$c0b[i, 1]
		b1b.n1 <- dat$c1b[i]
		b0b.n2 <- dat$c0b[i, 2]
		b1b.n2 <- dat$c1b[i]
		#
		p1<-cprob1 <- 1/(1 + exp( - (b0b.n1 - b1b.n1 * theta5)))
		p2<-cprob2 <- 1/(1 + exp( - (b0b.n2 - b1b.n2 * theta5)))
    #
  resp1<-resp2<-resp3<-1
		#
 info1<- -(-resp1*b1b.n1^2*p1*(1-p1))*p1
		#
 info2<- ((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1b.n2^2*((1-p2)*p2)+p1*(1-p1)*b1b.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1))*(p2-p1)
		#
 info3<- -(-resp3*b1b.n2^2*p2*(1-p2))*(1-p2)
		#
  info.tot<- -(-resp1*b1b.n1^2*p1*(1-p1))*p1+
((1/(p2-p1)^2)*resp2*((p2-p1)*(p2*(1-p2)*b1b.n2^2*((1-p2)*p2)+p1*(1-p1)*b1b.n1^2*(p1+(1-p1))))+(1/(p2-p1)^2)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1.n1)*(p2*(1-p2)*b1b.n2-p1*(1-p1)*b1b.n1))*(p2-p1)
-(-resp3*b1b.n2^2*p2*(1-p2))*(1-p2)
    #
    plot(theta5, info.tot, main = " ", type = "n", ylab = "Information", xlab = expression(paste(theta[5]," (Cognition)")),ylim=c(0,max(info.tot)))
		lines(theta5, info1, col = 2, lwd = 2, lty = 2)
		lines(theta5, info2, col = 3, lwd = 2, lty = 2)
		lines(theta5, info3, col = 4, lwd = 2, lty = 2)
		lines(theta5, info.tot, col = 1, lwd = 2)
		#
		leg.txt <- c("0", "1", "2", "total")
		legend("topright", leg.txt, col = c(2, 3, 4, 1), lwd = c(2, 2, 2, 2), lty = c(2, 2, 2, 1), cex = 0.8)
	}
   dev.off()

 }
  plot.cognition.iic(dat=comb.cognition.sim)