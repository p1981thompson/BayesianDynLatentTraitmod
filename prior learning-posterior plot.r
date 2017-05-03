
windows(record=T)
hist(comb.sep2010.theta.mix4miss$sims.list$s[,1],main=expression(paste("Histogram of posterior distribution for",alpha[1])),xlab=expression(paste("model parameter:",alpha[1])))
abline(v=my.lim[1],lty=1,lwd=2)
abline(v=my.lim[2],lty=2,lwd=2)
abline(v=my.lim[3],lty=2,lwd=2)
abline(v=0.8,lty=1,lwd=2,col="red")
legend("topright", c("True", "posterior mean", "95% credible interval"), col = c("red",1,1),lty=c(1,1,2),cex=0.75)
#
hist(comb.sep2010.theta.mix4miss$sims.list$s[,1],freq=F,main=expression(paste("Histogram of posterior distribution for",alpha[1])),xlab=expression(paste("model parameter:",alpha[1])),xlim=c(0,1),border=0)
lines(density(comb.sep2010.theta.mix4miss$sims.list$s[,1]),col="blue")

lines(density(rbeta(10000,2,2)),col="blue",lwd=2)
abline(h=0)
abline(v=my.lim[1],lty=1,lwd=2)
abline(v=my.lim[2],lty=2,lwd=2)
abline(v=my.lim[3],lty=2,lwd=2)
abline(v=0.8,lty=1,lwd=2,col="red")
legend("topleft", c("True", "posterior mean", "95% credible interval","posterior density","prior density"), col = c("red",1,1,"blue","blue"),lty=c(1,1,2,1,2),cex=0.75)

ts.plot(comb.sep2010.theta.mix4miss$sims.list$s[,1],ylim=c(0.6,1),col="grey",ylab=expression(paste("model parameter: ",alpha[1])))
abline(h=my.lim[1],lty=1,lwd=2)
abline(h=my.lim[2],lty=2,lwd=2)
abline(h=my.lim[3],lty=2,lwd=2)
abline(h=0.8,lty=1,lwd=2,col="red")





