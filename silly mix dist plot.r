

my.mix.his<-function(dat1,dat2)
{
    h1 <- hist(dat1, prob = TRUE, plot = FALSE)
    x1 <- seq(min(h1$breaks),max(h1$breaks), length = 100)
    y1 <- dnorm(x1)
    hist(dat1, prob = TRUE, ylim = c(0,1), xlab = expression(Omega), ylab = " ",
        main = " ",border=0,axes=F)
    axis(1,at=seq(-3,3,by=1),cex.axis=1.5,cex.lab=2)
    lines(x1, y1, col = 4,lwd=3)



    h2 <- hist(dat2, prob = TRUE, plot = FALSE)
    x2 <- seq(min(h2$breaks),max(h2$breaks), length = 100)

    one<-pnorm(0,0,1)
    two<-pnorm(0,1,0.5)
#print(one)
print(two)
#print(1-two-one)
	y2 <- dnorm(x2,-1,0.5)
	#y2<-y2/(1-two-one)

    lines(x2, y2, col = 4,lty=2,lwd=3)

# cap.txt<-c("PD","Controls")
#   legend("topright",cap.txt,lty=c(1,2),lwd=2,cex=1.1,bg="white",col=c("blue","blue"))
  abline(h=0)
    abline(v=0,lwd=0.5,lty=2)
      abline(v=-1,lwd=0.5,lty=2)
}


 my.mix.his(dat1=rnorm(100,0,1),dat2=rnorm(400,-1,0.5))

#  dev.off()