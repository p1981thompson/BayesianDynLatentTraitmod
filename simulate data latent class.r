########################################################
#                                                      #
#  simulate cognition trait/class dynamic model (ADNI) #
#                                                      #
########################################################
#
#
# date: 12/04/2012 #
#
# simulate theta #

sim.dyn.adni.cog<-function(ipat=1000)
{
  C<-matrix(0,1000,5)
  prior<-matrix(c(0.5,0.5,0.44,0.46,0.42,0.25,0.25,0.29,0.27,0.28,0.25,0.25,0.27,0.27,0.3),5,3)
   #
   C[,1]<-sample(1:3,1000,prob=prior[1,],replace=T)
   C[,2]<-sample(1:3,1000,prob=prior[2,],replace=T)
   C[,3]<-sample(1:3,1000,prob=prior[3,],replace=T)
   C[,4]<-sample(1:3,1000,prob=prior[4,],replace=T)
   C[,5]<-sample(1:3,1000,prob=prior[5,],replace=T)
   
  #
  delta1<-c(-1,-2,-2.2)
  delta2<-c(-1.1,-1.9,-2.1)
  delta3<-c(-1.2,-2.1,-2.9)
  delta4<-c(-1.4,-2.3,-2)
  delta5<-c(-1.8,-2,-2.5)
  #
  theta1 <- rnorm(ipat, 0, 1) + delta1[C[,1]]
  #
  sp<- rnorm(ipat,0,0.1)
  #
  e1<-e2<-e3<-e4<-rnorm(ipat,0,0.5)
  #
	theta2 <- theta1 + delta2[C[,2]] + sp + e1
	theta3 <- theta2 + delta3[C[,3]] + sp + e2
	theta4 <- theta3 + delta4[C[,4]] + sp + e3
	theta5 <- theta4 + delta5[C[,5]] + sp + e4
	#

################################################################################
# Cognition trait #
################################################################################
#
c0a = rep(-2.5,12)
c1a = rep(0.25,12)
c0 = rep(-2.5,10)
c1 = rep(0.25,10)
c0b = rep(1,6)
c1b = rep(0.5,6)
c0c = rep(10,2)
c1c = rep(0.5,2)

c0e = rep(1.5,6)
c1e = rep(0.5,6)

c0f = rep(1.5,30)
c1f = rep(0.5,30)

c0g =  as.matrix(t(matrix(rep(c(2,3,4),7), 3, 7)))
c1g = rep(0.5,7)

#
resp1a<-matrix(0,ipat,12)
resp1b<-matrix(0,ipat,10)
resp1c<-matrix(0,ipat,6)
resp1d<-matrix(0,ipat,2)
resp1f<-pois.res<-matrix(0,ipat,6)
resp1g<-matrix(0,ipat,30)
resp1h<-matrix(0,ipat,7)
#
resp2a<-matrix(0,ipat,12)
resp2b<-matrix(0,ipat,10)
resp2c<-matrix(0,ipat,6)
resp2d<-matrix(0,ipat,2)
resp2f<-pois.res<-matrix(0,ipat,6)
resp2g<-matrix(0,ipat,30)
resp2h<-matrix(0,ipat,7)
#
resp3a<-matrix(0,ipat,12)
resp3b<-matrix(0,ipat,10)
resp3c<-matrix(0,ipat,6)
resp3d<-matrix(0,ipat,2)
resp3f<-pois.res<-matrix(0,ipat,6)
resp3g<-matrix(0,ipat,30)
resp3h<-matrix(0,ipat,7)
#
resp4a<-matrix(0,ipat,12)
resp4b<-matrix(0,ipat,10)
resp4c<-matrix(0,ipat,6)
resp4d<-matrix(0,ipat,2)
resp4f<-pois.res<-matrix(0,ipat,6)
resp4g<-matrix(0,ipat,30)
resp4h<-matrix(0,ipat,7)
#
resp5a<-matrix(0,ipat,12)
resp5b<-matrix(0,ipat,10)
resp5c<-matrix(0,ipat,6)
resp5d<-matrix(0,ipat,2)
resp5f<-pois.res<-matrix(0,ipat,6)
resp5g<-matrix(0,ipat,30)
resp5h<-matrix(0,ipat,7)
#

for(f in 1:ipat)
  {
################################################################################
# Time = 1
################################################################################


## 1 ##
 for(j in 1:12){
    b0a.n1 <- c0a[j]
    b1a.n1 <- c1a[j]
    #
    prob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta1[f])))
    prob2 <- 1-prob1
    #
    resp1a[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
     }

## 2 ##

  for(j in 1:10){
    b0.n1 <- c0[j]
    b1.n1 <- c1[j]
    #
    prob1 <- 1/(1 + exp( - (b0.n1 + b1.n1 * theta1[f])))
    prob2 <- 1-prob1
    #
    resp1b[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
     }

## 3 ##

 for(j in 1:6){
    b0b.n1 <- c0b[j]
    b1b.n1 <- c1b[j]
    #
    resp1c[f,j] <- b0b.n1 + b1b.n1 * theta1[f]
     }

## 4 ##

 for(j in 1:2){
    b0c.n1 <- c0c[j]
    b1c.n1 <- c1c[j]
    #
    resp1d[f,j] <- b0c.n1 + b1c.n1 * theta1[f]
     }

## 5 ##

 for(j in 1:6){
    b0c.n1 <- c0e[j]
    b1c.n1 <- c1e[j]
    #
    pois.res[f,j] <- exp(b0c.n1 + b1c.n1 * theta1[f])

    resp1f[f,j] <- rpois(1,pois.res[f,j])
     }

## 6 ##

 for(j in 1:30){
    b0f.n1 <- c0f[j]
    b1f.n1 <- c1f[j]
    #
    prob1 <- 1/(1 + exp( - (b0f.n1 + b1f.n1 * theta1[f])))
    prob2 <- 1-prob1
    #
    resp1g[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
     }

 ## 7 ##

 for(j in 1:7){
                        b0g.n1 <- c0g[j,1]
                        b1g.n1 <- c1g[j]
                        b0g.n2 <- c0g[j,2]
                        b1g.n2 <- c1g[j]
                        b0g.n3 <- c0g[j,3]
                        b1g.n3 <- c1g[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0g.n1 - b1g.n1 * theta1[f])))
                        cprob2 <- 1/(1 + exp( - (b0g.n2 - b1g.n2 * theta1[f])))
                        cprob3 <- 1/(1 + exp( - (b0g.n3 - b1g.n3 * theta1[f])))

                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3

    resp1h[f,j]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))
    }

################################################################################
#  Time = 2
################################################################################


## 1 ##
 for(j in 1:12){
    b0a.n1 <- c0a[j]
    b1a.n1 <- c1a[j]
    #
    prob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta2[f])))
    prob2 <- 1-prob1
    #
    resp2a[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
     }

## 2 ##

  for(j in 1:10){
    b0.n1 <- c0[j]
    b1.n1 <- c1[j]
    #
    prob1 <- 1/(1 + exp( - (b0.n1 + b1.n1 * theta2[f])))
    prob2 <- 1-prob1
    #
    resp2b[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
     }

## 3 ##

 for(j in 1:6){
    b0b.n1 <- c0b[j]
    b1b.n1 <- c1b[j]
    #
    resp2c[f,j] <- b0b.n1 + b1b.n1 * theta2[f]
     }

## 4 ##

 for(j in 1:2){
    b0c.n1 <- c0c[j]
    b1c.n1 <- c1c[j]
    #
    resp2d[f,j] <- b0c.n1 + b1c.n1 * theta2[f]
     }

## 5 ##

 for(j in 1:6){
    b0c.n1 <- c0e[j]
    b1c.n1 <- c1e[j]
    #
    pois.res[f,j] <- exp(b0c.n1 + b1c.n1 * theta2[f])

    resp2f[f,j] <- rpois(1,pois.res[f,j])
     }

## 6 ##

 for(j in 1:30){
    b0f.n1 <- c0f[j]
    b1f.n1 <- c1f[j]
    #
    prob1 <- 1/(1 + exp( - (b0f.n1 + b1f.n1 * theta2[f])))
    prob2 <- 1-prob1
    #
    resp2g[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
     }

 ## 7 ##

 for(j in 1:7){
                        b0g.n1 <- c0g[j,1]
                        b1g.n1 <- c1g[j]
                        b0g.n2 <- c0g[j,2]
                        b1g.n2 <- c1g[j]
                        b0g.n3 <- c0g[j,3]
                        b1g.n3 <- c1g[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0g.n1 - b1g.n1 * theta2[f])))
                        cprob2 <- 1/(1 + exp( - (b0g.n2 - b1g.n2 * theta2[f])))
                        cprob3 <- 1/(1 + exp( - (b0g.n3 - b1g.n3 * theta2[f])))

                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3

    resp2h[f,j]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))
    }

################################################################################
#  Time = 3
################################################################################


## 1 ##
 for(j in 1:12){
    b0a.n1 <- c0a[j]
    b1a.n1 <- c1a[j]
    #
    prob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta3[f])))
    prob2 <- 1-prob1
    #
    resp3a[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
     }

## 2 ##

  for(j in 1:10){
    b0.n1 <- c0[j]
    b1.n1 <- c1[j]
    #
    prob1 <- 1/(1 + exp( - (b0.n1 + b1.n1 * theta3[f])))
    prob2 <- 1-prob1
    #
    resp3b[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
     }

## 3 ##

 for(j in 1:6){
    b0b.n1 <- c0b[j]
    b1b.n1 <- c1b[j]
    #
    resp3c[f,j] <- b0b.n1 + b1b.n1 * theta3[f]
     }

## 4 ##

 for(j in 1:2){
    b0c.n1 <- c0c[j]
    b1c.n1 <- c1c[j]
    #
    resp3d[f,j] <- b0c.n1 + b1c.n1 * theta3[f]
     }

## 5 ##

 for(j in 1:6){
    b0c.n1 <- c0e[j]
    b1c.n1 <- c1e[j]
    #
    pois.res[f,j] <- exp(b0c.n1 + b1c.n1 * theta3[f])

    resp3f[f,j] <- rpois(1,pois.res[f,j])
     }

## 6 ##

 for(j in 1:30){
    b0f.n1 <- c0f[j]
    b1f.n1 <- c1f[j]
    #
    prob1 <- 1/(1 + exp( - (b0f.n1 + b1f.n1 * theta3[f])))
    prob2 <- 1-prob1
    #
    resp3g[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
     }

 ## 7 ##

 for(j in 1:7){
                        b0g.n1 <- c0g[j,1]
                        b1g.n1 <- c1g[j]
                        b0g.n2 <- c0g[j,2]
                        b1g.n2 <- c1g[j]
                        b0g.n3 <- c0g[j,3]
                        b1g.n3 <- c1g[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0g.n1 - b1g.n1 * theta3[f])))
                        cprob2 <- 1/(1 + exp( - (b0g.n2 - b1g.n2 * theta3[f])))
                        cprob3 <- 1/(1 + exp( - (b0g.n3 - b1g.n3 * theta3[f])))

                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3

    resp3h[f,j]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))
    }

################################################################################
#  Time = 4
################################################################################


## 1 ##
 for(j in 1:12){
    b0a.n1 <- c0a[j]
    b1a.n1 <- c1a[j]
    #
    prob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta4[f])))
    prob2 <- 1-prob1
    #
    resp4a[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
     }

## 2 ##

  for(j in 1:10){
    b0.n1 <- c0[j]
    b1.n1 <- c1[j]
    #
    prob1 <- 1/(1 + exp( - (b0.n1 + b1.n1 * theta4[f])))
    prob2 <- 1-prob1
    #
    resp4b[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
     }

## 3 ##

 for(j in 1:6){
    b0b.n1 <- c0b[j]
    b1b.n1 <- c1b[j]
    #
    resp4c[f,j] <- b0b.n1 + b1b.n1 * theta4[f]
     }

## 4 ##

 for(j in 1:2){
    b0c.n1 <- c0c[j]
    b1c.n1 <- c1c[j]
    #
    resp4d[f,j] <- b0c.n1 + b1c.n1 * theta4[f]
     }

## 5 ##

 for(j in 1:6){
    b0c.n1 <- c0e[j]
    b1c.n1 <- c1e[j]
    #
    pois.res[f,j] <- exp(b0c.n1 + b1c.n1 * theta4[f])

    resp4f[f,j] <- rpois(1,pois.res[f,j])
     }

## 6 ##

 for(j in 1:30){
    b0f.n1 <- c0f[j]
    b1f.n1 <- c1f[j]
    #
    prob1 <- 1/(1 + exp( - (b0f.n1 + b1f.n1 * theta4[f])))
    prob2 <- 1-prob1
    #
    resp4g[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
     }

 ## 7 ##

 for(j in 1:7){
                        b0g.n1 <- c0g[j,1]
                        b1g.n1 <- c1g[j]
                        b0g.n2 <- c0g[j,2]
                        b1g.n2 <- c1g[j]
                        b0g.n3 <- c0g[j,3]
                        b1g.n3 <- c1g[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0g.n1 - b1g.n1 * theta4[f])))
                        cprob2 <- 1/(1 + exp( - (b0g.n2 - b1g.n2 * theta4[f])))
                        cprob3 <- 1/(1 + exp( - (b0g.n3 - b1g.n3 * theta4[f])))

                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3

    resp4h[f,j]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))
    }

################################################################################
#  Time = 5
################################################################################


## 1 ##
 for(j in 1:12){
    b0a.n1 <- c0a[j]
    b1a.n1 <- c1a[j]
    #
    prob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta5[f])))
    prob2 <- 1-prob1
    #
    resp5a[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
     }

## 2 ##

  for(j in 1:10){
    b0.n1 <- c0[j]
    b1.n1 <- c1[j]
    #
    prob1 <- 1/(1 + exp( - (b0.n1 + b1.n1 * theta5[f])))
    prob2 <- 1-prob1
    #
    resp5b[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
     }

## 3 ##

 for(j in 1:6){
    b0b.n1 <- c0b[j]
    b1b.n1 <- c1b[j]
    #
    resp5c[f,j] <- b0b.n1 + b1b.n1 * theta5[f]
     }

## 4 ##

 for(j in 1:2){
    b0c.n1 <- c0c[j]
    b1c.n1 <- c1c[j]
    #
    resp5d[f,j] <- b0c.n1 + b1c.n1 * theta5[f]
     }

## 5 ##

 for(j in 1:6){
    b0c.n1 <- c0e[j]
    b1c.n1 <- c1e[j]
    #
    pois.res[f,j] <- exp(b0c.n1 + b1c.n1 * theta5[f])

    resp5f[f,j] <- rpois(1,pois.res[f,j])
     }

## 6 ##

 for(j in 1:30){
    b0f.n1 <- c0f[j]
    b1f.n1 <- c1f[j]
    #
    prob1 <- 1/(1 + exp( - (b0f.n1 + b1f.n1 * theta5[f])))
    prob2 <- 1-prob1
    #
    resp5g[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
     }

 ## 7 ##

 for(j in 1:7){
                        b0g.n1 <- c0g[j,1]
                        b1g.n1 <- c1g[j]
                        b0g.n2 <- c0g[j,2]
                        b1g.n2 <- c1g[j]
                        b0g.n3 <- c0g[j,3]
                        b1g.n3 <- c1g[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0g.n1 - b1g.n1 * theta5[f])))
                        cprob2 <- 1/(1 + exp( - (b0g.n2 - b1g.n2 * theta5[f])))
                        cprob3 <- 1/(1 + exp( - (b0g.n3 - b1g.n3 * theta5[f])))

                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3

    resp5h[f,j]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))
    }


}

return(list(theta1=theta1,theta2=theta2,theta3=theta3,theta4=theta4,theta5=theta5,C=C,
resp1a=resp1a,resp1b=resp1b,resp1c=resp1c,resp1d=resp1d,resp1f=resp1f,resp1g=resp1g,resp1h=resp1h,
resp2a=resp2a,resp2b=resp2b,resp2c=resp2c,resp2d=resp2d,resp2f=resp2f,resp2g=resp2g,resp2h=resp2h,
resp3a=resp3a,resp3b=resp3b,resp3c=resp3c,resp3d=resp3d,resp3f=resp3f,resp3g=resp3g,resp3h=resp3h,
resp4a=resp4a,resp4b=resp4b,resp4c=resp4c,resp4d=resp4d,resp4f=resp4f,resp4g=resp4g,resp4h=resp4h,
resp5a=resp5a,resp5b=resp5b,resp5c=resp5c,resp5d=resp5d,resp5f=resp5f,resp5g=resp5g,resp5h=resp5h,
prior=prior
))

}

sim.cog.dyn<-sim.dyn.adni.cog()