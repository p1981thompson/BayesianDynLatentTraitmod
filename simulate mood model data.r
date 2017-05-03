##############################################################
#                                                            #
#  simulate 10 trait rearranged model (after EFA) data (pine) #
#                        (single second order trait)         #
##############################################################
#
#
# date: 03/03/2011 #
#
# simulate theta #

sim.9trt.pine.md<-function(ipat=500)
{

  sim.omega<-function(n=500)
  {
   omega<-control<-vector(mode="numeric",length=n)

   for(i in 1:n)
    {
     control[i]<-rbinom(1,1,0.55)
     omega[i]<-ifelse(control[i]==1,rnorm(1,-2,1),rnorm(1,0,1))
    }
  omega.dat<-data.frame(control,omega)
omega.dat
}
  zeta<-rnorm(ipat,0,1)
   print("1")
  omega.data<-sim.omega(n=ipat)
  omega<-omega.data[,2]
  control<-omega.data[,1]

  #
  nu1 <- rnorm(ipat, 0, 1)
  nu2 <- rnorm(ipat, 0, 1)
  nu3 <- rnorm(ipat, 0, 1)
  nu4 <- rnorm(ipat, 0, 1)
  nu5 <- rnorm(ipat, 0, 1)
  nu6 <- rnorm(ipat, 0, 1)
  nu7 <- rnorm(ipat, 0, 1)
  nu8 <- rnorm(ipat, 0, 1)
  nu9 <- rnorm(ipat, 0, 1)
  #
  #s <- rtruncnorm(5, a=-1, b=1, mean = 0, sd = 1)
  s<-c(0.8,0.8,0.8,0.8,0.8,0.8,0.8)
  sm<-c(0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8)
  #
  theta1 <- s[1]*omega + sqrt(1-(s[1]*s[1]))*nu1-(sm[1]*nu1) + sm[1]*zeta
  theta2 <- s[2]*omega + sqrt(1-(s[2]*s[2]))*nu2-(sm[2]*nu2) + sm[2]*zeta
  theta3 <- s[3]*omega + sqrt(1-(s[3]*s[3]))*nu3-(sm[3]*nu3) + sm[3]*zeta
  theta4 <- s[4]*omega + sqrt(1-(s[4]*s[4]))*nu4-(sm[4]*nu4) + sm[4]*zeta
  theta5 <- s[5]*omega + sqrt(1-(s[5]*s[5]))*nu5-(sm[5]*nu5) + sm[5]*zeta
  theta6 <- s[6]*omega + sqrt(1-(s[6]*s[6]))*nu6-(sm[6]*nu6) + sm[6]*zeta
  theta7 <- sm[7]*zeta + sqrt(1-(sm[7]*sm[7]))*nu7
  theta8 <- sm[8]*zeta + sqrt(1-(sm[8]*sm[8]))*nu8
  theta9 <- s[7]*omega + sqrt(1-(s[7]*s[7]))*nu9-(sm[9]*nu9) + sm[9]*zeta
  #
#
# theta
#}
#paul<-sim.9trt.pine.md()
# MOBILITY 1 #

ma0 = as.matrix(t(matrix(rep(c(2,3),4), 2, 4)))
ma1 = rep(0.5,4)
ma0a = rep(-1,2)
ma1a = rep(0.5,2)
ma0c = as.matrix(t(matrix(rep(c(0.5,1.8,2.3),2), 3, 2)))
ma1c = rep(0.5,2)
#
resp1a<-matrix(0,ipat,4)
resp2a<-matrix(0,ipat,2)
resp3a<-matrix(0,ipat,2)

#lambda1 <- vector(mode="numeric",length=ipat)

##############################

for(f in 1:ipat)
  {

## 1 ##

   for(j in 1:4){
                        b0.n1 <- ma0[j,1]
                        b1.n1 <- ma1[j]
                        b0.n2 <- ma0[j,2]
                        b1.n2 <- ma1[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta1[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta1[f])))

                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- 1 - cprob2

    resp1a[f,j]<-sample(1:3,1,prob=c(prob1,prob2,prob3))
    }

## 2 ##

for(j in 1:2){
    b0a.n1 <- ma0a[j]
    b1a.n1 <- ma1a[j]
    #
    prob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta1[f])))
    prob2 <- 1-prob1
    #
    resp2a[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
    }

## 3 ##

   for(j in 1:2){
                        b0.n1 <- ma0c[j,1]
                        b1.n1 <- ma1c[j]
                        b0.n2 <- ma0c[j,2]
                        b1.n2 <- ma1c[j]
                        b0.n3 <- ma0c[j,3]
                        b1.n3 <- ma1c[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta1[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta1[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta1[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3

    resp3a[f,j]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))
    }


}


##############################

# MOBILITY 2 #

mb0 = -1
mb1 = 0.5
mb0b = as.matrix(t(matrix(rep(c(0.5,1,1.8,2.3),5), 4, 5)))
mb1b = rep(0.5,5)
mb0c = c(0.5,1.8,2.3)
mb1c = 0.5
#
resp1b<-vector(mode="numeric",length=ipat)
resp2b<-matrix(0,ipat,5)
resp3b<-vector(mode="numeric",length=ipat)

#lambda1 <- vector(mode="numeric",length=ipat)
print("2")
##############################

for(f in 1:ipat)
  {

## 1 ##

    b0a.n1 <- mb0
    b1a.n1 <- mb1
    #
    prob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta2[f])))
    prob2 <- 1-prob1
    #
    resp1b[f]<-sample(0:1,1,prob=c(prob2,prob1))




## 3 ##
   for(j in 1:5){
                        b0.n1 <- mb0b[j,1]
                        b1.n1 <- mb1b[j]
                        b0.n2 <- mb0b[j,2]
                        b1.n2 <- mb1b[j]
                        b0.n3 <- mb0b[j,3]
                        b1.n3 <- mb1b[j]
                        b0.n4 <- mb0b[j,4]
                        b1.n4 <- mb1b[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta2[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta2[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta2[f])))
                        cprob4 <- 1/(1 + exp( - (b0.n4 - b1.n4 * theta2[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- cprob4 - cprob3
                        prob5 <- 1 - cprob4

    resp2b[f,j]<-sample(1:5,1,prob=c(prob1,prob2,prob3,prob4,prob5))
    }

## 4 ##

                        b0.n1 <- mb0c[1]
                        b1.n1 <- mb1c
                        b0.n2 <- mb0c[2]
                        b1.n2 <- mb1c
                        b0.n3 <- mb0c[3]
                        b1.n3 <- mb1c
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta2[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta2[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta2[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3

    resp3b[f]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))

}


##############################

# Dependency #

de0 = as.matrix(t(matrix(rep(c(2,3),4), 2, 4)))
de1 = rep(0.5,4)
de0a = rep(-1,5)
de1a = rep(0.5,5)
de0b = as.matrix(t(matrix(rep(c(0.5,1.8,2.3),6), 3, 6)))
de1b = rep(0.5,6)
de0c = as.matrix(t(matrix(rep(c(0.5,1,1.8,2.3),4), 4, 4)))
de1c = rep(0.5,4)
#
resp4<-matrix(0,ipat,4)
resp5<-matrix(0,ipat,5)
resp6<-matrix(0,ipat,6)
resp7<-matrix(0,ipat,4)

#lambda1 <- vector(mode="numeric",length=ipat)
print("3")
##############################

for(f in 1:ipat)
  {

## 1 ##
  for(j in 1:4)
  {
                        b0.n1 <- de0[j,1]
                        b1.n1 <- de1[j]
                        b0.n2 <- de0[j,2]
                        b1.n2 <- de1[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta3[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta3[f])))

                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- 1 - cprob2

    resp4[f,j]<-sample(1:3,1,prob=c(prob1,prob2,prob3))
  }
## 2 ##

for(j in 1:5){
    b0a.n1 <- de0a[j]
    b1a.n1 <- de1a[j]
    #
    prob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta3[f])))
    prob2 <- 1-prob1
    #
    resp5[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
    }

## 3 ##
   for(j in 1:6){
                        b0.n1 <- de0b[j,1]
                        b1.n1 <- de1b[j]
                        b0.n2 <- de0b[j,2]
                        b1.n2 <- de1b[j]
                        b0.n3 <- de0b[j,3]
                        b1.n3 <- de1b[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta3[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta3[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta3[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3
                        #
    resp6[f,j]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))
    }

## 4 ##
 for(j in 1:4)
 {
                        b0.n1 <- de0c[j,1]
                        b1.n1 <- de1c[j]
                        b0.n2 <- de0c[j,2]
                        b1.n2 <- de1c[j]
                        b0.n3 <- de0c[j,3]
                        b1.n3 <- de1c[j]
                        b0.n4 <- de0c[j,4]
                        b1.n4 <- de1c[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta3[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta3[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta3[f])))
                        cprob4 <- 1/(1 + exp( - (b0.n4 - b1.n4 * theta3[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- cprob4 - cprob3
                        prob5 <- 1 - cprob4
                        #
    resp7[f,j]<-sample(1:5,1,prob=c(prob1,prob2,prob3,prob4,prob5))
  }

}


##############################


#ADL#

a0 = c(2,3)
a1 = 0.5
a0b = c(0.5,1.8,2.3)
a1b = 0.5
a0c = as.matrix(t(matrix(rep(c(0,0.5,1.8,2.3),3), 4, 3)))
a1c = rep(0.5,3)
a0d = -2.5
a1d = 0.5
#
resp8<-vector(mode="numeric",length=ipat)
resp9<-vector(mode="numeric",length=ipat)
resp10<-matrix(0,ipat,3)
resp11<-vector(mode="numeric",length=ipat)
#

#lambda2<-matrix(0,ipat,5)
print("4")
##############################

for(f in 1:ipat)
{
## 1 ##

                        b0.n1 <- a0[1]
                        b1.n1 <- a1
                        b0.n2 <- a0[2]
                        b1.n2 <- a1
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta4[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta4[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- 1 - cprob2

            resp8[f]<-sample(1:3,1,prob=c(prob1,prob2,prob3))


## 3 ##

                        b0.n1 <- a0b[1]
                        b1.n1 <- a1b
                        b0.n2 <- a0b[2]
                        b1.n2 <- a1b
                        b0.n3 <- a0b[3]
                        b1.n3 <- a1b
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta4[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta4[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta4[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3

            resp9[f]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))


## 4 ##
for(j in 1:3){
                        b0.n1 <- a0c[j,1]
                        b1.n1 <- a1c[j]
                        b0.n2 <- a0c[j,2]
                        b1.n2 <- a1c[j]
                        b0.n3 <- a0c[j,3]
                        b1.n3 <- a1c[j]
                        b0.n4 <- a0c[j,4]
                        b1.n4 <- a1c[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta4[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta4[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta4[f])))
                        cprob4 <- 1/(1 + exp( - (b0.n4 - b1.n4 * theta4[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- cprob4 - cprob3
                        prob5 <- 1 - cprob4

            resp10[f,j]<-sample(1:5,1,prob=c(prob1,prob2,prob3,prob4,prob5))
            }

## 5 ##

            b0d.n1 <- a0d
            b1d.n1 <- a1d
            sigma <- 7
            #
            mu.b<-1/(1 + exp( - (b0d.n1 + b1d.n1 * theta4[f])))
            be1 <- mu.b*sigma
            be2 <- (1-mu.b)*sigma
            #
            resp11[f]<-rbeta(1,shape1=be1,shape2=be2)


}

##############################

##############################


# Motor 1 #

fa0 = as.matrix(t(matrix(rep(c(2,3),4), 2, 4)))
fa1 = rep(0.5,4)
fa0a = as.matrix(t(matrix(rep(c(0.5,1.8,2.3),16), 3, 16)))
fa1a = rep(0.5,16)
#
resp12a<-matrix(0,ipat,4)
resp13a<-matrix(0,ipat,16)
#

#lambda2<-matrix(0,ipat,5)
print("5")
##############################

for(f in 1:ipat)
{
## 1 ##
 for(j in 1:4)
 {
                        b0.n1 <- fa0[j,1]
                        b1.n1 <- fa1[j]
                        b0.n2 <- fa0[j,2]
                        b1.n2 <- fa1[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta5[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta5[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- 1 - cprob2

            resp12a[f,j]<-sample(1:3,1,prob=c(prob1,prob2,prob3))
 }

## 2 ##
 for(j in 1:16)
       {
                        b0.n1 <- fa0a[j,1]
                        b1.n1 <- fa1a[j]
                        b0.n2 <- fa0a[j,2]
                        b1.n2 <- fa1a[j]
                        b0.n3 <- fa0a[j,3]
                        b1.n3 <- fa1a[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta5[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta5[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta5[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3

            resp13a[f,j]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))
            }
}


##############################


# Motor 2 #

fb0 = as.matrix(t(matrix(rep(c(2,3),5), 2, 5)))
fb1 = rep(0.5,5)
fb0a = as.matrix(t(matrix(rep(c(0.5,1.8,2.3),4), 3, 4)))
fb1a = rep(0.5,4)
#
resp12b<-matrix(0,ipat,5)
resp13b<-matrix(0,ipat,4)
#

#lambda2<-matrix(0,ipat,5)
print("6")
##############################

for(f in 1:ipat)
{
## 1 ##
 for(j in 1:5)
 {
                        b0.n1 <- fb0[j,1]
                        b1.n1 <- fb1[j]
                        b0.n2 <- fb0[j,2]
                        b1.n2 <- fb1[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta6[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta6[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- 1 - cprob2

            resp12b[f,j]<-sample(1:3,1,prob=c(prob1,prob2,prob3))
 }

## 2 ##
 for(j in 1:4)
       {
                        b0.n1 <- fb0a[j,1]
                        b1.n1 <- fb1a[j]
                        b0.n2 <- fb0a[j,2]
                        b1.n2 <- fb1a[j]
                        b0.n3 <- fb0a[j,3]
                        b1.n3 <- fb1a[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta6[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta6[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta6[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3

            resp13b[f,j]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))
            }
}



##############################


# Mood 1 #

d0 = rep(-2.5,15)
d1 = rep(0.25,15)
d0a = c(0.5,1.8,2.5)
d1a = 0.5
d0b = c(0,0.5,1.8,2.3)
d1b = 0.5
d0c = as.matrix(t(matrix(rep(c(2,3),4), 2, 4)))
d1c = rep(0.5,4)

#
resp14<-matrix(0,ipat,15)
resp15<-vector(mode="numeric",length=ipat)
resp16<-vector(mode="numeric",length=ipat)
resp17<-matrix(0,ipat,4)
#
print("7")
##############################

for(f in 1:ipat)
  {

## 1 ##

 for(j in 1:15){
    b0a.n1 <- d0[j]
    b1a.n1 <- d1[j]
    #
    prob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta7[f])))
    prob2 <- 1-prob1
    #
    resp14[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
    }

## 2 ##


                        b0.n1 <- d0a[1]
                        b1.n1 <- d1a
                        b0.n2 <- d0a[2]
                        b1.n2 <- d1a
                        b0.n3 <- d0a[3]
                        b1.n3 <- d1a
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta7[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta7[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta7[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3

    resp15[f]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))



## 3 ##

 #for(j in 1:5){
                        b0.n1 <- d0b[1]
                        b1.n1 <- d1b
                        b0.n2 <- d0b[2]
                        b1.n2 <- d1b
                        b0.n3 <- d0b[3]
                        b1.n3 <- d1b
                        b0.n4 <- d0b[4]
                        b1.n4 <- d1b
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta7[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta7[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta7[f])))
                        cprob4 <- 1/(1 + exp( - (b0.n4 - b1.n4 * theta7[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- cprob4 - cprob3
                        prob5 <- 1 - cprob4

    resp16[f]<-sample(1:5,1,prob=c(prob1,prob2,prob3,prob4,prob5))
  #  }

## 4 ##


 for(j in 1:4){
                        b0.n1 <- d0c[j,1]
                        b1.n1 <- d1c[j]
                        b0.n2 <- d0c[j,2]
                        b1.n2 <- d1c[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta7[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta7[f])))

                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- 1 - cprob2

    resp17[f,j]<-sample(1:3,1,prob=c(prob1,prob2,prob3))
    }

}

##############################


##############################


# Mood 2 #

d0d = rep(-2.5,6)
d1d = rep(0.25,6)
d0f = as.matrix(t(matrix(rep(c(0.5,1.8,2.5),3), 3, 3)))
d1f = rep(0.5,3)
d0g = c(0,0.5,1.8,2.3)
d1g = 0.5
d0h = c(2,3)
d1h = 0.5
#
resp18<-matrix(0,ipat,6)
resp19<-matrix(0,ipat,3)
resp20<-vector(mode="numeric",length=ipat)
resp21<-vector(mode="numeric",length=ipat)
#
 print("8")
##############################

for(f in 1:ipat)
  {

## 1 ##

 for(j in 1:6){
    b0a.n1 <- d0d[j]
    b1a.n1 <- d1d[j]
    #
    prob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta8[f])))
    prob2 <- 1-prob1
    #
    resp18[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
    }

## 2 ##

 for(j in 1:3){
                        b0.n1 <- d0f[j,1]
                        b1.n1 <- d1f[j]
                        b0.n2 <- d0f[j,2]
                        b1.n2 <- d1f[j]
                        b0.n3 <- d0f[j,3]
                        b1.n3 <- d1f[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta8[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta8[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta8[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3

    resp19[f,j]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))
    }

## 3 ##


                        b0.n1 <- d0g[1]
                        b1.n1 <- d1g
                        b0.n2 <- d0g[2]
                        b1.n2 <- d1g
                        b0.n3 <- d0g[3]
                        b1.n3 <- d1g
                        b0.n4 <- d0g[4]
                        b1.n4 <- d1g
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta8[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta8[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta8[f])))
                        cprob4 <- 1/(1 + exp( - (b0.n4 - b1.n4 * theta8[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- cprob4 - cprob3
                        prob5 <- 1 - cprob4

    resp20[f]<-sample(1:5,1,prob=c(prob1,prob2,prob3,prob4,prob5))

## 4 ##


                        b0.n1 <- d0h[1]
                        b1.n1 <- d1h
                        b0.n2 <- d0h[2]
                        b1.n2 <- d1h
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta8[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta8[f])))

                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- 1 - cprob2

    resp21[f]<-sample(1:3,1,prob=c(prob1,prob2,prob3))


}

##############################



# COG #

c0 = rep(-2.5,21)
c1 = rep(0.25,21)
c0a = c(0.5,1.8,2.3)
c1a = 0.5
c0b =  as.matrix(t(matrix(rep(c(2,3),2), 2, 2)))
c1b = rep(0.5,2)
#
resp22<-matrix(0,ipat,21)
resp23<-vector(mode="numeric",length=ipat)
resp24<-matrix(0,ipat,2)
#
print("9")
##############################

for(f in 1:ipat)
  {

## 1 ##
 for(j in 1:21){
    b0a.n1 <- c0[j]
    b1a.n1 <- c1[j]
    #
    prob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta9[f])))
    prob2 <- 1-prob1
    #
    resp22[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
     }

## 2 ##


                        b0.n1 <- c0a[1]
                        b1.n1 <- c1a
                        b0.n2 <- c0a[2]
                        b1.n2 <- c1a
                        b0.n3 <- c0a[3]
                        b1.n3 <- c1a
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta9[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta9[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta9[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3

    resp23[f]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))


## 3 ##

  for(j in 1:2){
                        b0.n1 <- c0b[j,1]
                        b1.n1 <- c1b[j]
                        b0.n2 <- c0b[j,2]
                        b1.n2 <- c1b[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta9[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta9[f])))

                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- 1 - cprob2

    resp24[f,j]<-sample(1:3,1,prob=c(prob1,prob2,prob3))
    }

}

##############################


##############################


return(list(control=omega.data[,1],omega=omega.data[,2],zeta=zeta,theta1=theta1,theta2=theta2,theta3=theta3,theta4=theta4,theta5=theta5,theta6=theta6,theta7=theta7,theta8=theta8,theta9=theta9,resp1a=resp1a,resp2a=resp2a,resp3a=resp3a,resp1b=resp1b,resp2b=resp2b,resp3b=resp3b,resp4=resp4,resp5=resp5,resp6=resp6,resp7=resp7,resp8=resp8,resp9=resp9,resp10=resp10,resp11=resp11,resp12a=resp12a,resp13a=resp13a,resp12b=resp12b,resp13b=resp13b,resp14=resp14,resp15=resp15,resp16=resp16,resp17=resp17,resp18=resp18,resp19=resp19,resp20=resp20,resp21=resp21,resp22=resp22,resp23=resp23,resp24=resp24))

}

sim.9trt.md<-sim.9trt.pine.md()