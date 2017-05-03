#####################################################
#                                                   #
#  simulate combined mixture beta model data (pine) #
#                                                   #
#####################################################



# date: 19/01/2011 #

# simulate theta #

sim.mix.beta.pine<-function(ipat=500)
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
  #
  #s <- rtruncnorm(5, a=-1, b=1, mean = 0, sd = 1)
  s<-c(0.8,0.8,0.8,0.8,0.8)
  #
  theta1 <-  s[1]*omega + sqrt(1-(s[1]*s[1]))*nu1
  theta2 <-  s[2]*omega + sqrt(1-(s[2]*s[2]))*nu2
  theta3 <-  s[3]*omega + sqrt(1-(s[3]*s[3]))*nu3
  theta4 <-  s[4]*omega + sqrt(1-(s[4]*s[4]))*nu4
  theta5 <-  s[5]*omega + sqrt(1-(s[5]*s[5]))*nu5
  #
#MOBILITY#

m0 = as.matrix(t(matrix(rep(c(2,3),9), 2, 9)))
m1 = rep(0.5,9)
m0a = rep(-1,3)
m1a = rep(0.5,3)
m0b = as.matrix(t(matrix(rep(c(0.5,1,1.8,2.3),10), 4, 10)))
m1b = rep(0.5,10)
m0c = as.matrix(t(matrix(rep(c(0.5,1.8,2.3),20), 3, 20)))
m1c = rep(0.5,20)
#
resp1<-matrix(0,ipat,9)
resp2<-matrix(0,ipat,3)
resp3<-matrix(0,ipat,10)
resp4<-matrix(0,ipat,20)
#lambda1 <- vector(mode="numeric",length=ipat)

##############################

for(f in 1:ipat)
  {

## 1 ##
   for(j in 1:9){
                        b0.n1 <- m0[j,1]
                        b1.n1 <- m1[j]
                        b0.n2 <- m0[j,2]
                        b1.n2 <- m1[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta1[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta1[f])))

                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- 1 - cprob2

    resp1[f,j]<-sample(1:3,1,prob=c(prob1,prob2,prob3))
    }

## 2 ##
for(j in 1:3){
    b0a.n1 <- m0a[j]
    b1a.n1 <- m1a[j]
    #
    prob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta1[f])))
    prob2 <- 1-prob1
    #
    resp2[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
    }

## 3 ##
   for(j in 1:10){
                        b0.n1 <- m0b[j,1]
                        b1.n1 <- m1b[j]
                        b0.n2 <- m0b[j,2]
                        b1.n2 <- m1b[j]
                        b0.n3 <- m0b[j,3]
                        b1.n3 <- m1b[j]
                        b0.n4 <- m0b[j,4]
                        b1.n4 <- m1b[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta1[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta1[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta1[f])))
                        cprob4 <- 1/(1 + exp( - (b0.n4 - b1.n4 * theta1[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- cprob4 - cprob3
                        prob5 <- 1 - cprob4

    resp3[f,j]<-sample(1:5,1,prob=c(prob1,prob2,prob3,prob4,prob5))
    }

## 4 ##
   for(j in 1:20){
                        b0.n1 <- m0c[j,1]
                        b1.n1 <- m1c[j]
                        b0.n2 <- m0c[j,2]
                        b1.n2 <- m1c[j]
                        b0.n3 <- m0c[j,3]
                        b1.n3 <- m1c[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta1[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta1[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta1[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3

    resp4[f,j]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))
    }


}


##############################

print("2")

#ADL#

a0 = as.matrix(t(matrix(rep(c(2,3),10), 2, 10)))
a1 = rep(0.5,10)
a0a = rep(-2.5,5)
a1a = rep(0.25,5)
a0b = as.matrix(t(matrix(rep(c(0.5,1.8,2.3),11), 3, 11)))
a1b = rep(0.5,11)
a0c = as.matrix(t(matrix(rep(c(0,0.5,1.8,2.3),4), 4, 4)))
a1c = rep(0.5,4)
a0d = -2.5
a1d = 0.5
#
resp5<-matrix(0,ipat,10)
resp6<-matrix(0,ipat,5)
resp7<-matrix(0,ipat,11)
resp8<-matrix(0,ipat,4)
resp9<-vector(mode="numeric",length=ipat)
#

#lambda2<-matrix(0,ipat,5)

##############################

for(f in 1:ipat)
{
## 1 ##
 for(j in 1:10){
                        b0.n1 <- a0[j,1]
                        b1.n1 <- a1[j]
                        b0.n2 <- a0[j,2]
                        b1.n2 <- a1[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta2[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta2[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- 1 - cprob2

            resp5[f,j]<-sample(1:3,1,prob=c(prob1,prob2,prob3))
            }

## 2 ##
 for(j in 1:5){
            b0.n1 <- a0a[j]
            b1.n1 <- a1a[j]
            #
            prob1 <- 1/(1 + exp( - (b0.n1 + b1.n1 * theta2[f])))
            prob2 <- 1-prob1
            #
            resp6[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
            }

## 3 ##
 for(j in 1:11){
                        b0.n1 <- a0b[j,1]
                        b1.n1 <- a1b[j]
                        b0.n2 <- a0b[j,2]
                        b1.n2 <- a1b[j]
                        b0.n3 <- a0b[j,3]
                        b1.n3 <- a1b[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta2[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta2[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta2[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3

            resp7[f,j]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))
            }

## 4 ##
for(j in 1:4){
                        b0.n1 <- a0c[j,1]
                        b1.n1 <- a1c[j]
                        b0.n2 <- a0c[j,2]
                        b1.n2 <- a1c[j]
                        b0.n3 <- a0c[j,3]
                        b1.n3 <- a1c[j]
                        b0.n4 <- a0c[j,4]
                        b1.n4 <- a1c[j]
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

            resp8[f,j]<-sample(1:5,1,prob=c(prob1,prob2,prob3,prob4,prob5))
            }

## 5 ##
 #for(j in 1:5){
            b0d.n1 <- a0d
            b1d.n1 <- a1d
            sigma <- 7
            #
            mu.b<-1/(1 + exp( - (b0d.n1 + b1d.n1 * theta2[f])))
            be1 <- mu.b*sigma
            be2 <- (1-mu.b)*sigma
            #
            resp9[f]<-rbeta(1,shape1=be1,shape2=be2)
           #}

}

##############################
print("3")
#Depression#

d0 = rep(-2.5,27)
d1 = rep(0.25,27)
d0a = as.matrix(t(matrix(rep(c(0.5,1.8,2.5),6), 3, 6)))
d1a = rep(0.5,6)
d0b = c(0,0.5,1.8,2.3)
d1b = 0.5
d0c = as.matrix(t(matrix(rep(c(2,3),9), 2, 9)))
d1c = rep(0.5,9)

#
resp10<-matrix(0,ipat,27)
resp11<-matrix(0,ipat,6)
resp12<-vector(mode="numeric",length=ipat)
resp13<-matrix(0,ipat,9)


##############################

for(f in 1:ipat)
  {

## 1 ##

 for(j in 1:27){
    b0a.n1 <- d0[j]
    b1a.n1 <- d1[j]
    #
    prob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta3[f])))
    prob2 <- 1-prob1
    #
    resp10[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
    }

## 2 ##

 for(j in 1:6){
                        b0.n1 <- d0a[j,1]
                        b1.n1 <- d1a[j]
                        b0.n2 <- d0a[j,2]
                        b1.n2 <- d1a[j]
                        b0.n3 <- d0a[j,3]
                        b1.n3 <- d1a[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta3[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta3[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta3[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3

    resp11[f,j]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))
    }


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

    resp12[f]<-sample(1:5,1,prob=c(prob1,prob2,prob3,prob4,prob5))
  #  }

## 4 ##


 for(j in 1:9){
                        b0.n1 <- d0c[j,1]
                        b1.n1 <- d1c[j]
                        b0.n2 <- d0c[j,2]
                        b1.n2 <- d1c[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta3[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta3[f])))

                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- 1 - cprob2

    resp13[f,j]<-sample(1:3,1,prob=c(prob1,prob2,prob3))
    }

}

##############################
print("4")
#PAIN#

p0 = c(2,3)
p1 = 0.5
p0a = as.matrix(t(matrix(rep(c(0.5,1.8,2.3),2), 3, 2)))
p1a = rep(0.5,2)
p0b = c(0,0.5,1.8,2.3)
p1b = 0.5
p0c = -2.5
p1c = 0.5
p0d = c(-2,-1,0,1,2,3)
p1d = 0.5
p0f = -2.5
p1f = 0.5

#
resp14<-vector(mode="numeric",length=ipat)
resp15<-matrix(0,ipat,2)
resp16<-vector(mode="numeric",length=ipat)
resp17<-vector(mode="numeric",length=ipat)
resp18<-vector(mode="numeric",length=ipat)
resp19<-vector(mode="numeric",length=ipat)

##############################

for(f in 1:ipat)
  {

## 1 ##


# for(j in 1:5){
                        b0.n1 <- p0[1]
                        b1.n1 <- p1
                        b0.n2 <- p0[2]
                        b1.n2 <- p1
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta4[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta4[f])))

                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- 1 - cprob2

resp14[f]<-sample(1:3,1,prob=c(prob1,prob2,prob3))
#    }

    ## 2 ##


 for(j in 1:2){
                        b0.n1 <- p0a[j,1]
                        b1.n1 <- p1a[j]
                        b0.n2 <- p0a[j,2]
                        b1.n2 <- p1a[j]
                        b0.n3 <- p0a[j,3]
                        b1.n3 <- p1a[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta4[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta4[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta4[f])))

                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3

    resp15[f,j]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))
    }

    ## 3 ##


# for(j in 1:5){
                        b0.n1 <- p0b[1]
                        b1.n1 <- p1b
                        b0.n2 <- p0b[2]
                        b1.n2 <- p1b
                        b0.n3 <- p0b[3]
                        b1.n3 <- p1b
                        b0.n4 <- p0b[4]
                        b1.n4 <- p1b
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta4[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta4[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta4[f])))
                        cprob4 <- 1/(1 + exp( - (b0.n4 - b1.n4 * theta4[f])))

                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- cprob4 - cprob3
                        prob5 <- 1 - cprob4

resp16[f]<-sample(1:5,1,prob=c(prob1,prob2,prob3,prob4,prob5))
#    }


  ## 4 ##

# for(j in 1:5){
    b0a.n1 <- p0c
    b1a.n1 <- p1c
    #
    prob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta4[f])))
    prob2 <- 1-prob1
    #
 resp17[f]<-sample(0:1,1,prob=c(prob2,prob1))
#    }


  ## 5 ##
# for(j in 1:5){
                        b0.n1 <- p0d[1]
                        b1.n1 <- p1d
                        b0.n2 <- p0d[2]
                        b1.n2 <- p1d
                        b0.n3 <- p0d[3]
                        b1.n3 <- p1d
                        b0.n4 <- p0d[4]
                        b1.n4 <- p1d
                        b0.n5 <- p0d[5]
                        b1.n5 <- p1d
                        b0.n6 <- p0d[6]
                        b1.n6 <- p1d
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta4[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta4[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta4[f])))
                        cprob4 <- 1/(1 + exp( - (b0.n4 - b1.n4 * theta4[f])))
                        cprob5 <- 1/(1 + exp( - (b0.n5 - b1.n5 * theta4[f])))
                        cprob6 <- 1/(1 + exp( - (b0.n6 - b1.n6 * theta4[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- cprob4 - cprob3
                        prob5 <- cprob5 - cprob4
                        prob6 <- cprob6 - cprob5
                        prob7 <- 1 - cprob6

resp18[f]<-sample(1:7,1,prob=c(prob1,prob2,prob3,prob4,prob5,prob6,prob7))
#    }

## 6 ##
# for(j in 1:5){
            b0f.n1 <- p0f
            b1f.n1 <- p1f
            sigma2 <- 7
            #
            mu.b2 <- 1/(1 + exp( - (b0f.n1 + b1f.n1 * theta4[f])))
            be3 <- mu.b2*sigma2
            be4 <- (1-mu.b2)*sigma2
            #
 resp19[f]<-rbeta(1,shape1=be3,shape2=be4)
#          }

}

##############################
print("5")
#COGNITION#

c0 = rep(-2.5,24)
c1 = rep(0.25,24)
c0a =  as.matrix(t(matrix(rep(c(0.5,1.8,2.3),8), 3, 8)))
c1a = rep(0.5,8)
c0b =  as.matrix(t(matrix(rep(c(2,3),2), 2, 2)))
c1b = rep(0.5,2)
#
resp20<-matrix(0,ipat,24)
resp21<-matrix(0,ipat,8)
resp22<-matrix(0,ipat,2)
#
##############################

for(f in 1:ipat)
  {

## 1 ##
 for(j in 1:24){
    b0a.n1 <- c0[j]
    b1a.n1 <- c1[j]
    #
    prob1 <- 1/(1 + exp( - (b0a.n1 + b1a.n1 * theta5[f])))
    prob2 <- 1-prob1
    #
    resp20[f,j]<-sample(0:1,1,prob=c(prob2,prob1))
     }

## 2 ##

  for(j in 1:8){
                        b0.n1 <- c0a[j,1]
                        b1.n1 <- c1a[j]
                        b0.n2 <- c0a[j,2]
                        b1.n2 <- c1a[j]
                        b0.n3 <- c0a[j,3]
                        b1.n3 <- c1a[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta5[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta5[f])))
                        cprob3 <- 1/(1 + exp( - (b0.n3 - b1.n3 * theta5[f])))
                        #
                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- cprob3 - cprob2
                        prob4 <- 1 - cprob3

    resp21[f,j]<-sample(1:4,1,prob=c(prob1,prob2,prob3,prob4))
    }

## 3 ##

  for(j in 1:2){
                        b0.n1 <- c0b[j,1]
                        b1.n1 <- c1b[j]
                        b0.n2 <- c0b[j,2]
                        b1.n2 <- c1b[j]
                        #
                        cprob1 <- 1/(1 + exp( - (b0.n1 - b1.n1 * theta5[f])))
                        cprob2 <- 1/(1 + exp( - (b0.n2 - b1.n2 * theta5[f])))

                        prob1 <- cprob1
                        prob2 <- cprob2 - cprob1
                        prob3 <- 1 - cprob2

    resp22[f,j]<-sample(1:3,1,prob=c(prob1,prob2,prob3))
    }

}

##############################

return(list(control=omega.data[,1],omega=omega.data[,2],theta1=theta1,theta2=theta2,theta3=theta3,theta4=theta4,theta5=theta5,resp1=resp1,resp2=resp2,resp3=resp3,resp4=resp4,resp5=resp5,resp6=resp6,resp7=resp7,resp8=resp8,resp9=resp9,resp10=resp10,resp11=resp11,resp12=resp12,resp13=resp13,resp14=resp14,resp15=resp15,resp16=resp16,resp17=resp17,resp18=resp18,resp19=resp19,resp20=resp20,resp21=resp21,resp22=resp22))

}

simmix.beta.pine2<-sim.mix.beta.pine()