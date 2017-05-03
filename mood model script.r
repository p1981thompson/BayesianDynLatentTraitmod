###############################################################################################
#  WinBUGs script: Pine data combined trait including controls + mixed model + simulated data #
###############################################################################################
#
#  18-01-2011
#
# This model differs from the previous script as it includes the
# item information curves.
#
## load R2WinBUGS library to link R to WinBUGS ##
library(R2WinBUGS)
library(coda)
## write R data into winbugs format ##


#mobility simulated data#
resp1 = sim.beta.pine.mood$resp1
resp2 = sim.beta.pine.mood$resp2
resp3 = sim.beta.pine.mood$resp3
resp4 = sim.beta.pine.mood$resp4

#ADL simulated data#
resp5 = sim.beta.pine.mood$resp5
resp6 = sim.beta.pine.mood$resp6
resp7 = sim.beta.pine.mood$resp7
resp8 = sim.beta.pine.mood$resp8
resp9 = sim.beta.pine.mood$resp9

#Depression simulated data#
resp10 = sim.beta.pine.mood$resp10
resp11 = sim.beta.pine.mood$resp11
resp12 = sim.beta.pine.mood$resp12
resp13 = sim.beta.pine.mood$resp13

#Pain simulated data#
resp14 = sim.beta.pine.mood$resp14
resp15 = sim.beta.pine.mood$resp15
resp16 = sim.beta.pine.mood$resp16
resp17 = sim.beta.pine.mood$resp17
resp18 = sim.beta.pine.mood$resp18
resp19 = sim.beta.pine.mood$resp19

#Cognition simulated data#

resp20 = sim.beta.pine.mood$resp20
resp21 = sim.beta.pine.mood$resp21
resp22 = sim.beta.pine.mood$resp22

Ncut = c(2,4,3,2,3,4,3,4,2,2,3,4,6,3,2)
control<- sim.beta.pine.mood$control
lambda<-0

I = length(resp1[,1])

bugs.data(list("I","Ncut","control","resp1","resp2","resp3","resp4","resp5","resp6","resp7","resp8","resp9","resp10","resp11","resp12","resp13","resp14","resp15","resp16","resp17","resp18","resp19","resp20","resp21","resp22","lambda"),dir="C:/Documents and Settings/p1thompson/Desktop/work/", data.file = "simbetapinemood.txt",digits=3)

comb.sim.inits <- list(
m0 = as.matrix(t(matrix(rep(c(1,2),9), 2, 9))),
m1 = rep(1,9),
m0a = rep(-1,3),
m1a = rep(1,3),
m0b = as.matrix(t(matrix(rep(c(1,2,3,4),10), 4, 10))),
m1b = rep(1,10),
m0c = as.matrix(t(matrix(rep(c(1,2,3),20), 3, 20))),
m1c = rep(1,20),
a0 = as.matrix(t(matrix(rep(c(1,2),10), 2, 10))),
a1 = rep(1,10),
a0a = rep(-1,5),
a1a = rep(1,5),
a0b = as.matrix(t(matrix(rep(c(1,2,3),11), 3, 11))),
a1b = rep(1,11),
a0c = as.matrix(t(matrix(rep(c(1,2,3,4),4), 4, 4))),
a1c = rep(1,4),
a0d = -1,
a1d = 1,
p0 = c(1,2),
p1 = 1,
p0a = as.matrix(t(matrix(rep(c(1,2,3),2), 3, 2))),
p1a = rep(1,2),
p0b = c(1,2,3,4),
p1b = 1,
p0c = -1,
p1c = 1,
p0d = c(1,2,3,4,5,6),
p1d = 1,
p0f = -1,
p1f = 1,
d0 = rep(-1,27),
d1 = rep(1,27),
d0a = as.matrix(t(matrix(rep(c(1,2,3),6), 3, 6))),
d1a = rep(1,6),
d0b = c(1,2,3,4),
d1b = 1,
d0c = as.matrix(t(matrix(rep(c(1,2),9), 2, 9))),
d1c = rep(1,9),
c0 = rep(-1,24),
c1 = rep(1,24),
c0a = as.matrix(t(matrix(rep(c(1,2,3),8), 3, 8))),
c1a = rep(1,8),
c0b = as.matrix(t(matrix(rep(c(1,2),2), 2, 2))),
c1b = rep(1,2),
s = c(0.8,0.8,0.8,0.8), delta=-2, sigma=7,sigma2=7)


##Run winBUGS model##
model.beta.pine.mood<-bugs(data="C:/Documents and Settings/p1thompson/Desktop/work/simbetapinemood.txt",inits = list(comb.sim.inits,comb.sim.inits,comb.sim.inits), model.file="C:/Documents and Settings/p1thompson/Desktop/work/betamoodpine2.bug",
parameters=c("mu","theta1","theta2","theta3","theta4","omega","zeta","s","m0","m1","m0a","m1a","m0b","m1b","m0c","m1c","a0","a1","a0a","a1a","a0b","a1b","a0c","a1c","a0d","a1d","p0","p1","p0a","p1a","p0b","p1b","p0c","p1c","p0d","p1d","p0f","p1f","d0","d1","d0a","d1a","d0b","d1b","d0c","d1c","c0","c1","c0a","c1a","c0b","c1b","delta","sigma","sigma2","alpha","alpha2"),n.chains=3, n.iter=2000,n.thin=1,bugs.directory="c:/Program Files/WinBUGS14",debug=T,n.burnin=500,bin=200,save.history=T)

