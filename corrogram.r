require(corrgram)

real.dat<-read.xlsx("d:/Local Data/p1thompson/CHES/NIHR GRANT/Programs and software documentation/Tinn-R program scripts/PINE/model 9trt dw/paper correlation tables.xlsx",sheetName="R data real")


sim.dat<-read.xlsx("d:/Local Data/p1thompson/CHES/NIHR GRANT/Programs and software documentation/Tinn-R program scripts/PINE/model 9trt dw/paper correlation tables.xlsx",sheetName="R data sim")


theta.mat<-matrix(0,483,9)
theta.mat[,1]<-real.dat[1:483,6]
theta.mat[,2]<-real.dat[484:966,6]
theta.mat[,3]<-real.dat[967:1449,6]
theta.mat[,4]<-real.dat[1450:1932,6]
theta.mat[,5]<-real.dat[1933:2415,6]
theta.mat[,6]<-real.dat[2416:2898,6]
theta.mat[,7]<-real.dat[2899:3381,6]
theta.mat[,8]<-real.dat[3382:3864,6]
theta.mat[,9]<-real.dat[3865:4347,6]

theta.mat2<-matrix(0,483,9)
theta.mat2[,1]<-real.dat[1:483,8]
theta.mat2[,2]<-real.dat[484:966,8]
theta.mat2[,3]<-real.dat[967:1449,8]
theta.mat2[,4]<-real.dat[1450:1932,8]
theta.mat2[,5]<-real.dat[1933:2415,8]
theta.mat2[,6]<-real.dat[2416:2898,8]
theta.mat2[,7]<-real.dat[2899:3381,8]
theta.mat2[,8]<-real.dat[3382:3864,8]
theta.mat2[,9]<-real.dat[3865:4347,8]


corrgram(theta.mat, order=TRUE,
         main="THETA IRT correlation ellipses",
         panel=panel.ellipse,
		 text.panel=panel.txt,
		 diag.panel=panel.minmax)
		 

corrgram(theta.mat, order=TRUE,
         main="THETA HIER correlation ellipses",
         panel=panel.ellipse,
		 text.panel=panel.txt,
		 diag.panel=panel.minmax)
		 
		 

theta.mat3<-matrix(0,500,9)
theta.mat3[,1]<-sim.dat[1:500,6]
theta.mat3[,2]<-sim.dat[501:1000,6]
theta.mat3[,3]<-sim.dat[1001:1500,6]
theta.mat3[,4]<-sim.dat[1501:2000,6]
theta.mat3[,5]<-sim.dat[2001:2500,6]
theta.mat3[,6]<-sim.dat[2501:3000,6]
theta.mat3[,7]<-sim.dat[3001:3500,6]
theta.mat3[,8]<-sim.dat[3501:4000,6]
theta.mat3[,9]<-sim.dat[4001:4500,6]

theta.mat4<-matrix(0,500,9)
theta.mat4[,1]<-sim.dat[1:500,8]
theta.mat4[,2]<-sim.dat[1001:1500,8]
theta.mat4[,3]<-sim.dat[1001:1500,8]
theta.mat4[,4]<-sim.dat[1501:2000,8]
theta.mat4[,5]<-sim.dat[2001:2500,8]
theta.mat4[,6]<-sim.dat[2501:3000,8]
theta.mat4[,7]<-sim.dat[3001:3500,8]
theta.mat4[,8]<-sim.dat[3501:4000,8]
theta.mat4[,9]<-sim.dat[4001:4500,8]

