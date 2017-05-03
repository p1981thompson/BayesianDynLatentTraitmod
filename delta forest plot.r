library(rmeta)

#Hierarchical model

delta.dat<-read.xlsx("d:/Local Data/p1thompson/CHES/NIHR GRANT/Programs and software documentation/Tinn-R program scripts/PINE/model 9trt dw/comparison of models.xlsx",sheetName="Sheet15")

tabletext<-cbind(c("Delta",as.character(delta.dat[,1])),
                 c("Mean",delta.dat[,2]),
                 c("2.5%",delta.dat[,3]),
                 c("97.5%",delta.dat[,4])
                 )

 forestplot(labeltext=tabletext,c(NA,delta.dat[,2]),c(NA,delta.dat[,3]),c(NA,delta.dat[,4]),zero=0,is.summary=c(TRUE,rep(FALSE,7)),
   col=meta.colors(box="royalblue",line="darkblue", summary="royalblue"),boxsize=0.5,xticks=seq(-3,0.5,by=0.5), graphwidth = unit(3,"inches"),xlab=expression(delta))
   
   
#IRT model
   
#   delta.dat<-read.xlsx("d:/Local Data/p1thompson/CHES/NIHR GRANT/Programs and software documentation/Tinn-R program scripts/PINE/model 9trt dw/comparison of models.xlsx",sheetName="Sheet15")

tabletext<-cbind(c("Delta",as.character(delta.dat[,5])),
                 c("Mean",delta.dat[,6]),
                 c("2.5%",delta.dat[,7]),
                 c("97.5%",delta.dat[,8])
                 )

 forestplot(labeltext=tabletext,c(NA,delta.dat[,6]),c(NA,delta.dat[,7]),c(NA,delta.dat[,8]),zero=0,is.summary=c(TRUE,rep(FALSE,7)),
   col=meta.colors(box="royalblue",line="darkblue", summary="royalblue"),boxsize=0.5,xticks=seq(-3,0.5,by=0.5), graphwidth = unit(3,"inches"),xlab=expression(delta))