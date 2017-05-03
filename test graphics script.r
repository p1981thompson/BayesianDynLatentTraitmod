bug1<-read.bugs("C:/Documents and Settings/p1thompson/Local Settings/Temp/Rtmpii4pX5/coda1.txt")

bug.mat1<-as.matrix(bug1)
fix(bug.mat1)
bug.mat.new<-bug.mat1[,c(1:467,1232:1265)]


post.mean.bug2<-apply(bug1[[1]],2,mean)
png(file="C:/Documents and Settings/p1thompson/Desktop/work/graphics baseline/Simulated/Mobility/trace plots/item%04d.png",height=5,width=5,units="in",res=300)
traceplot(bug2[[1]])
dev.off()

