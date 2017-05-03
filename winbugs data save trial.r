


try<-capture.output(bugs(data="C:/Documents and Settings/p1thompson/Desktop/work/sim11trtIRTadj2.txt",inits = list(comb.sim.inits,comb.sim.inits,comb.sim.inits), model.file="d:/Local Data/p1thompson/CHES/NIHR GRANT/Programs and software documentation/Tinn-R program scripts/PINE/model 9trt dw/IRT 9trt model adj2.bug",
parameters=c("theta1","theta2","theta3","theta4","theta5","theta6","theta7","theta8","theta9","theta10","theta11","ma0","ma1","ma0a","ma1a","ma0c","ma1c","mb0a","mb1a","mb0b","mb1b","mb0c","mb1c","a0","a1","a0b","a1b","a0c","a1c","a0d","a1d","de0","de1","de0a","de1a",
"de0b","de1b","de0c","de1c","de0d","de1d","de0e","de1e","de0f","de1f","fa0","fa1","fa0a","fa1a","fb0","fb1","fb0a","fb1a","d0","d1","d0a","d1a","d0b","d1b","d0c","d1c","d0d","d1d","d0e","d1e","d0f","d1f","d0g","d1g","d0h","d1h","c0","c1","c0a","c1a","c0b","c1b","delta","sigma"),n.chains=3, n.iter=10,n.thin=1,bugs.directory="c:/Program Files/WinBUGS14",debug=T,n.burnin=2,bin=2,save.history=T),file="d:/Local Data/p1thompson/paul")

sink("d:/Local Data/p1thompson/paul1.txt")
try<-bugs(data="C:/Documents and Settings/p1thompson/Desktop/work/sim11trtIRTadj2.txt",inits = list(comb.sim.inits,comb.sim.inits,comb.sim.inits), model.file="d:/Local Data/p1thompson/CHES/NIHR GRANT/Programs and software documentation/Tinn-R program scripts/PINE/model 9trt dw/IRT 9trt model adj2.bug",
parameters=c("theta1","theta2","theta3","theta4","theta5","theta6","theta7","theta8","theta9","theta10","theta11","ma0","ma1","ma0a","ma1a","ma0c","ma1c","mb0a","mb1a","mb0b","mb1b","mb0c","mb1c","a0","a1","a0b","a1b","a0c","a1c","a0d","a1d","de0","de1","de0a","de1a",
"de0b","de1b","de0c","de1c","de0d","de1d","de0e","de1e","de0f","de1f","fa0","fa1","fa0a","fa1a","fb0","fb1","fb0a","fb1a","d0","d1","d0a","d1a","d0b","d1b","d0c","d1c","d0d","d1d","d0e","d1e","d0f","d1f","d0g","d1g","d0h","d1h","c0","c1","c0a","c1a","c0b","c1b","delta","sigma"),n.chains=3, n.iter=1000,n.thin=1,bugs.directory="c:/Program Files/WinBUGS14",debug=T,n.burnin=250,bin=500,codaPkg=T,save.history=T)
print(try)
sink()
unlink("d:/Local Data/p1thompson/paul1.txt")