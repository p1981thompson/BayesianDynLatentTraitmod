
#################################################################
# Function to read coda files in pieces and save to a data base #
#################################################################

library(ff)

#change working directory to location of coda files, usually in temp folder.#



#Load coda index file
index <- read.table("codaIndex.txt", header = FALSE, sep = "\t")

#How many chains were run in WinBUGS#
n.chains<-3

#put coda files into a list#
sims.files <- paste("coda", 1:n.chains, ".txt", sep = "")

#number of iterations in each chain#
n.keep <- index[, 3] - index[, 2] + 1

#Create a storage dataframe using ff#
p.data<-ff(0,dim=c(n.keep[1],length(index[,1])))


#for(j in 1:n.chains)
#{
for(i in 1:length(index[,1]))
{
my.dat<-scan(sims.files[1],nlines=n.keep[1],skip=(index[i,2]-1))[2*(1:n.keep[i])]
p.data[,i]<-my.dat
}
#}
colnames(p.data)<-index[,1]


