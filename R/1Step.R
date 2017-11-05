library(data.table)
library(dplyr)
library(recommenderlab)
library(NNLM)
library(qlcMatrix)

K=5
path="D:/mode/926code"
setwd(path)

path.RecF="R"
path.data="data/ml100k"

dataset="ML100K"

filename.I<-"CI"
filename.U<-"CUfold"

setwd(path)
source(paste(path.RecF,"/X Recfun.R",sep=""))
source(paste(path.RecF,"/X0 loading and fold data.R",sep=""))
write.csv(UR,"data/ml100k/woyoudu.csv",row.name =T)
