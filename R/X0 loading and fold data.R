library(dplyr)
library(data.table)


.f.fold.index <- function(n.row, n.group) {
  ii <- sample(rep(seq_len(n.group), length.out = n.row))
  sample(seq_len(n.group))[ii]
}

path.loadD=path.data
filename="ML100K"
UR<-fread(paste(path.loadD,"/",filename,".csv",sep=""),header=T)

UR<-arrange(UR5,user)
temp<-UR%>%
  group_by(user)%>%
  summarise(countR=n())

UR$fold<-unlist(mapply(function(i){
  return(.f.fold.index(temp$countR[i],K))
},1:dim(temp)[1]))

# View(UR)

rm(temp,path.loadD)
