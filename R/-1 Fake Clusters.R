

setwd(paste(path.data,"/clusters",sep=""))


CI<-data.frame(item=unique(UR$item),C=sample(0:5, size = length(unique(UR$item)),replace = T))
write.table(CI,paste(dataset,"-CI.txt",sep=""),row.names=F,col.names=F)


for(fold.ith in 1:K){
  URfold<-as.data.frame(filter(UR,fold!=fold.ith))
  CU<-data.frame(user=unique(URfold$user),C=sample(0:5,size=length(unique(URfold$user)),replace=T))
  write.table(CU,paste(dataset,"-CUfold",fold.ith,".txt",sep=""),row.names = F,col.names = F)
}

setwd(path)