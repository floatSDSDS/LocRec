library(dplyr)
library(data.table)
library(recommenderlab)
library(NNLM)
library(fpc)
library(qlcMatrix)
library(LICORS)


K=5
path="E:/bitbucket/n2vlocrec-x"
setwd(path)

path.RecF="R"
path.data="data/ml100k"

dataset="ML100K"

filename.I<-"CI"
filename.U<-"CUfold"

setwd(path)
source(paste(path.RecF,"/X Recfun.R",sep=""))
source(paste(path.RecF,"/X0 loading and fold data.R",sep=""))
source(paste(path.RecF,"/-1 Fake Clusters.R",sep=""))

CI<-as.data.frame(fread(paste(path,"/",path.data,"/clusters/",dataset,"-",filename.I,".txt",sep=""),header=F))
names(CI)[1]<-"item"

CU<-list()
CU.list<-vector()
for(i in 1:K){
  input.name<-paste(path,"/",path.data,"/clusters/",dataset,"-",filename.U,i,".txt",sep="")
  CU[[i]]<-as.data.frame(fread(input.name,header=F))
  CU.list<-unique(c(CU.list,CU[[i]]$V1))
  names(CU[[i]])<-c("user",paste("fold5_",i,sep=""))
}

CU.5<-data.frame(
  user=CU.list
)

for(i in 1:K){
  CU.5<-left_join(CU.5,CU[[i]][,1:2],id.vars="user")
  CU.5[is.na(CU.5[,i+1]),i+1]<-0
}

CU<-CU.5
rm(CU.5,i)

for( fold.ith in 1:K){
  CU[CU[,fold.ith+1]==0,fold.ith+1]<-f.allocate.index(sum(CU[,fold.ith+1]==0),table(CU[,fold.ith+1]))
}


fold.UR<-list()
fold.test<-list()
fold.init<-list()
for(fold.ith in 1:K){
  
  fold.test[[fold.ith]]<-filter(UR,fold==fold.ith)%>%
    select(user,item,rating)
  fold.UR[[fold.ith]]<-as(
    UR%>%
      filter(fold!=fold.ith)%>%
      select(user,item,rating)%>%
      filter(user %in% CU$user,item %in% CI$item),"realRatingMatrix")
  fold.UR[[fold.ith]]<-as(fold.UR[[fold.ith]],"dgCMatrix")
  fold.init[[fold.ith]]<-Matrix(0,nrow=dim(fold.UR[[fold.ith]])[1],ncol=dim(fold.UR[[fold.ith]])[2],
                                dimnames = dimnames(fold.UR[[fold.ith]]))
}




# f.SM.U2B(1,1,1,1,CU,CI,fold.UR[[fold.ith]],"dgC")

# for(fold.ith in 1:K){
#   print(table(CU[,fold.ith+1]))
# }


######### check the dim of the each subset
check.freq=list()
for(fold.ith in 1:K){
  check.freq[[fold.ith]]<-matrix(0,max(CU[,2]),max(CI[,2]))
  
  for(uith in 1:max(CU[,2])){
    
    for(bith in 1:max(CI[,2])){
      check.freq[[fold.ith]][uith,bith]=f.SM.U2B(fold.ith,1,uith,bith,CU,CI,fold.UR[[fold.ith]],"frq")
    }
  }
}

check.flag<-list()
k.no=4
for(fold.ith in 1:K){
  check.flag[[fold.ith]]<-matrix(T,max(CU[,2]),max(CI[,2]))
  for(uith in 1:max(CU[,2])){
    kmpp<-kmeanspp(check.freq[[fold.ith]][uith,],k.no)
    kmIndex=kmpp$cluster
    kmMean=kmpp$centers[,1]
    
    check.flag[[fold.ith]][uith,which(kmIndex==which(kmMean==min(kmMean)))]<-F
  }
}

# training
model.nmf<-list()
model.ubcf<-list()
model.ibcf<-list()
model.popular<-list()
for( fold.ith in 1:K ){
  
  model.nmf[[fold.ith]]<-fold.init[[fold.ith]]
  model.ubcf[[fold.ith]]<-fold.init[[fold.ith]]
  model.ibcf[[fold.ith]]<-fold.init[[fold.ith]]
  model.popular[[fold.ith]]<-fold.init[[fold.ith]]
  
  for( uith in 1:max(CU[,2]) ){
    for( bith in 1:max(CI[,2]) ){
      if(check.flag[[fold.ith]][uith,bith]){
        cat("entry-fold.ith-",fold.ith,"-uith-",uith,"-bith-",bith,"\n")
        SMUR<-f.SM.U2B(fold.ith,1,uith,bith,CU,CI,fold.UR[[fold.ith]],key="dgC")
        SMUR$item<-dimnames(fold.UR[[fold.ith]])[[2]]
        model.nmf[[fold.ith]]<-f.recommend(SMUR,model.nmf[[fold.ith]],fold.UR[[fold.ith]],key="nmf")
        model.ubcf[[fold.ith]]<-f.recommend(SMUR,model.ubcf[[fold.ith]],fold.UR[[fold.ith]],key="ubcf")
        model.ibcf[[fold.ith]]<-f.recommend(SMUR,model.ibcf[[fold.ith]],fold.UR[[fold.ith]],key="ibcf")
        model.popular[[fold.ith]]<-f.recommend(SMUR,model.popular[[fold.ith]],fold.UR[[fold.ith]],key="popular")
      }
    }
  }
}

for(fold.ith in 1:K){
  for(i in 1:dim(model.nmf[[fold.ith]])[1]){
    cat("deleting history",i,"\r")
    usr<-dimnames(model.nmf[[fold.ith]])[[1]][i]
    index<-fold.UR[[fold.ith]][usr,]!=0
    u2b.traj<-names(index[index])
    model.nmf[[fold.ith]][usr,u2b.traj]<- -1
    model.ubcf[[fold.ith]][usr,u2b.traj]<- -1
    model.ibcf[[fold.ith]][usr,u2b.traj]<- -1
    model.popular[[fold.ith]][usr,u2b.traj]<- -1
  }
}



# evaluation
eva<-list()
method.list<-c("precision","recall","fmeasure","HR","ARHR")

for(method.ith in 1:length(method.list)){
  eva[[method.ith]]<-matrix(0,4,30,dimnames=list(c("nmf","ubcf","ibcf","popular"),
                                                 paste(paste("fold",1:5,"N",sep=""),rep(seq(5,30,5),rep(5,6)),sep="")))
  for(fold.ith in 1:K){
    
    for(N. in seq(5,30,5)){
      cat("\n -------fold-",fold.ith,"-method-",method.list[method.ith],"-N-",N.,"---------\n")
      cat("\n nmf----")
      temp.nmf<-mapply(f.evaluate,dimnames(fold.UR[[fold.ith]])[[1]],
                       MoreArgs = list(model=model.nmf[[fold.ith]],test=fold.test[[fold.ith]],
                                       key=method.list[method.ith],N=N.))
      
      cat("\n ubcf----")
      temp.ubcf<-mapply(f.evaluate,dimnames(fold.UR[[fold.ith]])[[1]],
                        MoreArgs = list(model=model.ubcf[[fold.ith]],test=fold.test[[fold.ith]],
                                        key=method.list[method.ith],N=N.))
      cat("\n ibcf----")
      temp.ibcf<-mapply(f.evaluate,dimnames(fold.UR[[fold.ith]])[[1]],
                        MoreArgs = list(model=model.ibcf[[fold.ith]],test=fold.test[[fold.ith]],
                                        key=method.list[method.ith],N=N.))
      
      cat("\n popular----")
      temp.popular<-mapply(f.evaluate,dimnames(fold.UR[[fold.ith]])[[1]],
                           MoreArgs = list(model=model.popular[[fold.ith]],test=fold.test[[fold.ith]],
                                           key=method.list[method.ith],N=N.))
      
      p=((N./5)-1)*5+fold.ith
      eva[[method.ith]][1,p]<-mean(temp.nmf)
      eva[[method.ith]][2,p]<-mean(temp.ubcf)
      eva[[method.ith]][3,p]<-mean(temp.ibcf)
      eva[[method.ith]][4,p]<-mean(temp.popular)
    }
  }
}

res.eva<-list()
for(method.ith in 1:length(method.list)){
  res.eva[[method.ith]]<-matrix(0,4,6,dimnames = list(c("nmf","ubcf","ibcf","popular"),
                                                      paste("N",seq(5,30,5),sep="")))
  for(i in 1:6){
    for(j in 1:4){
      res.eva[[method.ith]][j,i]<-mean(eva[[method.ith]][j,((1:5)+((i-1)*5))])
      
    }
    
  }
  write.table(res.eva[[method.ith]],
              paste("res/evaluation/",dataset,"L-",method.list[method.ith],".txt",sep=""),
              row.names=T,col.names=T)
}

