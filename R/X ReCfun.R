# Local fun

f.popMostSimilar.cos<-function(DF,N=30,T.=T){
  # DF<-as(DF,"realRatingMatrix")
  # DF<-as(DF,"dgCMatrix")
  if(T.){
    cos<-cosSparse(t(DF))
  }
  else{
    cos<-cosSparse(DF)
  }
  # cat("cos matrix compelete","\r")
  
  dimnames(cos)[[1]]<-1:dim(cos)[1]
  dimnames(cos)[[2]]<-1:dim(cos)[1]
  
  
  cos1<-cos[1:floor(dim(cos)[1]/2),]
  cos1<-as(cos1,"realRatingMatrix")
  cos1<-as(cos1,"data.frame")
  
  cos2<-cos[(floor(dim(cos)[1]/2)+1):dim(cos)[1],]
  cos2<-as(cos2,"realRatingMatrix")
  cos2<-as(cos2,"data.frame")
  
  # cat("rbinding cos1 and cos2","\r")
  
  cos.DF<-rbind(cos1,cos2)
  print()
  # cat("rbinding compelete, start ranking","\r")
  
  cos.DF$user<-as.numeric(cos.DF$user)
  cos.DF$item<-as.numeric(cos.DF$item)
  
  cos.DF<-arrange(cos.DF,-rating)%>%
    group_by(user)%>%
    mutate(rank=row_number())%>%
    filter(rank<=N)%>%
    select(user,item,rating)
  
  # cat("ranking compelete, output result","\r")
  
  return(cos.DF)
}


f.evaluate<-function(usr.id,model,test,key,N=30){
  # fold.ith=1
  # N.=30
  # usr.id=dimnames(fold.UR[[fold.ith]])[[1]][1]
  # model=model.nmf[[fold.ith]]
  # test=fold.test[[fold.ith]]
  # key=method.list[1]
  # N=N.
  
  
  
  test.rst.id<-unique(filter(test,user==usr.id)$item)
  res.rst.id<-names(-sort(-model[usr.id,])[1:N])
  inter.rst.id<-intersect(res.rst.id,test.rst.id)
  
  Precision=length(inter.rst.id)/N
  
  if(length(inter.rst.id)!=0){
    cat(length(inter.rst.id),"hit:)")
  }
  Recall=ifelse(length(test.rst.id)==0,0,length(inter.rst.id)/length(test.rst.id))
  
  switch(key,
         precision=Precision,
         recall=Recall,
         fmeasure=ifelse(length(inter.rst.id)==0,0,Precision*Recall*2/(Precision+Recall)),
         HR=ifelse(length(inter.rst.id)==0,F,T),
         ARHR=ifelse(length(inter.rst.id)==0,0,sum(1/which(res.rst.id %in% inter.rst.id)))
  )
}

# f.evaluate<-function(usr.id,model,test,key,N=30){
#   # fold.ith=1
#   # N.=30
#   # usr.id=dimnames(fold.UR[[fold.ith]])[[1]][1]
#   # model=model.nmf[[fold.ith]]
#   # test=fold.test[[fold.ith]]
#   # key=method.list[1]
#   # N=N.
#   
#   
#   test.rst.id<-unique(filter(test,user==usr.id)$item)
#   res.rst.id<-names(-sort(-model[usr.id,])[1:N])
#   inter.rst.id<-sum(res.rst.id%in%test.rst.id)
#   # inter.rst.id<-sum(test.rst.id%in%res.rst.id)
#   
#   Precision=inter.rst.id/N
#   
#   # if(inter.rst.id!=0){
#   #   cat(inter.rst.id,"hit:)")
#   # }
#   Recall=ifelse(test.rst.id==0,0,inter.rst.id/length(test.rst.id))
#   
#   switch(key,
#          precision=Precision,
#          recall=Recall,
#          fmeasure=ifelse(inter.rst.id==0,0,Precision*Recall*2/(Precision+Recall)),
#          HR=ifelse(inter.rst.id==0,F,T),
#          ARHR=ifelse(inter.rst.id==0,0,sum(1/which(res.rst.id %in% inter.rst.id)))
#   )
# }


# return index to allocate by p.group distruibution 
f.allocate.index<-function(n.row, p.group){
  # n.row=19
  # p.group=table(CU$fold5_1)
  vec.output<-rep(1,n.row)
  p.group<-as.vector(p.group[2:length(p.group)])
  for(i in 1:n.row){
    p=which(p.group==min(p.group))
    vec.output[i]=p[sample(length(p),1)]
    p.group[p]<-p.group[p]+1
  }
  return(vec.output)
  
  # rm(n.row,p.group,p)
  
}

# U.ith and B.ith annotated which columns to choose in the U.index and B.index, respectively
# cu and cb annotated which cluster to select
# SMUR is a named sparse Matrix for User-item Pool

f.SM.U2B<-function(U.ith,B.ith,cu,cb,U.index,B.index,SMUR,key){
  
  
  # cat("extrat sparse rating matrix for user cluster",U.ith,"and item cluster",B.ith,"\n")
  Ulist<-as.character(U.index[U.index[,U.ith+1] == cu,1])
  Blist<-as.character(B.index[B.index[,B.ith+1] == cb,1])
  
  Ulist<-Ulist[Ulist %in% dimnames(SMUR)[[1]]]
  Blist<-Blist[Blist %in% dimnames(SMUR)[[2]]]
  
  # cat("output SMu2b",B.ith,"-",U.ith,"\n")
  output=SMUR[Ulist,Blist]
  switch(key,
         dgC=list(
           user=Ulist,
           item=Blist
         ),
         frq=nnzero(output))
}

f.recommend<-function(subset,SMUR.iter,SMUR.ori,scale.flag=F,key="ubcf"){
  
  if(scale.flag){
    DFUR<-as(.SMUR.ori,"realRatingMatrix")
    DFUR<-as(DFUR,"data.frame")
    DFUR$rating<-scale(DFUR$rating)[,]
    DFUR<-as(DFUR,"realRatingMatrix")
    .SMUR.ori<-as(DFUR,"dgCMatrix")
  }
  
  SMUR.sub=SMUR.ori[subset$user,subset$item]
  subindex.U<-dimnames(SMUR.sub)[[1]]
  subindex.B<-dimnames(SMUR.sub)[[2]]
  
  # print(dim(SMUR.sub))
  
  if(key=="nmf"){
    NMF.model<-nnmf(as.matrix(SMUR.sub), k=ceiling(min(dim(SMUR.sub))^.5), 
                    max.iter = 1000, rel.tol = 1e-3, method = 'lee')
    
    nmf.res<-mapply(function(i){
      mapply(function(j){
        cat(i,"-",j,"\r")
        return(sum(NMF.model$W[i,]*NMF.model$H[,j]))
      },1:dim(NMF.model$H)[2])
    },1:dim(NMF.model$W)[1])
    nmf.res<-t(nmf.res)
    
    # cat(dim(nmf.res),dim(SMUR.iter))
    SMUR.iter[subindex.U,subindex.B]=SMUR.iter[subindex.U,subindex.B]+nmf.res
    
  }
  
  if(key=="ubcf"){
    cos.U<-cosSparse(t(SMUR.sub))
    for(i in 1:length(subindex.U)){
      cat("ubcf-",i,"\r")
      usr<-subindex.U[i]
      usim<--sort(-cos.U[i,])[i:nnzero(cos.U[i,])]
      
      if(length(usim)>=1){
        b2u<-SMUR.sub[names(usim),]
        if(length(usim)>1){
          b2u<-colSums(b2u*usim)
        }
        else{
          b2u<-b2u*usim
        }
        b2u<-b2u[b2u!=0]
        u.blist<-names(-sort(-SMUR.ori[usr,])[1:nnzero(SMUR.ori[usr,])])
        b2u<-b2u[!names(b2u)%in%u.blist]
        SMUR.iter[usr,names(b2u)]=SMUR.iter[usr,names(b2u)]+b2u
        
      }
    }
  }
  
  
  if(key=="ibcf"){
    cos.B<-cosSparse(SMUR.sub)
    
    for(i in 1:length(subindex.U)){
      cat("ibcf-",i,"\n")
      usr<-subindex.U[i]
      u.blist<--sort(-SMUR.ori[usr,])[1:nnzero(SMUR.ori[usr,])]
      u.blist<-u.blist[names(u.blist)%in%dimnames(cos.B)[[1]]]
      if(length(u.blist)>=1){
        bsim.Matrix<-cos.B[names(u.blist),]
        bsim.Matrix=bsim.Matrix*u.blist
        if(length(u.blist)>1){
          bsim.Matrix=colSums(bsim.Matrix)
        }
        bsim.Matrix=bsim.Matrix[names(bsim.Matrix)%in%names(u.blist)]
        SMUR.iter[usr,names(bsim.Matrix)]<-SMUR.iter[usr,names(bsim.Matrix)]+bsim.Matrix
      }
    }
  }
  
  if(key=="popular"){
    c.pop<-colSums(SMUR.sub)
    SMUR.iter[subindex.U,subindex.B]<-SMUR.iter[subindex.U,subindex.B]+c.pop
    
  }
  
  
  return(SMUR.iter)
  
  
}

# 
# test.SMUR<-f.SM.U2B(1,1,1,2,CU,CB,fold.UR[[1]],"dgC")
# a<-f.recommend(test.SMUR,fold.init[[1]],fold.UR[[1]],key="popular")
