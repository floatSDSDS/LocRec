
library(data.table)
library(dplyr)
library(rjson)
library(jsonlite)
library(stringr)
# install.packages("rjson")

setwd("D:/dataset/amazon")

list.files()
UR<-fread("MscInstruments.csv")

names(UR)<-c("user","item","rating","time")

UR5<-group_by(UR,user)%>%
  mutate(nU=n())%>%
  filter(nU>5)%>%
  select(-nU)

UR5$time<- as.Date(as.POSIXlt(UR5$time,origin = "1970-01-01",tz="UTC"))

cate <- readLines("categories/meta_Musical_Instruments.json")
asin<-str_match(cate,"\'asin\':.*?,")
asin<-gsub("..$","",asin)
asin<-gsub("^.........","",asin)
categories<-str_match(cate,"\'categories\':.*?]]")
categories<-gsub("\'.*?: ","",categories)
categories<-gsub("^..","",categories)
categories<-gsub("..$","",categories)
categories<-gsub("\\], \\[",", ",categories)

item<-data.frame(item=asin[,1],tags=categories[,1])

UR5tags<-left_join(UR,item)
UR5tags<-select(UR5tags,user,item,rating,time,tags,fold)

write.csv(UR5tags,"amazonUR-MscIstruments.csv",row.names = F)
