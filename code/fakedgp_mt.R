###Fake Data Generation Process###

library(readxl)
altersstruktur_deutschland <- read_excel("C:/Users/Raphael (limited)/Downloads/altersstruktur deutschland.xlsx")
ListeKrankenkassen <- read.csv("C:/Users/Raphael (limited)/Downloads/ListeKrankenkassen_up.csv")
icd10_blocks <- read_excel("icd10_blocks.xlsx")
Impf3 <- read.csv("C:/Users/Raphael (limited)/Downloads/RS_DB_v02/RS_DB_Impf3_v02.csv", sep=";")
Labor3 <- read.csv2("C:/Users/Raphael (limited)/Downloads/RS_DB_v02/RS_DB_Labor3_v02.csv")

art.data<- function(n){
  #creates a list of data frames with n entries, with entries trying to emulate the original data sets. 
  ID.levels<- seq(1,n/4)
  earliest.date<- date2TG_DateNum("2016-01-01")
  latest.date<- date2TG_DateNum("2022-12-31")
  date.vec<- seq(earliest.date,latest.date)
  art.stamm<- data.frame(
    uniPatID= ID.levels,
    PatID= NA,
    TG_DateNum= sample(date.vec,n/4, replace = TRUE),
    index_i=NA,
    PraxisID=NA,
    PLZ= NA,
    Entfernung=NA,
    Geburtsjahr= sample(1920:2010,n/4,replace = TRUE),
    Geburtsmonat= sample(1:12,n/4, replace = TRUE),
    Transgen= 0,
    Divers=0,
    Geschlundef=0,
    IK=sample(ListeKrankenkassen$IK,n/4,replace = TRUE),
    Kasse=NA
  )
  art.stamm$Maennl<- sample(0:1,n/4, replace = TRUE)
  art.stamm$Weibl<- 1-art.stamm$Maennl
  art.stamm$Status_M<- sample(0:1,n/4, replace = TRUE)
  art.stamm$Status_F<- 1-art.stamm$Maennl
  art.stamm$Status_R<- sample(0:1,n/4, replace = TRUE)
  
  diag.dates<- sample(date.vec,n,replace = TRUE)
  art.diag<- data.frame(
    uniPatID= sample(ID.levels,n, replace = TRUE),
    TG_DateNum= diag.dates,
    DiagTyp=sample(c("D","DD"),n,replace = TRUE, prob = c(0.95,0.05)),
    icd10=sample(icd10_blocks$start_code,n,replace = TRUE)
  )
  probs<- runif(n)
  for(i in seq(2,n)){
    if(probs[i]<=0.1){
      art.diag$uniPatID[i]<- art.diag$uniPatID[i-1]
      art.diag$TG_DateNum[i]<- art.diag$TG_DateNum[i-1]
    }
  }
  
  art.impf<- Impf3[sample(1:nrow(Impf3),n,replace = TRUE),]
  art.impf$uniPatID<- sample(ID.levels,n, replace = TRUE)
  art.impf$TG_DateNum<- sample(date.vec,n,replace = TRUE)
  
  art.ipc<- data.frame(
    uniPatID= sample(ID.levels,n, replace = TRUE),
    TG_DateNum= diag.dates-sample(0:75,n, replace = TRUE),
    AnamnTyp=sample(c("A","B"),n,replace = TRUE),
    ipc2="R05"
  )
  probs<- runif(n)
  for(i in seq(2,n)){
    if(probs[i]<=0.1){
      art.ipc$uniPatID[i]<- art.ipc$uniPatID[i-1]
      art.ipc$TG_DateNum[i]<- art.ipc$TG_DateNum[i-1]
    }
  }
  
  art.lab<- Labor3[sample(1:nrow(Labor3),n,replace = TRUE),]
  art.lab$uniPatID<- sample(ID.levels,n, replace = TRUE)
  art.lab$TG_DateNum<- sample(date.vec,n,replace = TRUE)
  probs<- runif(n)
  for(i in seq(2,n)){
    if(probs[i]<=0.1){
      art.lab$uniPatID[i]<- art.lab$uniPatID[i-1]
      art.lab$TG_DateNum[i]<- art.lab$TG_DateNum[i-1]
    }
  }
  
  art.lu<- data.frame(
    uniPatID= sample(ID.levels,n, replace = TRUE),
    TG_DateNum= sample(date.vec,n,replace = TRUE)
  )
  
  art.pzn<- data.frame(
    uniPatID= sample(ID.levels,n, replace = TRUE),
    TG_DateNum= sample(date.vec,n,replace = TRUE),
    PZN=sample(1:10000,10*n,replace = TRUE)
  )
  
  art.ueberweis<- data.frame(
    uniPatID= sample(ID.levels,n, replace = TRUE),
    TG_DateNum= sample(date.vec,n,replace = TRUE),
    Uberw_Pneumo= sample(0:1,n,replace = TRUE),
    Uberw_Radiol= sample(0:1,n,replace = TRUE),
    Uberw_KH= sample(0:1,n,replace = TRUE)
  )
  
  return(list(
    art.diag,art.impf,art.ipc,art.lab,art.lu,art.pzn,art.stamm,art.ueberweis
  ))
}    

names.vec<- c("Diag3","Impf3","IPC23","Labor3","LU3","PZN3","Stamm3","Ueberweis3")

set.seed(531351)
dl<- art.data(10000)
for(i in 1:8){
  assign(names.vec[i],dl[[i]])
}












