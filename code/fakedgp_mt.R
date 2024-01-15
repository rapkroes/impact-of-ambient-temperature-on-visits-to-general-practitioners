###Fake Data Generation Process###

art.data<- function(n){
  browser()
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
    PLZ= as.factor(sample(1:10,n/4,replace = TRUE)),
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

  art.konsul<- data.frame(
    uniPatID= ID.levels,
    PatID= NA,
    TG_DateNum= art.stamm$TG_DateNum,
    index_i=NA,
    PraxisID= sample(1:7,n/4, replace = TRUE),
    Praxisbesuch=NA,
    Anz_EBMs= NA,
    UeberweiserBSNR= NA,
    UeberweiserLANR= NA,
    PZN= NA,
    TG_NKH= NA,
    TG_Ueberw= NA,
    TG_AUB= NA,
    TG_AUDauer= NA,
    TG_Hausbes= NA,
    TG_HausbesMFA= NA,
    TG_HAVertrag= NA,
    TG_A_Rauchen= sample(c(0,1),n/4, replace = TRUE),
    TG_RF_Rauchen= sample(c(0,1),n/4, replace = TRUE),
    TG_A_Alkohol= sample(c(0,1),n/4, replace = TRUE),
    TG_RF_Alkohol= sample(c(0,1),n/4, replace = TRUE),
    TG_A_Sport= sample(c(0,1),n/4, replace = TRUE),
    TG_RF_Sport= sample(c(0,1),n/4, replace = TRUE),
    verzogen= NA,
    verstorben= NA
  )
  
  
  return(list(
    art.diag,art.stamm,art.konsul
  ))
}    

names.vec<- c("Diag3","Stamm3","Konsul3")

set.seed(81461650)
dl<- art.data(10000)
for(i in seq_along(names.vec)){
  assign(names.vec[i],dl[[i]])
}












