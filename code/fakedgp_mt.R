###Fake Data Generation Process###

art.data<- function(n){
  #creates a list of data frames with n entries, with entries trying to emulate the original data sets. 
  ID.levels<- seq(1, n/4)
  earliest.date<- date2TG_DateNum("2016-03-01")
  latest.date<- date2TG_DateNum("2022-12-31")
  date.vec<- seq(earliest.date, latest.date)
  art.stamm<- data.frame(
    uniPatID= ID.levels,
    PatID= NA,
    TG_DateNum= sample(date.vec, n/4, replace = TRUE),
    index_i=NA,
    PraxisID=sample(c(1:6,8), n/4, replace = TRUE),
    PLZ= as.factor(sample(1:10, n/4, replace = TRUE)),
    Entfernung=NA,
    Geburtsjahr= sample(1920:2010, n/4, replace = TRUE),
    Geburtsmonat= sample(1:12, n/4, replace = TRUE),
    Transgen= 0,
    Divers=0,
    Geschlundef=0,
    IK=sample(ListeKrankenkassen$IK, n/4, replace = TRUE),
    Kasse=NA
  )
  art.stamm$Maennl<- sample(0:1, n/4, replace = TRUE)
  art.stamm$Weibl<- 1 - art.stamm$Maennl
  art.stamm$Status_M<- sample(0:1, n/4, replace = TRUE)
  art.stamm$Status_F<- 1 - art.stamm$Maennl
  art.stamm$Status_R<- sample(0:1, n/4, replace = TRUE)
  
  diag.dates<- sample(date.vec, n, replace = TRUE)
  sample.codes<- c(icd10_blocks$start_code, "X31", "X30", "P81.0")
  art.diag<- data.frame(
    uniPatID= sample(ID.levels, n, replace = TRUE),
    TG_DateNum= diag.dates,
    Typ=sample(c("D","DD"), n, replace = TRUE, prob = c(0.95,0.05)),
    icd10=sample(sample.codes, n, replace = TRUE)
  )
  
  probs<- runif(n - 1) <= 0.1
  art.diag$uniPatID[seq(2, n)[probs]]<- art.diag$uniPatID[seq(1, n - 1)[probs]]
  art.diag$TG_DateNum[seq(2, n)[probs]]<- art.diag$TG_DateNum[seq(1, n - 1)[probs]]
  
  art.konsul<- data.frame(
    uniPatID= ID.levels,
    PatID= NA,
    TG_DateNum= art.stamm$TG_DateNum,
    index_i=NA,
    PraxisID= sample(c(1:6,8), n/4, replace = TRUE),
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
    A_Rauchen= sample(c(0,1), n/4, replace = TRUE),
    RF_Rauchen= sample(c(0,1), n/4, replace = TRUE),
    A_Alkohol= sample(c(0,1), n/4, replace = TRUE),
    RF_Alkohol= sample(c(0,1), n/4, replace = TRUE),
    A_Sport= sample(c(0,1), n/4, replace = TRUE),
    RF_Sport= sample(c(0,1), n/4, replace = TRUE),
    verzogen= NA,
    verstorben= NA
  )
  
  assign("Diag3", art.diag, envir = .GlobalEnv)
  assign("Stamm3", art.stamm, envir = .GlobalEnv)
  assign("Konsul3", art.konsul, envir = .GlobalEnv)
}    

set.seed(81461650)
art.data(100000)













