#############functions#############functions#############functions#############functions#############functions#############functions#############functions#############functions#############functions#############functions#############functions#############functions#############functions

#icd10.to.class: finds the disease/ injury classification of an icd10 vector as listed in the preregistration
#suspicious.Diag.entries: Since the icd10 field is a text field in the GP's software, the data might contain typing errors or atypical notation of diseases. This function returns a vector that filters out whose entries are =1 if they are 'suspicious' and 0 otherwise. An entry is considered suspicious if it does not have a length of 4, 6, or 7 characters. If it has 4, 6, or 7 characters it is suspicious if it does not follow either of the patterns 
  #1. capital letter, two numbers, capital letter
  #2. capital letter, two numbers, fullstop, one or two numbers, capital letter
#


icd10.to.class<- function(icd10vec){
  #finds the disease/ injury classification of an icd10 vector as listed in the preregistration
  first<- substr(icd10vec,1,1)
  no<- as.numeric(substr(icd10vec,2,3))
  out<- numeric(length(icd10vec))
  
  for(i in seq_along(out)){
    selector<- which(first[i]== classification.matrix$letter & no[i]>=classification.matrix$start_no& no[i]<=classification.matrix$end_no)
    if(length(selector)==0){
      out[i]<- 11
    }else if(length(selector)==1){
      out[i]<- classification.matrix$class[selector]
    }else{
      stop(paste("For entry",i, "selector has a length longer than 1."))
    }
  }
  
  return(out)
}


suspicious.Diag.entries<- function(icd10vec){
  #Since the icd10 field is a text field in the GP's software, the data might contain typing errors or atypical notation of diseases. This function returns a vector that filters out whose entries are =1 if they are 'suspicious' and 0 otherwise. An entry is considered suspicious if it does not have a length of 3, 5, or 6 characters. If it has 3, 5, or 6 characters it is suspicious if it does not follow either of the patterns 
  #1. capital letter, two numbers, capital letter
  #2. capital letter, two numbers, fullstop, one or two numbers, capital letter
  first<- substr(icd10vec,1,1)
  no<- as.numeric(substr(icd10vec,2,3))
  out<- numeric(length(icd10vec))
  
  for(i in seq_along(icd10vec)){
    entry<- icd10vec[i]
    nc<- nchar(entry)
    
    pattern_1<- grepl("^[A-Z]\\d{2}[A-Z]$", entry)
    pattern_2<- grepl("^[A-Z]\\d{2}\\.\\d{1,2}[A-Z]$", entry)
    has.comma.or.minus<- grepl("[,\\-]", entry)
    
    if(!(nc %in% c(3,5,6))){
      out[i]<- 1
    }else{
      if(!(pattern_1|pattern_2|has.comma.or.minus)){
        out[i]<- 1
      }
    }
  }
  return(out)
}


TG_DateNum2date<- function(TG_DateNum){
  #takes a vector of TG_DateNum dates and returns them as yyyy-mm-dd dates
  as.Date(TG_DateNum, origin = "0000-01-01")
}


TG_DateNum2dow<- function(tgdatenumvec){
  #takes a vector of TG_DateNum dates and returns them as their respective days of the week as ordered factor variable
  out<- wday(TG_DateNum2date(tgdatenumvec), label = TRUE)
}

TG_DateNum2week<- function(tgdatenumvec){
  #takes a vector of TG_DateNum dates and returns them as their respective week within the year according to ISO 8601 as factor variable
  out<- as.factor(isoweek(TG_DateNum2date(tgdatenumvec)))
}

TG_DateNum2month<- function(tgdatenumvec){
  #takes a vector of TG_DateNum dates and returns them as their respective month as ordered factor variable
  out<- month(TG_DateNum2date(tgdatenumvec), label = TRUE)
}

TG_DateNum2year<- function(tgdatenumvec){
  #takes a vector of TG_DateNum dates and returns them as their respective month as ordered factor variable
  out<- as.factor(isoyear(TG_DateNum2date(tgdatenumvec)))
}

