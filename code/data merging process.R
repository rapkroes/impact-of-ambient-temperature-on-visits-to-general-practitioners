###Data Merging Process###Data Merging Process###Data Merging Process###Data Merging Process###Data Merging Process###Data Merging Process###Data Merging Process###Data Merging Process###Data Merging Process###Data Merging Process###Data Merging Process###

###load the rest of the (publicly available) data

location.name<- c("aalen","ammerbuch","baiersbronn","boeblingen","boennigheim","pforzheim","pleidelsheim","salach","schluchsee","schopfheim","schwaebisch_gmuend","waldachtal","wendlingen")

#weather data
for(i in seq_along(location.name)){
  url_string<- paste0("https://raw.githubusercontent.com/rapkroes/impact-of-ambient-temperature-on-visits-to-general-practitioners/main/weather%20data/wetter%20",location.name[i],".csv")
  im<- read.csv(url_string)
  assign(paste0("wetter_",location.name[i]),im)
}

#daylight data
for(i in seq_along(location.name)){
  url_string<- paste0("https://raw.githubusercontent.com/rapkroes/impact-of-ambient-temperature-on-visits-to-general-practitioners/main/weather%20data/wetter%20",location.name[i],".csv")
  im<- read.csv(url_string)
  assign(paste0("wetter_",location.name[i]),im)
}