###Data Merging Process###Data Merging Process###Data Merging Process###Data Merging Process###Data Merging Process###Data Merging Process###Data Merging Process###Data Merging Process###Data Merging Process###Data Merging Process###Data Merging Process###

# 1. Use Diag3 as baseline
# 2. add Stamm3 data
# 3. add Konsul3 data
# 4. add weather data
# 5. add daylight data
# 6. add Covid-19 data

# 1
fulldf_1<- Diag3

# 2
fulldf_2<- add.stamm.new.par(fulldf_1, Stamm3, 50, 2)

# 3
fulldf_3<- add.konsul(fulldf_2, Konsul3, 50, 2)

# 4
fulldf_4<- add.weather(fulldf_3, location_information)
