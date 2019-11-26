
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinyBS))
suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(data.table))
#suppressPackageStartupMessages(library(summarytools))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(png))
suppressPackageStartupMessages(library(qrencoder))
suppressPackageStartupMessages(library(rintrojs))
suppressPackageStartupMessages(library(visNetwork))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(highcharter))
suppressPackageStartupMessages(library(htmltools))
suppressPackageStartupMessages(library(bsplus))
suppressPackageStartupMessages(library(WriteXLS))
suppressPackageStartupMessages(library(dataframes2xls))
suppressPackageStartupMessages(library(rhandsontable))

source("busy_indicator.R")
# load data

source("cassavaclonetrackerv1.R")

# SCREENHOUSE & DIAGNOSTICS
#database = fread("data/cassavadata.csv")
database[database==999] = NA

d_factors = c("Priority", "Category", "Source_of_ICK_germplasm","Original_Source", "Contact_person","Bay_in_Glasshouse","CMD_symptoms_in_glasshouse", "CBSD_symptoms_in_glasshouse",
            "Virus_status")
d_numbers = grep("Number", names(database), value=T)
d_dates = grep("Date", names(database), value=T)

database[,names(database)[names(database) %in% d_factors]] = database[,lapply(.SD, as.factor), .SDcols = names(database)[names(database) %in% d_factors]]
database[,names(database)[names(database) %in% d_numbers]] = database[,lapply(.SD, as.integer), .SDcols = names(database)[names(database) %in% d_numbers]]
#database[,names(database)[names(database) %in% d_dates]] = database[,lapply(.SD, as.character,anytime::anydate), .SDcols = names(database)[names(database) %in% d_dates]]

# MERISTEMS
#meristemsdata = fread("data/meristemdata.csv")
meristemsdata = setDT(meristemsdata)

m_factors = c("CMD_Symptoms_after_hardening","CBSD_Symptoms_after_hardening","Symptoms_after_hardening","EACMV_End_point","ACMV_End_point", "CBSD_Real_time","UCBSD_Real_time",
            "Next_step","Summary")
m_numbers = grep("Number", names(meristemsdata), value=T)
m_dates = grep("Date", names(meristemsdata), value=T)

meristemsdata[,names(meristemsdata)[names(meristemsdata) %in% m_factors]] = meristemsdata[,lapply(.SD, as.factor), .SDcols = names(meristemsdata)[names(meristemsdata) %in% m_factors]]
meristemsdata[,m_numbers] = meristemsdata[,lapply(.SD, as.integer), .SDcols = m_numbers]
#meristemsdata[,m_dates] = meristemsdata[,lapply(.SD, as.character, anytime::anydate), .SDcols = m_dates]
  
# MERISTEM
 
meristems = database[,c("IITA_Cassava_Kephis_ICK","Category")][meristemsdata, on="IITA_Cassava_Kephis_ICK"] %>%
   .[complete.cases(.$MeristemID),] %>%
   setorder(.,-MeristemID)
meristems[,c("Available_Plants_in_Hardening","Date_of_hardening_2","LastLabNo","CMD_infected", "CBSD_infected")] = NULL


# Summary Table

meristems_numbers = meristems[,c("IITA_Cassava_Kephis_ICK","Number_of_plants","Number_of_test_tubes","Number_of_baby_jars")][,lapply(.SD, sum, na.rm=T), by=IITA_Cassava_Kephis_ICK]

#meristems_dates = meristems[,c("IITA_Cassava_Kephis_ICK","Date_of_meristem_excise","Date_of_meristem_multiplication","Date_of_hardening")][, lapply(.SD,max, na.rm=T), by=IITA_Cassava_Kephis_ICK]

meristems_symptoms = setorder(meristems[,c("IITA_Cassava_Kephis_ICK","Date_of_recording_symptoms","CMD_Symptoms_after_hardening","CBSD_Symptoms_after_hardening","Symptoms_after_hardening")], -Date_of_recording_symptoms) # sort by date desc
meristems_symptoms$index = rowSums(!is.na(meristems_symptoms)) # count missing values in a row
meristems_symptoms = meristems_symptoms[meristems_symptoms$index>1,] # keeps rows with less missing values
meristems_symptoms = meristems_symptoms[!duplicated(meristems_symptoms$IITA_Cassava_Kephis_ICK),] # drop duplicates by ICK
meristems_symptoms$index = NULL

meristems_endpoint = setorder(meristems[,c("IITA_Cassava_Kephis_ICK","Date_of_CMD_diagnostics","EACMV_End_point", "ACMV_End_point")], -Date_of_CMD_diagnostics)
meristems_endpoint$index = rowSums(!is.na(meristems_endpoint)) # count missing values in a row
meristems_endpoint = meristems_endpoint[meristems_endpoint$index>1,] # keeps rows with less missing values
meristems_endpoint = meristems_endpoint[!duplicated(meristems_endpoint$IITA_Cassava_Kephis_ICK),] # drop duplicates by ICK
meristems_endpoint$index = NULL

meristems_realtime = setorder(meristems[,c("IITA_Cassava_Kephis_ICK","Date_of_CBSD_diagnostics","CBSD_Real_time","UCBSD_Real_time","Next_step","Summary")],-Date_of_CBSD_diagnostics)
meristems_realtime$index = rowSums(!is.na(meristems_realtime)) # count missing values in a row
meristems_realtime = meristems_realtime[meristems_realtime$index>1,] # keeps rows with less missing values
meristems_realtime = meristems_realtime[!duplicated(meristems_realtime$IITA_Cassava_Kephis_ICK),] # drop duplicates by ICK
meristems_realtime$index = NULL


# SUMMARY TABLE

# summaryTable = Reduce(function(x,y)merge(x,y, all.x=T, by = "IITA_Cassava_Kephis_ICK"), list(database, meristems_dates))
# summaryTable = Reduce(function(x,y)merge(x,y, all.x=T, by = "IITA_Cassava_Kephis_ICK"), list(summaryTable, meristems_numbers))
# summaryTable = Reduce(function(x,y)merge(x,y, all.x=T, by = "IITA_Cassava_Kephis_ICK"), list(summaryTable, meristems_symptoms))
# summaryTable = Reduce(function(x,y)merge(x,y, all.x=T, by = "IITA_Cassava_Kephis_ICK"), list(summaryTable, meristems_endpoint))
# summaryTable = Reduce(function(x,y)merge(x,y, all.x=T, by = "IITA_Cassava_Kephis_ICK"), list(summaryTable, meristems_realtime))
# 
# # Rename names
 colnames(meristems) = gsub("[.]"," ",gsub("_"," ", names(meristems)))
 colnames(database) = gsub("[.]"," ",gsub("_"," ", names(database)))
# colnames(summaryTable) = gsub("[.]"," ",gsub("_"," ", names(summaryTable)))
#  
# # summaryTable = setcolorder(summaryTable, c(names(database),
# #                                          "Date of meristem multiplication","Number of test tubes","Number of baby jars",
# #                                          "Date of hardening", "Number of plants", 
# #                                          "Date of recording symptoms","CMD Symptoms after hardening","CBSD Symptoms after hardening","Symptoms after hardening",
# #                                          "Date of CMD diagnostics","EACMV End point","ACMV End point",
# #                                          "Date of CBSD diagnostics","CBSD Real time","UCBSD Real time",
# #                                          "Next step","Summary"
# # ))
# #summaryTable$`Date of meristem excise` = NULL
# 
# s_dates = grep("Date", names(summaryTable), value = T)
# s_number = grep("Number", names(summaryTable), value = T)
# s_factor = c("Priority","Category","Source of ICK germplasm","Original Source","Contact person","Bay in Glasshouse","CMD symptoms in glasshouse",
# "CBSD symptoms in glasshouse","Virus status","CMD Symptoms after hardening","CBSD Symptoms after hardening","Symptoms after hardening",
# "EACMV End point","ACMV End point","CBSD Real time","UCBSD Real time","Next step","Summary")                               
# 
# summaryTable[,s_factor] = summaryTable[,lapply(.SD, as.factor), .SDcols = s_factor]
# summaryTable[,s_number] = summaryTable[,lapply(.SD, as.integer), .SDcols = s_number]
# summaryTable[,s_dates] = summaryTable[,lapply(.SD, as.character, anytime::anydate), .SDcols = s_dates]

source("cleantable.R")

