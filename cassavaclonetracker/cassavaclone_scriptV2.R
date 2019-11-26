
cat("\014")
rm(list = ls(all=T))

suppressPackageStartupMessages(library(koboloadeR))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(RCurl))
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(naniar))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(rowr))
suppressPackageStartupMessages(library(zip))
suppressPackageStartupMessages(library(naniar))
suppressPackageStartupMessages(library(rdrop2)) # devtools::install_github("karthik/rdrop2")

#---------------Get Data from ONA--------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------------

kephis = koboloadeR::kobo_data_downloader("447577", "seedtracker:Seedtracking101", api="ona")
kephis = setDT(kephis)
kephis[,names(kephis):= lapply(.SD, as.character)]
kephis[kephis=="n/a"] = NA

# 1. Entry Received at KEPHIS
entry_cols = c(grep("new_plant_name$", names(kephis)),
               grep("ickassign$", names(kephis)),
               grep("source$", names(kephis)),
               grep("category$", names(kephis)),
               grep("category_other$", names(kephis)),
               grep("source_other$", names(kephis)),
               grep("contact$", names(kephis)),
               grep("contact_add$", names(kephis)),
               grep("entrydate$", names(kephis)),
               grep("total_sticks_received$", names(kephis)), 
               grep("number_sent_to_glasshouse$", names(kephis)),
               grep("ick_bay_in_glasshouse$", names(kephis)),
               grep("ick_bay_in_glasshouse_other$", names(kephis)),
               grep("number_sent_to_thermotherapy$", names(kephis)))
new_entry = kephis[,..entry_cols]

new_entry = new_entry[complete.cases(new_entry$`new_plants/ickassign`),]
colnames(new_entry) = c("Unique_Cassava_ID_UCID","IITA_Cassava_Kephis_ICK","Source_of_ICK_germplasm","Other_source","Category","Other_category","Contact_person","Other_contact","Date_brought_to_KEPHIS",
                        "Number_received","Number_sent_glasshouse","Bay_in_glasshouse","Bay_other","Number_sent_thermotherapy")
new_entry$Source_of_ICK_germplasm = ifelse(is.na(new_entry$Source_of_ICK_germplasm) & !is.na(new_entry$Other_source), new_entry$Other_source, new_entry$Source_of_ICK_germplasm)
new_entry$Category = ifelse(is.na(new_entry$Category) & !is.na(new_entry$Other_category), new_entry$Other_category, new_entry$Category)
new_entry$Contact_person = ifelse(is.na(new_entry$Contact_person) & !is.na(new_entry$Other_contact), new_entry$Other_contact, new_entry$Contact_person)
new_entry$Bay_in_glasshouse = ifelse(is.na(new_entry$Bay_in_glasshouse) & !is.na(new_entry$Bay_other), new_entry$Bay_other, new_entry$Bay_in_glasshouse)
new_entry = new_entry[,-c("Other_source","Other_category","Other_contact","Bay_other")]
new_entry$Bay_in_glasshouse = gsub("_"," ", new_entry$Bay_in_glasshouse)
new_entry$Contact_person = gsub("_"," ", new_entry$Contact_person)

# 3. Glasshouse Diagnostics

# CMD
cmd_cols = c(
  grep("ick_cmd$", names(kephis)),
  grep("cmd_visual_symptoms_date$", names(kephis)),
  grep("cmd_visual_symptoms$", names(kephis)))
cmd = kephis[,..cmd_cols] %>%
  .[complete.cases(.),]
colnames(cmd) = c("IITA_Cassava_Kephis_ICK","Date_of_CMD_visual_symptoms","CMD_visual_symptoms")

# CBSD
cbsd_cols = c(
  grep("ick_cbsd$", names(kephis)),
  grep("cbsd_visual_symptoms_date$", names(kephis)),
  grep("cbsd_visual_symptoms$", names(kephis)))
cbsd = kephis[,..cbsd_cols] %>%
  .[complete.cases(.),]
colnames(cbsd) = c("IITA_Cassava_Kephis_ICK","Date_of_CBSD_visual_symptoms","CBSD_visual_symptoms")

glasshouse_diagnostics = dplyr::left_join(cmd, cbsd,by="IITA_Cassava_Kephis_ICK") %>%
  .[complete.cases(.$IITA_Cassava_Kephis_ICK),]

# Laboratory diagnostics
cmd_diag_col = c(
  grep("glasshousediagnostics$", names(kephis)),
  grep("meristemCMD$", names(kephis)),
  grep("date_of_cmd$", names(kephis)),
  grep("EACMV_diagnostics$", names(kephis)),
  grep("ACMV_diagnostics$", names(kephis))
) %>%
  unique()
  
cmd_diag = kephis[,..cmd_diag_col]
colnames(cmd_diag) = c("Diagnostics","IITA_Cassava_Kephis_ICK","Date_of_CMD_diagnostics","EACMV_symptoms","ACMV_symptoms")

cmd_diag$Diagnostics = NULL
if(nrow(cmd_diag)>0){
  cmd_diag = cmd_diag[grepl("_", cmd_diag$IITA_Cassava_Kephis_ICK) == F]# keep only the ICKs
}
cmd_diag = cmd_diag %>%
  .[complete.cases(.),]

cbsd_diag_col = c(
  grep("glasshousediagnostics$", names(kephis)),
  grep("meristemCBSV$", names(kephis)),
  grep("date_of_cbsv$", names(kephis)),
  grep("CBSV_diagnostics$", names(kephis)),
  grep("UCBSV_diagnostics$", names(kephis))
) %>%
  unique()

cbsd_diag = kephis[,..cbsd_diag_col]
colnames(cbsd_diag) = c("Diagnostics","IITA_Cassava_Kephis_ICK","Date_of_CBSD_diagnostics","UCBSV_symptoms",
                        "CBSV_symptoms")
cbsd_diag$Diagnostics = NULL
if(nrow(cbsd_diag)>0){
  cbsd_diag = cbsd_diag[grepl("_", cbsd_diag$IITA_Cassava_Kephis_ICK) == F]# keep only the ICKs
}
cbsd_diag = cbsd_diag %>%
  .[complete.cases(.),]
glasshouse_diagnostics = Reduce(function(x,y)merge(x,y, all.x=T,by = "IITA_Cassava_Kephis_ICK"), list(glasshouse_diagnostics,cmd_diag,cbsd_diag))


# 4. Tips

tips_cols = c(grep("ick_tips$", names(kephis)),grep("tips_date$", names(kephis)),grep("number_tips$", names(kephis)))
tips = kephis[,..tips_cols]
colnames(tips) = c("IITA_Cassava_Kephis_ICK","Tips_date","Number_of_tips")



# 5. Meristem excision

meristems_cols = c(grep("ick_meristem$", names(kephis)), 
             # grep("getmeristemUCID$", names(kephis)),
              grep("meristem_date$", names(kephis)), 
              grep("meristems_number$", names(kephis)))
meristems = kephis[,..meristems_cols]
colnames(meristems) = c("IITA_Cassava_Kephis_ICK","Date_of_meristem_excise","Number_of_meristems_excised")
meristems = meristems[complete.cases(meristems$IITA_Cassava_Kephis_ICK),]

# generate meristems-ID
# meristems[,c("IITA_Cassava_Kephis_ICK","Number_of_meristems_excised","Date_of_meristem_excise","Date_of_meristem_excise")] = 
#   meristems[,c("IITA_Cassava_Kephis_ICK","Number_of_meristems_excised","Date_of_meristem_excise","Date_of_meristem_excise")][,lapply(.SD, sum, na.rm=T), by=IITA_Cassava_Kephis_ICK]

if(nrow(meristems)>0){
  meristems = meristems %>%
    dplyr::group_by(IITA_Cassava_Kephis_ICK) %>%
    summarise(
      Number_of_meristems_excised = sum(as.integer(na.omit(Number_of_meristems_excised))),
      First_Date_of_meristem_excise = min(na.omit(Date_of_meristem_excise)),
      Last_Date_of_meristem_excise = max(na.omit(Date_of_meristem_excise))
    )
}else {
  meristems$First_Date_of_meristem_excise = character()
  meristems$Last_Date_of_meristem_excise = character()
}
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#********************************************************************************************************************************************************************************
# -----------------------------------------------------------------------------------------------------------------

# 1. SCREENHOUSE DIAGNOSTICS

cassavadata = Reduce(function(x,y)merge(x,y,all=T, by="IITA_Cassava_Kephis_ICK"), 
                     list(new_entry, glasshouse_diagnostics, meristems, tips)) %>%
  .[complete.cases(.$IITA_Cassava_Kephis_ICK),]

# Screenhouse and diagnostics Database -----------------------------------------------------------------------------------
screenhouse_database = koboloadeR::kobo_data_downloader("454626", "seedtracker:Seedtracking101", api="ona")  %>% 
  mutate_all(as.character) %>%
  dplyr::select(-starts_with("_"))
screenhouse_database[,c("Lab_number","Additional_lab_no")] = NULL
screenhouse_database[screenhouse_database=="n/a"] <- NA
colnames(screenhouse_database) = gsub("realtime","Real_time", names(screenhouse_database))

# Merge Current Data and Database (Update Database)

database = plyr::rbind.fill(screenhouse_database, cassavadata)
database <- database %>% 
  dplyr::group_by(IITA_Cassava_Kephis_ICK) %>%
  fill(everything(), .direction = "down") %>%
  fill(everything(), .direction = "up") %>%
  slice(1)

# Select variable to keep

database = database[,c("IITA_Cassava_Kephis_ICK", "Unique_Cassava_ID_UCID", "Alternative_identifier","Priority", "Category", "Date_brought_to_KEPHIS", 
                "Source_of_ICK_germplasm", "Original_Source", "Contact_person", "Number_received", "Number_sent_glasshouse","Number_sent_thermotherapy",
                "Bay_in_Glasshouse", "Date_of_CMD_visual_symptoms","CMD_visual_symptoms",
                "Date_of_CBSD_visual_symptoms","CBSD_visual_symptoms", "Date_of_CMD_diagnostics","EACMV_symptoms","ACMV_symptoms","CMD_symptoms_in_glasshouse","Date_of_CBSD_diagnostics",    
                "UCBSV_symptoms","CBSV_symptoms",'CBSD_symptoms_in_glasshouse',"Virus_status", "Tips_date", "Number_of_tips", "Number_of_meristems_excised", 
                "First_Date_of_meristem_excise", "Last_Date_of_meristem_excise")]


database$CMD_symptoms_in_glasshouse = ifelse(!is.na(database$EACMV_symptoms) & database$EACMV_symptoms=="positive" | !is.na(database$ACMV_symptoms) & database$ACMV_symptoms=="positive", "positive",
                                         ifelse(!is.na(database$EACMV_symptoms) & database$EACMV_symptoms=="negative" & !is.na(database$ACMV_symptoms) & database$ACMV_symptoms=="negative", "negative",
                                              ifelse(!is.na(database$EACMV_symptoms) & database$EACMV_symptoms=="inconclusive" | !is.na(database$ACMV_symptoms) & database$ACMV_symptoms=="inconclusive", "inconclusive",
                                                     database$CMD_symptoms_in_glasshouse)))
database$EACMV_symptoms = NULL
database$ACMV_symptoms = NULL

database$CBSD_symptoms_in_glasshouse = ifelse(!is.na(database$UCBSV_symptoms) & database$UCBSV_symptoms=="positive" | !is.na(database$CBSV_symptoms) & database$CBSV_symptoms=="positive", "positive",
                                             ifelse(!is.na(database$UCBSV_symptoms) & database$UCBSV_symptoms=="negative" & !is.na(database$CBSV_symptoms) & database$CBSV_symptoms=="negative", "negative",
                                                    ifelse(!is.na(database$UCBSV_symptoms) & database$UCBSV_symptoms=="inconclusive" | !is.na(database$CBSV_symptoms) & database$CBSV_symptoms=="inconclusive", "inconclusive",
                                                           database$CBSD_symptoms_in_glasshouse)))
database$UCBSV_symptoms = NULL
database$CBSV_symptoms = NULL

database$Virus_status = ifelse(database$CMD_symptoms_in_glasshouse=="positive" & database$CBSD_symptoms_in_glasshouse=="positive", "infected",
                               ifelse(database$CMD_symptoms_in_glasshouse=="positive" & database$CBSD_symptoms_in_glasshouse=="negative", "infected",
                                      ifelse(database$CMD_symptoms_in_glasshouse=="negative" & database$CBSD_symptoms_in_glasshouse=="positive", "infected",
                                             ifelse(database$CMD_symptoms_in_glasshouse=="positive" & database$CBSD_symptoms_in_glasshouse=="inconclusive", "infected",
                                                    ifelse(database$CMD_symptoms_in_glasshouse=="inconclusive" & database$CBSD_symptoms_in_glasshouse=="positive", "infected",
                                                           ifelse(database$CMD_symptoms_in_glasshouse=="negative" & database$CBSD_symptoms_in_glasshouse=="negative", "virus free","infected"
                                                           ))))))


# Set data types

db_fac = c("Priority","Category", "Bay_in_Glasshouse","Virus_status", "Source_of_ICK_germplasm","Original_Source","Contact_person",
           "CMD_symptoms_in_glasshouse", "CBSD_symptoms_in_glasshouse")
db_nums = grep("Number", names(database), value=T)
db_dates = grep("Date", names(database), value=T)

database[,db_fac] %<>% mutate_all(as.factor)
database[,db_nums] %<>% mutate_all(as.integer)
database[,db_dates] %<>% mutate_all(anytime::anydate)

# Write Screenhouse & Diagnostics in the Disk
write.csv(database, file = "data/cassavadata.csv", row.names = F)
saveRDS(database, file = "data/cassavadata.rds")

zipr("data/cassavadata.zip", "data/cassavadata.csv")

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#**********************************************************************************************************************************************************************************
database[database==999]=NA

# MERISTEMS-IDs
total_meristems = database %>%
  group_by(IITA_Cassava_Kephis_ICK) %>%
  summarise(Number_of_meristems_excised = sum(na.omit(Number_of_meristems_excised)))
# total_meristems = meristems[,c("IITA_Cassava_Kephis_ICK","Number_of_meristems_excised")][,lapply(.SD, sum, na.rm=T), by=IITA_Cassava_Kephis_ICK]


#database = dplyr::left_join(total_meristems, database, by="IITA_Cassava_Kephis_ICK")
database <- database %>% 
  dplyr::group_by(IITA_Cassava_Kephis_ICK) %>%
  fill(everything(), .direction = "down") %>%
  fill(everything(), .direction = "up") %>%
  slice(1)

# database$Number_of_meristems_excised = ifelse(database$Number_of_meristems_excised.x==0, database$Number_of_meristems_excised.y, database$Number_of_meristems_excised.x)
# database$Number_of_meristems_excised.x=NULL
# database$Number_of_meristems_excised.y=NULL
database = database %>%
  .[complete.cases(.$IITA_Cassava_Kephis_ICK),]
N_Meristems = database %>%
  dplyr::filter(Number_of_meristems_excised > 0 )# | Number_of_tips > 0)

N_Meristems = N_Meristems[rep(row.names(N_Meristems), N_Meristems$Number_of_meristems_excised),] 
N_Meristems = data.table::setDT(N_Meristems)
N_Meristems = N_Meristems[, Number := 1:.N, by=IITA_Cassava_Kephis_ICK]

N_Meristems$MeristemID = paste0(N_Meristems$IITA_Cassava_Kephis_ICK,"_M",N_Meristems$Number)
N_Meristems$Date_of_meristem_excise = N_Meristems$Last_Date_of_meristem_excise
N_Meristems$First_Date_of_meristem_excise = NULL
N_Meristems$Last_Date_of_meristem_excise = NULL
N_Meristems$Number = NULL
N_Meristems$Number_of_meristems_excised = NULL

Non_Meristems = database %>%
  dplyr::filter(Number_of_tips > 0)
if(nrow(Non_Meristems)>0){
  Non_Meristems$MeristemID = paste0(Non_Meristems$IITA_Cassava_Kephis_ICK,"_Tip")
}
all_meristems = plyr::rbind.fill(N_Meristems, Non_Meristems)
all_meristems <- all_meristems %>% 
  dplyr::group_by(MeristemID) %>%
  fill(everything(), .direction = "down") %>%
  fill(everything(), .direction = "up") %>%
  slice(1)
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ****************************************************************************************************************************************************************************************************************

# 6. Multiplication

multiplication_cols = c(
  grep("meristemIDMultiplication", names(kephis)), 
  grep("date_multiplication", names(kephis)),
  grep("number_of_test_tubes", names(kephis)),
  grep("number_of_baby_jars", names(kephis)))
multiplication =  kephis[,..multiplication_cols] %>%  .[complete.cases(.),]
colnames(multiplication) = c("MeristemID","Date_of_meristem_multiplication","Number_of_test_tubes","Number_of_baby_jars")

# ----------Hardening

harden_cols = c(
  grep("meristemid$", names(kephis)),
  grep("hardening_date$", names(kephis)),
  grep("number_of_plants$", names(kephis)))
harden = kephis[,..harden_cols] %>%
  .[complete.cases(.),]
colnames(harden) = c("MeristemID","Date_of_hardening","Number_of_plants")

died_cols = c(
  grep("meristemid$", names(kephis)),
  grep("hardening_date$", names(kephis)),
  grep("number_died$", names(kephis)))
died = kephis[,..died_cols] %>%
  .[complete.cases(.),]
colnames(died) = c("MeristemID","Date_of_hardening_status","Number_of_plants_that_died")

hardening = dplyr::left_join(harden, died, by="MeristemID")

if(nrow(hardening)>0){
hardening = hardening %>%
  dplyr::group_by(MeristemID) %>%
  summarise(
    Date_of_hardening = max(Date_of_hardening),
    Number_of_plants = sum(as.integer(na.omit(Number_of_plants))), 
    Number_of_plants_that_died = sum(as.integer(na.omit(Number_of_plants_that_died))))
}

# 7. Laboratory Diagnostics

# a. symptoms

symptom_cols= c(
  grep("meristemSymptoms$", names(kephis)),
  grep("date_of_recording_symptoms$", names(kephis)),
  grep("cmd_symptoms$", names(kephis)),
  grep("cbsd_symptoms$", names(kephis)))

symptoms = kephis[,..symptom_cols] %>%
  .[complete.cases(.[,1]),]
colnames(symptoms) = c("MeristemID","Date_of_recording_symptoms","CMD_Symptoms_after_hardening","CBSD_Symptoms_after_hardening")

# b. sampling
sampling_cols = c(
  grep("meristemSampling$", names(kephis)),
  grep("labnumber$", names(kephis)),
  grep("additional_labNo$", names(kephis)),
  grep("date_of_sampling$", names(kephis)))
sampling = kephis[,..sampling_cols] %>%
  .[complete.cases(.),]

colnames(sampling) = c("MeristemID","Lab_number","Additional_lab_number","Date_of_sampling")

# c. CMD
endpoint_cols = c(
  grep("meristemCMD$", names(kephis)),
  grep("date_of_cmd$", names(kephis)),
  grep("EACMV_diagnostics$", names(kephis)),
  grep("ACMV_diagnostics$", names(kephis))) %>%
  unique()
endpoint = kephis[,..endpoint_cols] %>%
  .[complete.cases(.),]
colnames(endpoint) = c("MeristemID", "Date_of_CMD_diagnostics", "EACMV_End_point", "ACMV_End_point")

if(nrow(endpoint)>0){ 
  endpoint = endpoint[grepl("_", endpoint$MeristemID) == T]# keep only the meristems
}

# CBSD
realtime_cols = c(
  grep("meristemCBSV$", names(kephis)),
  grep("date_of_cbsv$", names(kephis)),
  grep("CBSV_diagnostics$", names(kephis)),
  grep("UCBSV_diagnostics$", names(kephis))) %>%
  unique()

realtime = kephis[,..realtime_cols]%>%
  .[complete.cases(.),]
colnames(realtime) = c("MeristemID", "Date_of_CBSV_diagnostics", "CBSV_Real_time", "UCBSV_Real_time")

if(nrow(realtime)>0){
  realtime = realtime[grepl("_", realtime$MeristemID) == T]# keep only the meristems
}
# Taken to slow growth
slowgrowth_cols = c(
  grep("date_taken_to_slow_growth$", names(kephis)),
  grep("slowgrowthID$", names(kephis)),
  grep("number_taken_to_slow_growth$", names(kephis)))

slowgrowth = kephis[,..slowgrowth_cols] %>%
  .[complete.cases(.),]
colnames(slowgrowth) = c("MeristemID", "Date_taken_to_slow_growth","Number_taken_to_slow_growth")

# --------------------------------------------------------------------------------------------------------------------------------------------------------
# MERGE DATASET
# --------------------------------------------------------------------------------------------------------------------------------------------------------

# 2. MERISTEMS

if(nrow(N_Meristems)>0){
  meristems.df = Reduce(function(x,y)merge(x,y,all.x=T, all.y=T, by="MeristemID"), list(all_meristems, multiplication, hardening, symptoms, sampling, 
                                                                             endpoint, realtime, slowgrowth))
}
meristems.df$Number_of_baby_jars = as.integer(meristems.df$Number_of_baby_jars) 
meristems.df$Number_of_test_tubes = as.integer(meristems.df$Number_of_test_tubes) 
meristems.df$Number_of_plants_that_died = as.integer(meristems.df$Number_of_plants_that_died)
meristems.df$Number_of_plants = as.integer(meristems.df$Number_of_plants)

meristems.df$Available_Plants_in_Hardening = rowSums(meristems.df[,c("Number_of_baby_jars", "Number_of_test_tubes")], na.rm=TRUE) - rowSums(meristems.df[,c("Number_of_plants_that_died", "Number_of_plants")], na.rm=TRUE)
  
# Meristems Database 
meristems_database = koboloadeR::kobo_data_downloader("457596", "seedtracker:Seedtracking101", api="ona")  %>%  
  mutate_all(as.character) %>%
  dplyr::select(-starts_with("_"))
meristems_database$Symptoms_after_hardening = gsub(" symptoms","",meristems_database$Symptoms_after_hardening)
meristems_database[meristems_database=="n/a"] <- NA
meristems_database[meristems_database=="negative/negative"] <- "negative"

meristems_database$Date_of_CBSV_diagnostics = meristems_database$Date_of_diagnostics
meristems_database$Date_of_CMD_diagnostics = meristems_database$Date_of_diagnostics

# Merge Current and Database (Update Meristem Database)
# meristemsdata = plyr::rbind.fill(meristems.df, meristems_database) %>%
#   unique()
# meristems.df[,names(meristems_database)[names(meristems_database) %in% names(meristems.df)]] %<>% mutate_all(as.character)
# meristems_database[,names(meristems_database)[names(meristems_database) %in% names(meristems.df)]] %<>% mutate_all(as.character)
meristemsdata = plyr::rbind.fill(meristems.df, meristems_database)

meristemsdata <- meristemsdata %>% 
  dplyr::group_by(MeristemID) %>%
  fill(everything(), .direction = "down") %>%
  fill(everything(), .direction = "up") %>%
  slice(1)

meristemsdata = meristemsdata[,c(
    "MeristemID","IITA_Cassava_Kephis_ICK","Unique_Cassava_ID_UCID","Date_of_meristem_excise","Tips_date","Number_of_tips","Date_of_meristem_multiplication", "Number_of_test_tubes",
    "Number_of_baby_jars","Date_of_hardening","Number_of_plants","Date_of_hardening_2","Number_of_plants_that_died","Available_Plants_in_Hardening","Date_of_recording_symptoms",     
    "CMD_Symptoms_after_hardening","CBSD_Symptoms_after_hardening","Symptoms_after_hardening","Lab_number","Additional_lab_number","Date_of_sampling",
    "Date_of_CMD_diagnostics","EACMV_End_point","ACMV_End_point","Date_of_CBSV_diagnostics","CBSV_Real_time","UCBSV_Real_time", "Pots_in_BecA_glasshouse","Next_step"               
    )] # 

# SYMPTOMS AFTER HARDENING

meristemsdata$Symptoms_after_hardening1 = ifelse(meristemsdata$CMD_Symptoms_after_hardening=="True" & meristemsdata$CBSD_Symptoms_after_hardening=="True", paste0("CMD, CBSD"),
                                               ifelse(meristemsdata$CMD_Symptoms_after_hardening=="True" & meristemsdata$CBSD_Symptoms_after_hardening=="False", "CMD",
                                                      ifelse(meristemsdata$CMD_Symptoms_after_hardening=="False" & meristemsdata$CBSD_Symptoms_after_hardening=="True", "CBSD",
                                                             ifelse(meristemsdata$CMD_Symptoms_after_hardening=="False" & meristemsdata$CBSD_Symptoms_after_hardening=="False","No symptoms",
                                                                    meristemsdata$Symptoms_after_hardening))))
meristemsdata$Symptoms_after_hardening = ifelse(!is.na(meristemsdata$Symptoms_after_hardening),meristemsdata$Symptoms_after_hardening, meristemsdata$Symptoms_after_hardening1)
meristemsdata$Symptoms_after_hardening1 = NULL

# NEXT STEP
meristemsdata$CMD_infected = ifelse(meristemsdata$EACMV_End_point=="positive" | meristemsdata$ACMV_End_point=="positive", "CMD infected",
                               ifelse(meristemsdata$EACMV_End_point=="negative" & meristemsdata$ACMV_End_point=="negative", "virus free", NA))

meristemsdata$CBSV_infected = ifelse(meristemsdata$CBSV_Real_time=="positive" | meristemsdata$UCBSV_Real_time=="positive","CBSV infected",
                                             ifelse(meristemsdata$CBSV_Real_time=="negative" & meristemsdata$UCBSV_Real_time=="negative", "virus free",NA))
                                     

meristemsdata$Summary = ifelse(meristemsdata$CMD_infected == "virus free" & meristemsdata$CBSV_infected == "virus free", "virus free",
                               ifelse(meristemsdata$CMD_infected == "virus free" & meristemsdata$CBSV_infected == "CBSV infected", "CBSV infected",
                                      ifelse(meristemsdata$CMD_infected == "CMD infected" & meristemsdata$CBSV_infected == "virus free", "CMD infected",NA)))
  
meristemsdata$Next_step = ifelse(meristemsdata$Summary=="virus free", "Go to slow growth",
                                 ifelse(!meristemsdata$Summary=="virus free" & !is.na(meristemsdata$Summary), "Go to thermotherapy",NA))
# meristemsdata[,c("CMD_infected", "CBSV_infected")] = NULL

# Set data types

mdb_fac = c("Symptoms_after_hardening","EACMV_End_point","ACMV_End_point","CBSV_Real_time","UCBSV_Real_time","Next_step")
mdb_nums = c("Pots_in_BecA_glasshouse", "Lab_number", "Number_of_tips")
mdb_dates = grep("Date", names(meristemsdata), value=T)

meristemsdata[,mdb_fac] %<>% mutate_all(as.factor)
meristemsdata[,mdb_nums] %<>% mutate_all(as.integer)
meristemsdata[,mdb_dates] %<>% mutate_all(anytime::anydate)

meristemsdata$n = as.integer(gsub("\\D", "", stringr::str_split_fixed(meristemsdata$MeristemID,"_",2)[,2]))
meristemsdata$Lab_number = as.integer(meristemsdata$Lab_number)
meristemsdata$LastLabNo = max(meristemsdata$Lab_number[!is.na(meristemsdata$Lab_number)])

meristemsdata = meristemsdata  %>%
  dplyr::filter(!is.na(MeristemID)) %>%
  dplyr::arrange(IITA_Cassava_Kephis_ICK, desc(MeristemID))
meristemsdata$n = NULL

# Write Meristems Database in the Disk 

write.csv(meristemsdata, file = "data/meristemdata.csv", row.names = F)
saveRDS(meristemsdata, file = "data/meristemdata.rds")
zipr("data/meristemdata.zip", "data/meristemdata.csv")

# MERISTEMS PER ICK
meristems_per_ick = meristemsdata[,c("IITA_Cassava_Kephis_ICK","MeristemID", "Next_step")] %>%
  dplyr::filter(is.na(Next_step)| Next_step=="Go to slow growth") 
meristems_per_ick$Next_step = NULL

meristems_per_ick = meristems_per_ick %>%
  dplyr::arrange(MeristemID) %>%
  data.table::setDT()
meristems_per_ick = meristems_per_ick[, N := 1:.N, by=IITA_Cassava_Kephis_ICK]
meristems_per_ick = reshape(meristems_per_ick, direction = "wide", idvar = "IITA_Cassava_Kephis_ICK", timevar = "N") %>%
  data.frame()
if(ncol(meristems_per_ick)>2){
  meristems_per_ick$NumberMeristems = rowSums(!is.na(meristems_per_ick))-1
}
colnames(meristems_per_ick) = gsub("[.]","", names(meristems_per_ick))
write.csv(meristems_per_ick, file = "data/meristems_list.csv", row.names = F)
zipr("data/meristems_list.zip", "data/meristems_list.csv")

# ---------------------------------------------------------------------------------------------------------------------------------
# UPDATE ONA MEDIA FILES
# ---------------------------------------------------------------------------------------------------------------------------------

 # updated database
 
  # TOKEN
  raw.result <- GET("https://api.ona.io/api/v1/user.json", authenticate(user = "seedtracker",password = "Seedtracking101"))
  raw.result.char<-rawToChar(raw.result$content)
  raw.result.json<-fromJSON(raw.result.char)
  TOKEN_KEY <- raw.result.json$temp_token
  
  #########cassva data##############################################
  #delete
  cassavaid <- readChar("data/cassavaid.txt", 10)
  hdr=c(Authorization=paste("Temptoken ",TOKEN_KEY))
  DELETE(paste("https://api.ona.io/api/v1/metadata/",cassavaid),add_headers(Authorization=paste("Temptoken ",TOKEN_KEY)))
  
  # upload
  new_cassavaid = ''
  while(new_cassavaid==''){
  header=c(Authorization=paste("Temptoken ", TOKEN_KEY), `Content-Type` = 'multipart/form-data')
  post.results <- postForm("https://api.ona.io/api/v1/metadata.json",
                                  data_value='data/cassavadata.zip',data_type='media',xform=447577,
                                  data_file=fileUpload(filename = "data/cassavadata.zip",contentType = 'text/csv'),
                                  .opts=list(httpheader=header), verbose = TRUE)
  
  ## get ID
  raw.result.json<-fromJSON(post.results)
  new_cassavaid <- raw.result.json$id
  }
  cat(new_cassavaid, file = "data/cassavaid.txt")
  
  ######### meristems
  
  #########cassva data##############################################
  #delete
  meristemid <- readChar("data/meristemid.txt", 10)
  hdr=c(Authorization=paste("Temptoken ",TOKEN_KEY))
  DELETE(paste("https://api.ona.io/api/v1/metadata/",meristemid),add_headers(Authorization=paste("Temptoken ",TOKEN_KEY)))
  
  # upload
  new_meristemid = ''
  while(new_meristemid==''){
    header=c(Authorization=paste("Temptoken ", TOKEN_KEY), `Content-Type` = 'multipart/form-data')
    post.results <- postForm("https://api.ona.io/api/v1/metadata.json",
                             data_value='data/meristemdata.zip',data_type='media',xform=447577,
                             data_file=fileUpload(filename = "data/meristemdata.zip",contentType = 'text/csv'),
                             .opts=list(httpheader=header), verbose = TRUE)
    
    ## get ID
    raw.result.json<-fromJSON(post.results)
    new_meristemid <- raw.result.json$id
  }
  cat(new_meristemid, file = "data/meristemid.txt")
    
# List of ICK Meristems
  #delete
  listid <- readChar("data/listid.txt", 10)
  hdr=c(Authorization=paste("Temptoken ",TOKEN_KEY))
  DELETE(paste("https://api.ona.io/api/v1/metadata/",listid),add_headers(Authorization=paste("Temptoken ",TOKEN_KEY)))
  
  # upload
  new_listid = ''
  while(new_listid==''){
    header=c(Authorization=paste("Temptoken ", TOKEN_KEY), `Content-Type` = 'multipart/form-data')
    post.results <- postForm("https://api.ona.io/api/v1/metadata.json",
                             data_value='data/meristems_list.zip',data_type='media',xform=447577,
                             data_file=fileUpload(filename = "data/meristems_list.zip",contentType = 'text/csv'),
                             .opts=list(httpheader=header), verbose = TRUE)
    
    ## get ID
    raw.result.json<-fromJSON(post.results)
    new_listid <- raw.result.json$id
  }
  cat(new_listid, file = "data/listid.txt")
  
  