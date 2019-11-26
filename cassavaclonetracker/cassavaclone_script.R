#library(profvis)


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
suppressPackageStartupMessages(library(rdrop2)) # devtools::install_github("karthik/rdrop2")

#---------------Get Data from ONA--------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------------

kephis = koboloadeR::kobo_data_downloader("447577", "seedtracker:Seedtracking101", api="ona")  %>%
  mutate_all(as.character)
kephis[kephis=='n/a'] <- NA

# 1. Entry Received at KEPHIS

new_entry= kephis %>%
  dplyr::select(ends_with("/new_plant_name"), ends_with("/ickassign"), ends_with("/source"),ends_with("/category"), ends_with("/contact"), 
          ends_with("/entrydate"),ends_with("/total_sticks_received"),ends_with("/number_sent_to_glasshouse"),
          ends_with("ick_bay_in_glasshouse"),ends_with("/number_sent_to_thermotherapy"))
colnames(new_entry) = c("Unique_Cassava_ID_UCID","IITA_Cassava_Kephis_ICK","Source_of_ICK_germplasm","Category","Contact_person","Date_brought_to_KEPHIS",
                        "Number_received","Number_sent_glasshouse","Bay_in_Glasshouse","Number_sent_thermotherapy")
new_entry = new_entry[complete.cases(new_entry$IITA_Cassava_Kephis_ICK),]

# 3. Glasshouse Diagnostics

glasshouse_visual = kephis %>% 
  dplyr::select(ends_with("ick_visual"),ends_with("visual_symptoms_date"),ends_with("cmd_visual_symptoms"), ends_with("cbsd_visual_symptoms"))
colnames(glasshouse_visual) = c("IITA_Cassava_Kephis_ICK","Date_of_glasshouse_visual_symptoms","CMD_visual_symptoms","CBSD_visual_symptoms")
glasshouse_visual = glasshouse_visual[complete.cases(glasshouse_visual$IITA_Cassava_Kephis_ICK),]

# ---------------------------------------------------------------------------------------
# Laboratory diagnostics

cmd_diag = kephis %>%
  dplyr::select(ends_with("meristemCMD"),ends_with("date_of_cmd"),ends_with("EACMV_diagnostics"),ends_with("ACMV_diagnostics")) 
colnames(cmd_diag) = c("IITA_Cassava_Kephis_ICK","Date_of_CMD_diagnostics","EACMV_symptoms","ACMV_symptoms")
cmd_diag = cmd_diag[complete.cases(cmd_diag$IITA_Cassava_Kephis_ICK),]
if(nrow(cmd_diag)>0){
  cmd_diag$Diagnostics = NULL
  cmd_diag = cmd_diag[grepl("_", cmd_diag$IITA_Cassava_Kephis_ICK) == F]# keep only the ICKs
}

# cbsd
cbsd_diag = kephis %>%
  dplyr::select(
  ends_with("meristemCBSD"), ends_with("date_of_cbsd"),
  ends_with("CBSD_diagnostics"), ends_with("UCBSD_diagnostics")) 
colnames(cbsd_diag) = c("IITA_Cassava_Kephis_ICK","Date_of_CBSD_diagnostics","UCBSD_symptoms",
                        "CBSD_symptoms")
cbsd_diag = cbsd_diag[complete.cases(cbsd_diag$IITA_Cassava_Kephis_ICK),]
if(nrow(cbsd_diag)>0){
  cbsd_diag = cbsd_diag[grepl("_", cbsd_diag$IITA_Cassava_Kephis_ICK) == F]# keep only the ICKs
}
glasshouse_diagnostics = Reduce(function(x,y)merge(x,y, all.x=T,by = "IITA_Cassava_Kephis_ICK"), list(glasshouse_visual,cmd_diag,cbsd_diag))

# 4. Tips

tips = dplyr::select(kephis, ends_with("/ick_tips"), ends_with("/tips_date"), ends_with("/number_tips")) %>%
  .[complete.cases(.),]
colnames(tips) = c("IITA_Cassava_Kephis_ICK","Tips_date","Number_of_tips")



# 5. Meristem excision

meristems = dplyr::select(kephis, ends_with("ick_meristem"),#ends_with("getmeristemUCID"),
                          ends_with("meristem_date"), ends_with("meristems_number")) %>%
  .[complete.cases(.),]
colnames(meristems) = c("IITA_Cassava_Kephis_ICK","Date_of_meristem_excise","Number_of_meristems_excised")

# generate meristems-ID
if(nrow(meristems)>0){
  meristems = meristems %>%
    dplyr::group_by(IITA_Cassava_Kephis_ICK) %>%
    summarise(
      Number_of_meristems_excised = sum(as.integer(na.omit(Number_of_meristems_excised))),
      First_Date_of_meristem_excise = min(na.omit(Date_of_meristem_excise)),
      Last_Date_of_meristem_excise = max(na.omit(Date_of_meristem_excise))
    )
}
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1. SCREENHOUSE DIAGNOSTICS

cassavadata = Reduce(function(x,y)merge(x,y,all=T, by="IITA_Cassava_Kephis_ICK"), 
                     list(new_entry,glasshouse_diagnostics, meristems, tips))

# Screenhouse and diagnostics Database -----------------------------------------------------------------------------------
screenhouse_database = koboloadeR::kobo_data_downloader("464224", "seedtracker:Seedtracking101", api="ona")  %>% 
  mutate_all(as.character) %>%
  dplyr::select(-starts_with("_"))
screenhouse_database[,c("Lab_number","Additional_lab_no")] = NULL
screenhouse_database[screenhouse_database=="n/a"] <- NA
colnames(screenhouse_database) = gsub("realtime","Real_time", names(screenhouse_database))

# Merge Current Data and Database (Update Database)
existing_ick = cassavadata$IITA_Cassava_Kephis_ICK[cassavadata$IITA_Cassava_Kephis_ICK %in% screenhouse_database$IITA_Cassava_Kephis_ICK]
existing_cassava = subset(cassavadata, (cassavadata$IITA_Cassava_Kephis_ICK %in% existing_ick)) %>% setDT()
existing_cassava = janitor::remove_empty(existing_cassava, "cols")

new_cassava = subset(cassavadata, !(cassavadata$IITA_Cassava_Kephis_ICK %in% existing_ick))

database = plyr::rbind.fill(screenhouse_database, new_cassava) %>% setDT()
common_cols = names(existing_cassava)[names(existing_cassava) %in% names(database)]
database = Reduce(function(x,y)merge(x,y,all=T, by = common_cols), list(database, existing_cassava))

# Select variable to keep

database = database %>%
  dplyr::select(IITA_Cassava_Kephis_ICK, Unique_Cassava_ID_UCID, Alternative_identifier,Priority, Category, Date_brought_to_KEPHIS, 
                Source_of_ICK_germplasm, Original_Source, Contact_person, Number_received, Number_sent_glasshouse,Number_sent_thermotherapy,
                Bay_in_Glasshouse, Date_Received_in_Glasshouse,Number_Received_In_Glasshouse,Date_of_CMD_diagnostics_in_glasshouse,EACMV_symptoms_in_glasshouse,
                ACMV_symptoms_in_glasshouse,CMD_symptoms_in_glasshouse,Date_of_CBSD_diagnostics_in_glasshouse,UCBSV_symptoms_in_glasshouse,CBSV_symptoms_in_glasshouse,
                CBSD_symptoms_in_glasshouse, Virus_status, Tips_date, Number_of_tips, Number_of_meristems_excised, 
                First_Date_of_meristem_excise, Last_Date_of_meristem_excise#, everything()
  )



database$CMD_symptoms_in_glasshouse = ifelse(is.na(database$CMD_symptoms_in_glasshouse) & database$EACMV_symptoms_in_glasshouse=="positive" & database$ACMV_symptoms_in_glasshouse=="positive", "positive",
                                             ifelse(is.na(database$CMD_symptoms_in_glasshouse) & database$EACMV_symptoms_in_glasshouse=="positive" & database$ACMV_symptoms_in_glasshouse=="negative", "positive", 
                                                    ifelse(is.na(database$CMD_symptoms_in_glasshouse) & database$EACMV_symptoms_in_glasshouse=="negative" & database$ACMV_symptoms_in_glasshouse=="positive", "positive",# ------1
                                                           ifelse(is.na(database$CMD_symptoms_in_glasshouse) & database$EACMV_symptoms_in_glasshouse=="positive" & database$ACMV_symptoms_in_glasshouse=="inconclusive", "inconclusive",
                                                                  ifelse(is.na(database$CMD_symptoms_in_glasshouse) & database$EACMV_symptoms_in_glasshouse=="inconclusive" & database$ACMV_symptoms_in_glasshouse=="positive", "inconclusive", # ---2
                                                                         ifelse(is.na(database$CMD_symptoms_in_glasshouse) & database$EACMV_symptoms_in_glasshouse=="negative" & database$ACMV_symptoms_in_glasshouse=="negative", "negative",
                                                                                ifelse(is.na(database$CMD_symptoms_in_glasshouse) & database$EACMV_symptoms_in_glasshouse=="negative" & database$ACMV_symptoms_in_glasshouse=="inconclusive", "inconclusive",
                                                                                       ifelse(is.na(database$CMD_symptoms_in_glasshouse) & database$EACMV_symptoms_in_glasshouse=="inconclusive" & database$ACMV_symptoms_in_glasshouse=="negative", "inconclusive",
                                                                                              database$CMD_symptoms_in_glasshouse
                                                                                       ))))))))
database$EACMV_symptoms_in_glasshouse = NULL
database$ACMV_symptoms_in_glasshouse = NULL

database$CBSD_symptoms_in_glasshouse = ifelse(is.na(database$CBSD_symptoms_in_glasshouse) & database$UCBSV_symptoms_in_glasshouse=="positive" & database$CBSV_symptoms_in_glasshouse=="positive", "positive",
                                              ifelse(is.na(database$CBSD_symptoms_in_glasshouse) & database$UCBSV_symptoms_in_glasshouse=="positive" & database$CBSV_symptoms_in_glasshouse=="negative", "positive", 
                                                     ifelse(is.na(database$CBSD_symptoms_in_glasshouse) & database$UCBSV_symptoms_in_glasshouse=="negative" & database$CBSV_symptoms_in_glasshouse=="positive", "positive",# ------1
                                                            ifelse(is.na(database$CBSD_symptoms_in_glasshouse) & database$UCBSV_symptoms_in_glasshouse=="positive" & database$CBSV_symptoms_in_glasshouse=="inconclusive", "inconclusive",
                                                                   ifelse(is.na(database$CBSD_symptoms_in_glasshouse) & database$UCBSV_symptoms_in_glasshouse=="inconclusive" & database$CBSV_symptoms_in_glasshouse=="positive", "inconclusive", # ---2
                                                                          ifelse(is.na(database$CBSD_symptoms_in_glasshouse) & database$UCBSV_symptoms_in_glasshouse=="negative" & database$CBSV_symptoms_in_glasshouse=="negative", "negative",
                                                                                 ifelse(is.na(database$CBSD_symptoms_in_glasshouse) & database$UCBSV_symptoms_in_glasshouse=="negative" & database$CBSV_symptoms_in_glasshouse=="inconclusive", "inconclusive",
                                                                                        ifelse(is.na(database$CBSD_symptoms_in_glasshouse) & database$UCBSV_symptoms_in_glasshouse=="inconclusive" & database$CBSV_symptoms_in_glasshouse=="negative", "inconclusive",
                                                                                               database$CBSD_symptoms_in_glasshouse
                                                                                        ))))))))

database$UCBSV_symptoms_in_glasshouse = NULL
database$CBSV_symptoms_in_glasshouse = NULL

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

zipr("data/cassavadata.zip", "data/cassavadata.csv")

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#**********************************************************************************************************************************************************************************
database[database==999]=NA

# MERISTEMS-IDs
total_meristems = database %>%
  group_by(IITA_Cassava_Kephis_ICK) %>%
  summarise(Number_of_meristems_excised = sum(na.omit(Number_of_meristems_excised)))

database = dplyr::left_join(total_meristems, database, by="IITA_Cassava_Kephis_ICK")
database <- database %>% 
  dplyr::group_by(IITA_Cassava_Kephis_ICK) %>%
  fill(everything(), .direction = "down") %>%
  fill(everything(), .direction = "up") %>%
  slice(1)

database$Number_of_meristems_excised = ifelse(database$Number_of_meristems_excised.x==0, database$Number_of_meristems_excised.y, database$Number_of_meristems_excised.x)
database$Number_of_meristems_excised.x=NULL
database$Number_of_meristems_excised.y=NULL
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

multiplication = dplyr::select(kephis, 
                               ends_with("meristemIDMultiplication"), 
                               ends_with("date_multiplication"),
                                 ends_with("number_of_test_tubes"),
                               ends_with("number_of_baby_jars")) %>%
  .[complete.cases(.),]
colnames(multiplication) = c("MeristemID","Date_of_meristem_multiplication","Number_of_test_tubes","Number_of_baby_jars")

# ----------Hardening

harden = dplyr::select(kephis, 
                         ends_with("hardening/meristemid"),
                         ends_with("hardening/hardening_date"),
                         ends_with("hardening/number_of_plants")) %>%
  .[complete.cases(.),]
colnames(harden) = c("MeristemID","Date_of_hardening","Number_of_plants")

died = dplyr::select(kephis, 
                          ends_with("hardening/meristemid"),
                          ends_with("hardening/hardening_date"),
                          ends_with("number_died")) %>%
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

symptomID= dplyr::select(kephis, ends_with("meristemSymptoms")) %>%
  tibble::rowid_to_column() %>%
  tidyr::gather(id, MeristemID, ends_with("meristemSymptoms"), na.rm = T) %>%
  dplyr::select(-starts_with("id")) %>%
  unique()
  
symptomsDate = dplyr::select(kephis, ends_with("date_of_recording_symptoms")) %>%
  tibble::rowid_to_column() %>%
  tidyr::gather(id, Date_of_recording_symptoms, ends_with("date_of_recording_symptoms"), na.rm = T) %>%
  dplyr::select(-starts_with("id")) %>%
  unique()
  
symptomsType = dplyr::select(kephis, contains("selectSymptoms")) %>%
  tibble::rowid_to_column() %>%
  tidyr::gather(id, Symptoms, ends_with("selectSymptoms"), na.rm = T) %>%
  dplyr::select(-starts_with("id")) %>%
  .[complete.cases(.),]
colnames(symptomsType) = c("rowid","CMD_Symptoms_after_hardening","CBSD_Symptoms_after_hardening", "NO_Symptoms_after_hardening")

symptoms = Reduce(function(x,y)merge(x,y,by="rowid"), list(symptomID, symptomsDate, symptomsType))


# b. sampling
sampling = dplyr::select(kephis, ends_with("meristemSampling"),
                         ends_with("labnumber"),
                         ends_with("additional_labNo"),
                         ends_with("date_of_sampling")) %>%
  .[complete.cases(.),]

colnames(sampling) = c("MeristemID","Lab_number","Additional_lab_number","Date_of_sampling")

# c. CMD
endpoint = dplyr::select(kephis, 
                       ends_with("meristemCMD"),
                       ends_with("date_of_cmd"),
                       ends_with("EACMV_End_point"),
                       ends_with("ACMV_End_point")) %>%
  .[complete.cases(.),]
colnames(endpoint) = c("MeristemID", "Date_of_CMD_diagnostics", "EACMV_End_point", "ACMV_End_point")

# CBSD
realtime = dplyr::select(kephis, 
                        ends_with("meristemCBSV"),
                        ends_with("date_of_cbsv"),
                        ends_with("CBSV_Real_time"),
                        ends_with("UCBSV_Real_time")) %>%
  .[complete.cases(.),]

colnames(realtime) = c("MeristemID", "Date_of_CBSV_diagnostics", "CBSV_Real_time", "UCBSV_Real_time")

# Taken to slow growth
slowgrowth = dplyr::select(kephis,
                           ends_with("date_taken_to_slow_growth"),
                           ends_with("slowgrowthID"),
                           ends_with("number_taken_to_slow_growth")) %>%
  .[complete.cases(.),]
colnames(slowgrowth) = c("MeristemID", "Date_taken_to_slow_growth","Number_taken_to_slow_growth")

# --------------------------------------------------------------------------------------------------------------------------------------------------------
# MERGE DATASET
# --------------------------------------------------------------------------------------------------------------------------------------------------------

# 2. MERISTEMS

if(nrow(N_Meristems)>0){
  meristems.df = Reduce(function(x,y)merge(x,y,all=T, by="MeristemID"), list(all_meristems, multiplication, hardening, symptoms, sampling, endpoint, realtime, slowgrowth))
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

meristemsdata = meristemsdata %>%
  dplyr::select(
    "MeristemID","IITA_Cassava_Kephis_ICK","Unique_Cassava_ID_UCID","Date_of_meristem_excise","Tips_date","Number_of_tips","Date_of_meristem_multiplication", "Number_of_test_tubes",
    "Number_of_baby_jars","Date_of_hardening","Number_of_plants","Date_of_hardening_2","Number_of_plants_that_died","Available_Plants_in_Hardening","Date_of_recording_symptoms",     
    "CMD_Symptoms_after_hardening","CBSD_Symptoms_after_hardening","Symptoms_after_hardening","Lab_number","Additional_lab_number","Date_of_sampling",
    "Date_of_CMD_diagnostics","EACMV_End_point","ACMV_End_point","Date_of_CBSV_diagnostics","CBSV_Real_time","UCBSV_Real_time", "Pots_in_BecA_glasshouse","Next_step"               
    )

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
  dplyr::arrange(desc(Lab_number,n))
meristemsdata$n = NULL

# Write Meristems Database in the Disk 

write.csv(meristemsdata, file = "data/meristemdata.csv", row.names = F)
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
  
  