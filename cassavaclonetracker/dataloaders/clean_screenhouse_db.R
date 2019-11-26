cat("\014")
rm(list = ls(all=T))
# 
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

#----------------------------------------------------------------------------------------------------------------------------------
#------Database in Dropbox-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------
setwd("../data")
 
# token <- drop_auth()
# saveRDS(token, file = "token.rds")
drop_auth(rdstoken = "token.rds")

# Download data 
drop_download("KEPHIS/Final consolidated screenhouse diagnostics and in vitro.xlsx", overwrite = T)
drop_download("KEPHIS/Final multiplication hardening and diagnostics.xlsx", overwrite = T)

screenhouse_diagnostics_in_vitro = read_excel("Final consolidated screenhouse diagnostics and in vitro.xlsx")
colnames(screenhouse_diagnostics_in_vitro) = gsub("[(]","",gsub("[)]","",gsub(" 1","",gsub("KEPHIS ","",gsub("endpoint","End point", names(screenhouse_diagnostics_in_vitro))))))

ref_ick = data.frame(screenhouse_diagnostics_in_vitro[,c("Unique Cassava ID UCID","IITA Cassava Kephis ICK")])
colnames(ref_ick) = c("Unique Cassava ID UCID","IITA Cassava Kephis ICK")

# combine duplicate rows with NA to one complete row
screenhouse <- screenhouse_diagnostics_in_vitro %>%
  arrange(`IITA Cassava Kephis ICK`)
screenhouse = screenhouse %>% 
  group_by(`IITA Cassava Kephis ICK`)
scols = names(screenhouse)[!names(screenhouse) %in% "IITA Cassava Kephis ICK"]
screenhouse = screenhouse %>%
   fill(scols, .direction = "down") %>%
   fill(scols, .direction = "up") %>%
   slice(1) %>%
  ungroup()

colnames(screenhouse) = gsub("[.]","",(gsub(" ","_",(gsub("[)]","",(gsub("[(]","",trimws(names(screenhouse)))))))))
screenhouse$Number_of_baby_jars_of_tips = screenhouse$`Number_of_baby_jars_of_tips or_orginal_plant`
screenhouse$`Number_of_baby_jars_of_tips or_orginal_plant` <- NULL
screenhouse$Number_of_tips = screenhouse$`Number_of_test_tubes_of_tips or_orginal_plant`
screenhouse$`Number_of_test_tubes_of_tips or_orginal_plant` <- NULL

# remove white spaces and replace space with "_"
screenhouse$IITA_Cassava_Kephis_ICK = gsub(" ","_",trimws(screenhouse$IITA_Cassava_Kephis_ICK))
screenhouse = remove_empty(screenhouse, "cols")
screenhousedt = dplyr::select(screenhouse, IITA_Cassava_Kephis_ICK, everything())

## ------------------------------------------------------------------------------------------------------------------------------ database

dnames = names(screenhousedt)
screenhousedt %<>% mutate_all(as.character)

# remove double spaces
screenhousedt = screenhousedt %>%
  mutate_if(is.character, str_trim) 

# clean data
screenhousedt[screenhousedt == "Positive"] <- "positive"
screenhousedt[screenhousedt == "Negative"] <- "negative"

screenhousedt$Category = gsub("5CP clone", "5 CP clone", screenhousedt$Category)
screenhousedt$Category = gsub("DSC clones", "DSC clone", screenhousedt$Category)

screenhousedt$Bay_in_Glasshouse = stringr::str_split_fixed(screenhousedt$Bay_in_Glasshouse, "_",2)
screenhousedt$Date_Received_in_Glasshouse = screenhousedt$Bay_in_Glasshouse[,2]
screenhousedt$Bay_in_Glasshouse = screenhousedt$Bay_in_Glasshouse[,1]
screenhousedt$Date_Received_in_Glasshouse = ifelse(grepl("LOST", screenhousedt$Bay_in_Glasshouse)==T, str_split_fixed(screenhousedt$Bay_in_Glasshouse," ",2)[,2], screenhousedt$Date_Received_in_Glasshouse)
screenhousedt$Bay_in_Glasshouse = ifelse(grepl("LOST", screenhousedt$Bay_in_Glasshouse)==T, str_split_fixed(screenhousedt$Bay_in_Glasshouse," ",2)[,1], screenhousedt$Bay_in_Glasshouse)

# format dates
screenhousedt$Date_Received_in_Glasshouse = stringr::str_split_fixed(screenhousedt$Date_Received_in_Glasshouse, " ",3)
screenhousedt$`Date_Received_in_Glasshouse1` = ifelse(grepl("th", screenhousedt$Date_Received_in_Glasshouse[,1])==T, gsub("th","", screenhousedt$Date_Received_in_Glasshouse[,1]),
                                                      ifelse(grepl("st", screenhousedt$Date_Received_in_Glasshouse[,1])==T,gsub("st","", screenhousedt$Date_Received_in_Glasshouse[,1]),
                                                             ifelse(grepl("rd", screenhousedt$Date_Received_in_Glasshouse[,1])==T,gsub("rd","", screenhousedt$Date_Received_in_Glasshouse[,1]),
                                                                    ifelse(grepl("nd", screenhousedt$Date_Received_in_Glasshouse[,1])==T,gsub("rd","", screenhousedt$Date_Received_in_Glasshouse[,1]),
                                                                           screenhousedt$Date_Received_in_Glasshouse[,1])))) 
screenhousedt$Date_Received_in_Glasshouse = with(screenhousedt, lubridate::ymd(sprintf('%04d%02d%02d', 
                                                                                       lubridate::year(anytime::anydate(as.character(screenhousedt$Date_Received_in_Glasshouse[,3]))),
                                                                                       lubridate::month(anytime::anydate(as.character(screenhousedt$Date_Received_in_Glasshouse[,2]))),
                                                                                       lubridate::day(anytime::anydate(as.character(screenhousedt$`Date_Received_in_Glasshouse1`)))
)))
screenhousedt$`Date_Received_in_Glasshouse1` <- NULL

screenhousedt$`Date_brought_to_KEPHIS` = stringr::str_split_fixed(screenhousedt$`Date_brought_to_KEPHIS`, ",",2)
screenhousedt$`Date_brought_to_KEPHIS1` = screenhousedt$`Date_brought_to_KEPHIS`[,1]
screenhousedt$`Date_brought_to_KEPHIS2` = screenhousedt$`Date_brought_to_KEPHIS`[,2]

screenhousedt$`Date_brought_to_KEPHIS1` = stringr::str_split_fixed(screenhousedt$`Date_brought_to_KEPHIS1`, " ",3)

# if year in month move to year
screenhousedt$`Date_brought_to_KEPHIS1`[,3] = ifelse(grepl("\\d", screenhousedt$`Date_brought_to_KEPHIS1`[,2])==TRUE, screenhousedt$`Date_brought_to_KEPHIS1`[,2], screenhousedt$`Date_brought_to_KEPHIS1`[,3])
screenhousedt$`Date_brought_to_KEPHIS1`[,1] = gsub("th","",screenhousedt$`Date_brought_to_KEPHIS1`[,1]) # regex multiple patterns with singlar replacement
screenhousedt$`Date_brought_to_KEPHIS1`[,1] = gsub("st","",screenhousedt$`Date_brought_to_KEPHIS1`[,1]) # regex multiple patterns with singlar replacement
screenhousedt$`Date_brought_to_KEPHIS1`[,1] = gsub("rd","",screenhousedt$`Date_brought_to_KEPHIS1`[,1]) # regex multiple patterns with singlar replacement
screenhousedt$`Date_brought_to_KEPHIS1`[,1] = gsub("nd","",screenhousedt$`Date_brought_to_KEPHIS1`[,1]) # regex multiple patterns with singlar replacement
screenhousedt$`Date_brought_to_KEPHIS1`[,2] = ifelse(grepl("[A-Za-z]", screenhousedt$`Date_brought_to_KEPHIS1`[,1])==T, screenhousedt$`Date_brought_to_KEPHIS1`[,1], screenhousedt$`Date_brought_to_KEPHIS1`[,2])
screenhousedt$`Date_brought_to_KEPHIS1`[,1] = 
  
screenhousedt$`Date_brought_to_KEPHIS day` = ifelse(grepl("-", screenhousedt$`Date_brought_to_KEPHIS1`[,1])==T, 
                                                      lubridate::day(anytime::anydate(as.character(screenhousedt$`Date_brought_to_KEPHIS1`[,1]))), screenhousedt$`Date_brought_to_KEPHIS1`[,1])

screenhousedt$`Date_brought_to_KEPHIS month` = ifelse(grepl("-", screenhousedt$`Date_brought_to_KEPHIS1`[,1])==T, 
                                                      lubridate::month(anytime::anydate(as.character(screenhousedt$`Date_brought_to_KEPHIS1`[,1]))), screenhousedt$`Date_brought_to_KEPHIS1`[,2])

screenhousedt$`Date_brought_to_KEPHIS year` = ifelse(grepl("-", screenhousedt$`Date_brought_to_KEPHIS1`[,1])==T, 
                                                     lubridate::year(anytime::anydate(as.character(screenhousedt$`Date_brought_to_KEPHIS1`[,1]))), screenhousedt$`Date_brought_to_KEPHIS1`[,3])

screenhousedt$`Date_brought_to_KEPHIS` = with(screenhousedt, lubridate::ymd(sprintf('%04d%02d%02d', 
                                                                                    lubridate::year(anytime::anydate(as.character(screenhousedt$`Date_brought_to_KEPHIS year`))),
                                                                                    lubridate::month(anytime::anydate(as.character(screenhousedt$`Date_brought_to_KEPHIS month`))),
                                                                                    lubridate::day(anytime::anydate(as.character(screenhousedt$`Date_brought_to_KEPHIS day`)))
)))

screenhousedt[, c("Date_brought_to_KEPHIS1","Date_brought_to_KEPHIS month","Date_brought_to_KEPHIS year","Date_brought_to_KEPHIS day")] <- NULL # drop unwanted columns

# find replace 
screenhousedt$`Source_of_ICK_germplasm` = gsub("RAB  Rubona station, germplasm collection","RAB  Rubona station",screenhousedt$`Source_of_ICK_germplasm`)
screenhousedt$`Source_of_ICK_germplasm` = gsub("Naliendele","Naliendele field genebank",screenhousedt$`Source_of_ICK_germplasm`)
vars = c("CMD_symptoms_in_glasshouse","CBSD_symptoms_in_glasshouse","EACMV_End_point","ACMV_End_point",
         "EACMV_End_point_2","ACMV_End_point_2","CBSV_realtime","UCBSV_realtime")        

# virus status
screenhousedt$Virus_status = tolower(screenhousedt$Virus_status)
screenhousedt$Virus_status = ifelse(screenhousedt$Virus_status=="lost assumed positive", "infected",
                                    ifelse(screenhousedt$Virus_status=="inconclusive", "infected",
                                           ifelse(screenhousedt$Virus_status=="diagnostics pending", NA,
                                                  screenhousedt$Virus_status)))


# set types
db_factors = c("Priority","Category","Bay_in_Glasshouse","Virus_status","Original_Source", 
               "CMD_symptoms_in_glasshouse","CBSD_symptoms_in_glasshouse","EACMV_End_point",        
               "ACMV_End_point","EACMV_End_point_2","ACMV_End_point_2","CBSV_realtime",            
               "UCBSV_realtime","Reintroduced_to_thermotherapy")

db_nums = c(grep("Number", names(screenhousedt), value = T)) # c("Number_of_pots","Number_of_plants","No_meristems", "No_tips", "number_received_glasshouse")
db_dates = grep("Date", names(screenhousedt), value = T)

screenhousedt[,db_factors] %<>% mutate_all(as.factor)
screenhousedt[,db_nums] %<>% mutate_all(as.integer)
screenhousedt[,db_dates] %<>% mutate_all(anytime::anydate)

# order names as original data
screenhousedt %<>%
  dplyr::select(dnames, everything())
screenhousedt$Number_of_meristems_excised = screenhousedt$Number_of_meristems
screenhousedt$Number_of_meristems = NULL

# drop empty cols
screenhousedtdt = janitor::remove_empty(screenhousedt, "cols")
saveRDS(screenhousedt, file = "Final consolidated screenhouse diagnostics and in vitro.rds")


# #Check its existence
# if (file.exists("Final consolidated screenhouse diagnostics and in vitro.xlsx")){
#   #Delete file if it exists
#   file.remove("Final consolidated screenhouse diagnostics and in vitro.xlsx")
# }
