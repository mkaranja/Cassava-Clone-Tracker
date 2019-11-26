cat("\014")
#rm(list = ls(all=T))

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

# token <- drop_auth()
# saveRDS(token, file = "token.rds")
#  
# # or add path if file is not in root
# token = readRDS("data/token.rds")

# drop_download("KEPHIS/Final consolidated screenhouse diagnostics and in vitro.xlsx", overwrite = T)
# drop_download("KEPHIS/Final multiplication hardening and diagnostics.xlsx", overwrite = T)
# drop_download("KEPHIS/Final hardening and diagnostics.xlsx", overwrite = T)

screenhouse_diagnostics_in_vitro = read_excel(paste0(getwd(),"/data/Final consolidated screenhouse diagnostics and in vitro.xlsx")) %>%
  mutate_all(as.character)
colnames(screenhouse_diagnostics_in_vitro) = gsub("[(]","",gsub("[)]","",gsub(" 1","",gsub("KEPHIS ","",gsub("endpoint","End point", names(screenhouse_diagnostics_in_vitro))))))

ref_ick = data.frame(screenhouse_diagnostics_in_vitro[,c("Unique Cassava ID UCID","IITA Cassava Kephis ICK")])
colnames(ref_ick) = c("Unique Cassava ID UCID","IITA Cassava Kephis ICK")


# combine duplicate rows with NA to one complete row
screenhouse <- screenhouse_diagnostics_in_vitro %>%
  group_by(`IITA Cassava Kephis ICK`) %>%
  fill(everything(), .direction = "down") %>%
  fill(everything(), .direction = "up") %>%
  slice(1)

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
# "EACMV_in_glasshouse","ACMV_in_glasshouse","UCBSV_in_glasshouse","CBSV_in_glasshouse")

db_nums = c(grep("Number", names(screenhousedt), value = T)) # c("Number_of_pots","Number_of_plants","No_meristems", "No_tips", "number_received_glasshouse")
db_dates = grep("Date", names(screenhousedt), value = T)

screenhousedt[,db_factors] %<>% mutate_all(as.factor)
screenhousedt[,db_nums] %<>% mutate_all(as.integer)
screenhousedt[,db_dates] %<>% mutate_all(anytime::anydate)

# order names as original data
screenhousedt %<>%
  dplyr::select(dnames)
# drop empty cols
screenhousedtdt = janitor::remove_empty(screenhousedt, "cols")


# MERISTEMS
#-------------------------------------------------------------------------------------------------------------------------------------

multiplication_hardening_diagnostics = read_excel(paste0(getwd(),"/data/Final multiplication hardening and diagnostics.xlsx")) %>%
  mutate_all(as.character)
names(multiplication_hardening_diagnostics)[names(multiplication_hardening_diagnostics) == "Clone Name"] = "Unique Cassava ID UCID" # rename a single column 
multiplication_hardening_diagnostics = dplyr::left_join(multiplication_hardening_diagnostics, ref_ick, by="Unique Cassava ID UCID")
multiplication_hardening_diagnostics$MeristemID = paste0(multiplication_hardening_diagnostics$`IITA Cassava Kephis ICK`, "_", multiplication_hardening_diagnostics$`Meristem/Tip/Explant`)
colnames(multiplication_hardening_diagnostics) = gsub("[(]","",gsub("[)]","",gsub(" 1","",gsub("KEPHIS ","",gsub("endpoint","End point", names(multiplication_hardening_diagnostics))))))

hardening_diagnostics = read_excel("data/Final hardening and diagnostics.xlsx") %>%
  mutate_all(as.character)
names(hardening_diagnostics)[names(hardening_diagnostics) == "Clone Name"] = "Unique Cassava ID UCID" # rename a single column 
hardening_diagnostics = dplyr::left_join(hardening_diagnostics, ref_ick, by="Unique Cassava ID UCID")
hardening_diagnostics$MeristemID = paste0(hardening_diagnostics$`IITA Cassava Kephis ICK`,"_",hardening_diagnostics$`Meristem/Tip/Explant`)
colnames(hardening_diagnostics) = gsub("[(]","",gsub("[)]","",gsub(" 1","",gsub("KEPHIS ","",gsub("endpoint","End point", names(hardening_diagnostics))))))

multi_hardening_diagnostics = plyr::rbind.fill(multiplication_hardening_diagnostics, hardening_diagnostics)
# multi_hardening_diagnostics = Reduce(function(x,y)merge(x,y,all=T,by="IITA Cassava Kephis ICK"),
#                                      list(multiplication_hardening_diagnostics, hardening_diagnostics))

multi_hardening_diagnostics <- multi_hardening_diagnostics %>%
  group_by(MeristemID) %>%
  fill(everything(), .direction = "down") %>%
  fill(everything(), .direction = "up") %>%
  slice(1)
#multi_hardening_diagnostics = plyr::ddply(`IITA Cassava Kephis ICK` ~ ., multi_hardening_diagnostics)
multi_hardening_diagnostics %<>%
  dplyr::select("IITA Cassava Kephis ICK","MeristemID","Unique Cassava ID UCID", everything())

multi_hardening_diagnostics$`Meristem/Tip/Explant` <- NULL
colnames(multi_hardening_diagnostics) = gsub(" ","_",trimws(names(multi_hardening_diagnostics)))
multiNames = names(multi_hardening_diagnostics)

##########################################################################################################
## GENERATE MERISTEM ID'S FROM SCREENHOUSE DIAGNOSTICS AND IN VITRO DATA

dt = screenhousedt[,c("IITA_Cassava_Kephis_ICK", "Number_of_meristems")] %>%
  .[complete.cases(.),]
dt$`Number of meristems` = as.integer(dt$Number_of_meristems)
dt = dt[dt$Number_of_meristems>0 & !dt$Number_of_meristems==999,]

dt = dt[rep(row.names(dt), dt$Number_of_meristems),]
dt = dt %>% dplyr::arrange(IITA_Cassava_Kephis_ICK)
dt = data.table::setDT(dt)
dt = dt[,index := 1:.N, by = 'IITA_Cassava_Kephis_ICK']
dt$MeristemID = paste0(dt$IITA_Cassava_Kephis_ICK,"_M",dt$index)
dt[,c("Number_of_meristems","index")] <- NULL

new_meristem_dt = dplyr::left_join(dt, screenhousedt, by="IITA_Cassava_Kephis_ICK")

###########################################################################################
# MERGE WITH MULTIPLICATION AND HARDENING 

meristemsdt = plyr::rbind.fill(multi_hardening_diagnostics, new_meristem_dt) %>%
  dplyr::group_by(MeristemID)  %>%
  dplyr::select(multiNames)

# ------------------- clean meristems data
meristemsdt$Pots_in_BecA_glasshouse = gsub(" in pots in BecA glasshouse","",meristemsdt$Pots_in_BecA_glasshouse)
meristem_dates = meristemsdt[, c("MeristemID", grep("Date", names(meristemsdt), value=T))] 
meristem_dates$index = rowSums(!is.na(meristem_dates))# 786
meristem_dates %<>%
  dplyr::filter(index>1)# 190
meristem_dates$index = NULL

meristem_dates$Date_of_hardening = ifelse(is.na(meristem_dates$Date_of_hardening) & !is.na(meristem_dates$Date_of_hardening_2), 
                                          meristem_dates$Date_of_hardening_2, meristem_dates$Date_of_hardening)
meristem_dates$hardening_day = as.integer(str_extract(stringr::str_split_fixed(meristem_dates$Date_of_hardening," ",3)[,1], "\\-*\\d+\\.*\\d*"))
meristem_dates$hardening_day = ifelse((nchar(meristem_dates$hardening_day)==1), paste0('0', meristem_dates$hardening_day), meristem_dates$hardening_day)
mnths = c("May","June","August","Sept")
meristem_dates$hardening_month = stringr::str_split_fixed(meristem_dates$Date_of_hardening," ",3)[,2]
meristem_dates$hardening_month = gsub("Sept","September", meristem_dates$hardening_month)
meristem_dates$hardening_month = match(meristem_dates$hardening_month, month.name)
meristem_dates$hardening_month = as.integer(meristem_dates$hardening_month)
meristem_dates$hardening_month = ifelse((nchar(meristem_dates$hardening_month)==1), paste0('0', meristem_dates$hardening_month), meristem_dates$hardening_month)
meristem_dates$hardening_year = as.integer(str_extract(stringr::str_split_fixed(meristem_dates$Date_of_hardening," ",3)[,3], "\\-*\\d+\\.*\\d*"))
meristem_dates = data.frame(meristem_dates)
meristem_dates$hardening1 = paste0(meristem_dates$hardening_year,"-",meristem_dates$hardening_month,"-", meristem_dates$hardening_day)

meristem_dates$hardening1 = ifelse(!is.na(as.Date(lubridate::as_date(meristem_dates$Date_of_hardening))),meristem_dates$Date_of_hardening, meristem_dates$hardening1)
meristem_dates$hardening1 = gsub('NA-','',meristem_dates$hardening1)
meristem_dates$Date_of_hardening = meristem_dates$hardening1
meristem_dates[,c("hardening1","hardening_day","hardening_month","hardening_year")] = NULL

# --------------------
meristem_dates$diagnostics_day = as.integer(str_extract(stringr::str_split_fixed(meristem_dates$Date_of_diagnostics," ",3)[,1], "\\-*\\d+\\.*\\d*"))
meristem_dates$diagnostics_day = ifelse((nchar(meristem_dates$diagnostics_day)==1), paste0('0', meristem_dates$diagnostics_day), meristem_dates$diagnostics_day)
mnths = c("May","June","August","Sept")
meristem_dates$diagnostics_month = stringr::str_split_fixed(meristem_dates$Date_of_diagnostics," ",3)[,2]
meristem_dates$diagnostics_month = gsub("Sept","September", meristem_dates$diagnostics_month)
meristem_dates$diagnostics_month = match(meristem_dates$diagnostics_month, month.name)
meristem_dates$diagnostics_month = as.integer(meristem_dates$diagnostics_month)
meristem_dates$diagnostics_month = ifelse((nchar(meristem_dates$diagnostics_month)==1), paste0('0', meristem_dates$diagnostics_month), meristem_dates$diagnostics_month)
meristem_dates$diagnostics_year = as.integer(str_extract(stringr::str_split_fixed(meristem_dates$Date_of_diagnostics," ",3)[,3], "\\-*\\d+\\.*\\d*"))
meristem_dates = data.frame(meristem_dates)
meristem_dates$diagnostics1 = paste0(meristem_dates$diagnostics_year,"-",meristem_dates$diagnostics_month,"-", meristem_dates$diagnostics_day)

meristem_dates$diagnostics1 = ifelse(!is.na(as.Date(lubridate::as_date(meristem_dates$Date_of_diagnostics))),meristem_dates$Date_of_diagnostics, meristem_dates$diagnostics1)
meristem_dates$diagnostics1 = gsub('NA-','',meristem_dates$diagnostics1)
meristem_dates$Date_of_diagnostics = meristem_dates$diagnostics1
meristem_dates[,c("diagnostics1","diagnostics_day","diagnostics_month","diagnostics_year")] = NULL

# write.csv(meristemsdt, file="data/meristemdt.csv", row.names = F)

meristemsdt[,names(meristem_dates)[-1]] = NULL

meristemsdt = dplyr::left_join(meristemsdt, meristem_dates, by="MeristemID")

# multiple hardening
hardening = meristemsdt %>%
  dplyr::select(MeristemID,  starts_with("Date_of_hardening")) %>%
  tidyr::gather(id, Date_of_hardening, starts_with("Date_of_hardening"), na.rm=T) %>%
  dplyr::group_by(MeristemID) %>%
  summarise(Date_of_hardening = min(na.omit(Date_of_hardening)),
            Date_of_hardening_2 = max(na.omit(Date_of_hardening))
  )
hardening$Date_of_hardening_2 = ifelse(hardening$Date_of_hardening==hardening$Date_of_hardening_2,NA,hardening$Date_of_hardening_2)
meristemsdt[,c("Date_of_hardening", "Date_of_hardening_2")] = NULL
meristemsdt %<>% unique()
meristemsdt <- meristemsdt %>%
  group_by(MeristemID) %>%
  fill(everything(), .direction = "down") %>%
  fill(everything(), .direction = "up") %>%
  slice(1)
meristemsdt = dplyr::left_join(meristemsdt, hardening, by="MeristemID")
meristemsdt = data.frame(meristemsdt)

m_factors = c("IITA_Cassava_Kephis_ICK", "Unique_Cassava_ID_UCID","Pots_in_BecA_glasshouse","Symptoms_after_hardening","EACMV_End_point","ACMV_End_point","CBSV_Real_time","UCBSV_Real_time","Next_step")
m_dates = grep("Date", names(meristemsdt), value = T)

# meristemsdt[,m_factors] %<>% mutate_all(as.factor)
# 
# meristemsdt[, m_dates] %<>% mutate_all(anytime::anydate)
# 

