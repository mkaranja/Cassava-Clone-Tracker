# MERISTEMS
#-------------------------------------------------------------------------------------------------------------------------------------

multiplication_hardening_diagnostics = read_excel("../data/Final multiplication hardening and diagnostics.xlsx") %>%
  mutate_all(as.character)
names(multiplication_hardening_diagnostics)[names(multiplication_hardening_diagnostics) == "Clone Name"] = "Unique Cassava ID UCID" # rename a single column 
multiplication_hardening_diagnostics$`Unique Cassava ID UCID` = gsub(" ","",multiplication_hardening_diagnostics$`Unique Cassava ID UCID`) # remove saces
multiplication_hardening_diagnostics = dplyr::left_join(multiplication_hardening_diagnostics, ref_ick, by="Unique Cassava ID UCID")

multiplication_hardening_diagnostics$MeristemID = ifelse(!is.na(multiplication_hardening_diagnostics$`IITA Cassava Kephis ICK`) & !is.na(multiplication_hardening_diagnostics$`Meristem/Tip/Explant`),
                                                         paste0(multiplication_hardening_diagnostics$`IITA Cassava Kephis ICK`, "_",
                                                                multiplication_hardening_diagnostics$`Meristem/Tip/Explant`),"")
colnames(multiplication_hardening_diagnostics) = gsub("[(]","",gsub("[)]","",gsub(" 1","",gsub("KEPHIS ","",gsub("endpoint","End point", names(multiplication_hardening_diagnostics))))))

# hardening_diagnostics = read_excel("../data/Final hardening and diagnostics.xlsx") %>%
#   mutate_all(as.character)
# names(hardening_diagnostics)[names(hardening_diagnostics) == "Clone Name"] = "Unique Cassava ID UCID" # rename a single column 
# hardening_diagnostics = dplyr::left_join(hardening_diagnostics, ref_ick, by="Unique Cassava ID UCID")
# hardening_diagnostics$MeristemID = paste0(hardening_diagnostics$`IITA Cassava Kephis ICK`,"_",hardening_diagnostics$`Meristem/Tip/Explant`)
# colnames(hardening_diagnostics) = gsub("[(]","",gsub("[)]","",gsub(" 1","",gsub("KEPHIS ","",gsub("endpoint","End point", names(hardening_diagnostics))))))

#multi_hardening_diagnostics = plyr::rbind.fill(multiplication_hardening_diagnostics, hardening_diagnostics)

multi_hardening_diagnostics <- multiplication_hardening_diagnostics %>%
  arrange(MeristemID) 
multi_hardening_diagnostics = multi_hardening_diagnostics %>% group_by(MeristemID)
mcols = names(multi_hardening_diagnostics)[!names(multi_hardening_diagnostics) %in% "MeristemID"]
multi_hardening_diagnostics = multi_hardening_diagnostics %>%
   fill(mcols, .direction = "down") %>%
   fill(mcols, .direction = "up") %>%
   slice(1) %>%
  ungroup()

multi_hardening_diagnostics %<>%
  dplyr::select("IITA Cassava Kephis ICK","MeristemID","Unique Cassava ID UCID", everything())

multi_hardening_diagnostics$`Meristem/Tip/Explant` <- NULL
colnames(multi_hardening_diagnostics) = gsub(" ","_",trimws(names(multi_hardening_diagnostics)))
multiNames = names(multi_hardening_diagnostics)

##########################################################################################################
## GENERATE MERISTEM ID'S FROM SCREENHOUSE DIAGNOSTICS AND IN VITRO DATA
screenhousedt$Lab_number = NULL
dt = screenhousedt[,c("IITA_Cassava_Kephis_ICK", "Number_of_meristems_excised")] %>%
  .[complete.cases(.),]

dt$Number_of_meristems_excised = as.integer(dt$Number_of_meristems_excised)
dt = dt[dt$Number_of_meristems_excised>0 & !dt$Number_of_meristems_excised==999,]

dt = dt[rep(row.names(dt), dt$Number_of_meristems_excised),]
dt = dt %>% dplyr::arrange(IITA_Cassava_Kephis_ICK)
dt = data.table::setDT(dt)
dt = dt[,index := 1:.N, by = 'IITA_Cassava_Kephis_ICK']
dt$MeristemID = paste0(dt$IITA_Cassava_Kephis_ICK,"_M",dt$index)
dt[,c("Number_of_meristems_excised","index")] <- NULL

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
# write.csv(meristemsdt, file="../data/meristemdt.csv", row.names = F)

meristemsdt[,names(meristem_dates)[-1]] = NULL

meristemsdt = dplyr::left_join(meristemsdt, meristem_dates, by="MeristemID")
meristemsdt$`Date of hardening 2` = NULL

meristemsdt <- meristemsdt %>%
  arrange(MeristemID)
meristemsdt = meristemsdt %>%
  group_by(MeristemID) 
mcols = names(meristemsdt)[!names(meristemsdt) %in% "MeristemID"]
meristemsdt = meristemsdt %>%
   fill(mcols, .direction = "down") %>%
   fill(mcols, .direction = "up") %>%
   slice(1) %>%
  ungroup()

meristemsdt <- meristemsdt %>%
  .[complete.cases(.$IITA_Cassava_Kephis_ICK),]

saveRDS(meristemsdt, file="Final multiplication hardening and diagnostics.rds")

