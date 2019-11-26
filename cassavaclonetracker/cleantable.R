

# Meristem_Excision = meristems[,c("IITA Cassava Kephis ICK","Category","MeristemID","Unique Cassava ID UCID","Date of meristem excise")] %>%
#   .[!is.na(.$`Date of meristem excise`),]
# colnames(Meristem_Excision) = c("ICK","Category","MeristemID","UCID","Date")
# Meristem_Excision$Stage = "Meristem Excision"
# Meristem_Excision$Date = anytime::anydate(as.character(Meristem_Excision$Date))
# 
# Tips = meristems[,c("IITA Cassava Kephis ICK","Category","MeristemID","Unique Cassava ID UCID","Tips date", "Number of tips")] %>%
#   .[!is.na(.$`Tips date`)]
# colnames(Tips) = c("ICK","Category","MeristemID","UCID","Date","Number of tips")
# Tips$Stage = "Tips"
# Tips$Date = anytime::anydate(as.character(Tips$Date))

Multiplication = meristems[,c("IITA Cassava Kephis ICK","Category","MeristemID","Unique Cassava ID UCID","Date of meristem multiplication", "Number of test tubes","Number of baby jars")] %>%
  .[!is.na(.$`Date of meristem multiplication`)]
colnames(Multiplication) = c("ICK","Category","MeristemID","UCID","Date","Test Tubes","Baby Jars")
Multiplication$Stage = "Multiplication"
Multiplication$Date = anytime::anydate(Multiplication$Date)

Hardening = meristems[,c("IITA Cassava Kephis ICK","Category","MeristemID","Unique Cassava ID UCID","Date of hardening","Number of plants")] %>%
  .[!is.na(.$`Date of hardening`)]
colnames(Hardening) = c("ICK","Category","MeristemID","UCID","Date","Number of plants")
Hardening$Stage = "Hardening"
Hardening$Date = anytime::anydate(as.character(Hardening$Date))

CMD_Diagnostics = meristems[,c("IITA Cassava Kephis ICK","Category","MeristemID","Unique Cassava ID UCID","Date of CMD diagnostics")] %>%
  .[!is.na(.$`Date of CMD diagnostics`)]
colnames(CMD_Diagnostics) = c("ICK","Category","MeristemID","UCID","Date")
CMD_Diagnostics$Stage = "Laboratory CMD Diagnostics"
CMD_Diagnostics$Date = anytime::anydate(as.character(CMD_Diagnostics$Date))

CBSD_Diagnostics = meristems[,c("IITA Cassava Kephis ICK","Category","MeristemID","Unique Cassava ID UCID","Date of CBSD diagnostics")] %>%
  .[!is.na(.$`Date of CBSD diagnostics`)]
colnames(CBSD_Diagnostics) = c("ICK","Category","MeristemID","UCID","Date")
CBSD_Diagnostics$Stage = "Laboratory CBSD Diagnostics"
CBSD_Diagnostics$Date = anytime::anydate(as.character(CBSD_Diagnostics$Date))

cleantable = rbindlist(list(#Meristem_Excision, Tips, 
                            Multiplication, Hardening, CMD_Diagnostics, CBSD_Diagnostics), fill = T) %>%
  setorder(., -Date) #%>%
  #.[.$Date > (Sys.Date()-60)]
