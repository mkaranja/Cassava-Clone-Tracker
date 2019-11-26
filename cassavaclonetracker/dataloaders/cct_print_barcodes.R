
library(qrencoder)
library(readxl)
library(WriteXLS)
library(dplyr)
library(magrittr)
library(janitor)
library(stringr)

setwd("D:/Dropbox/CASSAVA/cassavaclonetracker/data/")

cassava_screenhouse_invitro = read_excel("Final consolidated screenhouse diagnostics and in vitro.xlsx")
colnames(cassava_screenhouse_invitro) = gsub("[.]","",(gsub(" ","_",(gsub("[)]","",(gsub("[(]","",trimws(names(cassava_screenhouse_invitro)))))))))

# remove white spaces and replace space with "_"
cassava_screenhouse_invitro$IITA_Cassava_Kephis_ICK = gsub(" ","_",trimws(cassava_screenhouse_invitro$IITA_Cassava_Kephis_ICK))
cassava_screenhouse_invitro = remove_empty(cassava_screenhouse_invitro, "cols")
cassava_screenhouse_invitro = dplyr::select(cassava_screenhouse_invitro, IITA_Cassava_Kephis_ICK, everything())

# split No meristems column 
cassava_screenhouse_invitro$Number_of_meristems = stringr::str_split_fixed(cassava_screenhouse_invitro$Number_of_meristems,"[(]",2)

# select the desired variables
label_vars = c("Unique_Cassava_ID_UCID","IITA_Cassava_Kephis_ICK","Category","Number_of_pots")
gen_labels = cassava_screenhouse_invitro[,label_vars]
gen_labels$Number_of_pots = as.integer(gen_labels$Number_of_pots)

gen_labels  %<>%
  dplyr::filter(Number_of_pots > 0)

# replicate rows by number of pots for each
result = gen_labels[rep(row.names(gen_labels), gen_labels$Number_of_pots),]
result = result %>% dplyr::arrange(IITA_Cassava_Kephis_ICK)
# generate barcodes

pdf(file = "cassava_labels.pdf", width=2.0, height = 11, paper = 'letter', pagecentre=F) # right align width=6.0 # left width=2.0,
par(mfrow=c(10, 1),mar=c(0,0,3,0), oma=c(0.5,1,0.5,0)) # right align mar=c(0,30,3,0)
for(i in 1:(nrow(result))){
  image(qrencode_raster(as.character(result[i,2])), # QRcode
        cex.main = 1.5, cex.sub = 1, asp=1, col=c("white", "black"), axes=F, 
        xlab="", ylab="", subtitle = mtext(paste(as.character(result[i,2]),"\n", result[i,1], "\n", result[i,3]), side = 4, line = 0,
        outer = F, at = NA, adj = 0, padj = 0.5, cex = 1, col = 1, las=1, font = 10))
  
}
dev.off()

# Thermotherapy labels - meristems
cassava_screenhouse_invitro$In_thermotherapy = as.Date(cassava_screenhouse_invitro$In_thermotherapy)
thermotherapy_11thSept2019 = dplyr::select(cassava_screenhouse_invitro, IITA_Cassava_Kephis_ICK, Unique_Cassava_ID_UCID, Category, In_thermotherapy) %>%
  dplyr::filter(In_thermotherapy == "2019-09-11") 

# make 6 copies of each
thermolabels = thermotherapy_11thSept2019[rep(row.names(thermotherapy_11thSept2019), 6),]
thermolabels  %<>% dplyr::arrange(IITA_Cassava_Kephis_ICK)

write.csv(thermolabels, file = "thermo11thSept2019_labels.csv")


# meristem ids
meristemid = cassava_screenhouse_invitro[,c(1,3,4,30)]
meristemid$Number_of_meristems = meristemid$Number_of_meristems[,1]
meristemid %<>%
  dplyr::filter(Number_of_meristems>0 & !Number_of_meristems==999)

# rep
meristemid = meristemid[rep(row.names(meristemid), meristemid$Number_of_meristems),]
meristemid = meristemid %>% dplyr::arrange(IITA_Cassava_Kephis_ICK)
meristemid = data.table::setDT(meristemid)
result = meristemid[,number := 1:.N, by = IITA_Cassava_Kephis_ICK]
result$MeristemICK = paste0(result$IITA_Cassava_Kephis_ICK,"_M",result$number)
result$MeristemICID = paste0(result$Unique_Cassava_ID_UCID,"_M",result$number)

result[,c("IITA_Cassava_Kephis_ICK","Unique_Cassava_ID_UCID","Number_of_meristems","number")] <- NULL
write.csv(result, file = "meristemsid.csv", row.names = F)


category = cassava_screenhouse_invitro[,c("IITA_Cassava_Kephis_ICK","Category")]
meristemdata = read.csv("virus_free.csv")

hardenedid = meristemdata[,c("IITA_Cassava_Kephis_ICK", "MeristemID", "Unique_Cassava_ID_UCID","Pots_in_BecA_glasshouse", "Meristem.Tip.Explant", "Date_of_hardening")]
hardenedid_virusfree = hardenedid %>%
  filter(Pots_in_BecA_glasshouse == "Virus free in pots in BecA glasshouse")
hardenedid_virusfree$MeristemUCID = paste0(hardenedid_virusfree$Unique_Cassava_ID_UCID,"_",hardenedid_virusfree$Meristem.Tip.Explant) 
hardenedid_virusfree = dplyr::left_join(hardenedid_virusfree, category, by="IITA_Cassava_Kephis_ICK")
hardenedid_virusfree = hardenedid_virusfree[,c("MeristemID","MeristemUCID", "Category")] %>%
  .[complete.cases(.),]
write.csv(hardenedid_virusfree, file = "virus_free_meristems.csv", row.names = F)

hardenedid_2nd_and_24_sep = read.csv("meristems_2nd_and_24thSept.csv")[,c("IITA_Cassava_Kephis_ICK", "MeristemID", "Unique_Cassava_ID_UCID","Pots_in_BecA_glasshouse", "Meristem.Tip.Explant", "Date_of_hardening")]
hardenedid_2nd_and_24_sep$MeristemUCID = paste0(hardenedid_2nd_and_24_sep$Unique_Cassava_ID_UCID,"_",hardenedid_2nd_and_24_sep$Meristem.Tip.Explant)  
hardenedid_2nd_and_24_sep = janitor::remove_empty(hardenedid_2nd_and_24_sep, "rows")
hardenedid_2nd_and_24_sep = dplyr::left_join(hardenedid_2nd_and_24_sep, category, by="IITA_Cassava_Kephis_ICK")
hardenedid_2nd_and_24_sep = hardenedid_2nd_and_24_sep[,c("MeristemID", "MeristemUCID", "Category")] %>%
  .[complete.cases(.),]
write.csv(hardenedid_2nd_and_24_sep, file = "meristems_hardened_2nd_and_24th_sep.csv", row.names = F)
