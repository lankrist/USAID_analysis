#country data clean template
setwd("/Documents/USAID_Internship2017")

country = "Mozambique"
tracker=read.csv(file = "dataset/orders/Mozambique Order tracker July 2017.csv", stringsAsFactors = F, header = T)
names(tracker)
colnames(tracker)[2]= "RO_tracked"

Art = read.csv(file = "dataset/ARTMIS/RO_history_20170809.csv", header = T, stringsAsFactors = F)
Artmis=Art[Art$Destination.Country==country,]

track=tracker[!(tracker$RO_tracked %in% Artmis$RO),]
#tracker[(tracker$RO_tracked %in% Artmis$RO), "RO_tracked"] #ROs in tracker and ARTMIS
track = track[track[,1] !="",] #remove excess rows
write.csv(x= track ,
          file = paste("dataset/orders/output/",country, "_ARTMIS_missing_RO.csv", sep = ""),
          row.names = F)


#tkr[,c("RO.Number", "PO.DO.IO.Number")] %in% At[,c("RO.Number", "PO.DO.IO.Number")]

