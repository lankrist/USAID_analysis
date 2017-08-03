#country data clean template
setwd("/Documents/USAID_Internship2017")

country = "Cote d'Ivoire"
tracker=read.csv(file = "dataset/orders/Cote d'Ivoire Country Update 7.19.17.csv", stringsAsFactors = F, header = T)
names(tracker)

tkr = tracker[,c("WSC.RO..",  "PO..", "Ordering.Country", "ComUSAID.1", "Item.Description", "Quantity", "Status.Code")]
tkr$tracker = "Tracker"
head(tkr, 3)

Art = read.csv(file = "dataset/ARTMIS/ALL ORDERS_report_20170802.csv", header = T, stringsAsFactors = F)
Artmis=Art[Art$Country==country,]
names(Art)

At= Artmis[,c("RO.Number",  "PO.DO.IO.Number","Country","Commodity.SubCategory.Description",  "Product.Name", "Ordered.Quantity", "Status.Description")]
At$tracker = "ARTMIS"
head(At, 2)

colnames(tkr) = colnames(At)

tkr$Ordered.Quantity=as.numeric(gsub(pattern=",", "", tkr$Ordered.Quantity))
At$Ordered.Quantity=as.numeric(gsub(pattern=",", "", At$Ordered.Quantity))

track=tkr[!(tkr$RO.Number %in% At$RO.Number),]
tkr[(tkr$RO.Number %in% At$RO.Number),"RO.Number"] #ROs in tracker and ARTMIS
track =track[!(track$RO.Number==""),] #exlude empty RO#
write.csv(x= track ,
          file = paste("dataset/orders/output/",country, "_ARTMIS_missing_RO.csv", sep = ""),
          row.names = F)



#tkr[,c("RO.Number", "PO.DO.IO.Number")] %in% At[,c("RO.Number", "PO.DO.IO.Number")]

