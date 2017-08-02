#zambia data
library(data.table)
library(plyr)
setwd("/Documents/USAID_Internship2017")

zam = read.csv(file = "dataset/orders/Current Zambia Orders - June 19 2017.csv", header = T, stringsAsFactors = F)
names(zam)
zamA = read.csv(file = "dataset/ARTMIS/ALL ORDERS_report_20170802.csv", header = T, stringsAsFactors = F)
names(zamA)
zamt = zam[,c("PSM.RO..","PO..", "ComUSAID.1", "Item.Description", "Quantity", "Delivery.Date")]
zamt$tracker = "Tracker"
zamAt= zamA[,c("RO.Number", "PO.DO.IO.Number", "Commodity.SubCategory.Description",  "Product.Name", "Ordered.Quantity", "Actual.Delivery.Date")]
zamAt$tracker = "ARTMIS"
colnames(zamt) = colnames(zamAt)

#exclude replenishment orders, these are sent to warehouse not to country
zamAt = zamAt[zamAt$RO.Number != "Replenishment Orders",]

zamc = rbind(zamt, zamAt)
zamc$Ordered.Quantity=as.numeric(gsub(pattern=",", "", zamc$Ordered.Quantity))

#EXCLUDED laboratory commodities
zamc2 =zamc[!grepl("Laboratory", zamc$Commodity.SubCategory.Description), ]
# 
zamo = zamc2[order(zamc2$RO.Number, zamc2$PO.DO.IO.Number, zamc2$Product.Name), ] #ordered non_Lab RO
# 
# #as.Date(zam$Estimated.Delivery.Date, by = "%d-%b-%Y")

zamdt= as.data.table(zamc2)[,sum(Ordered.Quantity, na.rm = T), by = .(RO.Number, PO.DO.IO.Number,tracker)]
zamu = zamdt[duplicated(zamdt[,c("RO.Number", "PO.DO.IO.Number", "V1")]),]
#12 matches
zam2 = zamc2[!(zamc2$RO.Number %in% zamu$RO.Number),] #find tracker ROs not in ARTMIS
zam3 = subset(zam2,zam2$tracker== "Tracker")[,"RO.Number"]
zam3= unique(zam3[zam3 != ""]) #list of tracker ROs not in ARTMIS

z = zamc2[zamc2$RO.Number %in% zam3, ]

write.csv(x= z, file = "dataset/orders/zambia_ARTMIS_missing_RO.csv")
