#zambia data
library(data.table)
setwd("/Documents/USAID_Internship2017")

zam = read.csv(file = "dataset/orders/Current Zambia Orders - June 19 2017.csv", header = T, stringsAsFactors = F)
names(zam)

zamA = read.csv(file = "dataset/ARTMIS/ALL ORDERS_report_20170802.csv", header = T, stringsAsFactors = F)
names(zamA)
nrow(zamA)
zamt = zam[,c("PSM.RO..","PO..","Item.Description", "Quantity", "Delivery.Date")]
zamt$tracker = "Tracker"
zamAt= zamA[,c("RO.Number", "PO.DO.IO.Number", "Product.Name", "Ordered.Quantity", "Actual.Delivery.Date")]
zamAt$tracker = "ARTMIS"
nrow(zamAt)

colnames(zamt) = colnames(zamAt)

zamc = rbind(zamt, zamAt)

zamc$Ordered.Quantity

zamc$Ordered.Quantity=as.numeric(gsub(pattern=",", "", zamc$Ordered.Quantity))

zamc[order(zamc$RO.Number, zamc$PO.DO.IO.Number, zamc$Product.Name, zamc$Ordered.Quantity), ]
