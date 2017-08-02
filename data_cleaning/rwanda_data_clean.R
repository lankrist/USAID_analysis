#Rwanda data cleaning
library(data.table)
setwd("/Documents/USAID_Internship2017")
rwand = read.csv(file = "dataset/orders/rwanda_20170706_orders.csv", header = T)
head(rwand)
rwandA = read.csv(file = "dataset/orders/rwanda_20170706_orders+artmis.csv", header = T)

names(rwand)
sapply(rwand[,c(1:2, 9:13, 16, 24:25, 28)], levels) #look at values for columns

rwand$Supplier = as.character(rwand$Supplier)
rwand[rwand$Supplier == "Abbvie","Supplier"] = "AbbVie" 
rwand$Supplier = as.factor(rwand$Supplier)
#note that strings are converted to factors
rwand$Item.Description = as.character(rwand$Item.Description)
rwand$WSC.RO..= as.character(rwand$WSC.RO..)
rwand$PO.DO..= as.character(rwand$PO.DO..)

names(rwandA)
rwandA$RO.Number = as.character(rwandA$RO.Number)
rwandA$PO.DO.IO.Number = as.character(rwandA$PO.DO.IO.Number)
rwandA$Product.Name= as.character(rwandA$Product.Name)

rwandA$tracker = "ARTMIS"
rwand$tracker = "Tracker"

rand = rwand[,c("TO.", "Country", "WSC.RO..", "PO.DO..", 
         "Commodity.Group", "Item.Description", "Item.OUM", "Line.Item.Price", 
         "Quantity", "Supplier",  
         "Signed.RO.Received" ,
         "Agreed.Delivery.Date..Based.on.RO.",
         "Estimated.Delivery.Date", "Summary.Status", "Date.Delivered", "tracker" )]

rand0 = rwandA[,c("Task.Order", "Country" , "RO.Number" , "PO.DO.IO.Number",  
          "Commodity.SubCategory.Description", "Product.Name", "Product.Name", "Line.Total" , 
          "Ordered.Quantity", "Parent.Profile.Name", 
          "PO.Released.For.Fulfillment.Date",  #no date when ps, client recieves PO
          "Agreed.Delivery.Date",  
          "Estimated.Delivery.Date", "Status.Description",  "Actual.Delivery.Date", "tracker" )]

rand0$Product.Name.1  = sub("^\\s+", "", sub("\\s+$", "", sub(".*,", "", rwandA$Product.Name))) 
#get rid of spaces, get units

names(rand0)[7] = "Product.Unit"


colnames(rand) = names(rand0)

# Convert dates and quantities
as.Date(rand$PO.Released.For.Fulfillment.Date, format="%m/%d/%Y")
sapply(rand[,c("PO.Released.For.Fulfillment.Date", "Agreed.Delivery.Date", 
               "Estimated.Delivery.Date", "Actual.Delivery.Date")], as.Date, format = "%m/%d/%Y")
rand$Agreed.Delivery.Date
rand$Estimated.Delivery.Date
rand$Actual.Delivery.Date
rand[,c("PO.Released.For.Fulfillment.Date", "Agreed.Delivery.Date", 
         "Estimated.Delivery.Date", "Actual.Delivery.Date")]

rand0[,c("Line.Total", "Ordered.Quantity")] = sapply(rand0[,c("Line.Total", "Ordered.Quantity")], function(x) gsub(pattern=",", "", x))
rand0[,c("Line.Total", "Ordered.Quantity")] = sapply(rand0[,c("Line.Total", "Ordered.Quantity")], as.numeric)
class(rand0$Line.Total)


rands = rbind(rand0, rand)

names(rands)
attach(rands)
rands= rands[order(rands$RO.Number, rands$PO.DO.IO.Number, rands$Product.Name, rands$Product.Unit),]

#change categories
cat = "Commodity.SubCategory.Description"
rands[rands$Commodity.SubCategory.Description == "ARV", cat] = "HIV/AIDS Pharmaceuticals"
rands[rands$Commodity.SubCategory.Description == "Condom",cat] = "Male Condoms"
rands[rands$Commodity.SubCategory.Description == "Male Condom",cat] = "Male Condoms"
rands[rands$Commodity.SubCategory.Description == "EM",cat] = "Essential Medicines"
rands[grepl("eagent",rands$Product.Name), cat] = "Laboratory Reagents"
rands[rands$Commodity.SubCategory.Description == "Lab",cat] = "Laboratory Consumables"
rands$Commodity.SubCategory.Description= as.factor(as.character(rands$Commodity.SubCategory.Description))


write.csv(x = rands, file = "dataset/orders/rwanda_comparison.csv")

#clean product data
nrow(rands)

sum(rands$Line.Total, na.rm = T)

randt= as.data.table(rands)[,sum(Line.Total, na.rm = T), by = .(RO.Number, PO.DO.IO.Number, tracker)]

randu = randt[duplicated(randt[,c("RO.Number", "PO.DO.IO.Number", "V1")]),]
# 27 matches with tracker
nrow(randt) - nrow(randu)*2
# 53 differences

