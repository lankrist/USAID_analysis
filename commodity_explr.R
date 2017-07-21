#2015-2017 commodity data 
library(ggplot2)
library(scales) #pretty_breaks()
commodity <- read.csv("~/USAID_Internship2017/dataset/country_commodity_TO1_SO.csv", stringsAsFactors=FALSE)

colnames(commodity) = c("Country", "Supplier", "Supplying Method", "Stage", "Product",
                        "Fiscal Year", "Quantity", "Total Cost")
names(commodity)

View(commodity)
dim(commodity)

#convert dollar character to numeric
commodity$`Total Cost` = as.numeric(gsub('\\$|,', '', commodity$`Total Cost`))
#convert year to factor
commodity$Country = as.factor(commodity$Country)
commodity$`Fiscal Year`= as.integer(commodity$`Fiscal Year`)

#encode new category
commodity$commodity.type = "NA"

commodity[grepl("Condom", commodity$Product),"commodity.type"] = "Condoms"
commodity[grepl("Lubricant", commodity$Product),"commodity.type"] = "Condoms"

commodity[grepl("Tablet", commodity$Product), "Product"] 
commodity[grepl("Tablet", commodity$Product), "commodity.type"] = "Drug" 
commodity[grepl("mg/mL", commodity$Product), "commodity.type"] = "Drug" 
commodity[grepl("Pellets", commodity$Product), "commodity.type"] = "Drug" 

table(commodity$commodity.type)

commodity[grepl("HIV", commodity$Product), "Product"]

commodity[grepl("MC Kit", commodity$Product), "commodity.type"] = "VMMC"






commodity[grepl("Pallet", commodity$Global.Product), "commodity.category"] = "Pallet"
commodity[grepl("COBAS", commodity$Global.Product), "Global.Product"] = "COBAS"
commodity[grepl("mSystems", commodity$Global.Product), "commodity.category"] = "mSystems"


commodity[commodity$commodity.category == "Drug", "Global.Product"]

#BLANK THEME
blank_theme = theme_minimal() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  )

dev.new(width = 8, height = 8)

#Order data (not necessary)
commodity = commodity[order(commodity$Product.Line),]

total = sum(commodity$PO.Line.Item.Cost)
total2017 = sum(commodity[commodity$PO.Received.Year == 2017,"PO.Line.Item.Cost"])

#Pie chart that shows how much commodities cost by profuct line
bp = ggplot(commodity, aes(x = "", y = PO.Line.Item.Cost, fill = commodity.category))+
  geom_bar(width = 1, stat = "identity")
bp

pie = bp + coord_polar(theta = "y", start = 0)

pie + scale_fill_brewer("Commodity Expense") + blank_theme +
  theme(axis.text.x =element_blank())+
  labs(title = '2015-2017 Commodity Expense by Product Line 20170718 BI&A')

dev.new()
#Box plot that shows unit cost of commodities by direct drop vs warehouse
threshold = 100
com2 = commodity[commodity$PO.Line.Item.Unit.Cost <= threshold,]

sp = ggplot(com2, aes(x = commodity.category, y = PO.Line.Item.Unit.Cost, 
                      color = Supply.Chain.Framework, shape = Supply.Chain.Framework))+
  geom_boxplot()

sp + blank_theme + labs(title = paste("2015-2017 Commodity Unit Cost by Product Line (under $", threshold, ")"))
#show count on products
commodity[grepl("NA", commodity$commodity.category), "Global.Product"]

#Stacked area chart
dev.new()
comsac = aggregate(PO.Line.Item.Cost~PO.Received.Year+Country, commodity, sum)
comsa = expand.grid(unique(commodity$PO.Received.Year), levels(commodity$Country),0)
colnames(comsa) = names(comsac)
com = rbind(comsac, comsa)
coms = aggregate(PO.Line.Item.Cost~PO.Received.Year+Country, com, sum)

sac = ggplot(coms, aes(x = PO.Received.Year, y = PO.Line.Item.Cost,
                            fill = Country))

sac + geom_area()+
  blank_theme+labs(x = "Year", y = "Expense", 
                   title = "Commodity Cost by Country from 2015-2017")+
  scale_x_continuous(breaks = unique(coms$PO.Received.Year)) #displays only integers

countryTotal= aggregate(PO.Line.Item.Cost~Country, com, sum)
ct = head(countryTotal[order(-countryTotal$PO.Line.Item.Cost),])
#join with commodity data frame
