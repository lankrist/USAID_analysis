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

commodity[grepl("HIV 1/2", commodity$Product), "commodity.type"] = "HIV RTK"

commodity[grepl("MC Kit", commodity$Product), "commodity.type"] = "VMMC"

arv_list = c("Ritonavir", "Lamivudine", "Zidovudine", "Nevirapine", "Abacavir",
              "Atazanavir", "Darunavir", "Efavirenz", "Tenofovir", "Emtricitabine",
              "Stavudine", "Lopinavir", "Saquinavir",  "Didanosine", "Etravirine", 
             "Dolutegravir", "Saquinavir", "Raltegravir", "Cobicistat")

drug=sapply(arv_list, function(arv_list, y) grepl(arv_list, commodity$Product))
colSums(sapply(arv_list, function(arv_list, y) grepl(arv_list, commodity$Product)))
dim(drug)
commodity[rowSums(drug)>=1, "commodity.type"] = "ARV" 



# Laboratory ############NEEDS more specificity and accuracy
commodity[grepl("Reagent", commodity$Product), "commodity.type"] = "Lab" 

table(commodity$commodity.type)

# Contraceptives
commodity[grepl("IUD", commodity$Product), "commodity.type"] = "Contraceptives"
commodity[grepl("Contraceptive", commodity$Product), "commodity.type"] = "Contraceptives"

commodity[commodity$commodity.type == "NA", "Product"]

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
commodity = commodity[order(commodity$commodity.type),]

total = sum(commodity$`Total Cost`)
total2017 = sum(commodity[commodity$`Fiscal Year` == 2017,"PO.Line.Item.Cost"])

#Pie chart that shows how much commodities cost by profuct line
bp = ggplot(commodity, aes(x = "", y = `Total Cost`, fill = commodity.type))+
  geom_bar(width = 1, stat = "identity")
bp

pie = bp + coord_polar(theta = "y", start = 0)

pie + scale_fill_brewer("Commodity Expense") + blank_theme +
  theme(axis.text.x =element_blank())+
  labs(title = '2013-2017 Commodity Expense by Category BI&A')

dev.new()
#Box plot that shows unit cost of commodities by direct drop vs warehouse
threshold = 1000
com2 = commodity[(commodity$`Total Cost`/commodity$Quantity) <= threshold,]

sp = ggplot(com2, aes(x = commodity.type, y = (commodity$`Total Cost`/commodity$Quantity), 
                      color = Supply.Chain.Framework, shape = Supply.Chain.Framework))+
  geom_boxplot()

sp + blank_theme + labs(title = paste("2013-2017 Commodity Unit Cost by Product Line (under $", threshold, ")"))
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
