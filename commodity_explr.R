#2015-2017 commodity data 
library(ggplot2)
library(scales) #pretty_breaks()

commodity <- read.csv("~/USAID_Internship2017/dataset/country_commodity_TO1_SO.csv", stringsAsFactors=FALSE)
category = read.csv("dataset/product_categories.csv", stringsAsFactors = F)


colnames(commodity) = c("Country", "Supplier", "Supplying Method", "Stage", "Product",
                        "Fiscal Year", "Quantity", "Total Cost")

colnames(category) = c("Categories", "Product")
names(commodity)

View(commodity)
dim(commodity)

# returns string w/o trailing whitespace   // category procudts have whitespaces in the end
trim.trailing <- function (x) sub("\\s+$", "", x)
category$Product=  trim.trailing(category$Product)

#convert dollar character to numeric
commodity$`Total Cost` = as.numeric(gsub('\\$|,', '', commodity$`Total Cost`))
#convert year to factor
commodity$Country = as.factor(commodity$Country)
commodity$`Fiscal Year`= as.integer(commodity$`Fiscal Year`)
commodity$Quantity= as.numeric(gsub("\\,","",commodity$Quantity))

#Encode new category  OTHER OPTION is merge with catgories from ARTMIS
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

#Join with categories
commodity2= merge(commodity, category, by = "Product")
commodity2 = unique(commodity2) #exclueds a lot of commodites without pairs
dim(commodity2)
names(commodity2)

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
commodity2 = commodity2[order(commodity2$Categories),]

total = sum(commodity2$`Total Cost`)
total2017 = sum(commodity2[commodity2$`Fiscal Year` == 2017,"Total Cost"])

#Pie chart that shows how much commodities cost by profuct line
bp = ggplot(commodity2, aes(x = "", y = `Total Cost`, fill = Categories))+
  geom_bar(width = 1, stat = "identity")
bp

pie = bp + coord_polar(theta = "y", start = 0)

pie + scale_fill_brewer("Commodity Expense") + blank_theme +
  theme(axis.text.x =element_blank())+
  labs(title = '2013-2017 Commodity Expense by Category BI&A')

dev.new()
#Box plot that shows unit cost of commodities by direct drop vs warehouse
threshold = 500
com2 = commodity2[(commodity2$`Total Cost`/commodity2$Quantity) <= threshold,]
dim(com2);dim(commodity2)
#Quantitry is factor?
sp = ggplot(com2, aes(x = Categories, y = (`Total Cost`/Quantity), 
                      color = `Supplying Method`, shape = `Supplying Method`))+
  geom_boxplot()

sp + blank_theme + labs(title = paste("2013-2017 Commodity Unit Cost by Product Line (under $", threshold, ")"))
#show count on products
commodity[grepl("NA", commodity$commodity.category), "Global.Product"]

#Stacked area chart
dev.new()
comsac = aggregate(`Total Cost`~ `Fiscal Year`+Country, commodity2, sum)
comsa = expand.grid(unique(commodity2$`Fiscal Year`), levels(commodity2$Country),0)
colnames(comsa) = names(comsac)
com = rbind(comsac, comsa)
coms = aggregate(`Total Cost`~`Fiscal Year`+Country, com, sum)

sac = ggplot(coms, aes(x = `Fiscal Year`, y = `Total Cost`,
                            fill = Country))

sac + geom_area()+
  blank_theme+labs(x = "Year", y = "Expense", 
                   title = "Commodity Cost by Country from 2013-2017")+
  scale_x_continuous(breaks = unique(coms$`Fiscal Year`)) #displays only integers

#sum cost by country
countryTotal= aggregate(`Total Cost`~Country, com, sum)
ct = head(countryTotal[order(-countryTotal$`Total Cost`),])

aggregate(`Total Cost`~`Fiscal Year`, commodity, sum)
# Fiscal   Year Total Cost
# 1        2013  109530098
# 2        2014   98184438
# 3        2015  106402040
# 4        2016  104234723
# 5        2017 1376384298

#join with commodity data frame
