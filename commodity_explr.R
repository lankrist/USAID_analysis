#2015-2017 commodity data 
library(ggplot2)

commodity <- read.csv("~/USAID_Internship2017/dataset/country_commodity_20170718.csv", stringsAsFactors=FALSE)

names(commodity)
# "Country"                "Task.Order"             "Information.Provider"  
# "Program"                "Supply.Chain.Framework" "Product.Line"          
# "SO.Supplier"            "SO.Pipeline.Stage"      "Global.Product"        
# "PO.Received.Year"       "PO.Received.Date"       "PO.Line.Item.Cost"     
# "PO.Line.Item.Qty"       "PO.Line.Item.Unit.Cost"
View(commodity)
dim(commodity)

#convert dollar character to numeric
commodity$PO.Line.Item.Cost = as.numeric(gsub('\\$|,', '', commodity$PO.Line.Item.Cost))
#convert year to factor
commodity$PO.Received.Year = as.factor(commodity$PO.Received.Year)

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

dev.new(width = 5, height = 4)

#Order data
commodity = commodity[order(commodity$Product.Line),]

#Pie chart that shows how much commodities cost by profuct line
bp = ggplot(commodity, aes(x = "", y = PO.Line.Item.Cost, fill = Product.Line))+
  geom_bar(width = 1, stat = "identity")
bp

pie = bp + coord_polar(theta = "y", start = 0)
pie

pie + scale_fill_brewer("Commodity Expense") + blank_theme +
  theme(axis.text.x =element_blank())+
  labs(title = '2015-2017 Commodity Expense by Product Line 20170718 BI&A')

#Box plot that shows unit cost of commodities by direct drop vs warehouse
threshold = 100
com2 = commodity[commodity$PO.Line.Item.Unit.Cost <= threshold,]

sp = ggplot(com2, aes(x = Product.Line, y = PO.Line.Item.Unit.Cost, 
                      color = Supply.Chain.Framework, shape = Supply.Chain.Framework))+
  geom_boxplot()

sp + blank_theme + labs(title = paste("2015-2017 Commodity Unit Cost by Product Line (under $", threshold, ")"))
#show count on products

#Stacked area chart
sac = ggplot(commodity, aes(group = PO.Received.Year, x = PO.Received.Year, y = PO.Line.Item.Cost, fill = Country))

sac + geom_area()

(commodity$PO.Received.Year)


