#Commodify Tab data exploration
library(ggplot2)
library(plotly) #interactive graph
library(shiny)

setwd(dir = "/Documents/USAID_Internship2017/supply_chain_management/")

master = read.csv(file = "../dataset/supply_plan/COP17_master_verbose.csv")
names(master)
master$Unit.price = as.numeric(as.character(master$Unit.price))
master$Quantity = as.numeric(as.character(master$Quantity))
colnames(master)[6] = "Item"

levels(master$Category)

arv2 = master[grepl("line",master$Category), ]; arv3 = master[grepl("ARV", master$Category),]
arv_ = rbind(arv2, arv3)
arv = aggregate(Quantity~Item, data = arv_,FUN = sum)
arvh = arv[order(-arv$Quantity),]; arvh[1:10,]
arvc = aggregate(Quantity~Country, arv_, sum)

arv1=arv_[arv_$Item %in% arvh[1:10,]$Item,]

#shiny
ui <- fluidPage(plotlyOutput("distPlot"))

server <- function(input, output) {
  output$distPlot <- renderPlotly({
    f1
  })
}

shinyApp(ui, server)

#commodity tabs
dev.new()
g = ggplot(arv1, aes(x = Country, y = Quantity, fill = Item))
g1 = g+geom_bar(stat = "Identity")+labs(title = "Top 10 ARV Procurement Quantity by Country")

dev.new()
f = ggplot(arv1, aes(x = Country, y = Unit.price, colour = Item, size = Quantity))
f1 = f + geom_point()+labs(title = "Top 10 ARV Procurement Unit Price by Country")
#port to Shiny 

dev.new()
f2 = ggplot(arv1, aes(x = Item, y = Unit.price, colour = Country, size = Quantity))
f1 = f2+geom_point()+labs(title = "Top 10 ARV Procurement Unit Price by Commodity")

f3 = ggplot(arv1, aes(x = Category, y = Unit.price, colour = Item, size = Quantity))
f1 = f3+geom_point()+labs(title = "Top 10 ARV Procurement Unit Price by Category")

#require historical data to see more prices
f3_1 = ggplot(arv1, aes(x = Item, y = Unit.price, color = Country))
f3_1 + geom_boxplot()

#All Commodities
f10 = ggplot(master, aes(x = Quantity, y = Unit.price, colour = Category))
f10+geom_point()

f10_1 = ggplot(master[master$Unit.price <150,], 
               aes(x = Country, y = Unit.price, colour = Category, size = Quantity))
f1 = f10_1+geom_point()+labs(title = "Commodity Unit Price by Country and Quantity")

#RTK
rtk = master[master$Category == "RTKs", ]
rtk_=aggregate(Quantity~Item, data = rtk,FUN = sum)
rtkh = rtk_[order(-rtk_$Quantity),]; rtkh[1:10,]

rtk1 = rtk[rtk$Item %in% rtkh[1:10,]$Item,]
rtk2=rtk1[rtk1$Unit.price<150,]

dev.new()
f4 = ggplot(rtk2, aes(x = Quantity, y = Unit.price, colour = Item))
f4 + geom_point()+labs(title = "RTK Quantity VS Unit Price", caption = "(Excluded Outliers for Unit Price)")

dev.new()
f5 = ggplot(rtk2, aes(x = Country, y = Unit.price, colour = Item, size = Quantity))
f1 = f5+geom_point()+labs(title = "Top 10 RTK Procurement Unit Price by County")

#Essential Meds
meds = master[master$Category == "Essential Meds", ]
