#2015-2017 commodity data 
library(ggplot2)

commodity <- read.csv("~/USAID_Internship2017/dataset/country_commodity_20170718.csv", stringsAsFactors=FALSE)

names(commodity)
#"Country"          "Program"              "Supply.Chain.Framework"  "Product.Line"           
#"Global.Product"   "PO.Received.Year"     "PO.Line.Item.Value" 
View(commodity)

#convert dollar character to numeric
commodity$PO.Line.Item.Value = as.numeric(gsub('\\$|,', '', commodity$PO.Line.Item.Value))


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

bp = ggplot(commodity, aes(x = "", y = PO.Line.Item.Value, fill = Product.Line))+
  geom_bar(width = 1, stat = "identity")
bp+blank_theme

pie = bp + coord_polar(theta = "y", start = 0)
pie+blank_theme

pie + scale_fill_brewer("Commodity Expense") + blank_theme +
  theme(axis.text.x =element_blank())+
  labs(title = '2015-2017 Commodity Expense by Product Line 20170717 BI&A')

attach(commodity)
#qplot
qplot(Product.Line, PO.Line.Item.Value, data = commodity)
