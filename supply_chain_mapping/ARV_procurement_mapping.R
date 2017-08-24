#Lopinavir+Ritonavir

library(shiny)
library(RJSONIO)
library(magrittr)
library(leaflet)
library(rgdal)
library(geojsonio)
library(shinydashboard)


#Set up dataset
setwd("/Documents/USAID_Internship2017/supply_chain_management/")
countries <- readOGR("../dataset/countries.geojson", "OGRGeoJSON")

order = read.csv("../dataset/ARTMIS/RO_history_20170824.csv")
commodity_order=order[order$Product.Category == "HIV/AIDS Pharmaceuticals",]

#GEOLOCATIONS function
geocodeAdddress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}

#Deal with country naming here Conversions
commodity_order$Destination.Country = as.character(commodity_order$Destination.Country)
commodity_order[grepl("United States",commodity_order$Destination.Country),
                "Destination.Country"] = "United States of America"
unique(commodity_order$Destination.Country) %in% countries$ADMIN

#DATA FORMATTING  ##automate#################
#Geolocations ///////////////////////////////////////////////////////
#excludes empty string NOTE: there are orders with no destination country
corder = unique(commodity_order$Destination.Country)[unique(commodity_order$Destination.Country)!= ""]
coords = as.data.frame(t(sapply(corder, geocodeAdddress)))
coords$country = rownames(coords); colnames(coords) = c("long", "lat", "country" )

#Converting factors to order dates
fact_to_date = function(fact_val){
  return(as.Date(as.character(fact_val), format = "%Y/%m/%d"))
}
commodity_order[,c(13:17)] = as.data.frame(lapply(commodity_order[,c(13:17)], 
                                                  fact_to_date))

#Converting factors to numerics
fact_to_num = function(fact_val){
  return(as.numeric(gsub(",","",as.character(fact_val))))
}
commodity_order[,c(18:22)] = as.data.frame(lapply(commodity_order[,c(18:22)], 
                                                  fact_to_num))


#Shipment method
commodity_order[grepl("AIR", commodity_order$Shipment.Method), "Shipment.Method"] = "Air"
commodity_order[grepl("LAND", commodity_order$Shipment.Method), "Shipment.Method"] = "Land"
commodity_order[grepl("SEA", commodity_order$Shipment.Method), "Shipment.Method"] = "Sea"
commodity_order$Shipment.Method = as.factor(as.character(commodity_order$Shipment.Method))

#Status
status= table(commodity_order$Destination.Country, commodity_order$Status)
rownames(status)[!(rownames(status) %in% countries$name)]  #check
#convert to the same as geospatial data 
status_=as.data.frame.matrix(status) #preserves matrix format
cs = as.data.frame(rowSums(status))
cs$country = rownames(cs); colnames(cs)[1] = "Orders"

#Expense
expense = aggregate(Line.Total ~Destination.Country, data = commodity_order, sum)

#Merging
temp = merge(coords, expense, by.x = "country", by.y = "Destination.Country")
temp = merge(temp, cs, by = "country")

late = commodity_order$Actual.Delivery.Date - commodity_order$Agreed.Delivery.Date 
#unique(commodity_order$Status); class(commodity_order$late)
#paste("Shipment Delivered:", sum(commodity_order$Status == "Shipment Delivered"))
#paste("Cancelled:", sum(commodity_order$Status == "Cancelled"))
commodity_late=as.data.frame.matrix(t(table(late > 7, commodity_order$Destination.Country)))
commodity_late$country =rownames(commodity_late)
colnames(commodity_late) = c("On time", "Late", "country")
lateDays = aggregate(Actual.Delivery.Date-Agreed.Delivery.Date~Destination.Country,
                     commodity_order, mean)
commodity_late = merge(commodity_late, lateDays, by.x = "country", by.y = "Destination.Country")
colnames(commodity_late)[4] = "AvgLateDays"

#should merge with commodity_select
commodity_select = merge(temp, commodity_late, by = "country")

com =merge(countries, commodity_select, by.x = "ADMIN", by.y = "country")

#MAP ATTRIBUTES
#ICONS


#PALETTE
pal <- colorNumeric(
  palette = "Blues",
  domain = com$Late)

comor = c("RO.", "Destination.Country", "Product.Category", "Item.Description",
          "Status", "Shipped.Quantity", "Line.Total", "Agreed.Delivery.Date", 
          "Actual.Delivery.Date" )

#MAP
print.money <- function(x, ...) {
  paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=","))
}


#need to work on popuplinks
m = leaflet(com) %>% addTiles() %>% 
  setView(lng = 25, lat = -2, zoom = 2.5) %>%
  addEasyButton(easyButton(
    icon="fa-globe", title="Zoom to World View",
    onClick=JS("function(btn, map){ map.setZoom(2); }")))

m_ = m %>%
  # Base groups
  addPolygons(stroke = FALSE, smoothFactor = 0.2, 
              fillOpacity = 1, color = ~pal(Late), 
              label = ~ADMIN,
              popup = paste("Late Orders:", com$Late, "<br>",
                            "Average Number of Days Late:", 
                            round(as.numeric(com$AvgLateDays),1), "days<br>",
                            "Expense:", print.money(com$Line.Total)),
              highlight = highlightOptions(
                weight = 5,
                color = "#888",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              group = "Orders") %>%
  addLegend("bottomright", pal = pal, values = ~Late,
            title = "Number of Late Orders",
            opacity = 1)


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


ui = dashboardPage(title = "Order Lookup",
                   dashboardHeader(title = "ARV Procurement Orders"),
                   dashboardSidebar(
                     sidebarMenu(
                       menuItem("Data Dashboard", tabName = "datavis", icon = icon("dashboard")),
                       menuItem("Select Country", icon = icon("map"),
                                selectizeInput("country", "Click on Country", 
                                               choices = commodity_order$Destination.Country, 
                                               multiple = TRUE)),
                       menuItem("Select Product", icon = icon("search"),
                                selectizeInput("product", "Click on Product",
                                               choices = commodity_order$Item.Description,
                                               multiple = T)),
                       sliderInput("time_range","Timeframe:",
                                   min = min(as.Date(commodity_order$Order.Entry.Date, "%Y/%m/%d")), 
                                   max = max(as.Date(commodity_order$Order.Entry.Date, "%Y/%m/%d")), 
                                   value = c(as.Date("2016-01-01", "%Y/%m/%d"),
                                             Sys.Date()))
                       ///////////////////////////////////
                         //////////////////////////////// should also affect map and table
                       
                     )
                   ),
                   dashboardBody(
                     tabItems(
                       tabItem(tabName = "datavis",
                               h4("Map and Orders"),
                               fluidRow(box(width = 12, leafletOutput("map")),
                                        box(width = 12, dataTableOutput("table")))
                       )
                     )
                   )
)


server <- function(input, output, session) {
  
  country_sub <- reactive({
    temp = commodity_order[commodity_order$Destination.Country %in% input$country,comor]
    colnames(temp) = c("RO#", "Country", "Category", "Product", "Status", 
                       "Shipped Quantity", "RO Total Cost", "Agreed Delivery Date", 
                       "Actual Delivery Date")
    temp$`RO Total Cost` = print.money(temp$`RO Total Cost`)
    #temp$`Agreed Delivery Date` = as.factor(temp$`Agreed Delivery Date`)
    subset(temp, temp$Status != "Cancelled") 
    
  })
  
  #does order matter with country_sun and product_sub???
  product_sub = reactive({
    commodity_order[commodity_order$Item.Description %in% input$product,]
    /////////////////////////////////////
      ///////////////////////////// should change map and table
      //////////////////////////////
      //////////////////////////////
  })
  
  output$table <- renderDataTable(country_sub(), 
                                  options = list(scrollX = TRUE))
                                  #datatable width changes with window size
  
  output$map <- renderLeaflet({
    m_ %>%
      /////////////////////////////Overlay product specific map
      ///////////////////////////
      //////////////////////////
      # Layers control
      addLayersControl(
        baseGroups = c("Orders"),
        overlayGroups = c("Product"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
}

shinyApp(ui, server)
