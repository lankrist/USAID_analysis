#ARV dashboard Late Orders
#Kristine Lan

library(shiny)
library(RJSONIO)
library(magrittr)
library(leaflet)
library(rgdal)
library(geojsonio)
library(shinydashboard)
library(htmlwidgets) #onRender()

#Set up dataset
setwd("/Documents/USAID_Internship2017/supply_chain_management/")
countries <- readOGR("../dataset/countries.geojson", "OGRGeoJSON")

order = read.csv("../dataset/ARTMIS/RO_history_20170824.csv")
comor = c("RO.", "Destination.Country", "Product.Category", "Item.Description",
          "Status", "Shipped.Quantity", "Unit.Cost", "Agreed.Delivery.Date", 
          "Actual.Delivery.Date", "Order.Entry.Date")

commodity_order=order[,comor]
commodity_order= subset(commodity_order, commodity_order$Status != "Cancelled")

#Functions
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
#convert factor to date function
fact_to_date = function(fact_val){
  return(as.Date(as.character(fact_val), format = "%Y/%m/%d"))
}
#convert factor to numeric function
fact_to_num = function(fact_val){
  return(as.numeric(gsub(",","",as.character(fact_val))))
}
#trim leading spaces
trim.leading <- function (x)  sub("^\\s+", "", x)
#trim trailing spaces
trim.trailing <- function (x) sub("\\s+$", "", x)
#trim spaces for leading and traling
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#Money print out format
print.money <- function(x, ...) {
  paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=","))
}

#Deal with country naming here Conversions
commodity_order$Destination.Country = as.character(commodity_order$Destination.Country)
commodity_order[grepl("United States",commodity_order$Destination.Country),
                "Destination.Country"] = "United States of America"
#unique(commodity_order$Destination.Country) %in% countries$ADMIN

#DATA FORMATTING 
#Converting factors to order dates
commodity_order[,c(8:10)] = as.data.frame(lapply(commodity_order[,c(8:10)], 
                                                  fact_to_date))

#Converting factors to numerics
commodity_order[,c(6:7)] = as.data.frame(lapply(commodity_order[,c(6:7)], 
                                                  fact_to_num))

#Shipment method should be of 4 types: land, air, sea, ""
##possible expansion
#commodity_order$Shipment.Method = as.factor(trim(toupper(commodity_order$Shipment.Method)))


############################################################################################
#Data frame
#Coordinates: Longitude and Latitude  #NOTE: there are orders with no destination country
# corder = unique(commodity_order$Destination.Country)[unique(commodity_order$Destination.Country)!= ""]
# coords = as.data.frame(t(sapply(corder, geocodeAdddress)))
# coords$country = rownames(coords) 
# colnames(coords) = c("long", "lat", "country" )

#Shipment order count
order_count = as.data.frame(table(commodity_order$Destination.Country))
colnames(order_count) = c("country", "Shipment Orders")

#Expense
expense = aggregate((Unit.Cost*Shipped.Quantity) ~Destination.Country, data = commodity_order, sum)
colnames(expense)= c("country","Cost.Product.Shipped")

#Late
lateDays = commodity_order$Actual.Delivery.Date - commodity_order$Agreed.Delivery.Date 
commodity_late=as.data.frame.matrix(t(table(lateDays > 7, commodity_order$Destination.Country)))
colnames(commodity_late) = c("country", "Late") # Orders with no dates generates NA
commodity_late$country = rownames(commodity_late)
AvglateDays = aggregate(lateDays~Destination.Country,commodity_order, mean)
commodity_late = merge(commodity_late, AvglateDays, by.x = "country", by.y = "Destination.Country")
colnames(commodity_late)[3] = "AvgLateDays"

#MERGE
temp = Reduce(function(x, y) merge(x, y, all=TRUE), list(order_count, expense, commodity_late))

com =merge(countries, temp, by.x = "ADMIN", by.y = "country")

#MAP ATTRIBUTES
#ICONS


#PALETTE
pal <- colorNumeric(
  palette = "Blues",
  domain = com$Late)


#MAP
m = leaflet(com) %>% addTiles() %>% 
  setView(lng = 25, lat = -2, zoom = 3) %>%
  addEasyButton(easyButton(
    icon="fa-globe", title="Zoom to World View",
    onClick=JS("function(btn, map){ map.setZoom(2); }")))


ui = dashboardPage(title = "Order Lookup",
                   dashboardHeader(title = "Commodity Procurement Orders"),
                   dashboardSidebar(
                     sidebarMenu(
                       menuItem("Data Dashboard", tabName = "datavis", icon = icon("dashboard")),
                       menuItem("Select Country", icon = icon("map"),
                                selectizeInput("country", "Click on Country", 
                                               choices = commodity_order$Destination.Country, 
                                               multiple = TRUE )),
                       # menuItem("Select Product Category", icon = icon("search"),
                       #          selectizeInput("product_cat", "Click on Category",
                       #                         choices = commodity_order$Product.category,
                       #                         multiple = T)),
                       menuItem("Select Product", icon = icon("search"),
                                selectizeInput("product", "Click on Product",
                                               choices = commodity_order$Item.Description,
                                               multiple = T)),
                       sliderInput("integer", "Number of Days Late:",
                                   min=0, max=90, value=7),
                       sliderInput("time_range","Timeframe:",
                                   min = min(as.Date(commodity_order$Order.Entry.Date, "%Y/%m/%d")), 
                                   max = max(as.Date(commodity_order$Order.Entry.Date, "%Y/%m/%d")), 
                                   value = c(as.Date("2016-01-01", "%Y/%m/%d"),
                                             Sys.Date()))
                       #   //////////////////////////////// should also affect map and table
                       
                     )
                   ),
                   dashboardBody(
                     tabItems(
                       tabItem(tabName = "datavis",
                               h4("Map and Orders"),
                               fluidRow(box(width = 12, leafletOutput("map")),
                                        #is product_map necessary, it will be out put at same box
                                        box(width = 12, dataTableOutput("table")))
                       )
                     )
                   )
)


server <- function(input, output, session) {
  
  sub <- reactive({
    temp= commodity_order[,-10]
    # if (!is.null(input$time_range)){ #filter by entry date
    #   temp = temp[temp$Order.Entry.Date > input$time_range,]
    # }
    if (!is.null(input$country)){ #filter by country
      temp = temp[temp$Destination.Country %in% input$country,]
    }
    if (!is.null(input$product)){ #filter by product
      temp = temp[temp$Item.Description %in% input$product,]
    }
    
    temp$Unit.Cost = temp$Unit.Cost*temp$Shipped.Quantity
    colnames(temp) = c("RO#", "Country", "Category", "Product", "Status", 
                       "Shipped Quantity", "RO Total Cost", "Agreed Delivery Date", 
                       "Actual Delivery Date")
    temp$`RO Total Cost` = print.money(temp$`RO Total Cost`)
    
    #subset(temp, temp$Status != "Cancelled")
    return(temp)
  })
  
  #map values need to change: define other reactive fuction?
  
  output$table <- renderDataTable(sub(), 
                                  options = list(scrollX = TRUE))
                                  #datatable width changes with window size
  
  output$map <- renderLeaflet({
    m%>%
        # Base groups
      addPolygons(stroke = FALSE, smoothFactor = 0.2, weight = 1,
                  fillOpacity = 1, color = ~pal(Late), 
                  label = ~ADMIN,
                  popup = paste("Late Orders:", com$Late, "<br>",
                                  "Average Number of Days Late:", 
                                  round(as.numeric(com$AvgLateDays),1), "days<br>",
                                  "Expense of Commodities Arrived:", 
                                  print.money(com$Cost.Product.Shipped)),
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#888",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  group = "Orders") %>%
      # addPolygons(stroke = FALSE, smoothFactor = 0.2,
      #             fillOpacity = 1, color = ~pal(Late),
      #             label = ~ADMIN,
      #             group = "Warehouse")%>%
      
      addLegend("bottomright", pal = pal, values = ~Late,
                title = "Number of Late Orders",
                opacity = 1)%>%
      
      # Layers control
      addLayersControl(
        baseGroups = c("Orders"),
        overlayGroups = c("Warehouse"),
        options = layersControlOptions(collapsed = FALSE))%>%
      addControl(html="<input id=\"slide\" type=\"range\" min=\"0\" max=\"1\" step=\"0.1\" value=\"1\">",
                 position = "bottomleft") %>%
      onRender("
        function(el,x,data){
          var map = this;
          var evthandler = function(e){
            var labels = map.layerManager._byGroup.Warehouse;
            Object.keys(labels).forEach(function(el){
              labels[el]._container.style.opacity = +e.target.value;
            });
          };
          $('#slide').on('mousemove',L.DomEvent.stopPropagation);
          $('#slide').on('input', evthandler);
        }
               ") 
      
    
  })
}

shinyApp(ui, server)

