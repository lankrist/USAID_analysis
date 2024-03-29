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
          "Actual.Delivery.Date")

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
commodity_order[,c(8:9)] = as.data.frame(lapply(commodity_order[,c(8:9)], 
                                                  fact_to_date))
commodity_order$lateDays = commodity_order$Actual.Delivery.Date - commodity_order$Agreed.Delivery.Date

#Converting factors to numerics
commodity_order[,c(6:7)] = as.data.frame(lapply(commodity_order[,c(6:7)], 
                                                  fact_to_num))


#Product names
commodity_order$Item.Description = as.factor(gsub("\\s*\\[[^\\)]+\\]","",
                                                  as.character(commodity_order$Item.Description)))
#will need to b editted

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


#MAP ATTRIBUTES
#ICONS

#PALLETTE
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = c(0,300))


#MAPPING

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
                       #          selectizeInput("product_cat", "Select Category",
                       #                         choices = commodity_order$Product.Category,
                       #                         multiple = T)),
                       menuItem("Select Product", icon = icon("search"),
                                selectizeInput("product", "Select Product",
                                               choices = commodity_order$Item.Description,
                                               multiple = T)),
                       # menuItem("Select Status", icon = icon("tasks"),
                       #          selectizeInput("status", "Select Status",
                       #                         choices = commodity_order$Status,
                       #                         multiple = T)),
                       sliderInput("late_cut", "Number of Days Late:",
                                   min=0, max=90, value=7),
                       sliderInput("time_range","Timeframe:",
                                   min = as.Date("2015-01-01"), 
                                   max = Sys.Date(), 
                                   value = c(as.Date("2015-01-01"),
                                             Sys.Date())),
                       checkboxGroupInput("status_filters", "Order Status:",
                                         choices = c("Late" = "late_order",
                                           "On time" = "on_time",
                                           "Processing" = "processing",
                                           "Other" = "other"),
                                         selected = c("Late" = "late_order",
                                                      "On time" = "on_time",
                                                      "Processing" = "processing",
                                                      "Other" = "other"))
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
  
  tab_out <- reactive({
    temp= commodity_order
    # if (!is.null(input$time_range)){ #filter by entry date
    #   temp = temp[temp$Order.Entry.Date < input$time_range[1],]
    #   temp = temp[temp$Order.Entry.Date > input$time_range[2],]
    # }
    
    # if (!is.null(input$status)){ #filter by country
    #   temp = temp[temp$Status %in% input$status,]
    # }
    if (!is.null(input$country)){ #filter by country
      temp = temp[temp$Destination.Country %in% input$country,]
    }
    if (!is.null(input$product)){ #filter by product, product should be affected by category
      temp = temp[temp$Item.Description %in% input$product,]
    }
    
    {#Status that interests the country backstop
    temp$status_filter = "Other"
    temp[!is.na(temp$Agreed.Delivery.Date), "status_filter"] = "Processing"
    temp[!is.na(temp$Actual.Delivery.Date) &
           temp$status_filter == "Processing", "status_filter"] = "On time"
    temp[temp$status_filter == "On time" &
            temp$lateDays > input$late_cut, "status_filter"] = "Late"
    # if (!is.na(input$status_filters)){
    #   temp = temp[temp$status_filter %in% input$status_filters,]
    #   }
    
    }
    
    return(temp)
  })
  
  
  map_out = reactive({ #will output the spatialpolygonsdataframe
    
    comods = commodity_order #filter by product and late days
    
    if (!is.null(input$product)){ #filter by product, product should be affected by category
      comods = comods[comods$Item.Description %in% input$product,]
    }

    #Shipment order count
    order_count = as.data.frame(table(comods$Destination.Country))
    colnames(order_count) = c("country", "Shipment Orders")

    #Expense
    expense = aggregate((Unit.Cost*Shipped.Quantity)~Destination.Country, data = comods, sum)
    colnames(expense)= c("country","Cost.Product.Shipped")

    #Late
    commodity_late=as.data.frame.matrix(t(table(comods$lateDays > input$late_cut, comods$Destination.Country)))
    colnames(commodity_late)[colnames(commodity_late) == "TRUE"] = "Late"
    commodity_late$country = rownames(commodity_late)
    AvglateDays = aggregate(lateDays~Destination.Country,comods, mean)
    commodity_late = merge(commodity_late, AvglateDays, by.x = "country", by.y = "Destination.Country")
    colnames(commodity_late)[colnames(commodity_late) == "lateDays"] = "AvgLateDays"

    #MERGE
    temp = Reduce(function(x, y) merge(x, y, all=TRUE), list(order_count, expense, commodity_late))
    com = merge(countries, temp, by.x = "ADMIN", by.y = "country")

    return(com)
  })
  
  
  output$table <- renderDataTable({
    orders = tab_out()
    orders$Unit.Cost = orders$Unit.Cost*orders$Shipped.Quantity
    colnames(orders) = c("RO#", "Country", "Category", "Product", "Status",
                      "Shipped Quantity", "RO Total Cost", "Agreed Delivery Date",
                      "Actual Delivery Date", "Late days","Status_filter")
    orders$`RO Total Cost` = print.money(orders$`RO Total Cost`)
    orders},  #hid Late days and Status_filter in final prodcut
    options = list(scrollX = TRUE))
                                  #datatable width changes with window size
  
  output$map <- renderLeaflet({
    com = map_out()
    leaflet(com) %>% addTiles() %>%
      setView(lng = 25, lat = -2, zoom = 3) %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to World View",
        onClick=JS("function(btn, map){ map.setZoom(2); }")))%>%
        # Base groups
      addPolygons(stroke = FALSE, smoothFactor = 0.2, weight = 1,
                  fillOpacity = 1, color = ~pal(Late),
                  label = ~ADMIN,
                  popup = paste("Late Shipments:", com$Late, "<br>",
                                  "Average Number of Days Late:",
                                  round(as.numeric(com$AvgLateDays),1), "days<br>",
                                  "Value of Commodities Arrived:",
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
                title = "Number of Late Shipments",
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

