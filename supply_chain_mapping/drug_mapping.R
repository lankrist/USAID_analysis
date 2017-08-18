#Map web browser practice
library(magrittr)  #%>%  operator for pipe entry
library(shiny) #for webapp
library(leaflet) #for map interface
library(rgdal) #country shapes
library(RJSONIO) #function to convert address to coordinates


setwd("/Documents/USAID_Internship2017/supply_chain_management/")
countries <- readOGR("../dataset/countries.geojson", "OGRGeoJSON")

#commodity suppliers
dat=read.csv(file = "../dataset/ARV/USAID GHSC Eligible ARV Product List (SHARED 28 JUL 2017).csv", stringsAsFactors = F)
names(dat); commodity_list = unique(dat$Active.Ingredients); como=commodity_list[18]

#SELECT COMMODITY (will add in drop down menu, and set default commodity)
commodity_information = dat[dat$Active.Ingredients==como, ]

order = read.csv("../dataset/ARTMIS/RO_history_20170809.csv")
commodity_order=order[grepl("Efavirenz/Lamivudine/Tenofovir", order$Item.Description),]

# budget and expense

#GEOLOCATIONS for "static" data i.e. supplier location
commodity_information$FPP.Manufacturing.Site = gsub("\n"," ",commodity_information$FPP.Manufacturing.Site)
geocodeAdddress <- function(address) {
  #require(RJSONIO)
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
#get rid of parenthesis and &
address_string =gsub("\\s*\\([^\\)]+\\)", "",gsub(".*&", "", commodity_information$FPP.Manufacturing.Site))
#NEED TO FIX!
address_string2 = sub("^\\s+", "", 
                      gsub("^.*?\\,","", 
                           gsub("^.*?\\,","", 
                                gsub("^.*?\\,","",address_string))))

address = lapply(address_string2, geocodeAdddress)
address = t(as.data.frame(address))

commodity_information$Latitude=address[,2]; commodity_information$Longitude=address[,1]

#Deal with country naming here Conversions
commodity_order$Destination.Country = as.character(commodity_order$Destination.Country); unique(commodity_order$Destination.Country)
countries[grepl("Cote d'Ivoire", countries$ADMIN),]$ADMIN #checking: called Ivory Coast
# country_name_change = function(original, changed){
#   commodity_order[grepl(original,commodity_order$Destination.Country),
#                   "Destination.Country"] = changed
# }
#country_name_change("Congo DRC", "Democratic Republic of the Congo")
#is the function pass by refernce of pass by value, this function changes the data frame

commodity_order[grepl("C?te d'Ivoire",commodity_order$Destination.Country),
                "Destination.Country"] = "Ivory Coast"
commodity_order[grepl("Congo DRC",commodity_order$Destination.Country),
                "Destination.Country"] = "Democratic Republic of the Congo" 
commodity_order[grepl("Tanzania",commodity_order$Destination.Country),
                "Destination.Country"] = "United Republic of Tanzania"
commodity_order[grepl("United States",commodity_order$Destination.Country),
                "Destination.Country"] = "United States of America"

#DATA FORMATTING  ##automate#################
#Geolocations ///////////////////////////////////////////////////////
#excludes empty string NOTE: there are orders with destination country
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
commodity_order$Line.Total= fact_to_num(commodity_order$Line.Total)

#Status
status= table(commodity_order$Destination.Country, commodity_order$Status)
rownames(status)[!(rownames(status) %in% countries$ADMIN)]  #check
#convert to the same as geospatial data 
status_=as.data.frame.matrix(status) #preserves matrix format
status_c = status_[-1,]; status_c$country = rownames(status_c)

#Expense
expense = aggregate(Line.Total ~Destination.Country, data = commodity_order, sum)

#Merging
temp = merge(coords, status_c, by="country")
temp = merge(temp, expense, by.x = "country", by.y = "Destination.Country")

#spaltialpolugondataframe ####consider utility of loading all information to single table
commodity_select = merge(countries, status_c, by.x= "ADMIN", by.y = "country")
cs = as.data.frame(commodity_select) #for sake of calulations
commodity_select$activity = rowSums(cs[,c(-1,-2)]) #exclude non-numeric values

commodity_order$late = commodity_order$Agreed.Delivery.Date - commodity_order$Actual.Delivery.Date 
#unique(commodity_order$Status); class(commodity_order$late)
#paste("Shipment Delivered:", sum(commodity_order$Status == "Shipment Delivered"))
#paste("Cancelled:", sum(commodity_order$Status == "Cancelled"))
commodity_late=as.data.frame.matrix(t(table(commodity_order$late > 0, commodity_order$Destination.Country)))
commodity_late$country =rownames(commodity_late)
colnames(commodity_late) = c("On time", "Late", "Destination.Country")
plot(commodity_late$Late)

#should merge with commodity_select
commodity_select = merge(commodity_select, expense, by.x = "ADMIN", 
                          by.y = "Destination.Country")

commodity_select = merge(commodity_select, commodity_late, by.x = "ADMIN", 
                         by.y = "Destination.Country")

commodity_forecast

#MAP ATTRIBUTES
#ICONS
supplier = unique(commodity_information$Supplier)
supplier_logo = c("https://media.glassdoor.com/sqll/518267/aurobindo-pharma-squarelogo-1424867466991.png",
                  "https://media.licdn.com/media/AAEAAQAAAAAAAAuCAAAAJGIyODkwODhlLTc4Y2MtNGViMy05ZTI5LWVlZTY0ZGQ4NWRiMw.png",
                  "http://www.hariwill.in/Gallery/Customer/1355740104_054864200.png.pagespeed.ce.PHUl8ozPnG.png",
                  "https://media.glassdoor.com/sqll/515179/macleods-pharma-squarelogo-1424700234624.png",
                  "https://pbs.twimg.com/profile_images/3156893280/65941e74cd6d04e0e4e45e5c2527e29c_400x400.jpeg")
drugIcons = icons(
  iconUrl = ifelse(commodity_information$Supplier == supplier[1],#"Aurobindo Pharma Limited"
    supplier_logo[1],
    ifelse(commodity_information$Supplier == supplier[2], #"Cipla Limited"
           supplier_logo[2],
           ifelse(commodity_information$Supplier == supplier[3], #"Hetero Labs Limited"
                  supplier_logo[3],
                  ifelse(commodity_information$Supplier == supplier[4], #"Macleods Pharmaceuticals Limited"
                         supplier_logo[4],
                         ifelse(commodity_information$Supplier == supplier[5], #"Mylan Laboratories Limited"  
                                supplier_logo[5],
                                ""))))),
  iconWidth = 20, iconHeight = 20 #icon size to change as map zooms in and out
)


#PALETTE
pal <- colorNumeric(
  palette = "Blues",
  domain = commodity_select$activity)

#MAP
print.money <- function(x, ...) {
 paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=","))
}


#need to work on popuplinks
m = leaflet(commodity_select) %>% addTiles() %>% 
  setView(lng = 25, lat = -2, zoom = 2.5) %>%
  addEasyButton(easyButton(
    icon="fa-globe", title="Zoom to World View",
    onClick=JS("function(btn, map){ map.setZoom(2); }")))

m %>%
  # Base groups
  addPolygons(stroke = FALSE, smoothFactor = 0.2, 
              fillOpacity = 1, color = ~pal(commodity_select$activity), 
              label = commodity_select$ADMIN,
              popup = paste("Late Orders:", '<b><a href = "http://www-personal.umich.edu/~lankrist/orders.html">',
                            commodity_select$Late, "</a></b>" , "<br>",
                            "Expense:", print.money(commodity_select$Line.Total)),
              highlight = highlightOptions(
                weight = 5,
                color = "#888",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              group = "Orders") %>%
  addLegend("bottomright", pal = pal, values = ~activity,
            title = "Number of Orders",
            opacity = 1)%>%
  #Overlay groups
  addMarkers(data=commodity_information,lng = ~Longitude, lat= ~Latitude, 
             popup = "Manufacturer", label = ~Supplier,  #include product recall inoformation, also market share
             icon = drugIcons, group = "Manufacturer") %>%
  # addMarkers(popup = "Image", 
  #             group = "Expense") %>%
  # Layers control
  addLayersControl(
    baseGroups = c("Orders"),
    overlayGroups = c("Manufacturer", "Expense"),
    options = layersControlOptions(collapsed = FALSE)
  )

#Shiny
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
)


server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    m %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.2, 
                  fillOpacity = 1, color = ~pal(commodity_select$activity), 
                  label = commodity_select$ADMIN,
                 popup = "<img src =  
                 https://www.presentationmagazine.com/powerpoint-templates/0/0/00810/free-graph-powerpoint-template_1.jpg>", 
                 highlight = highlightOptions(
                   weight = 5,
                   color = "#888",
                   fillOpacity = 0.7,
                   bringToFront = TRUE))
    
  })
}

shinyApp(ui, server)


















