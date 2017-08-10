#Map web browser practice
library(magrittr)  #%>%  operator for pipe entry
library(shiny) #for webapp
library(leaflet) #for map interface
library(rgdal) #country shapes
library(RJSONIO) #function to convert address to coordinates

setwd("/Documents/USAID_Internship2017/supply_chain_management/")
countries <- readOGR("../dataset/countries.geojson", "OGRGeoJSON")

dat=read.csv(file = "../dataset/ARV/USAID GHSC Eligible ARV Product List (SHARED 28 JUL 2017).csv", stringsAsFactors = F)
names(dat)
commodity_list = unique(dat$Active.Ingredients); commodity_list[18]

#SELECT COMMODITY (will add in drop down menu, and set default commodity)
commodity_information = dat[dat$Active.Ingredients==commodity_list[18], ]

order = read.csv("../dataset/ARTMIS/RO_history_20170809.csv")
commodity_order=order[grepl("Efavirenz/Lamivudine/Tenofovir", order$Item.Description),]

#GEOLOCATIONS
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
gsub(".*,","",gsub(".*&", "", commodity_information$FPP.Manufacturing.Site))

geocodeAdddress()




commodity_information$Latitude=runif(nrow(commodity_information),-90, 90)
commodity_information$Longitude=runif(nrow(commodity_information), -180, 180)

#DATA FORMATTING  ##automate#################
status= table(commodity_order$Destination.Country, commodity_order$Status)
rownames(status)
rownames(status)[!(rownames(status) %in% countries$ADMIN)] #convert to the same as geospatial data 
countries[grepl("Cote d'Ivoire", countries$ADMIN),]$ADMIN #checking
##conversion
rownames(status)[4] = "The Bahamas"
rownames(status)[13] = "Ivory Coast"
rownames(status)[18] = "Democratic Republic of the Congo" 
rownames(status)[49] = "United Republic of Tanzania"
rownames(status)[55] = "United States of America"
status_=as.data.frame.matrix(status) #preserves matrix format
status_c = status_[-1,]
status_c$country = rownames(status_c)


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

#spaltialpolugondataframe ####consider utility of loading all information to single table
commodity_select = merge(countries, status_c, by.x= "ADMIN", by.y = "country")
commodity_select$activity = rowSums(cs[,c(-1,-2)]) #exclude non-numeric values
cs = as.data.frame(commodity_select)

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
m = leaflet(commodity_select) %>% addTiles()

m %>% setView(lng = 25, lat = -10, zoom = 1) %>%
  addEasyButton(easyButton(
    icon="fa-globe", title="Zoom to World View",
    onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
  # Base groups
  addPolygons(stroke = FALSE, smoothFactor = 0.2, 
              fillOpacity = 1, color = ~pal(commodity_select$activity), 
              label = commodity_select$ADMIN,
              popup = paste("Delivered Orders:", commodity_select$`Shipment Delivered` , "<br>",
                            "Requested Orders:", commodity_select$`Order Received`, "<br>"),
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
  addMarkers(data=commodity_information,lng = ~Longitude, lat=~Latitude, 
             popup = "Manufacturer", label = ~Supplier,
             icon = drugIcons, group = "Manufacturer") %>%
  # Layers control
  addLayersControl(
    baseGroups = c("Orders"),
    overlayGroups = c("Manufacturer"),
    options = layersControlOptions(collapsed = FALSE)
  )





















