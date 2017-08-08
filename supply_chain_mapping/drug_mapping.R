#Map web browser practice
library(magrittr)  #%>%  operator for pipe entry
library(shiny) #for webapp
library(leaflet) #for map interface
library(rgdal) #country shapes

setwd("/Documents/USAID_Internship2017/supply_chain_management/")

countries <- readOGR("../dataset/countries.geojson", "OGRGeoJSON")
dat=read.csv(file = "../dataset/ARV/ARV_mylan_data.csv", stringsAsFactors = F)
names(dat)
order = read.csv("../dataset/ARV/ARV_delievered_orders_TLE_20170808_mylan_test.csv")

#CALCULATIONS
status= table(order$Destination.Country, order$Status.Name)
rownames(status)
rownames(status)[2] = "Democratic Republic of the Congo" #convert to the same
rownames(status)[8] = "United Republic of Tanzania"
status_ = as.data.frame(status[order(rownames(status)),]) ; colnames(status_) = "order_count"
#make sure alphabetically makes sense

countries$orders = NULL
countries[(countries$ADMIN %in% rownames(status_)),"orders"] = status_$order_count

#Order dates

as.Date(order$Estimated.Delivery.Date, format = "%b $d %Y") - 
  order$Agreed.Delivery.Date

order$Line.Total

#ICONS
drugIcons = icons(
  iconUrl = ifelse(dat$Org_type == "Supplier",
                   "https://pbs.twimg.com/profile_images/3156893280/65941e74cd6d04e0e4e45e5c2527e29c_400x400.jpeg",
                   "https://image.flaticon.com/icons/svg/109/109386.svg"
  ),
  iconWidth = 20, iconHeight = 20 #icon size to change as map zooms in and out
)


#PALETTE
pal <- colorNumeric(
  palette = "Blues",
  domain = countries$orders)

#MAP
m = leaflet(countries) %>% addTiles()

m %>%
  addMarkers(data=dat,lng = ~Longitude, lat=~Latitude, 
             popup = ~Org_type, label = ~Organization,
             icon = drugIcons, group = "Location") %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, 
              fillOpacity = 1, color = ~pal(orders), 
              popup = paste("Delivered Orders:", countries$orders , "<br>",
                            "Requested Orders:", "<br>",
                            "Late Orders:", "<br>"))



















