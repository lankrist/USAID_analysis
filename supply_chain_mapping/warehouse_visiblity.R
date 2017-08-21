#map level of visibility at warehouses
library(magrittr)  #%>%  operator for pipe entry
library(shiny) #for webapp
library(leaflet) #for map interface
library(rgdal) #country shapes
library(RJSONIO) #function to convert address to coordinates

setwd("/Documents/USAID_Internship2017/supply_chain_management/")
countries <- readOGR("../dataset/countries.geojson", "OGRGeoJSON")

levels(countries$ADMIN)
levels(countries$ADMIN)[grepl("Tanzania",levels(countries$ADMIN))]

stock_country = c("Angola", "Cameroon", "Ethiopia", "Ghana", "Guyana", "Haiti", 
                  "Lesotho", "Malawi", "Mozambique", "Namibia", "Nigeria", 
                  "Rwanda", "Swaziland", "United Republic of Tanzania", 
                  "Uganda", "Vietnam", "Zambia", "Zimbabwe")

#stock_country %in% countries$ADMIN 
countries$national_stock = NA
countries$site_stock = NA
countries$Distribution_point_stock = NA

nat = as.logical(c(rep(1, 4), 0, rep(1, 2), 0, rep(1, 6), rep(0,2), rep(1, 2)))
site = as.logical(c(1, 0, 1, 0, 0, rep(1, 4), 0, 1, rep(0, 5), rep(1, 2)))
distp = as.logical(c(0, rep(1,16),0))
visibility = as.data.frame(cbind(stock_country, nat, site, distp))
#visibility[,2:4] = sapply(visibility[,2:4], as.numeric)-1
visibility[,2:4] = sapply(visibility[,2:4],as.logical)

stock_ = merge(countries, visibility, by.x= "ADMIN", by.y = "stock_country")


YN = function(logical){
  return(ifelse(logical ==T, "Yes", "No"))
}
YN(NA)


pal <- colorNumeric(
  palette = "Blues",
  domain = stock_$visibility)

m = leaflet(stock_) %>% addTiles() %>% 
  addEasyButton(easyButton(
    icon="fa-globe", title="Zoom to World View",
    onClick=JS("function(btn, map){ map.setZoom(2); }"))) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, 
            fillOpacity = 1, 
            label = stock_$ADMIN,
            popup = paste("National Stock:" , YN(stock_$nat), "<br>",
                          "Site Stock:", YN(stock_$site), "<br>",
                          "Distribution Point Stock:", YN(stock_$distp),"<br>"),
            highlight = highlightOptions(
              weight = 5,
              color = "#888",
              fillOpacity = 0.7,
              bringToFront = TRUE),
            group = "Orders") 


#%>%
  addLegend("bottomright", pal = pal, values = ~visibility,
            title = "Visibility",
            opacity = 1)

m
