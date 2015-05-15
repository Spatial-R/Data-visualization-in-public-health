library(stringi)
library(rCharts)
df<-read.csv("YDF.csv",header=T)
df$Lb<-stri_replace_all_charclass(df$Lb, "\\p{WHITE_SPACE}", "")
library(RColorBrewer)
# max of 12 cats in colorbrewer, gotta add 6 more
colors <- c("#ADFF2F","#8470FF","#CD3278","#68228B")
df2 <- df
routes <- unique(df2$Lb)
df2$color <- colors[match(df2$Lb, routes)]

df2$popup <- paste0("<p>名字:  ", df$Name, 
                    "<br>地址:  ", df$Add, 
                   "<br>类别:  ",df2$Lb, "</p>")
tmp.data <- apply(df2, 1, as.list)

bus.map <- Leaflet$new()
bus.map$setView(c(30.3,120.2), zoom = 5)
bus.map$tileLayer(provider = 'Stamen.TonerLite')
# Add Data as GeoJSON Layer and Specify Popup and FillColor
bus.map$geoJson(toGeoJSON(tmp.data, lat = 'long', lon = 'lat'),
           onEachFeature = '#! function(feature, layer){
           layer.bindPopup(feature.properties.popup)
           } !#',
           pointToLayer =  "#! function(feature, latlng){
           return L.circleMarker(latlng, {
           radius: 5,
           fillColor: feature.properties.color || 'red', 
           color: '#000',
           weight: 1,
           fillOpacity: 0.8
           })
           } !#"           
           )
bus.map$set(width = 1600, height = 800)
bus.map$enablePopover(TRUE)
bus.map$save("index.html", cdn=T)