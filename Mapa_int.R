library(sf)
library(tidyr)
library(leaflet)
library(rgeos)
library(rgdal)
library(spatstat)
library(maptools)
library(leaflet.extras)
library(shiny)
library(deldir)
library(sp)
library(maps)
library(caret)
library(raster)
library(mapproj)
library(plyr)
library(dplyr)

### Creamos el mapa con shinyapp

shinyApp(
  ui <- leafletOutput("map"), 
  
  server <- function(input, output, session){
    
    output$map <- renderLeaflet({

Baja_california <- leaflet() %>% addTiles() %>%
  addCircleMarkers(data = ady_df, 
                   lat = ~ LATITUD, 
                   lng = ~ LONGITUD, radius = 5, color = "red") %>%

      ###  Dibujamos los poligonos que creamos
      
      addPolygons(data = vspf, 
                  color = "blue",
              highlight = highlightOptions(weight = 3,
                                           color = "green",
                                           bringToFront = TRUE),fill = T) %>%
      
      ## Ponemos las opciones para dibujar sobre el mapa
      
  addDrawToolbar(
    targetGroup = "draw",
    editOptions = editToolbarOptions(
      selectedPathOptions = selectedPathOptions()
    )
  )  %>%
  addLayersControl(overlayGroups = c('wards'),
                   options = layersControlOptions(collapsed=FALSE)) %>%
  addStyleEditor()
})

    
##    Ponemos una condicion para saber si ya existe el archivo o no
if (file.exists("Poligonos.csv") == TRUE){
      observeEvent(input$map_marker_click, {
            print(input$map_marker_click)
      })  
      
      observeEvent(input$map_draw_new_feature, {
            write.table(input$map_draw_new_feature, file = "Poligonos.csv", 
                        append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)      
            
            print(input$map_draw_new_feature)
      })
}
    
if (file.exists("Poligonos.csv") == FALSE){
      observeEvent(input$map_marker_click, {
             print(input$map_marker_click)
      })  

      observeEvent(input$map_draw_new_feature, {
            write.table(input$map_draw_new_feature, file = "Poligonos.csv", 
                  append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)      
  
      print(input$map_draw_new_feature)
      })
      }
})



#################################

## Leemos el archivo que creeamos con los nuevos poligonos

A <- read.table("Poligonos.csv", header = TRUE, sep = ",", dec = ".", fill = TRUE)

j <- length(A$type)


vspf <- c()

## Una iteracion para ir creando cada poligono que dibujamos
for (i in 1:j) {
      
B <- gather(A[i,], key = "Longitud")



B$Longitud <- NULL


##    Filtramos un poco la informacion
for (i in 1:4) {
      B[i,] <- NA
      
}
B <- B%>% drop_na("value")


##    Separamos la informacion por longitud y latitud

T  <- length(B$value)
Longitud <- c()

for (i in 1:T) {
      if(i %% 2 != 0){
            Longitud[i] <- B$value[i]
      }
      
}
Latitud <- c()
for (i in 1:T) {
      if(i %% 2 == 0){
            Latitud[i] <- B$value[i]
      }
      
}




##  Filtramos para que solo tengamos la informacion
Longitud <- as.data.frame(Longitud)

Longitud <- Longitud %>% drop_na("Longitud")

Latitud <- as.data.frame(Latitud)

Latitud <- Latitud %>% drop_na("Latitud")


## Ponemos la informacion de tipo numerico
Poligono <- cbind(Longitud,Latitud)

Poligono$Longitud <- as.character(Poligono$Longitud)
Poligono$Latitud <- as.character(Poligono$Latitud)

Poligono$Longitud <- as.numeric(Poligono$Longitud)
Poligono$Latitud <- as.numeric(Poligono$Latitud)



## Generamos el poligono

r <- Poligono %>% dplyr::select(Longitud,Latitud)
#  Creamos los puntos
pts <- as.matrix(r)
# hacemos coincidir el primero y yltimo
pts = rbind(pts, pts[1,])
# creamos un poligono 

sp = SpatialPolygons( list(  Polygons(list(Polygon(pts)), 1)))

v_sp <- st_as_sf(sp)
###v_sp <- st_buffer(v_sp, dist = .005) 
st_crs(v_sp) <- 4326


## Generamos una lista de los poligonos que se van haciendo

vspf <- rbind(vspf, v_sp)

}





plot(vspf)











