library(dplyr); library(magrittr); library(readr); library(here)
library(fasterize); library(sf); library(raster); library(rgeos)
library(ggplot2); library(shiny); 

# import basin shapefiles and simplify to 0.5d resolution

Dummyras <- raster()
extent(Dummyras) <- c(-180, 180, -90, 90)
res(Dummyras) <- c(0.5, 0.5)
crs(Dummyras) <- CRS('+init=EPSG:4326')

Basins_raw <- sf::st_read(dsn = here::here("Data", "GRDC"), layer = "MBotW")
Basins_raw_0d5RAS <- fasterize(sf = Basins_raw, raster = Dummyras, field = "BASWC4_ID", fun = "last")
Basins_raw_0d5 <- rasterToPolygons(Basins_raw_0d5RAS, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
names(Basins_raw_0d5) <- "layer"

Basins_raw_0d5 <- merge(x = Basins_raw_0d5, y = as.data.frame(Basins_raw), 
                        by.x = "layer", by.y = "BASWC4_ID", all = T)

library(tmap); library(tmaptools); library(wesanderson)
data(World)

palt <- wes_palette("Darjeeling1", 100, type = "continuous") 


tm_shape(Basins_raw_0d5) + tm_polygons("NAME", palette = "Set3", alpha=0.6, id = "NAME", legend.show = FALSE) +
  # tm_shape(World) +  tm_borders(lwd = 0.7) +
  tm_layout(legend.show = F) +
  tm_basemap(server = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png")+
  tm_style("white", legend.show = F, earth.boundary = c(-180, -60, 180, 88), earth.boundary.color = "white",
           space.color = "white", legend.frame = F, frame = F) +
  tm_view(set.view = 1.5) 
  

  
