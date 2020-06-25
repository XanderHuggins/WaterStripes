library(dplyr); library(magrittr); library(readr); library(here)
library(fasterize); library(sf); library(raster); library(rgeos)
library(ggplot2); library(shiny); library(tmap); library(tmaptools); library(wesanderson); library(leaflet)

# import basin shapefiles and simplify to 0.5d resolution

Dummyras <- raster()
extent(Dummyras) <- c(-180, 180, -90, 90)
res(Dummyras) <- c(0.5, 0.5)
crs(Dummyras) <- CRS('+init=EPSG:4326')

Basins_raw <- sf::st_read(dsn = here::here("Data", "GRDC"), layer = "MBotW")
Basins_raw_0d5RAS <- fasterize(sf = Basins_raw, raster = Dummyras, field = "BASWC4_ID", fun = "last")
Basins_raw_0d5 <- rasterToPolygons(Basins_raw_0d5RAS, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
names(Basins_raw_0d5) <- "BASWC4_ID"

Basins_raw_0d5$NAME <- Basins_raw$NAME[match(Basins_raw_0d5$BASWC4_ID, Basins_raw$BASWC4_ID)]

Basins_raw_0d5$URL <- paste("Download stripes for the ", Basins_raw_0d5$NAME, 
                            " basin ", "<a href=\"https://raw.githubusercontent.com/XanderHuggins/WaterStripes/master/Figures/", 
                            Basins_raw_0d5$NAME, "_id_", Basins_raw_0d5$BASWC4_ID, ".png\", target=\"_blank\">HERE</a>", sep = "")

# plot leaflet map
pal <- wes_palette("Zissou1", length(Basins_raw_0d5$NAME), type = "continuous")   

Basins_raw_0d5 %>% 
  leaflet() %>%
  addTiles(group = "OSM") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = pal,
              highlightOptions = highlightOptions(color = "red", weight = 3,bringToFront = TRUE),
              popup = ~URL) %>%
  setView(lat = 25, lng = 0, zoom = 2.25)


  
