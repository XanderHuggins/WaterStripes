library(dplyr); library(magrittr); library(readr); library(here)
library(fasterize); library(sf); library(raster); library(rgeos)
library(ggplot2); library(shiny); library(tmap); library(tmaptools); 
library(wesanderson); library(leaflet); library(leafpop); library(htmltools); library(stringr)

# import basin shapefiles and simplify to 0.5d resolution

Dummyras <- raster()
extent(Dummyras) <- c(-180, 180, -90, 90)
res(Dummyras) <- c(0.5, 0.5)
crs(Dummyras) <- CRS('+init=EPSG:4326')

Basins_raw <- sf::st_read(dsn = here::here("Data", "GRDC"), layer = "MBotW")
Basins_raw_0d5RAS <- fasterize(sf = Basins_raw, raster = Dummyras, field = "BASWC4_ID", fun = "last")
Basins_raw_0d5 <- rasterToPolygons(Basins_raw_0d5RAS, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
names(Basins_raw_0d5) <- "BASWC4_ID"

# Extract name 
Basins_raw_0d5$NAME <- Basins_raw$NAME[match(Basins_raw_0d5$BASWC4_ID, Basins_raw$BASWC4_ID)]

# Get URL for simple figure (no annotation)
Basins_raw_0d5$prev <- paste('https://raw.githubusercontent.com/XanderHuggins/WaterStripes/master/Figures/NoText/',
                            Basins_raw_0d5$NAME, '_id_', Basins_raw_0d5$BASWC4_ID, "_nolab",'.png', sep = "")
Basins_raw_0d5$prev <- str_replace_all(Basins_raw_0d5$prev, " ", "%20")

# plot leaflet map
pal <- wes_palette("Zissou1", length(Basins_raw_0d5$NAME), type = "continuous")   



labs <- lapply(seq(nrow(as.data.frame(Basins_raw_0d5))), function(i) {
  paste0( '<p>', "<big>", "<b>",as.data.frame(Basins_raw_0d5)[i, "NAME"],  " Basin", "</b>", "</big>", '<p></p>', 
          "<img src = ", as.data.frame(Basins_raw_0d5)[i, "prev"], " style='width:250px;height:125px;' ", ">", '</p>' ) 
})

# Get URL for hyperlink image (to full annotation image)
Basins_raw_0d5$URL <- paste("Download stripes for the ", Basins_raw_0d5$NAME, 
                            " basin ", "<a href=\"https://raw.githubusercontent.com/XanderHuggins/WaterStripes/master/Figures/", 
                            Basins_raw_0d5$NAME, "_id_", Basins_raw_0d5$BASWC4_ID, ".png\", target=\"_blank\">HERE</a>", sep = "")

# create interactive leaflet map 
Basins_raw_0d5 %>% 
  leaflet() %>%
  addTiles(group = "OSM") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = pal,
              highlightOptions = highlightOptions(color = "red", weight = 3,bringToFront = TRUE),
              label = lapply(labs, htmltools::HTML), 
              labelOptions = labelOptions(permanent = FALSE,
                                          direction = "auto",
                                          closeButton = TRUE,
                                          style = list("background-color" = "rgba(32,32,32,1)",
                                                       "border" = "none",
                                                       "color" = "white",
                                                       "line-height" = "0px",
                                                       "text-align" = "center",
                                                       "box-shadow" = "none",
                                                       "margin" = "1px !important",
                                                       "height" = "155px"),
                                          textsize = "14px"),
              group = "polygons",
              popup = ~URL) %>%
  setView(lat = 25, lng = 0, zoom = 2.25)

#paste0("<img src = ", Basins_raw_0d5$URL, ">"), #~URL,) %>%
# addPopupImages(image = paste0("<img src = ", Basins_raw_0d5$URL, ">"),
# group = "polygons",
# width = 250, height = NULL) %>%


  
  
