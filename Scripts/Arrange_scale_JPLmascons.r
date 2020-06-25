library(dplyr); library(magrittr); library(readr); library(here)
library(fasterize); library(sf); library(raster); library(rgeos); library(ncdf4); library(rgdal)

# Arrange and write each netCDF layer to GeoTiff in JPL mascon data after multiplying by gain factor and cleaning by ocean mask

#1 - load land mask
Land_mask <- nc_open(here::here("Data", "GRACE", "JPL_CRI_mascon", "LAND_MASK.CRI.nc"))
lon <- ncvar_get(Land_mask, "lon")
lat <- ncvar_get(Land_mask, "lat")
dname <- "land_mask"
ncatt_get(Land_mask, dname, "long_name")
ncatt_get(Land_mask, dname, "units")
Land_mask_get <- ncvar_get(Land_mask, dname)
dim(Land_mask_get)
Land_RAS <- raster(t(Land_mask_get[,]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
                   crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
Land_RAS <- flip(Land_RAS, direction = "y") # data is flipped in y direction
plot(Land_RAS)

#2 - load gain factor
Gain_factor <- nc_open(here::here("Data", "GRACE", "JPL_CRI_mascon", "CLM4.SCALE_FACTOR.JPL.MSCNv02CRI.nc"))
lon <- ncvar_get(Gain_factor, "lon")
lat <- ncvar_get(Gain_factor, "lat")
dname <- "scale_factor"
ncatt_get(Gain_factor, dname, "long_name")
ncatt_get(Gain_factor, dname, "units")
Gain_get <- ncvar_get(Gain_factor, dname)
dim(Gain_get)
Gain_RAS <- raster(t(Gain_get[,]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
                   crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
Gain_RAS <- flip(Gain_RAS, direction = "y") # data is flipped in y direction
plot(Gain_RAS)

#3 - load JPL mascon data, mask to land, multiply by gain, arrange from -180 to 180, then write GeoTiff
JPL_mascon_CRI <- nc_open(here::here("Data", "GRACE", "JPL_CRI_mascon", "GRCTellus.JPL.200204_202004.GLO.RL06M.MSCNv02CRI.nc"))
lon <- ncvar_get(JPL_mascon_CRI,"lon")
lat <- ncvar_get(JPL_mascon_CRI,"lat")
dname <- "lwe_thickness"
ncatt_get(JPL_mascon_CRI, dname, "long_name")
ncatt_get(JPL_mascon_CRI, dname, "units")
fillvalue <- ncatt_get(JPL_mascon_CRI, dname, "_FillValue")

# extract TWS trends
JPL_mascon_all <- ncvar_get(JPL_mascon_CRI, dname)
JPL_mascon_all[JPL_mascon_all==fillvalue$value] <- NA # assign fill values to 0
dim(JPL_mascon_all)

for(i in 1:dim(JPL_mascon_all)[3]){
  JPLras <- raster(t(JPL_mascon_all[,,i]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
                crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  
  JPLras <- flip(JPLras, direction = "y") # data is flipped in y direction
  
  Clip_ras <- JPLras*Land_RAS
  TWS_ras <- Clip_ras*Gain_RAS
  
  Arrange <- raster(TWS_ras)
  Arrange[,1:360] <- TWS_ras[,361:720]
  Arrange[,361:720] <- TWS_ras[,1:360]
  extent(Arrange) <- c(-180, 180, -90, 90)
  
  # write each layer to .tif
  writeRaster(Arrange, paste(here::here("Data", "GRACE", "JPL_CRI_mascon", "Month_GeoTiffs"), "/", "layerID", i, ".tif", sep = ""),
              format = "GTiff", overwrite = T)
  print(i)
}

# library(tmap); library(tmaptools); library(RColorBrewer)
# xpal <- brewer.pal(n = 11, name = "RdBu")
# 
# squishRas <- JPLras
# squishRas[squishRas < -25] <- -25
# squishRas[squishRas > 25] <- 25
# 
# plot(squishRas,
#      col = xpal,
#      breaks = c(seq(-25, 25, by = 5)))
