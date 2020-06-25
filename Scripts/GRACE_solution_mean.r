library(dplyr); library(magrittr); library(readr); library(here)
library(fasterize); library(sf); library(raster); library(rgeos)

# Create loop to average the three GRACE solutions, resample to 0.5 degree resolution, and write GeoTiff
CSR_Solutions = list.files(path = here::here("Data", "GRACE", "CSR"), pattern="*.tif")
GFZ_Solutions = list.files(path = here::here("Data", "GRACE", "GFZ"), pattern="*.tif")
JPL_Solutions = list.files(path = here::here("Data", "GRACE", "JPL"), pattern="*.tif")

TWS <- raster("./Rodell_SourceData/Rodell_etal_0d05.tif")
FPU_grid <- raster("./WaterScarcityAtlas/fpu30_hyde_compatible_v20d05.tif")
GridArea <- raster("./R_gis_exports/WGS84_cellArea_0d5res.tif") # Grid area estimate for WGS84 reference elipsoid

Dummyras <- raster()
extent(Dummyras) <- c(-180, 180, -90, 90)
res(Dummyras) <- c(0.5, 0.5)
crs(Dummyras) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

for (i in 1:length(CSR_Solutions)){
  tmp_CSR <- raster(paste(here::here("Data", "GRACE", "CSR"), "/", CSR_Solutions[i], sep = ""))
  tmp_GFZ <- raster(paste(here::here("Data", "GRACE", "GFZ"), "/", GFZ_Solutions[i], sep = ""))
  tmp_JPL <- raster(paste(here::here("Data", "GRACE", "JPL"), "/", JPL_Solutions[i], sep = ""))
  
  SolutionStack   <- stack(tmp_CSR, tmp_GFZ, tmp_JPL)
  SolutionMean   <- calc(SolutionStack, fun = mean, na.rm = T)
  
  SolutionMean_0d5 <- raster::resample(SolutionMean, Dummyras, method = "ngb")
  
  writeRaster(SolutionMean_0d5, paste(here::here("Data", "GRACE", "SolutionMean"), "/GRACE_index_",i, ".tif", sep=""), 
              format = "GTiff",overwrite = T)
  
  print(i)
}