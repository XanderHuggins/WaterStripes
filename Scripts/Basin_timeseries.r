library(dplyr); library(magrittr); library(readr); library(here)
library(fasterize); library(sf); library(raster); library(rgeos); library(spatstat);
library(ggplot2); library(RColorBrewer); library(scales)

# import grid area raster
GridArea <- raster(here::here("Data", "GridArea_0d5_wgs84.tif"))

# Import major basins of the world, and resample to 0.5 degrees in raster format
Basins_raw <- sf::st_read(dsn = here::here("Data", "GRDC"), layer = "MBotW")
Basins_raw_0d5RAS <- fasterize(sf = Basins_raw, raster = GridArea, field = "BASWC4_ID", fun = "last")

# Create index of all basin GRDC ids
Basin_id <- raster::unique(Basins_raw_0d5RAS)

# calculate mean TWSa per basin per GRACE month
Master_df = as.data.frame(matrix(numeric(),nrow = 184, ncol = length(Basin_id)+1))
colnames(Master_df)[1] = "GRACE_mo_id"
for(k in 2:229){colnames(Master_df)[k] = paste("Basin_",Basin_id[k-1], sep = "")}
Master_df$GRACE_mo_id <- c(seq(1, 184, by = 1))
  
#-- import GRACE data
for (j in 1:184) {
  TWS_anom_mo <- raster(paste(here::here("Data", "GRACE", "JPL_CRI_mascon", "Month_GeoTiffs"),
                              "/layerID", j, ".tif", sep = ""))
  
  df <- cbind(as.data.frame(Basins_raw_0d5RAS), as.data.frame(TWS_anom_mo), as.data.frame(GridArea)) %>%
    set_colnames(c("ID", "TWSa", "Area"))
  df <- df[complete.cases(df), ]
  
  Month_means <- df %>%
    group_by(ID) %>%
    summarise(TWSa_mo_mu = weighted.mean(TWSa, Area, na.rm = TRUE)) %>%  as.data.frame()
  
  for (m in 2:229) {
    Master_df[j, m] <- Month_means[m-1, 2]
  }
  
  print(j/184)
}
  
# For each basin, pull name, and plot 

for (i in 1:length(Basin_id)){
  # pull basin name from basin shapefile attributes
  name = Basins_raw %>% as.data.frame() %>% filter(BASWC4_ID == Basin_id[i]) %>% pull(NAME) %>% as.character()
  
  Basin_TWS_df <- cbind(as.data.frame(Master_df[,1]), as.data.frame(Master_df[,i+1])) %>%
                          set_colnames(c("Month_id", "TWSa"))
  # import csv of GRACE months included and those missing data
  GRACE_template <- readr::read_csv(here::here("Data", "GRACE", "GRACE_months.csv")) %>% as.data.frame()
  
  # merge results with GRACE data vailability template
  GRACE_template$TWSa_mean <- Basin_TWS_df$TWSa[match(GRACE_template$GRACE_index_no, Basin_TWS_df$Month_id)]
  
  ## now plot the data
  theme_strip <- theme_minimal()+
    theme(axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.title = element_blank(),
          panel.grid.major = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(size = 10, vjust = 3, color = "white"),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 16, face = "plain", hjust = 0.5, color = "white"),
          axis.ticks.x = element_line(color = c("white", "grey12")),
          plot.caption = element_text(color = "white"),
          plot.background = element_rect(fill = "grey10"))
  
  xcol <- brewer.pal(11, "RdBu")
  
  sd_04t09 <- sd(GRACE_template$TWSa_mean[22:93])
  
  ggplot(GRACE_template, aes(x = Mo_sr_no, y = 1, fill = TWSa_mean))+
    geom_tile()+
    scale_x_continuous(breaks = seq(10, 214, by = 6), 
                       labels = c("", 2003, "", 2004, "", 2005, "", 2006, "", 2007, "", 2008, 
                                  "", 2009, "", 2010, "", 2011, "", 2012, "", 2013, "", 2014,
                                  "", 2015, "", 2016, "", 2017, "", 2018, "", 2019, "")) +
    scale_y_continuous(expand = c(0, 0))+
    scale_fill_gradientn(colors = xcol, limits=c(-2.6*sd_04t09, 2.6*sd_04t09), 
                         oob=squish, na.value="grey77")+
    guides(fill = guide_colorbar(barwidth = 1))+
    labs(title = paste("Water availability anomalies for the ",name, 
                       " Basin since 2002", sep = ""),
         caption = "Data: JPL global mascons (RL06 v02)")+
    theme_strip
  
  ggsave(filename = paste(here::here("Figures"), "/", name,"_id_", Basin_id[i],".png" , sep = ""),
         plot = last_plot(),
         device = "png",
         dpi = 300,
         width = 10, 
         height = 5.63,
         units = "in")
  
  print(i)
}
