library(tidyverse)
library(raster)
library(sf)
library(rnaturalearth)
library(patchwork)
library(lubridate)

source("fZooMSS_Xtras.R")
res <- read_rds("/Users/jason/Nextcloud/MME2Work/ZooMSS/_LatestModel/20201016_CMIP5_Matrix/Output/res_20201016_CMIP5_Matrix.RDS")
enviro <- read_rds("/Users/jason/Nextcloud/MME2Work/ZooMSS/_LatestModel/20201016_CMIP5_Matrix/enviro_CMIP5_Matrix.RDS")
mdl <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20201016_CMIP5_Matrix/Output/model_20201016_CMIP5_Matrix.RDS")
w <- mdl$param$w
carbon <- mdl$param$Groups$Carbon

Bio <- fZooMSS_CarbonBiomass(res, w, carbon) # Convert to carbon biomass
enviro$BioSum <- fZooMSS_SumAll(Bio) # Sum the species

graphics.off()
x11(width = 6, height = 6)
ggplot(data = enviro, mapping = aes(x = (chlo), y = (BioSum), colour = sst)) +
  geom_point()
ggsave("Figures/ZooMSS_ChlRelationship.pdf")

var <- c("tpb", "tcb", "b10cm", "b10cm")

mollCRS <- CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
mollCRS_no <- 54009

robCRS <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
robCRS_no <- 54030

lonlatCRS <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
lonlatCRS_no <- 4326


plotGlobalChange <- function(x, y, tit, w_sf, clim){

  out <- calc(x[[1:10]], mean) # Average first decade
  out <- addLayer(out, calc(y[[(dim(y)[3]-9):dim(y)[3]]], mean)) #  Average last decade
  x_change <- ((out[[2]] - out[[1]])/out[[1]]) * 100

  dat <- st_as_sf(rasterToPolygons(x_change))
  dat <- st_transform(dat, crs = st_crs(robCRS)) # Convert to Robinson Projection

  gg <- ggplot() +
    geom_sf(data = dat, aes(fill = layer), colour = NA) +
    geom_sf(data = w_sf, size = 0.05, fill = "grey20") +
    scale_fill_gradient2(name = "Total Biomass Change (%)",
                         limits = clim,
                         midpoint = 0,
                         low = "blue",
                         mid = "white",
                         high = "red",
                         position = "right",
                         na.value = "grey80",
                         guide = "colourbar",
                         oob = scales::squish) +
    ggtitle(tit) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.title = element_text(angle = -90),
          panel.background = element_blank(),
          title = element_text(size = 8),
          legend.key.height = unit(2, "cm"),
          legend.title.align = 0.5) +
    guides(fill = guide_colourbar(title.position = "right"))

  return(gg)
}

plotGlobalYear <- function(dat, tit, w_sf){

  names(dat) <- "layer"
  dat <- st_as_sf(rasterToPolygons(dat)) %>%
    st_transform(crs = st_crs(robCRS)) %>% # Convert to Robinson Projection
    mutate(layer = log10(layer)) # Convert to kg

  gg <- ggplot() +
    geom_sf(data = dat, aes(fill = layer), colour = NA) +
    geom_sf(data = w_sf, size = 0.05, fill = "grey20") +
    scale_fill_gradient(name = expression("Total Biomass (log"[10]*"(g m"^-2*"))"),
                        limits = c(quantile(dat$layer, .10), quantile(dat$layer, .90)),
                        low = "yellow",
                        high = "red",
                        position = "right",
                        na.value = "grey80",
                        guide = "colourbar",
                        oob = scales::squish) +
    ggtitle(tit) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.title = element_text(angle = -90),
          panel.background = element_blank(),
          title = element_text(size = 8),
          legend.key.height = unit(1, "cm"),
          legend.title.align = 0.5) +
    guides(fill = guide_colourbar(title.position = "right"))

  return(gg)
}

plotTimeseries <- function(x, y, tit){
  out <- stack(hist, fut)

  df <- as.data.frame(out, xy = TRUE) %>%
    pivot_longer(cols = X1950.01.01:X2100.01.01, names_to = "Date", values_to = "Biomass") %>%
    mutate(Date = str_remove(Date, "X"),
           Date = ymd(Date),
           Year = year(Date))

  df2 <- df %>%
    group_by(Year) %>%
    summarise(Biomass = median(Biomass, na.rm = TRUE),
              .groups = "keep")

  # This doesn't seem to work in mutate. It just returns 0
  df2$BiomassChange = (df2$Biomass - mean(df2$Biomass[1:10], na.rm = TRUE))/mean(df2$Biomass[1:10], na.rm = TRUE) * 100

  gg <- ggplot(data = df2, aes(x = Year, y = BiomassChange)) +
    geom_point() +
    geom_smooth(method = "lm") +
    ggtitle(tit) +
    theme_bw() +
    theme(title = element_text(size = 8)) +
    ylab("Total Biomass Change (%)")
  rm(out)
  return(gg)
}

# Download and process world outline
world <- ne_countries(scale = "medium", returnclass = "sf")
world_sf <- st_transform(world, crs = st_crs(robCRS)) # Convert to different CRS


for(v in 1:length(var)){
  gg_map <- list()
  gg_map2100 <- list()
  gg_ts <- list()
  clim <- c(-50, 50)

  ### RCP26
  hist <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5/Output/gfdl-esm2m/historical/zoomss_gfdl-esm2m_nobc_historical_wo-diaz_no-fishing_no-oa_",var[v],"_global_annual_1950_2005.nc4"))
  fut <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5/Output/gfdl-esm2m/future/zoomss_gfdl-esm2m_nobc_rcp26_wo-diaz_no-fishing_no-oa_",var[v],"_global_annual_2006_2100.nc4"))
  tit <- paste0("GFDL RCP2p6 ",var[v]," (1950-2100 Change)")
  gg_map[[1]] <- plotGlobalChange(hist, fut, tit, world_sf, clim)
  gg_ts[[1]] <- plotTimeseries(hist, fut, tit)
  gg_map2100[[1]] <- plotGlobalYear(fut[[86]], str_replace(tit,"1950-2100 Change", "2100"), world_sf)
  rm(hist, fut)

  hist <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5/Output/ipsl-cm5a-lr/historical/zoomss_ipsl-cm5a-lr_nobc_historical_wo-diaz_no-fishing_no-oa_",var[v],"_global_annual_1950_2005.nc4"))
  fut <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5/Output/ipsl-cm5a-lr/future/zoomss_ipsl-cm5a-lr_nobc_rcp26_wo-diaz_no-fishing_no-oa_",var[v],"_global_annual_2006_2100.nc4"))
  tit <- paste0("IPSL RCP2p6 ",var[v]," (1950-2100 Change)")
  gg_map[[2]] <- plotGlobalChange(hist, fut, tit, world_sf, clim)
  gg_ts[[2]] <- plotTimeseries(hist, fut, tit)
  gg_map2100[[2]] <- plotGlobalYear(fut[[86]], str_replace(tit,"1950-2100 Change", "2100"), world_sf)
  rm(hist, fut)

  ### SSP585
  hist <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5/Output/gfdl-esm2m/historical/zoomss_gfdl-esm2m_nobc_historical_wo-diaz_no-fishing_no-oa_",var[v],"_global_annual_1950_2005.nc4"))
  fut <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5/Output/gfdl-esm2m/future/zoomss_gfdl-esm2m_nobc_rcp85_wo-diaz_no-fishing_no-oa_",var[v],"_global_annual_2006_2100.nc4"))
  tit <- paste0("GFDL RCP8p5 ",var[v]," (1950-2100 Change)")
  gg_map[[3]] <- plotGlobalChange(hist, fut, tit, world_sf, clim)
  gg_ts[[3]] <- plotTimeseries(hist, fut, tit)
  gg_map2100[[3]] <- plotGlobalYear(fut[[86]], str_replace(tit,"1950-2100 Change", "2100"), world_sf)
  rm(hist, fut)

  hist <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5/Output/ipsl-cm5a-lr/historical/zoomss_ipsl-cm5a-lr_nobc_historical_wo-diaz_no-fishing_no-oa_",var[v],"_global_annual_1950_2005.nc4"))
  fut <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5/Output/ipsl-cm5a-lr/future/zoomss_ipsl-cm5a-lr_nobc_rcp85_wo-diaz_no-fishing_no-oa_",var[v],"_global_annual_2006_2100.nc4"))
  tit <- paste0("IPSL RCP8p5 ",var[v]," (1950-2100 Change)")
  gg_map[[4]] <- plotGlobalChange(hist, fut, tit, world_sf, clim)
  gg_ts[[4]] <- plotTimeseries(hist, fut, tit)
  gg_map2100[[4]] <- plotGlobalYear(fut[[86]], str_replace(tit,"1950-2100 Change", "2100"), world_sf)
  rm(hist, fut)



  ### 1950-2100 Change Maps ###
  graphics.off()
  x11(width = 12, height = 8)
  wrap_plots(gg_map, ncol = 2, guides = "collect")
  ggsave(paste0("Figures/ZooMSS_MapDiff_",var[v],".pdf"))

  ### Time Series ###
  graphics.off()
  x11(width = 12, height = 6)
  wrap_plots(gg_ts, ncol = 2)
  ggsave(paste0("Figures/ZooMSS_TimeSeriesDiff_",var[v],".pdf"))


  ### 2100 Map ###
  graphics.off()
  x11(width = 12, height = 6)
  wrap_plots(gg_map2100, ncol = 2)
  ggsave(paste0("Figures/ZooMSS_Map2100_",var[v],".pdf"))
}



## Now look at Chlorophyll
plotChlTimeseries <- function(d1, d2, tit){

  dat <- rbind(d1, d2) %>%
    dplyr::select(-(EuclideanDist:SST_ZooMSS)) %>%
    group_by(year) %>%
    summarise(Chl = 10^(mean(Chl_log10, na.rm = TRUE)),
              .groups = "keep") %>%
    arrange(year) %>%
    ungroup()

  # This doesn't seem to work in mutate. It just returns 0
  dat$Change = (dat$Chl - median(dat$Chl[1:10], na.rm = TRUE))/median(dat$Chl[1:10], na.rm = TRUE) * 100

  gg <- ggplot(data = dat, aes(x = year, y = Change)) +
    geom_point() +
    geom_smooth(method = "lm") +
    ggtitle(tit) +
    theme_bw() +
    theme(title = element_text(size = 8)) +
    ylab("Total Chl Change (%)")
  return(gg)
}

d1 <- read_rds("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5/Input/Oyr_GFDL-ESM2M_historical_r1i1p1_onedeg_1950-2005_withZooMSS.rds")
d2 <- read_rds("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5/Input/Oyr_GFDL-ESM2M_rcp26_r1i1p1_onedeg_2006-2100_withZooMSS.rds")
tit <- "GFDL RCP2p6 Chl (1950-2100 Change)"
gg_chl[[1]] <- plotChlTimeseries(d1, d2, tit)
rm(d1, d2)

d1 <- read_rds("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5/Input/Oyr_IPSL-CM5A-LR_historical_r1i1p1_onedeg_1950-2005_withZooMSS.rds")
d2 <- read_rds("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5/Input/Oyr_IPSL-CM5A-LR_rcp26_r1i1p1_onedeg_2006-2100_withZooMSS.rds")
tit <- "IPSL RCP2p6 Chl (1950-2100 Change)"
gg_chl[[2]] <- plotChlTimeseries(d1, d2, tit)
rm(d1, d2)

d1 <- read_rds("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5/Input/Oyr_GFDL-ESM2M_historical_r1i1p1_onedeg_1950-2005_withZooMSS.rds")
d2 <- read_rds("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5/Input/Oyr_GFDL-ESM2M_rcp85_r1i1p1_onedeg_2006-2100_withZooMSS.rds")
tit <- "GFDL RCP8p5 Chl (1950-2100 Change)"
gg_chl[[3]] <- plotChlTimeseries(d1, d2, tit)
rm(d1, d2)

d1 <- read_rds("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5/Input/Oyr_IPSL-CM5A-LR_historical_r1i1p1_onedeg_1950-2005_withZooMSS.rds")
d2 <- read_rds("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5/Input/Oyr_IPSL-CM5A-LR_rcp85_r1i1p1_onedeg_2006-2100_withZooMSS.rds")
tit <- "IPSL RCP8p5 Chl (1950-2100 Change)"
gg_chl[[4]] <- plotChlTimeseries(d1, d2, tit)
rm(d1, d2)


### Time Series ###
graphics.off()
x11(width = 12, height = 6)
wrap_plots(gg_chl, ncol = 2)
ggsave("Figures/Chl_TimeSeriesDiff.pdf")




