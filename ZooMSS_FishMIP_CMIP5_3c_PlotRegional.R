
library(tidyverse)
library(patchwork)

dat <- read_rds("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5_RegionalCompare/Global_Model_Figures/regional_v_global_data_wZooMSS.RDS")


base_dir <- "/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5_RegionalCompare/Output/"

gg <- list()
cnt <- 0

scenario <- c("historical", "rcp85")

# Humboldt (1), North Sea(2), Med (3), Baltic (4), SE_Australia (5), East Bass Strait (6), Cook Strait (7).
region_names <- c("Humboldt", "North_Sea", "Mediterranean", "Baltic_Sea", "SE_Australia", "East_Bass_Strait", "Cook_Strait")

models <- c("gfdl-esm2m", "ipsl-cm5a-lr")

for (o in 1:length(scenario)){
  for (n in 1:length(region_names)){
    for (m in 1:length(models)){

        if (scenario[o] == "rcp85"){
          cnt <- cnt + 1
          out <- dat[[region_names[n]]][["global_models"]][["tcb"]][["unfished"]][[scenario[1]]][[models[m]]] %>%
            bind_rows(dat[[region_names[n]]][["global_models"]][["tcb"]][["unfished"]][[scenario[2]]][[models[m]]])

          if (!"apecosm" %in% colnames(out)){
            out$apecosm <- NA
          }

          if (!"macroecological" %in% colnames(out)){
            out$macroecological <- NA
          }

          if (!"dbpm" %in% colnames(out)){
            out$dbpm <- NA
          }


          out <- out %>%
            pivot_longer(cols = 2:dim(out)[2], values_to = "Biomass", names_to = "Model") %>%
            group_by(Model) %>%
            mutate(mn = mean(Biomass[1:10]),
                   delta_tcb = ((Biomass-mn)/Biomass) + 1)

          gg[[cnt]] <- ggplot(data = out, mapping = aes(x = Date..yyyy., y = delta_tcb, colour = Model)) +
            geom_line(size = 0.6) +
            geom_hline(yintercept = 1, colour = "black", size = 1) +
            ggtitle(paste0(region_names[n],"-",models[m])) +
            theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
            xlim(1975,2100)
          rm(out)
        }

    }
  }
}

graphics.off()
x11(width = 12, height = 12)
wrap_plots(gg, ncol = 2, byrow = TRUE, guides = "collect")
ggsave("Figures/CMIP5_ModelComparison.pdf")


# Look at Enviro Data Differences


cnt = 0
gg <- list()

for (n in 1:length(region_names)){

  cnt = cnt + 1
  sst <- dat[[region_names[n]]][["env_vars"]][["historical"]][["temp"]] %>%
    bind_rows(dat[[region_names[n]]][["env_vars"]][["rcp85"]][["temp"]])  %>%
    pivot_longer(cols = 2:dim(dat[[region_names[n]]][["env_vars"]][["rcp85"]][["temp"]])[2], values_to = "SST", names_to = "Model")

  gg[[cnt]] <- ggplot(data = sst, mapping = aes(x = Date..yyyy., y = SST, colour = Model)) +
    geom_line(size = 0.6) +
    ggtitle(region_names[n]) +
    theme(axis.title.x = element_blank()) +
    xlim(1975,2100)
  rm(sst)

  ####

  library(raster)
  # Humboldt (1), North Sea(2), Med (3), Baltic (4), SE_Australia (5), East Bass Strait (6), Cook Strait (7).

  r <- raster("fishMIP_regional_mask3_outputs.nc")
  r <- calc(r, fun=function(x){ifelse(x>10,NA,x)})

  cnt = cnt + 1
  t1 <- stack("/Users/jason/Nextcloud/MME1Data/ZooMSS_CMIP5/merged/tos/tos_Oyr_GFDL-ESM2M_historical_r1i1p1_onedeg_merged.nc")
  t1 <- t1[[26:56]]
  t2 <- stack("/Users/jason/Nextcloud/MME1Data/ZooMSS_CMIP5/merged/tos/tos_Oyr_GFDL-ESM2M_rcp85_r1i1p1_onedeg_merged.nc")
  t <- stack(t1, t2)
  rx <- calc(r, fun=function(x){ifelse(x==n,1,NA)})
  tMean1 = tibble(Temp = (cellStats(t*rx, "mean", na.rm = TRUE)), Date = 1975:2100, Model = "GFDL_ESM2M")
  rm(t1, t2, t, rx)

  t1 <- stack("/Users/jason/Nextcloud/MME1Data/ZooMSS_CMIP5/merged/tos/tos_Oyr_IPSL-CM5A-LR_historical_r1i1p1_onedeg_merged.nc")
  t1 <- t1[[26:56]]
  t2 <- stack("/Users/jason/Nextcloud/MME1Data/ZooMSS_CMIP5/merged/tos/tos_Oyr_IPSL-CM5A-LR_rcp85_r1i1p1_onedeg_merged.nc")
  t <- stack(t1, t2)
  rx <- calc(r, fun=function(x){ifelse(x==n,1,NA)})
  tMean2 = tibble(Temp = (cellStats(t*rx, "mean", na.rm = TRUE)), Date = 1975:2100, Model = "IPSL_CM5A_LR")
  rm(t1, t2, t, rx)

  tMean <- bind_rows(tMean1, tMean2)
  tMean$Temp <- tMean$Temp - 273

  gg[[cnt]] <- ggplot(data = tMean, mapping = aes(x = Date, y = Temp, colour = Model)) +
    geom_line(size = 0.6) +
    ggtitle(region_names[n]) +
    theme(axis.title.x = element_blank()) +
    xlim(1975,2100)
  rm(tMean)



  ####

  cnt = cnt + 1
  npp <- dat[[region_names[n]]][["env_vars"]][["historical"]][["npp"]] %>%
    bind_rows(dat[[region_names[n]]][["env_vars"]][["rcp85"]][["npp"]])  %>%
    pivot_longer(cols = 2:dim(dat[[region_names[n]]][["env_vars"]][["rcp85"]][["npp"]])[2], values_to = "NPP", names_to = "Model")

  gg[[cnt]] <- ggplot(data = npp, mapping = aes(x = Date..yyyy., y = NPP, colour = Model)) +
    geom_line(size = 0.6) +
    ggtitle(region_names[n]) +
    theme(axis.title.x = element_blank()) +
    xlim(1975,2100)
  rm(npp)



####

  cnt = cnt + 1
  c1 <- stack("/Users/jason/Nextcloud/MME1Data/ZooMSS_CMIP5/merged/chl/chlos_Oyr_GFDL-ESM2M_historical_r1i1p1_onedeg_merged.nc")
  c1 <- c1[[26:56]]
  c2 <- stack("/Users/jason/Nextcloud/MME1Data/ZooMSS_CMIP5/merged/chl/chlos_Oyr_GFDL-ESM2M_rcp85_r1i1p1_onedeg_merged.nc")
  c <- stack(c1, c2)
  rx <- calc(r, fun=function(x){ifelse(x==n,1,NA)})
  cMean1 = tibble(Chl = (cellStats(c*rx, "mean", na.rm = TRUE) * 1e6), Date = 1975:2100, Model = "GFDL_ESM2M")
  rm(c1, c2, c, rx)

  c1 <- stack("/Users/jason/Nextcloud/MME1Data/ZooMSS_CMIP5/merged/chl/chlos_Oyr_IPSL-CM5A-LR_historical_r1i1p1_onedeg_merged.nc")
  c1 <- c1[[26:56]]
  c2 <- stack("/Users/jason/Nextcloud/MME1Data/ZooMSS_CMIP5/merged/chl/chlos_Oyr_IPSL-CM5A-LR_rcp85_r1i1p1_onedeg_merged.nc")
  c <- stack(c1, c2)
  rx <- calc(r, fun=function(x){ifelse(x==n,1,NA)})
  cMean2 = tibble(Chl = (cellStats(c*rx, "mean", na.rm = TRUE) * 1e6), Date = 1975:2100, Model = "IPSL_CM5A_LR")

  cMean = bind_rows(cMean1, cMean2)

  gg[[cnt]] <- ggplot(data = cMean, mapping = aes(x = Date, y = Chl, colour = Model)) +
    geom_line(size = 0.6) +
    ggtitle(region_names[n]) +
    theme(axis.title.x = element_blank()) +
    xlim(1975,2100)
  rm(cMean)

}


graphics.off()
x11(width = 12, height = 12)
if (plotting == 1){
  wrap_plots(gg, ncol = 4, byrow = TRUE, guides = "collect")
}
ggsave("Figures/CMIP5_EnviroComparison.pdf")






