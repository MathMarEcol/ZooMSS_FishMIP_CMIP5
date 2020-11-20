# rm(list = ls())

# setwd("~/Desktop/Papers/FishMIP_RegionalvsGlobal")
source("./Figures/multi-model_plot.R")
## Plot regional v global comparison plots of mem output and esm model env var output

library(raster) # For global maps
# library(abind)
# library(egg)
library(ncdf4)
library(tidyverse)

regions_global <- c("Baltic_Sea", "Cook_Strait", "East_Bass_Strait", "Humboldt", "Mediterranean",
            "North_Sea", "SE_Australia")
regions_regional <- c("baltic.sea", "cook.strait", "east.bass.strait", "humboldt", "med.glob",
                      "north.sea", "se.australia")
region_savename <- c("Baltic Sea", "Cook Strait", "East Bass Strait", "Humboldt", "Mediterranean",
                     "North Sea", "SE Australia")

esms_global_mod <- c("GFDL-ESM2M", "IPSL-CM5A-LR", "GDFL_Reanalysis")
esms_regional_mod <- c("gfdl-esm2m", "ipsl-cm5a-lr", "gfdl-reanalysis")

region_startyear <- c(1975, 1975, 1994, 1992, 1975, 1991, 1980)
region_endyear <- c(2100,2100,2100,2100,2100,2100,2050)

fishing_mods <- c("fishing", "no-fishing")

clim_scens_mems <- c("hist", "rcp8p5")
clim_scens_esms <- c("hist", "rcp85")

dat <- read_rds("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5_RegionalCompare/Global_Model_Figures/regional_v_global_data_wZooMSS.RDS")



## Regional model files total consumer biomass
gfdl_regional_nofishing_hist <- read.csv(list.files(path = "./All_Regional_Models/gfdl-esm2m/no-fishing/", pattern = "hist", full.names = TRUE))
gfdl_regional_fishing_hist <- read.csv(list.files(path = "./All_Regional_Models/gfdl-esm2m/fishing/", pattern = "hist", full.names = TRUE)[2])
gfdl_regional_nofishing_rcp85 <- read.csv(list.files(path = "./All_Regional_Models/gfdl-esm2m/no-fishing/", pattern = "rcp8p5", full.names = TRUE))
gfdl_regional_fishing_rcp85 <- read.csv(list.files(path = "./All_Regional_Models/gfdl-esm2m/fishing/", pattern = "rcp8p5", full.names = TRUE)[2])

gfdl_regional_nofishing <- abind(gfdl_regional_nofishing_hist, gfdl_regional_nofishing_rcp85, along = 1)
gfdl_regional_fishing <- abind(gfdl_regional_fishing_hist, gfdl_regional_fishing_rcp85, along = 1)

ipsl_regional_nofishing_hist <- read.csv(list.files(path = "./All_Regional_Models/ipsl-cm5a-lr/no-fishing/", pattern = "hist", full.names = TRUE))
ipsl_regional_fishing_hist <- read.csv(list.files(path = "./All_Regional_Models/ipsl-cm5a-lr/fishing/", pattern = "hist", full.names = TRUE)[2])
ipsl_regional_nofishing_rcp85 <- read.csv(list.files(path = "./All_Regional_Models/ipsl-cm5a-lr/no-fishing/", pattern = "rcp8p5", full.names = TRUE))
ipsl_regional_fishing_rcp85 <- read.csv(list.files(path = "./All_Regional_Models/ipsl-cm5a-lr/fishing/", pattern = "rcp8p5", full.names = TRUE)[2])

ipsl_regional_nofishing <- abind(ipsl_regional_nofishing_hist, ipsl_regional_nofishing_rcp85, along = 1)
ipsl_regional_fishing <- abind(ipsl_regional_fishing_hist, ipsl_regional_fishing_rcp85, along = 1)

gfdlr_regional_nofishing <- as.data.frame(read.csv(list.files(path = "./All_Regional_Models/gfdl-reanalysis/no-fishing/", pattern = "hist", full.names = TRUE)))
gfdlr_regional_fishing <- as.data.frame(read.csv(list.files(path = "./All_Regional_Models/gfdl-reanalysis/fishing/", pattern = "hist", full.names = TRUE)[2]))






## Plot vars, plot GFDL and IPSL together, combining hist + rcp85. Plot GFDL-reanalysis seperately
for(i in 1:length(regions_global)){
  curr_region <- regions_global[i]

  ################################################
  #### ENVIRONMENTAL VARIABLE PLOTS
  ## Import and process environmental variables
  hist_npp <- read.csv(list.files(path = paste("./Regional/Input_Data/MultiModel/",curr_region,'/', sep = ""), pattern = glob2rx("hist*intpp*"), full.names = TRUE))
  hist_npp <- hist_npp[c(which(hist_npp$Date..yyyy. == region_startyear[i]):dim(hist_npp)[1]),]
  rcp85_npp <- read.csv(list.files(path = paste("./Regional/Input_Data/MultiModel/",curr_region,'/', sep = ""), pattern = glob2rx("rcp85*intpp*"), full.names = TRUE))
  rcp85_npp <- rcp85_npp[c(1:which(rcp85_npp$Date..yyyy. == region_endyear[i])),]
  reanalysis_npp <- hist_npp[,c(1,4)]
  reanalysis_npp[,2] <- reanalysis_npp[,2]*(12*60*60*24) # Convert from mol C m-2 s-1 to g C m-2 d-1
  ipsl_gfdl_npp <- abind(hist_npp[,c(1,2,3)], rcp85_npp[,c(1,2,3)], along = 1)
  ipsl_gfdl_npp[,c(2,3)] <- ipsl_gfdl_npp[,c(2,3)]*(12*60*60*24) # Convert from mol C m-2 s-1 to g C m-2 d-1

  hist_temp <- read.csv(list.files(path = paste("./Regional/Input_Data/MultiModel/",curr_region,'/', sep = ""), pattern = glob2rx("hist*sst*"), full.names = TRUE))
  hist_temp <- hist_temp[c(which(hist_temp$Date..yyyy. == region_startyear[i]):dim(hist_temp)[1]),]
  rcp85_temp <- read.csv(list.files(path = paste("./Regional/Input_Data/MultiModel/",curr_region,'/', sep = ""), pattern = glob2rx("rcp85*sst*"), full.names = TRUE))
  rcp85_temp <- rcp85_temp[c(1:which(rcp85_temp$Date..yyyy. == region_endyear[i])),]
  reanalysis_temp <- hist_temp[,c(1,4)]
  ipsl_gfdl_temp <- abind(hist_temp[,c(1,2,3)], rcp85_temp[,c(1,2,3)], along = 1)

  ################################################################
  ## 2X2 plot of variables for current region, for GFDL-Reanalysis
  rel_rean_npp <- reanalysis_npp
  rel_rean_npp[,2] <- rel_rean_npp[,2]/mean(rel_rean_npp[1:10,2])
  rel_rean_temp <- reanalysis_temp
  rel_rean_temp[,2] <- rel_rean_temp[,2]/mean(rel_rean_temp[1:10,2])

  ## Abs plot of intpp
  intpp_abs <- ggplot() + geom_line(data = reanalysis_npp, aes(x=Date..yyyy., y = GFDL_Reanalysis), col = "green", size = 0.8) + theme_bw()+
    xlab("Date (year)") + ylab(expression(paste("intpp (g C m"^-2, " d"^-1,")", sep = "")))+
    labs(subtitle = "Integrated Primary Production")+
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          title = element_text(size = 14))+
    ylim(c(min(reanalysis_npp$GFDL_Reanalysis, na.rm = TRUE)*.9, max(reanalysis_npp$GFDL_Reanalysis, na.rm = TRUE)*1.1))


  ## Abs plot of temp
  temp_abs <- ggplot() + geom_line(data = reanalysis_temp, aes(x=Date..yyyy., y = GFDL_Reanalysis), col = "red", size = 0.8) + theme_bw()+
    xlab("Date (year)") + ylab("Temperature, Celsius")+
    labs(subtitle = "Temperature")+
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          title = element_text(size = 14))+
    ylim(c(min(reanalysis_temp$GFDL_Reanalysis, na.rm = TRUE)*.9, max(reanalysis_temp$GFDL_Reanalysis, na.rm = TRUE)*1.1))

  ## Rel plot of intpp
  intpp_rel <- ggplot() + geom_line(data = rel_rean_npp, aes(x=Date..yyyy., y = GFDL_Reanalysis), col = "green", size = 0.8) + theme_bw()+
    xlab("Date (year)") + ylab(expression(paste(Delta, " intpp", sep = "")))+geom_hline(yintercept =1, col = "black", size = 0.5)+
    labs(subtitle = "Rel. Integrated Primary Production")+
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          title = element_text(size = 14))+
    ylim(c(min(rel_rean_npp$GFDL_Reanalysis, na.rm = TRUE)*.9, max(rel_rean_npp$GFDL_Reanalysis, na.rm = TRUE)*1.1))

  ## Rel plot of temp
  temp_rel <- ggplot() + geom_line(data = rel_rean_temp, aes(x=Date..yyyy., y = GFDL_Reanalysis), col = "red", size = 0.8) + theme_bw()+
    xlab("Date (year)") + ylab(expression(paste(Delta, " Temperature", sep = "")))+geom_hline(yintercept =1, col = "black", size = 0.5)+
    labs(subtitle = "Rel. Temperature")+
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          title = element_text(size = 14))+
    ylim(c(min(rel_rean_temp$GFDL_Reanalysis, na.rm = TRUE)*.9, max(rel_rean_temp$GFDL_Reanalysis, na.rm = TRUE)*1.1))


  ggsave(filename = paste("./Figures/", curr_region, "_env_vars_GFDL-Reanalysis.png", sep =""), plot = ggarrange(plots = list(intpp_abs, temp_abs, intpp_rel, temp_rel), nrow =2, top = paste(region_savename[i], " GFDL-Reanalysis")), width = 9, height = 7)


  ################################################################
  ## 2X2 plot of variables for current region, for GFDL-ESM2M and IPSL-CM5A-LR
  ipsl_gfdl_npp <- as.data.frame(ipsl_gfdl_npp)
  ipsl_gfdl_npp$Mean <- rowMeans(ipsl_gfdl_npp[,c(2,3)])
  rel_ig_npp <- ipsl_gfdl_npp
  rel_ig_npp[,c(2)] <- ipsl_gfdl_npp[,c(2)]/mean(ipsl_gfdl_npp[1:10,c(2)])
  rel_ig_npp[,c(3)] <- ipsl_gfdl_npp[,c(3)]/mean(ipsl_gfdl_npp[1:10,c(3)])
  rel_ig_npp[,c(4)] <- ipsl_gfdl_npp[,c(4)]/mean(ipsl_gfdl_npp[1:10,c(4)])

  rel_ig_npp <- melt(rel_ig_npp, measure.vars  = colnames(rel_ig_npp[,c(2:4)]))
  colnames(rel_ig_npp) = c("Date..yyyy.", "ESM", "Rel_Intpp")

  ipsl_gfdl_npp <- melt(ipsl_gfdl_npp, id.vars = "Date..yyyy.", measure_vars = colnames(ipsl_gfdl_npp[,c(2:4)]))
  colnames(ipsl_gfdl_npp) = c("Date..yyyy.", "ESM", "Intpp")


  ipsl_gfdl_temp <- as.data.frame(ipsl_gfdl_temp)
  ipsl_gfdl_temp$Mean <- rowMeans(ipsl_gfdl_temp[,c(2,3)])
  rel_ig_temp <- ipsl_gfdl_temp
  rel_ig_temp[,c(2)] <- ipsl_gfdl_temp[,c(2)]/mean(ipsl_gfdl_temp[1:10,c(2)])
  rel_ig_temp[,c(3)] <- ipsl_gfdl_temp[,c(3)]/mean(ipsl_gfdl_temp[1:10,c(3)])
  rel_ig_temp[,c(4)] <- ipsl_gfdl_temp[,c(4)]/mean(ipsl_gfdl_temp[1:10,c(4)])
  rel_ig_temp <- melt(rel_ig_temp, measure.vars  = colnames(rel_ig_temp[1:10,c(2:4)]))
  colnames(rel_ig_temp) = c("Date..yyyy.", "ESM", "Rel_Temp")

  ipsl_gfdl_temp <- melt(ipsl_gfdl_temp, id.vars = "Date..yyyy.", measure_vars = colnames(ipsl_gfdl_temp[,c(2:4)]))
  colnames(ipsl_gfdl_temp) = c("Date..yyyy.", "ESM", "Temp")

  ## Abs plot of intpp
  intpp_abs <- ggplot() + geom_line(data = ipsl_gfdl_npp, aes(x=Date..yyyy., y = Intpp, col = ESM), size = 0.8) + theme_bw()+
    xlab("Date (year)") + ylab(expression(paste("intpp (g C m"^-2, " d"^-1,")", sep = "")))+
    labs(subtitle = "Integrated Primary Production")+
    scale_color_manual(values=c('red', "blue", "black"))+
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          title = element_text(size = 14),
          legend.position = "none")+
    ylim(c(min(ipsl_gfdl_npp$Intpp, na.rm = TRUE)*.9, max(ipsl_gfdl_npp$Intpp, na.rm = TRUE)*1.1))

  ## Abs plot of temp
  temp_abs <- ggplot() + geom_line(data = ipsl_gfdl_temp, aes(x=Date..yyyy., y = Temp, col = ESM), size = 0.8) + theme_bw()+
    xlab("Date (year)") + ylab("Temperature, Celsius")+
    scale_color_manual(values=c('red', "blue", "black"))+
    labs(subtitle = "Temperature")+
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          title = element_text(size = 14),
          legend.position = "none")+
    ylim(c(min(ipsl_gfdl_temp$Temp, na.rm = TRUE)*.9, max(ipsl_gfdl_temp$Temp, na.rm = TRUE)*1.1))

  ## Rel plot of intpp
  intpp_rel <- ggplot() + geom_line(data = rel_ig_npp, aes(x=Date..yyyy., y = Rel_Intpp, col = ESM), size = 0.8) + theme_bw()+
    xlab("Date (year)") + ylab(expression(paste(Delta, "intpp", sep = "")))+geom_hline(yintercept =1, col = "black", size = 0.5)+
    scale_color_manual(values=c('red', "blue", "black"))+
    labs(subtitle = "Rel. Integrated Primary Production")+
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          title = element_text(size = 14),
          legend.position = "none")+
    ylim(c(min(rel_ig_npp$Rel_Intpp, na.rm = TRUE)*.9, max(rel_ig_npp$Rel_Intpp, na.rm = TRUE)*1.1))

  ## Rel plot of temp
  temp_rel <- ggplot() + geom_line(data = rel_ig_temp, aes(x=Date..yyyy., y = Rel_Temp, col = ESM), size = 0.8) + theme_bw()+
    xlab("Date (year)") + ylab(expression(paste(Delta, "Temperature", sep = "")))+geom_hline(yintercept =1, col = "black", size = 0.5)+
    scale_color_manual(values=c('red', "blue", "black"))+
    labs(subtitle = "Rel. Temperature")+
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          title = element_text(size = 14))+
    ylim(c(min(rel_ig_temp$Rel_Temp, na.rm = TRUE)*.9, max(rel_ig_temp$Rel_Temp, na.rm = TRUE)*1.1))


  ggsave(filename = paste("./Figures/", curr_region, "_env_vars_ipsl_gfdl.png", sep =""), plot = ggarrange(plots = list(intpp_abs, temp_abs, intpp_rel, temp_rel), nrow =2, top = paste(region_savename[i], " GFDL-ESM2M & IPSL-CM6A-LR")), width = 9, height = 7)


  ############################################################
  #### REGIONAL MODEL TCB PLOTS
  curr_regional_gfdl_fishing <- as.data.frame(gfdl_regional_fishing[,c('Date..yyyy.', regions_regional[i])])
  curr_regional_gfdl_nofishing <- as.data.frame(gfdl_regional_nofishing[,c('Date..yyyy.', regions_regional[i])])
  curr_regional_gfdl_fishing <- curr_regional_gfdl_fishing[c(which(curr_regional_gfdl_fishing$Date..yyyy. == region_startyear[i]):dim(curr_regional_gfdl_fishing)[1]),]
  curr_regional_gfdl_nofishing <- curr_regional_gfdl_nofishing[c(which(curr_regional_gfdl_nofishing$Date..yyyy. == region_startyear[i]):dim(curr_regional_gfdl_nofishing)[1]),]
  curr_regional_gfdl_fishing <- curr_regional_gfdl_fishing[c(1:which(curr_regional_gfdl_fishing$Date..yyyy. == region_endyear[i])),]
  curr_regional_gfdl_nofishing <- curr_regional_gfdl_nofishing[c(1:which(curr_regional_gfdl_nofishing$Date..yyyy. == region_endyear[i])),]

  curr_regional_ipsl_fishing <- as.data.frame(ipsl_regional_fishing[,c('Date..yyyy.', regions_regional[i])])
  curr_regional_ipsl_nofishing <- as.data.frame(ipsl_regional_nofishing[,c('Date..yyyy.', regions_regional[i])])
  curr_regional_ipsl_fishing <- curr_regional_ipsl_fishing[c(which(curr_regional_ipsl_fishing$Date..yyyy. == region_startyear[i]):dim(curr_regional_ipsl_fishing)[1]),]
  curr_regional_ipsl_nofishing <- curr_regional_ipsl_nofishing[c(which(curr_regional_ipsl_nofishing$Date..yyyy. == region_startyear[i]):dim(curr_regional_ipsl_nofishing)[1]),]
  curr_regional_ipsl_fishing <- curr_regional_ipsl_fishing[c(1:which(curr_regional_ipsl_fishing$Date..yyyy. == region_endyear[i])),]
  curr_regional_ipsl_nofishing <- curr_regional_ipsl_nofishing[c(1:which(curr_regional_ipsl_nofishing$Date..yyyy. == region_endyear[i])),]

  curr_regional_gfdlr_fishing <- as.data.frame(gfdlr_regional_fishing[,c('Date..yyyy.', regions_regional[i])])
  curr_regional_gfdlr_nofishing <- as.data.frame(gfdlr_regional_nofishing[,c('Date..yyyy.', regions_regional[i])])
  curr_regional_gr_nofishing <- curr_regional_gfdlr_nofishing[c(which(curr_regional_gfdlr_nofishing$Date..yyyy. == region_startyear[i]):dim(curr_regional_gfdlr_nofishing)[1]),]
  curr_regional_gr_fishing <- curr_regional_gfdlr_fishing[c(which(curr_regional_gfdlr_fishing$Date..yyyy. == region_startyear[i]):dim(curr_regional_gfdlr_fishing)[1]),]

  #curr_regional_ig_nofishing <- as.data.frame(cbind(curr_regional_gfdl_nofishing, curr_regional_ipsl_nofishing[,2]))
  #curr_regional_ig_fishing <-  as.data.frame(cbind(curr_regional_gfdl_fishing, curr_regional_ipsl_fishing[,2]))
  #colnames(curr_regional_ig_nofishing) <- c("Date", "GFDL-ESM2M", "IPSL-CM5A-LR")
  #colnames(curr_regional_ig_fishing) <- c("Date", "GFDL-ESM2M", "IPSL-CM5A-LR")

  plot_list = list()
  # GFDL-ESM2M no-fishing
  plot_list[[1]] = rel_line(curr_regional_gfdl_nofishing, plot_legend = NA, name = "GFDL-ESM2M", standin = "ttt", subtitlee = "No Fishing", lcols = "black", set_cols = NA, xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))

  # GFDL-ESM2M fishing
  plot_list[[2]] = rel_line(curr_regional_gfdl_fishing, plot_legend = NA, name = NA, standin = "ttt", subtitlee = "Fishing", lcols = "black", set_cols = NA,xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))

  # IPSL-CM6A-LR no-fishing
  plot_list[[3]] = rel_line(curr_regional_ipsl_nofishing, plot_legend = NA, name = "IPSL-CM5A-LR", standin = "ttt", subtitlee = "No Fishing", lcols = "black", set_cols = NA,xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))

  # IPSL-CM6A-LR fishing
  plot_list[[4]] = rel_line(curr_regional_ipsl_fishing, plot_legend = NA, name = NA, standin = "ttt", subtitlee = "Fishing", lcols = "black", set_cols = NA,xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))

  ggsave(filename = paste("./Figures/tcb_", curr_region, "_regionalmodels_gfdl-ipsl.png", sep =""), plot = ggarrange(plots = plot_list[c(1:4)], nrow =2, top = region_savename[i], byrow = FALSE), width = 9, height = 7)

  # GFDL-Reanalysis fishing
  plot_list[[5]] = rel_line(curr_regional_gr_nofishing, plot_legend = NA, name = "GFDL-Reanalysis", standin = "ttt", subtitlee = "No Fishing", lcols = "black", set_cols = NA,xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))

  # GFDL-Reanalysis no-fishing
  plot_list[[6]] = rel_line(curr_regional_gr_fishing, plot_legend = NA, name = "GFDL-Reanlysis", standin = "ttt", subtitlee = "Fishing", lcols = "black", set_cols = NA,xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))

  ggsave(filename = paste("./Figures/tcb_", curr_region, "_regionalmodels_gfdlr.png", sep =""), plot = ggarrange(plots = plot_list[c(5,6)], nrow =2, top = region_savename[i], font.label()), width = 5, height = 7)

  ############################################################
  #### GLOBAL MODEL TCB PLOTS
  ipsl_global_nofishing_hist <- as.data.frame(read.csv(list.files(path = paste("./Regional/Output_Data/MultiModel/no-fishing/", regions_global[i], '/',sep = ""), pattern = "hist_ipsl", full.names = TRUE)))
  ipsl_global_nofishing_rcp85 <- as.data.frame(read.csv(list.files(path = paste("./Regional/Output_Data/MultiModel/no-fishing/", regions_global[i], '/',sep = ""), pattern = "rcp8p5_ipsl", full.names = TRUE)))
  ipsl_global_nofishing <- rbind(ipsl_global_nofishing_hist, ipsl_global_nofishing_rcp85)
  ipsl_global_nofishing[c((dim(ipsl_global_nofishing)[1]-5):dim(ipsl_global_nofishing)[1]),-1] <- NA
  ipsl_global_nofishing <- ipsl_global_nofishing[c(which(ipsl_global_nofishing$Date..yyyy. == region_startyear[i]):dim(ipsl_global_nofishing)[1]),]
  ipsl_global_nofishing <- ipsl_global_nofishing[c(1:which(ipsl_global_nofishing$Date..yyyy. == region_endyear[i])),]

  ipsl_global_fishing_hist <- as.data.frame(read.csv(list.files(path = paste("./Regional/Output_Data/MultiModel/fishing/", regions_global[i], '/',sep = ""), pattern = "hist_ipsl", full.names = TRUE)))
  ipsl_global_fishing_rcp85 <- as.data.frame(read.csv(list.files(path = paste("./Regional/Output_Data/MultiModel/fishing/", regions_global[i], '/',sep = ""), pattern = "rcp8p5_ipsl", full.names = TRUE)))
  ipsl_global_fishing <- rbind(ipsl_global_fishing_hist, ipsl_global_fishing_rcp85)
  ipsl_global_fishing[c((dim(ipsl_global_fishing)[1]-5):dim(ipsl_global_fishing)[1]),-1] <- NA
  ipsl_global_fishing <- ipsl_global_fishing[c(which(ipsl_global_fishing$Date..yyyy. == region_startyear[i]):dim(ipsl_global_fishing)[1]),]
  ipsl_global_fishing <- ipsl_global_fishing[c(1:which(ipsl_global_fishing$Date..yyyy. == region_endyear[i])),]

  gfdl_global_nofishing_hist <- as.data.frame(read.csv(list.files(path = paste("./Regional/Output_Data/MultiModel/no-fishing/", regions_global[i], '/',sep = ""), pattern = "hist_gfdl-esm2m", full.names = TRUE)))
  gfdl_global_nofishing_rcp85 <- as.data.frame(read.csv(list.files(path = paste("./Regional/Output_Data/MultiModel/no-fishing/", regions_global[i], '/',sep = ""), pattern = "rcp8p5_gfdl-esm2m", full.names = TRUE)))
  gfdl_global_nofishing <- rbind(gfdl_global_nofishing_hist, gfdl_global_nofishing_rcp85)
  gfdl_global_nofishing[c((dim(gfdl_global_nofishing)[1]-5):dim(gfdl_global_nofishing)[1]),-1] <- NA
  gfdl_global_nofishing <- gfdl_global_nofishing[c(which(gfdl_global_nofishing$Date..yyyy. == region_startyear[i]):dim(gfdl_global_nofishing)[1]),]
  gfdl_global_nofishing <- gfdl_global_nofishing[c(1:which(gfdl_global_nofishing$Date..yyyy. == region_endyear[i])),]

  gfdl_global_fishing_hist <- as.data.frame(read.csv(list.files(path = paste("./Regional/Output_Data/MultiModel/fishing/", regions_global[i], '/',sep = ""), pattern = "hist_gfdl-esm2m", full.names = TRUE)))
  gfdl_global_fishing_rcp85 <- as.data.frame(read.csv(list.files(path = paste("./Regional/Output_Data/MultiModel/fishing/", regions_global[i], '/',sep = ""), pattern = "rcp8p5_gfdl-esm2m", full.names = TRUE)))
  gfdl_global_fishing <- rbind(gfdl_global_fishing_hist, gfdl_global_fishing_rcp85)
  gfdl_global_fishing[c((dim(gfdl_global_fishing)[1]-5):dim(gfdl_global_fishing)[1]),-1] <- NA
  gfdl_global_fishing <- gfdl_global_fishing[c(which(gfdl_global_fishing$Date..yyyy. == region_startyear[i]):dim(gfdl_global_fishing)[1]),]
  gfdl_global_fishing <- gfdl_global_fishing[c(1:which(gfdl_global_fishing$Date..yyyy. == region_endyear[i])),]

  gfdlr_global_nofishing <-as.data.frame(read.csv(list.files(path = paste("./Regional/Output_Data/MultiModel/no-fishing/", regions_global[i], '/',sep = ""), pattern = "hist_gfdl-reanalysis", full.names = TRUE)))
  gfdlr_global_nofishing <- gfdlr_global_nofishing[c(which(gfdlr_global_nofishing$Date..yyyy. == region_startyear[i]):dim(gfdlr_global_nofishing)[1]),]
  gfdlr_global_nofishing <- gfdlr_global_nofishing[-dim(gfdlr_global_nofishing)[1],]

  gfdlr_global_fishing <- as.data.frame(read.csv(list.files(path = paste("./Regional/Output_Data/MultiModel/fishing/", regions_global[i], '/',sep = ""), pattern = "hist_gfdl-reanalysis", full.names = TRUE)))
  gfdlr_global_fishing <- gfdlr_global_fishing[c(which(gfdlr_global_fishing$Date..yyyy. == region_startyear[i]):dim(gfdlr_global_fishing)[1]),]
  gfdlr_global_fishing <- gfdlr_global_fishing[-dim(gfdlr_global_fishing)[1],]

  ### GFDL-ESM2M No Fishing
  model_colors <- c("boats" = "green", "apecosm" = "red", "ecoocean" = "blue", "dbem" = "cyan", "dbpm" = "purple", "macroecological" = "orange", "Mean" = "black")
  plot_list = list()
  plot_list[[1]] = rel_line(gfdl_global_nofishing, plot_legend = NA, name = "GFDL-ESM2M", standin = "ttt", subtitlee = "No Fishing", lcols = rainbow(c(dim(gfdl_global_nofishing)[2])), set_cols = model_colors, xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))

  ### GFDL-ESM2M Fishing
  plot_list[[2]] = rel_line(gfdl_global_fishing, plot_legend = NA, name = NA, standin = "ttt", subtitlee = "Fishing", lcols = rainbow(c(dim(gfdl_global_nofishing)[2])), set_cols = model_colors,xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))

  ### IPSL-CM5A-LR No Fishing
  plot_list[[3]] = rel_line(ipsl_global_nofishing, plot_legend = NA, name = "IPSL-CM5A", standin = "ttt", subtitlee = "No Fishing", lcols = rainbow(c(dim(ipsl_global_nofishing)[2])), set_cols = model_colors, xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))

  ### IPSL-CM5A-LR Fishing
  plot_list[[4]] = rel_line(ipsl_global_fishing, plot_legend = "Model", name = NA, standin = "ttt", subtitlee = "Fishing", lcols = rainbow(c(dim(ipsl_global_nofishing)[2])), set_cols = model_colors, xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))

  ggsave(filename = paste("./Figures/tcb_", curr_region, "_globalmodels_gfdl-ipsl.png", sep =""), plot = ggarrange(plots = plot_list[c(1:4)], nrow =2, top = region_savename[i], byrow = FALSE, common.legend = TRUE), width = 9, height = 7)

  model_colors <- c("boats" = "green",  "ecoocean" = "blue",  "macroecological" = "orange", "Mean" = "black")
  ### GFDL-Reanalysis No Fishing
  plot_list[[5]] = rel_line(gfdlr_global_nofishing, plot_legend = NA, name = "GFDL-Reanalysis", standin = "ttt", subtitlee = "No Fishing", lcols = rainbow(c(dim(ipsl_global_nofishing)[2])), set_cols = model_colors, xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))

  ### GFDL-Reanalysis Fishing
  plot_list[[6]] = rel_line(gfdlr_global_fishing, plot_legend = "Model", name = NA, standin = "ttt", subtitlee = "Fishing", lcols = rainbow(c(dim(ipsl_global_nofishing)[2])), set_cols = model_colors, xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))

  ggsave(filename = paste("./Figures/tcb_", curr_region, "_globalmodels_gfdlreanalysis.png", sep =""), plot = ggarrange(plots = plot_list[c(5:6)], nrow =2, top = region_savename[i], byrow = FALSE, common.legend = TRUE), width = 5, height = 7)



  ##############################
  ## Combined plot of global and regional model tcb
  plot_list = list()
  colors <- c("Global_Model_Mean" = "red", "95%_CI_Global_Models" = "cornflowerblue", "Regional_Model_Mean" = "green")
  ### GFDL-ESM2M and IPSL-CM5A-LR
  gfdl_nofishing_rel <- sweep(gfdl_global_nofishing[,-1], 2, colMeans(gfdl_global_nofishing[c(1:10),-1], na.rm = TRUE), '/')
  gfdl_nofishing_combined <- data.frame('Date' = gfdl_global_nofishing$Date..yyyy., 'Global_Model_Mean' = rowMeans(gfdl_nofishing_rel, na.rm = TRUE),
                                        'Lower_95CI_Global' = rowMeans(gfdl_nofishing_rel, na.rm = TRUE) - apply(gfdl_nofishing_rel, 1, sd, na.rm = TRUE),
                                        'Upper_95CI_Global' = rowMeans(gfdl_nofishing_rel, na.rm = TRUE) + apply(gfdl_nofishing_rel, 1, sd, na.rm = TRUE),
                                        "Regional_Model_Mean" = curr_regional_gfdl_nofishing[,2]/mean(curr_regional_gfdl_nofishing[1:10,2], na.rm = TRUE))

  gfdl_fishing_rel <- sweep(gfdl_global_fishing[,-1], 2, colMeans(gfdl_global_fishing[c(1:10),-1], na.rm = TRUE), '/')
  gfdl_fishing_combined <- data.frame('Date' = gfdl_global_fishing$Date..yyyy., 'Global_Model_Mean' = rowMeans(gfdl_fishing_rel, na.rm = TRUE),
                                      'Lower_95CI_Global' = rowMeans(gfdl_fishing_rel, na.rm = TRUE) - apply(gfdl_fishing_rel, 1, sd, na.rm = TRUE),
                                      'Upper_95CI_Global' = rowMeans(gfdl_fishing_rel, na.rm = TRUE) + apply(gfdl_fishing_rel, 1, sd, na.rm = TRUE),
                                      "Regional_Model_Mean" = curr_regional_gfdl_fishing[,2]/mean(curr_regional_gfdl_fishing[1:10,2], na.rm = TRUE))

  ipsl_nofishing_rel <- sweep(ipsl_global_nofishing[,-1], 2, colMeans(ipsl_global_nofishing[c(1:10),-1], na.rm = TRUE), '/')
  ipsl_nofishing_combined <- data.frame('Date' = ipsl_global_nofishing$Date..yyyy., 'Global_Model_Mean' = rowMeans(ipsl_nofishing_rel, na.rm = TRUE),
                                        'Lower_95CI_Global' = rowMeans(ipsl_nofishing_rel, na.rm = TRUE) -apply(ipsl_nofishing_rel, 1, sd, na.rm = TRUE),
                                        'Upper_95CI_Global' = rowMeans(ipsl_nofishing_rel, na.rm = TRUE) + apply(ipsl_nofishing_rel, 1, sd, na.rm = TRUE),
                                        "Regional_Model_Mean" = curr_regional_ipsl_nofishing[,2]/mean(curr_regional_ipsl_nofishing[1:10,2], na.rm = TRUE))

  ipsl_fishing_rel <- sweep(ipsl_global_fishing[,-1], 2, colMeans(ipsl_global_fishing[c(1:10),-1], na.rm = TRUE), '/')
  ipsl_fishing_combined <- data.frame('Date' = ipsl_global_fishing$Date..yyyy., 'Global_Model_Mean' = rowMeans(ipsl_fishing_rel, na.rm = TRUE),
                                        'Lower_95CI_Global' = rowMeans(ipsl_fishing_rel, na.rm = TRUE) - apply(ipsl_fishing_rel, 1, sd, na.rm = TRUE),
                                        'Upper_95CI_Global' = rowMeans(ipsl_fishing_rel, na.rm = TRUE) + apply(ipsl_fishing_rel, 1, sd, na.rm = TRUE),
                                        "Regional_Model_Mean" = curr_regional_ipsl_fishing[,2]/mean(curr_regional_ipsl_fishing[1:10,2], na.rm = TRUE))


  gfdlr_nofishing_rel <- sweep(gfdlr_global_nofishing[,-1], 2, colMeans(gfdlr_global_nofishing[c(1:10),-1], na.rm = TRUE), '/')
  gfdlr_nofishing_combined <- data.frame('Date' = gfdlr_global_nofishing$Date..yyyy., 'Global_Model_Mean' = rowMeans(gfdlr_nofishing_rel, na.rm = TRUE),
                                         'Lower_95CI_Global' = rowMeans(gfdlr_nofishing_rel, na.rm = TRUE) - apply(gfdlr_nofishing_rel, 1, sd, na.rm = TRUE),
                                         'Upper_95CI_Global' = rowMeans(gfdlr_nofishing_rel, na.rm = TRUE) + apply(gfdlr_nofishing_rel, 1, sd, na.rm = TRUE),
                                         "Regional_Model_Mean" = curr_regional_gr_nofishing[-dim(curr_regional_gr_nofishing)[1],2]/mean(curr_regional_gr_nofishing[1:10,2], na.rm = TRUE))

  gfdlr_fishing_rel <- sweep(gfdlr_global_fishing[,-1], 2, colMeans(gfdlr_global_fishing[c(1:10),-1], na.rm = TRUE), '/')
  gfdlr_fishing_combined <- data.frame('Date' = gfdlr_global_fishing$Date..yyyy., 'Global_Model_Mean' = rowMeans(gfdlr_fishing_rel, na.rm = TRUE),
                                       'Lower_95CI_Global' = rowMeans(gfdlr_fishing_rel, na.rm = TRUE) - apply(gfdlr_fishing_rel, 1, sd, na.rm = TRUE),
                                       'Upper_95CI_Global' = rowMeans(gfdlr_fishing_rel, na.rm = TRUE) + apply(gfdlr_fishing_rel, 1, sd, na.rm = TRUE),
                                       "Regional_Model_Mean" = curr_regional_gr_fishing[-dim(curr_regional_gr_nofishing)[1],2]/mean(curr_regional_gr_fishing[1:10,2], na.rm = TRUE))


  # GFDL no fishing
  plot_list[[1]] <-  ggplot() + geom_ribbon(data = gfdl_nofishing_combined, aes(x = Date, ymin = Lower_95CI_Global, ymax = Upper_95CI_Global, fill = "95%_CI_Global_Models"))+
    geom_line(data = gfdl_nofishing_combined, aes(x = Date, y = Global_Model_Mean, colour = "Global_Model_Mean"), size = 0.7)+
    geom_line(data = gfdl_nofishing_combined, aes(x = Date, y = Regional_Model_Mean, colour = "Regional_Model_Mean"), size  = 0.7)+
    geom_hline(yintercept = 1, size = 0.5, col = "black")+theme_bw()+ scale_colour_manual(name = "", labels = c("Global Model Mean", "Regional Model"), values = colors)+
    scale_fill_manual(name = "", labels = "Global Model SD", values = colors)+
    theme(plot.subtitle = element_text(size = 14, color = "black"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = unit(c(0.2,0,0,0), unit = "cm")),
          legend.position = "none")+
    xlab('Year') + ylab(expression(paste(Delta, " Consumer Biomass", sep = ""))) + labs(subtitle = "No Fishing")  +
    ggtitle('GFDL-ESM2M')

  #GFDL fishing
  plot_list[[2]] <-  ggplot() + geom_ribbon(data = gfdl_fishing_combined, aes(x = Date, ymin = Lower_95CI_Global, ymax = Upper_95CI_Global, fill = "95%_CI_Global_Models"))+
    geom_line(data = gfdl_fishing_combined, aes(x = Date, y = Global_Model_Mean, colour = "Global_Model_Mean"), size = 0.7)+
    geom_line(data = gfdl_fishing_combined, aes(x = Date, y = Regional_Model_Mean, colour = "Regional_Model_Mean"), size  = 0.7)+
    geom_hline(yintercept = 1, size = 0.5, col = "black")+theme_bw()+ scale_colour_manual(name = "", labels = c("Global Model Mean", "Regional Model"), values = colors)+
    scale_fill_manual(name = "", labels = "Global Model SD", values = colors)+
    theme(plot.subtitle = element_text(size = 14, color = "black"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_blank(),
          legend.position = "none")+
    xlab('Year') + ylab(expression(paste(Delta, " Consumer Biomass", sep = ""))) + labs(subtitle = "Fishing")

  # IPSL no fishing
plot_list[[3]] <-  ggplot() + geom_ribbon(data = ipsl_nofishing_combined, aes(x = Date, ymin = Lower_95CI_Global, ymax = Upper_95CI_Global, fill = "95%_CI_Global_Models"))+
    geom_line(data = ipsl_nofishing_combined, aes(x = Date, y = Global_Model_Mean, colour = "Global_Model_Mean"), size = 0.7)+
    geom_line(data = ipsl_nofishing_combined, aes(x = Date, y = Regional_Model_Mean, colour = "Regional_Model_Mean"), size  = 0.7)+
    geom_hline(yintercept = 1, size = 0.5, col = "black")+theme_bw()+ scale_colour_manual(name = "", labels = c("Global Model Mean", "Regional Model"), values = colors)+
    scale_fill_manual(name = "", labels = "Global Model SD", values = colors)+
     theme(plot.subtitle = element_text(size = 14, color = "black"),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = unit(c(0.2,0,0,0), unit = "cm")),
           legend.position = "none")+
    xlab('Year') + ylab(expression(paste(Delta, " Consumer Biomass", sep = ""))) + labs(subtitle = "No Fishing")  +
    ggtitle('IPSL-CM5A-LR')

# IPSL fishing
plot_list[[4]] <-  ggplot() + geom_ribbon(data = ipsl_fishing_combined, aes(x = Date, ymin = Lower_95CI_Global, ymax = Upper_95CI_Global, fill = "95%_CI_Global_Models"))+
  geom_line(data = ipsl_fishing_combined, aes(x = Date, y = Global_Model_Mean, colour = "Global_Model_Mean"), size = 0.7)+
  geom_line(data = ipsl_fishing_combined, aes(x = Date, y = Regional_Model_Mean, colour = "Regional_Model_Mean"), size  = 0.7)+
  geom_hline(yintercept = 1, size = 0.5, col = "black")+theme_bw()+ scale_colour_manual(name = "", labels = c("Global Model Mean", "Regional Model"), values = colors)+
  scale_fill_manual(name = "", labels = "Global Model SD", values = colors)+
  theme(plot.subtitle = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_blank())+
  xlab('Year') + ylab(expression(paste(Delta, " Consumer Biomass", sep = ""))) + labs(subtitle = "Fishing")


ggsave(filename = paste("./Figures/tcb_", curr_region, "_global_regional_models_gfdl-ipsl.png", sep =""), plot = ggarrange(plots = plot_list[c(1:4)], nrow =2, top = region_savename[i], byrow = FALSE, common.legend = TRUE), width = 9, height = 7)

# GFDL-reanalysis no fishing
plot_list[[5]] <-  ggplot() + geom_ribbon(data = gfdlr_nofishing_combined, aes(x = Date, ymin = Lower_95CI_Global, ymax = Upper_95CI_Global, fill = "95%_CI_Global_Models"))+
  geom_line(data = gfdlr_nofishing_combined, aes(x = Date, y = Global_Model_Mean, colour = "Global_Model_Mean"), size = 0.7)+
  geom_line(data = gfdlr_nofishing_combined, aes(x = Date, y = Regional_Model_Mean, colour = "Regional_Model_Mean"), size  = 0.7)+
  geom_hline(yintercept = 1, size = 0.5, col = "black")+theme_bw()+ scale_colour_manual(name = "", labels = c("Global Model Mean", "Regional Model"), values = colors)+
  scale_fill_manual(name = "", labels = "Global Model SD", values = colors)+
  theme(plot.subtitle = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = unit(c(0.2,0,0,0), unit = "cm")),
        legend.position = "none")+
  xlab('Year') + ylab(expression(paste(Delta, " Consumer Biomass", sep = ""))) + labs(subtitle = "No Fishing")  +
  ggtitle('GFDL-Reanalysis')

#GFDL-reanalysis fishing
plot_list[[6]] <-  ggplot() + geom_ribbon(data = gfdlr_fishing_combined, aes(x = Date, ymin = Lower_95CI_Global, ymax = Upper_95CI_Global, fill = "95%_CI_Global_Models"))+
  geom_line(data = gfdlr_fishing_combined, aes(x = Date, y = Global_Model_Mean, colour = "Global_Model_Mean"), size = 0.7)+
  geom_line(data = gfdlr_fishing_combined, aes(x = Date, y = Regional_Model_Mean, colour = "Regional_Model_Mean"), size  = 0.7)+
  geom_hline(yintercept = 1, size = 0.5, col = "black")+theme_bw()+ scale_colour_manual(name = "", labels = c("Global Model Mean", "Regional Model"), values = colors)+
  scale_fill_manual(name = "", labels = "Global Model SD", values = colors)+
  theme(plot.subtitle = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_blank())+
  xlab('Year') + ylab(expression(paste(Delta, " Consumer Biomass", sep = ""))) + labs(subtitle = "Fishing")

ggsave(filename = paste("./Figures/tcb_", curr_region, "_global_regional_models_gfdlreanalysis.png", sep =""), plot = ggarrange(plots = plot_list[c(5:6)], nrow =2, top = region_savename[i], byrow = FALSE, common.legend = TRUE), width = 5, height = 7)
  }


### KEY RUNS
model_colors <- c("key.run" = "green", "ipsl" = "red", "gfdl" = "blue", "gfdlr" = "cyan", "Mean" = NA)
plot_list1 = plot_list2 = list()

regional_runs <- read.csv("./All_Regional_Models/key-runs/all regions sorted and cleaned with GCM-scenario2_with-key-runs 1970-2100 July 18 2019.csv")
key_runs <- regional_runs[regional_runs$GCM_scenario == "key-run",]

cs_key_tcb_fishing <- key_runs[which(key_runs$region == "cook-strait" & key_runs$exploitation == "fishing" & key_runs$variable == "tcb"),]
cs_key_tcb_fishing <- cs_key_tcb_fishing[which(cs_key_tcb_fishing$year >= 1975),]
cs_key_tcb_fishing <- cs_key_tcb_fishing[which(cs_key_tcb_fishing$year <= 2005),]
cs_key_tcb_fishing <- cs_key_tcb_fishing[seq(from = 2, to = dim(cs_key_tcb_fishing)[1], by = 2),]

cs_regional_fishing <- data.frame("year" = 1975:2005, "key-run" = cs_key_tcb_fishing$value,
                                    "ipsl" = ipsl_regional_fishing_hist[which(ipsl_regional_fishing_hist[,"Date..yyyy."] == 1975):dim(ipsl_regional_fishing_hist)[1],"cook.strait"],
                                    "gfdl" = gfdl_regional_fishing_hist[which(gfdl_regional_fishing_hist[,"Date..yyyy."] == 1975):dim(gfdl_regional_fishing_hist)[1],"cook.strait"],
                                    "gfdlr" = gfdlr_regional_fishing[which(gfdlr_regional_fishing[,"Date..yyyy."] == 1975):dim(gfdlr_regional_fishing)[1],"cook.strait"])

cs_key_tcb_nofishing <- key_runs[which(key_runs$region == "cook-strait" & key_runs$exploitation == "no-fishing" & key_runs$variable == "tcb"),]
cs_key_tcb_nofishing <- cs_key_tcb_nofishing[which(cs_key_tcb_nofishing$year >= 1975),]
cs_key_tcb_nofishing <- cs_key_tcb_nofishing[which(cs_key_tcb_nofishing$year <= 2005),]

cs_regional_nofishing <- data.frame("year" = 1975:2005, "key-run" = cs_key_tcb_nofishing$value,
                                    "ipsl" = ipsl_regional_nofishing_hist[which(ipsl_regional_nofishing_hist[,"Date..yyyy."] == 1975):dim(ipsl_regional_nofishing_hist)[1],"cook.strait"],
                                    "gfdl" = gfdl_regional_nofishing_hist[which(gfdl_regional_nofishing_hist[,"Date..yyyy."] == 1975):dim(gfdl_regional_nofishing_hist)[1],"cook.strait"],
                                    "gfdlr" = gfdlr_regional_nofishing[which(gfdlr_regional_nofishing[,"Date..yyyy."] == 1975):dim(gfdlr_regional_nofishing)[1],"cook.strait"])


plot_list1[[1]] = rel_line(data = cs_regional_nofishing, plot_legend = NA, name = NA, standin = "ttt", subtitlee = "Cook Strait", lcols = model_colors, set_cols = model_colors, xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))
plot_list2[[1]] = rel_line(data = cs_regional_fishing, plot_legend = NA, name = NA, standin = "ttt", subtitlee = "Cook Strait", lcols = model_colors, set_cols = model_colors, xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))

ebs_key_tcb_fishing <- key_runs[which(key_runs$region == "east-bass-strait" & key_runs$exploitation == "fishing" & key_runs$variable == "tcb"),]
ebs_regional_fishing <- data.frame("year" = 1994:2005, "key-run" = ebs_key_tcb_fishing$value,
                                  "ipsl" = ipsl_regional_fishing_hist[which(ipsl_regional_fishing_hist[,"Date..yyyy."] == 1994):dim(ipsl_regional_fishing_hist)[1],"east.bass.strait"],
                                  "gfdl" = gfdl_regional_fishing_hist[which(gfdl_regional_fishing_hist[,"Date..yyyy."] == 1994):dim(gfdl_regional_fishing_hist)[1],"east.bass.strait"],
                                  "gfdlr" = gfdlr_regional_fishing[which(gfdlr_regional_fishing[,"Date..yyyy."] == 1994):dim(gfdlr_regional_fishing)[1],"east.bass.strait"])


ebs_key_tcb_nofishing <- key_runs[which(key_runs$region == "east-bass-strait" & key_runs$exploitation == "no-fishing" & key_runs$variable == "tcb"),]
ebs_regional_nofishing <- data.frame("year" = 1994:2005, "key-run" = ebs_key_tcb_nofishing$value,
                                   "ipsl" = ipsl_regional_nofishing_hist[which(ipsl_regional_nofishing_hist[,"Date..yyyy."] == 1994):dim(ipsl_regional_nofishing_hist)[1],"east.bass.strait"],
                                   "gfdl" = gfdl_regional_nofishing_hist[which(gfdl_regional_nofishing_hist[,"Date..yyyy."] == 1994):dim(gfdl_regional_nofishing_hist)[1],"east.bass.strait"],
                                   "gfdlr" = gfdlr_regional_nofishing[which(gfdlr_regional_nofishing[,"Date..yyyy."] == 1994):dim(gfdlr_regional_nofishing)[1],"east.bass.strait"])


plot_list1[[2]] = rel_line(data = ebs_regional_nofishing, plot_legend = NA, name = NA, standin = "ttt", subtitlee = "East Bass Strait", lcols = model_colors, set_cols = model_colors, xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))
plot_list2[[2]] = rel_line(data = ebs_regional_fishing, plot_legend = NA, name = NA, standin = "ttt", subtitlee = "East Bass Strait", lcols = model_colors, set_cols = model_colors, xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))

ns_key_tcb_fishing <- key_runs[which(key_runs$region == "north-sea" & key_runs$exploitation == "fishing" & key_runs$variable == "tcb"),]
ns_regional_fishing <- data.frame("year" = 1991:2005, "key-run" = ns_key_tcb_fishing$value,
                                   "ipsl" = ipsl_regional_fishing_hist[which(ipsl_regional_fishing_hist[,"Date..yyyy."] == 1991):dim(ipsl_regional_fishing_hist)[1],"north.sea"],
                                   "gfdl" = gfdl_regional_fishing_hist[which(gfdl_regional_fishing_hist[,"Date..yyyy."] == 1991):dim(gfdl_regional_fishing_hist)[1],"north.sea"],
                                   "gfdlr" = gfdlr_regional_fishing[which(gfdlr_regional_fishing[,"Date..yyyy."] == 1991):dim(gfdlr_regional_fishing)[1],"north.sea"])

ns_key_tcb_nofishing <- key_runs[which(key_runs$region == "north-sea" & key_runs$exploitation == "no-fishing" & key_runs$variable == "tcb"),]
ns_regional_nofishing <- data.frame("year" = 1991:2005, "key-run" = ns_key_tcb_nofishing$value,
                                  "ipsl" = ipsl_regional_nofishing_hist[which(ipsl_regional_nofishing_hist[,"Date..yyyy."] == 1991):dim(ipsl_regional_nofishing_hist)[1],"north.sea"],
                                  "gfdl" = gfdl_regional_nofishing_hist[which(gfdl_regional_nofishing_hist[,"Date..yyyy."] == 1991):dim(gfdl_regional_nofishing_hist)[1],"north.sea"],
                                  "gfdlr" = gfdlr_regional_nofishing[which(gfdlr_regional_nofishing[,"Date..yyyy."] == 1991):dim(gfdlr_regional_nofishing)[1],"north.sea"])

plot_list1[[3]] = rel_line(data = ns_regional_nofishing, plot_legend = NA, name = NA, standin = "ttt", subtitlee = "North Sea", lcols = model_colors, set_cols = model_colors, xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))
plot_list2[[3]] = rel_line(data = ns_regional_fishing, plot_legend = NA, name = NA, standin = "ttt", subtitlee = "North Sea", lcols = model_colors, set_cols = model_colors, xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))


med_key_tcb_fishing <- colMeans(matrix(ncvar_get(nc_open("./All_Regional_Models/key-runs/ewe_key-run_nobc_hist_wo-diaz_fishing_no-oa_tcb_med-glob_monthly_1970_2005.nc4"),"tcb"), nrow = 12))
med_regional_fishing <- data.frame("year" = 1975:2005, "key-run" = med_key_tcb_fishing[-c(1:5)],
                                  "ipsl" = ipsl_regional_fishing_hist[which(ipsl_regional_fishing_hist[,"Date..yyyy."] == 1975):dim(ipsl_regional_fishing_hist)[1],"med.glob"],
                                  "gfdl" = gfdl_regional_fishing_hist[which(gfdl_regional_fishing_hist[,"Date..yyyy."] == 1975):dim(gfdl_regional_fishing_hist)[1],"med.glob"],
                                  "gfdlr" = gfdlr_regional_fishing[which(gfdlr_regional_fishing[,"Date..yyyy."] == 1975):dim(gfdlr_regional_fishing)[1],"med.glob"])

med_key_tcb_nofishing <- colMeans(matrix(ncvar_get(nc_open("./All_Regional_Models/key-runs/ewe_key-run_nobc_hist_wo-diaz_no-fishing_no-oa_tcb_med-glob_monthly_1970_2005.nc4"),"tcb"), nrow = 12))
med_regional_nofishing <- data.frame("year" = 1975:2005, "key-run" = med_key_tcb_nofishing[-c(1:5)],
                                   "ipsl" = ipsl_regional_nofishing_hist[which(ipsl_regional_nofishing_hist[,"Date..yyyy."] == 1975):dim(ipsl_regional_nofishing_hist)[1],"med.glob"],
                                   "gfdl" = gfdl_regional_nofishing_hist[which(gfdl_regional_nofishing_hist[,"Date..yyyy."] == 1975):dim(gfdl_regional_nofishing_hist)[1],"med.glob"],
                                   "gfdlr" = gfdlr_regional_nofishing[which(gfdlr_regional_nofishing[,"Date..yyyy."] == 1975):dim(gfdlr_regional_nofishing)[1],"med.glob"])


plot_list1[[4]] = rel_line(data = med_regional_nofishing, plot_legend = "Model", name = NA, standin = "ttt", subtitlee = "Mediterranean", lcols = model_colors, set_cols = model_colors, xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))
plot_list2[[4]] = rel_line(data = med_regional_fishing, plot_legend = "Model", name = NA, standin = "ttt", subtitlee = "Mediterranean", lcols = model_colors, set_cols = model_colors, xlabb = "Year", ylabb = expression(paste(Delta, " Consumer Biomass", sep = "")))

ggsave(filename = paste("./Figures/tcb_nofishing_regional_models_all.png", sep =""), plot = ggarrange(plots = plot_list1, nrow =2,  byrow = FALSE, top = "All Regional Models tcb No Fishing", common.legend = TRUE), width = 9, height = 7)
ggsave(filename = paste("./Figures/tcb_fishing_regional_models_all.png", sep =""), plot = ggarrange(plots = plot_list2, nrow =2,  byrow = FALSE, top = "All Regional Models tcb Fishing"), width = 9, height = 7)





