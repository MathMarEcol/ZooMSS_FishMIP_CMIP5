
library(tidyverse)

dat <- read_rds("/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5_RegionalCompare/Global_Model_Figures/regional_v_global_data.RDS")

base_dir <- "/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5_RegionalCompare/Output/"

cnt <- 0

scenario <- c("historical", "rcp85")

# Humboldt (1), North Sea(2), Med (3), Baltic (4), SE_Australia (5), East Bass Strait (6), Cook Strait (7).
region_names <- c("Humboldt", "North_Sea", "Mediterranean", "Baltic_Sea", "SE_Australia", "East_Bass_Strait", "Cook_Strait")

models <- c("gfdl-esm2m", "ipsl-cm5a-lr")

for (o in 1:length(scenario)){
  for (n in 1:length(region_names)){
    for (m in 1:length(models)){

      f <- list.files(path = paste0(base_dir,models[m]),
                      pattern = glob2rx(paste0("*", scenario[o], "*", region_names[n], "*")), full.names = TRUE)
      print(f)
      zoo <- read_csv(f) %>%
        filter(`Date (yyyy)` >= 1975 & `Date (yyyy)` <= 2095)

      if (region_names[n] == "East_Bass_Strait"){
        zoo <- zoo %>%
          filter(`Date (yyyy)` >= 1994)
      }

      if (region_names[n] == "Humboldt"){
        zoo <- zoo %>%
          filter(`Date (yyyy)` >= 1992)
      }

      if (region_names[n] == "North_Sea"){
        zoo <- zoo %>%
          filter(`Date (yyyy)` >= 1991)
      }

      if (region_names[n] == "SE_Australia"){
        zoo <- zoo %>%
          filter(`Date (yyyy)` >= 1980 & `Date (yyyy)` <= 2050)
      }

      dat[[region_names[n]]][["global_models"]][["tcb"]][["unfished"]][[scenario[o]]][[models[m]]]$zooms <- zoo$`tcb( g C m-2)`

    }
  }
}

write_rds(dat,"/Users/jason/Nextcloud/MME2Work/FishMIP/CMIP5_RegionalCompare/Global_Model_Figures/regional_v_global_data_wZooMSS.RDS")

