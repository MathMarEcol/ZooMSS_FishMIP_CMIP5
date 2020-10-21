library(tidync)
library(tidyverse)
library(PCICt)
library(lubridate)

in_dir <- paste0("~",.Platform$file.sep,
                 "Nextcloud",.Platform$file.sep,
                 "MME1Data",.Platform$file.sep,
                 "ZooMSS_CMIP5",.Platform$file.sep,
                 "merged")

out_dir <- paste0("~",.Platform$file.sep,
                  "Nextcloud",.Platform$file.sep,
                  "MME2Work",.Platform$file.sep,
                  "FishMIP",.Platform$file.sep,
                  "CMIP5",.Platform$file.sep,
                  "Input")

model <- c("IPSL", "GFDL")

experiment <- c("historical", "rcp26", "rcp85")

for (m in 1:length(model)){
  for (x in 1:length(experiment)){

    file_p = sort(list.files(path = paste0(in_dir, .Platform$file.sep, "chl"),
                             pattern = glob2rx(paste0("*chlos*",model[m],"*",experiment[x],"*")), full.names = FALSE))
    file_s = sort(list.files(path = paste0(in_dir, .Platform$file.sep, "tos"),
                             pattern = glob2rx(paste0("*tos*",model[m],"*",experiment[x],"*")), full.names = FALSE))

    # Load files into tibble
    nc <- hyper_tibble(paste0(in_dir, .Platform$file.sep, "chl",.Platform$file.sep, file_p)) # Load phytoplankton files

    nc2 <- hyper_tibble(paste0(in_dir, .Platform$file.sep, "tos",.Platform$file.sep, file_s)) %>%  # Load phytoplankton files
      dplyr::select("tos") %>%
      mutate(tos = tos - 273)
    nc <- bind_cols(nc, nc2) # Join temp and phyto dataframes

    nc <- nc %>%
      mutate(lon = round(lon,1), # Some lon/lat don't match up for some reason. Rounding them seems to fix it.
             lat = round(lat,1))
    rm(nc2)

    # Process time with PCICt due to 365 day years
    ori <- ncatt_get(nc_open(paste0(in_dir, .Platform$file.sep, "chl",.Platform$file.sep, file_p)),
                     "time", "units")$value # Get origin from file
    ori <- str_replace(ori, "days since ", "")

    time <- as.PCICt(nc$time*86400, cal="365_day", origin = ori, tz = "UTC")
    nc$time <- round_date(as_date(as.character(time)), unit = "year") # Round to year
    nc$year <- year(nc$time)
    rm(ori)

    nc <- nc %>%
      mutate(chl = chl * 1e6, # Conver to mg
             Chl_log10 = log10(chl)) %>%
      rename("SST" = tos) %>% # Clean up
      select(-chl) %>%
      filter(year >= 1950)

    file <- str_replace(file_s, "tos_", "") # Remove variable details
    file <- str_replace(file, ".nc", ".rds")
    file <- str_replace(file, "merged", paste0(min(nc$year),"-",max(nc$year)))

    write_rds(nc, paste0(out_dir,.Platform$file.sep,file)) # Save to RDM

    rm(nc, file_p, file_s, file, time)
  }
}
