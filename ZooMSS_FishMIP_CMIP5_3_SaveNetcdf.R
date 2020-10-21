library(tidyverse)
library(ncdf4)
library(lubridate)
# After running this, files are to be uploaded to:
# /work/bb0820/ISIMIP/ISIMIP2b/UploadArea/marine-fishery_global/ZooMSS/_tmp


#dbem_cesm1-bgc_nobc_historical_nosoc_co2_tcb_global_annual_1860-2005.nc
# ZooMSS_IPSL-CM5A-LR_nobc_historical_nat_default_tcb_global_annual_1961-2016.nc


base_dir <- paste0("~",.Platform$file.sep,
                           "Nextcloud",.Platform$file.sep,
                           "MME2Work",.Platform$file.sep,
                           "FishMIP",.Platform$file.sep,
                           "CMIP5")

o_Model <- "ZooMSS"
o_Forcing <- c("IPSL-CM5A-LR", "GFDL-ESM2M")

o_Bias <- "nobc"
o_Scenario <- c("historical", "rcp26", "rcp85")
o_Soc <- "nat"
o_Sens <- "default"
o_Variable <- c("tsb", "tpb", "tcb", "b10cm", "b30cm")
o_Region <- "global"
o_TempRes <- "annual"


for (m in 1:length(o_Forcing)){

  for (s in 1:length(o_Scenario)){

    file <- list.files(path = paste0(base_dir, .Platform$file.sep, "Output"), pattern = paste0(o_Forcing[m], "_", o_Scenario[s]), full.names = TRUE)

    for (f in 1:length(file)){

      dat <- read_rds(file[f])

      dat$days <- as.numeric(dat$time - as_date("1850-01-01"))

        lon <- -179.5:179.5
        lat <- -89.5:89.5

      days <- sort(unique(dat$days))

      full_res <- crossing(lon, lat, days)

      out <- left_join(full_res, dat, by = c("lon", "lat", "days")) %>%
        arrange(days, lat, lon)

      rm(full_res)

      # Convert to lon, lat, time grid
      tsb <- tibble(array = array(out$tsb, dim=c(length(lon), length(lat), length(days))),
                    name = "TOTAL system biomass",
                    units = "g C m-2")

      tcb <- tibble(array = array(out$tcb, dim=c(length(lon), length(lat), length(days))),
                    name = "TOTAL consumer biomass",
                    units = "g C m-2")

      tpb <- tibble(array = array(out$tpb, dim=c(length(lon), length(lat), length(days))),
                    name = "TOTAL pelagic biomass",
                    units = "g C m-2")

      b10cm <- tibble(array = array(out$b10cm, dim=c(length(lon), length(lat), length(days))),
                       name = "Biomass of consumers >10cm",
                       units = "g C m-2")

      b30cm <- tibble(array = array(out$b30cm, dim=c(length(lon), length(lat), length(days))),
                      name = "Biomass of consumers >30cm",
                      units = "g C m-2")

      rm(out)

      # define dimensions
      timedim <- ncdim_def("time", "days since 1850-01-01 00:00:00", as.double(days), unlim = TRUE, calendar = "standard")
      latdim <- ncdim_def("lat", "degrees_north", as.double(lat))
      londim <- ncdim_def("lon", "degrees", as.double(lon))

      # define variables
      fillvalue <- 1e20

      for (v in 1:length(o_Variable)){

        o_file <- str_to_lower(paste0(o_Model, "_", o_Forcing[m], "_", o_Bias, "_",
                         o_Scenario[s], "_", o_Soc, "_", o_Sens, "_",
                         o_Variable[v], "_", o_Region, "_", o_TempRes, "_",
                         min(year(dat$time)), "_", max(year(dat$time)),".nc"))

        o_file <- paste0(base_dir, .Platform$file.sep, "Output", .Platform$file.sep, o_file)

        var <- eval(parse(text = o_Variable[v]))

        def = ncvar_def(name = o_Variable[v], units = var$units, dim = list(londim, latdim, timedim), missval = fillvalue, longname = var$name, prec="double")

        # create netCDF
        ncout <- nc_create(o_file, def, force_v4=TRUE)

        # put variables
        ncvar_put(ncout, def, var$array)

        # put additional attributes into dimension and data variables
        ncatt_put(ncout,"time","axis","T")
        ncatt_put(ncout,"lat","axis","Y")
        ncatt_put(ncout,"lat","long_name", "latitude")
        ncatt_put(ncout,"lon","axis","X")
        ncatt_put(ncout,"lon","long_name", "longitude")

        # add global attributes
        ncatt_put(ncout, 0, "author", "Created by Jason Everett <Jason.Everett@uq.edu.au>")
        ncatt_put(ncout, 0, "institution", "University of Queensland, Australia")
        ncatt_put(ncout, 0, "ModelOwners", "Jason D. Everett, Ryan F. Heneghan, Anthony J. Richardson, Patrick J. Sykes")
        ncatt_put(ncout, 0, "date_created", now())
        ncatt_put(ncout, 0, "comments", "ZooMSS model output for ISIMIP2b experimental protocol")
        ncatt_put(ncout, 0, "Assumed Mixed Layer Depth", "60 m")

        # close the file, writing data to disk
        nc_close(ncout)
      }
    }
  }
}
