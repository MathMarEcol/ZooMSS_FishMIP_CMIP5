library(tidyverse)
library(ncdf4)
library(lubridate)
# After running this, files are to be uploaded to:
# /work/bb0820/ISIMIP/ISIMIP2b/UploadArea/marine-fishery_global/ZooMSS/_tmp


#FILENAME EXAMPLE
# macroecological_gfdl-esm2m_nobc_rcp2p6_wo-diaz_no-fishing_no-oa_b10cm_global_annual_2006_2100.nc4
#          zoomss_gfdl-esm2m_nobc_rcp26_wo-diaz_no-fishing_no-oa_b10cm_global_annual_2006_2100.nc4

base_dir <- paste0("~",.Platform$file.sep,
                   "Nextcloud",.Platform$file.sep,
                   "MME2Work",.Platform$file.sep,
                   "FishMIP",.Platform$file.sep,
                   "CMIP5")

o_modelname <- "ZooMSS"
o_gcm <- c("IPSL-CM5A-LR", "GFDL-ESM2M")
o_biascorrection <- "nobc"
o_climatescenario <- c("historical", "rcp26", "rcp85")
o_savescenario <- c("historical", "future", "future")
o_socscenario <- "wo-diaz_no-fishing"
o_co2sensscenarios <- "no-oa"
o_variable <- c("tsb", "tpb", "tcb", "b10cm", "b30cm")
o_region <- "global"
o_timestep <- "annual"

# m <- s <- f <- 1


# curr_file_time = xr.open_mfdataset("/Users/jason/Downloads/boats_ipsl-cm6a-lr_nobc_historical_nat_default_tcb_global_monthly_1950_2014.nc", decode_times=False)
# /Users/jason/opt/anaconda3/lib/python3.8/site-packages/xarray/conventions.py:492: SerializationWarning: variable 'tcb' has multiple fill values {1e+20, 1e+20}, decoding all values to NaN.
# new_vars[k] = decode_cf_variable(
#
#   curr_file_time
#   Out[7]:
#     <xarray.Dataset>
#     Dimensions:  (lat: 180, lon: 360, time: 780)
#   Coordinates:
#     * lon      (lon) float64 -179.5 -178.5 -177.5 -176.5 ... 177.5 178.5 179.5
#   * lat      (lat) float64 89.5 88.5 87.5 86.5 85.5 ... -86.5 -87.5 -88.5 -89.5
#   * time     (time) float64 1.0 2.0 3.0 4.0 5.0 ... 777.0 778.0 779.0 780.0
#   Data variables:
#     tcb      (time, lat, lon) float32 dask.array<chunksize=(780, 180, 360), meta=np.ndarray>
#     Attributes:
#     contact:       Jerome Guiet <jerome.c.guiet@gmail.com>, Daniele Bianchi <...
#   institution:   AOS - University of California Los Angeles
#   comment:       Impact model output for ISIMIP3b
#   date_created:  14/10/20-03:56
#


for (m in 1:length(o_gcm)){

  for (s in 1:length(o_climatescenario)){

    file <- list.files(path = paste0(base_dir, .Platform$file.sep, "Input"), pattern = paste0(o_gcm[m], "_", o_climatescenario[s]), full.names = TRUE)

    for (f in 1:length(file)){

      dat <- read_rds(file[f])
      dat$days <- as.numeric(dat$time - as_date("1850-01-01"))

      lon <- -179.5:179.5
      lat <- 89.5:-89.5
      days <- sort(unique(dat$days))

      full_res <- crossing(lon, lat, days)

      out <- left_join(full_res, dat, by = c("lon", "lat", "days")) %>%
        arrange(days, desc(lat), lon)
        # arrange(lon, desc(lat), days)

      rm(full_res)

      # nc_dim <- c(length(lon), length(lat), length(days))
      nc_dim <- c(length(days), length(lat), length(lon))

      # Convert to lon, lat, time grid
      tsb <- tibble(array = array(out$tsb, dim = nc_dim),
                    name = "TOTAL system biomass",
                    units = "g C m-2")

      tcb <- tibble(array = array(out$tcb, dim = nc_dim),
                    name = "TOTAL consumer biomass",
                    units = "g C m-2")

      tpb <- tibble(array = array(out$tpb, dim = nc_dim),
                    name = "TOTAL pelagic biomass",
                    units = "g C m-2")

      b10cm <- tibble(array = array(out$b10cm, dim = nc_dim),
                      name = "Biomass of consumers >10cm",
                      units = "g C m-2")

      b30cm <- tibble(array = array(out$b30cm, dim = nc_dim),
                      name = "Biomass of consumers >30cm",
                      units = "g C m-2")

      rm(out)

      # define dimensions
      londim <- ncdim_def("lon", "degrees_east", as.double(lon), create_dimvar = TRUE)
      latdim <- ncdim_def("lat", "degrees_north", as.double(lat), create_dimvar = TRUE)
      timedim <- ncdim_def("time", "days since 1850-01-01 00:00:00", as.double(days), unlim = TRUE, calendar = "standard")


      # define variables
      fillvalue <- 1e20

      for (v in 1:length(o_variable)){

        o_file <- str_to_lower(paste0(o_modelname, "_", o_gcm[m], "_", o_biascorrection, "_",
                                      o_climatescenario[s], "_", o_socscenario, "_", o_co2sensscenarios, "_",
                                      o_variable[v], "_", o_region, "_", o_timestep, "_",
                                      min(year(dat$time)), "_", max(year(dat$time)),".nc4"))

        o_file <- paste0(base_dir, .Platform$file.sep,
                         "Output", .Platform$file.sep,
                         o_gcm[m],.Platform$file.sep,
                         o_savescenario[s], .Platform$file.sep, o_file)

        var <- eval(parse(text = o_variable[v]))

        def = ncvar_def(name = o_variable[v],
                        units = var$units,
                        dim = list(londim, latdim, timedim),
                        # dim = list(timedim, latdim, londim),
                        missval = fillvalue,
                        longname = var$name,
                        prec = "double")

        # create netCDF
        ncout <- nc_create(o_file, def, force_v4 = TRUE)

        # put variables
        ncvar_put(ncout, def, var$array)

        # put additional attributes into dimension and data variables
        ncatt_put(ncout,"lon","axis","X")
        ncatt_put(ncout,"lon","long_name", "longitude")
        ncatt_put(ncout,"lat","axis","Y")
        ncatt_put(ncout,"lat","long_name", "latitude")
        ncatt_put(ncout,"time","axis","T")


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
