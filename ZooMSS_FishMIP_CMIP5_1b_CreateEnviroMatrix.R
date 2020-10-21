# Load and plot the CMIP model output to look at combinations of SST v Chl
# Jason Everett (UQ)
# 15th September 2020

library(tidyverse)
library(tidync)
library(raster)
base_dir <- "/Users/jason/Nextcloud/MME1Data/ZooMSS_CMIP5/merged/"

ModelArray <- c("IPSL-CM5A-LR", "GFDL-ESM2M")
ExpArray <- c("historical", "rcp26", "rcp85")


for (m in 1:length(ModelArray)){
  for (e in 1:length(ExpArray)){

    ftos <- list.files(paste0(base_dir, "tos"), pattern = paste0(ModelArray[m],"_",ExpArray[e]), full.names = TRUE)
    fchl <- list.files(paste0(base_dir, "chl"), pattern = paste0(ModelArray[m],"_",ExpArray[e]), full.names = TRUE)

    ttos <- as.data.frame(stack(ftos), xy = TRUE, na.rm = FALSE) %>%
      pivot_longer(cols = contains("X", ignore.case = FALSE), names_to = "Date", values_to = "SST")

    tchl <- as.data.frame(stack(fchl), xy = TRUE, na.rm = FALSE) %>%
      pivot_longer(cols = contains("X", ignore.case = FALSE), names_to = "Date", values_to = "Chl") %>%
      dplyr::select(Chl)

    temp <- bind_cols(ttos, tchl) %>%
      add_column(Model = ModelArray[m],
                 Experiment = ExpArray[e]) %>%
      mutate(SST = round(SST-273, digits = 1),
             Chl = Chl * 1e6, # Convert to mg m-3
             Chl_log10 = log10(Chl),
             Chl_log10 = round(Chl_log10, digits = 2))

    if (m == 1 & e == 1){
      df <- temp
    } else {
      df <- bind_rows(df, temp)
     }
    rm(temp, ttos, tchl)
  }
}


df <- df %>%
  rename("Lon" = x, "Lat" = y)

## Do the plotting for all the data

graphics.off()
x11(width = 6, height = 6)
ggplot(data = df, mapping = aes(x = SST, y = Chl_log10)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", trans = "log10") +
  theme_bw()
ggsave("Figures/SSTChl_All.pdf")

graphics.off()
x11(width = 10, height = 6)
ggplot(data = df, mapping = aes(x = SST, y = Chl_log10)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", trans = "log10") +
  theme_bw() +
  facet_wrap(facets = "Model", scales = "fixed")
ggsave("Figures/SSTChl_ModelFacet.pdf")


## Now check the distinct rows
ds <- df %>%
  distinct(SST, Chl_log10, .keep_all = TRUE)

graphics.off()
x11(width = 6, height = 6)
ggplot(data = ds, mapping = aes(x = SST, y = Chl_log10)) +
  geom_point() +
  # scale_fill_continuous(type = "viridis", trans = "log10") +
  theme_bw()
ggsave("Figures/SSTChl_Distinct.pdf")


# Now save environmental data space
Matrix2 <- ds %>%
  dplyr::select(c(SST, Chl_log10)) %>%
  arrange(desc(Chl_log10), desc(SST)) %>%
  rename(sst = SST) %>%
  filter(is.na(Chl_log10)==FALSE) %>%
  filter(is.na(sst)==FALSE)


Matrix1 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix.RDS") %>%
  mutate(Chl_log10 = round(log10(chlo),2)) %>%
  dplyr::select(-chlo) %>%
  distinct() %>%
  arrange(sst)

missing <- anti_join(Matrix2, Matrix1, join_by = c("Chl_log10", "sst")) %>%
  mutate(chlo = 10^Chl_log10) %>%
  dplyr::select(-Chl_log10)

saveRDS(missing, "~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20201016_CMIP5_Matrix/enviro_CMIP5_Matrix_missing.RDS")

#CMIP6 Matrix has 109270 rows
# Start new simulations from 109271
enviro_data <- bind_rows(read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix.RDS"), missing)

write_rds(enviro_data, "~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20201016_CMIP5_Matrix/enviro_CMIP5_Matrix.RDS")


