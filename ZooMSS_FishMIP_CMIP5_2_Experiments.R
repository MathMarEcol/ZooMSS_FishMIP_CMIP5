library(tidyverse)
library(yaImpute)

source("fZooMSS_Xtras.R")

base_dir <- paste0("~",.Platform$file.sep,
                  "Nextcloud",.Platform$file.sep,
                  "MME2Work",.Platform$file.sep,
                  "FishMIP",.Platform$file.sep,
                  "CMIP5",.Platform$file.sep)

# TOTAL consumer biomass density, tcb, g m-2, 1° grid, monthly, all consumers (trophic level >1, vertebrates and invertebrates)
# TOTAL consumer biomass density in log10 weight bins, tcblog10, g m-2, 1° grid, monthly, 6 size bins, If the model is size-structured, please provide biomass in equal log 10 g C weight bins (1g, 10g, 100g, 1kg, 10kg, 100kg)
#
# TOTAL pelagic biomass density, tpb, g m-2, 1° grid, monthly, all pelagic consumers (trophic level >1, vertebrates and invertebrates)
# Biomass density of small pelagics <30cm, bp30cm, g m-2, 1° grid, monthly, if a pelagic species and L infinity is <30 cm, include in this variable
# Biomass density of medium pelagics >=30cm and <90cm, bp30to90cm, g m-2, 1° grid, monthly, if a pelagic species and L infinity is >=30 cm and <90cm, include in this variable
# Biomass density of large pelagics >=90cm, b90cm, g m-2, 1° grid, monthly, if a pelagic species and L infinity is >=90cm, include in this variable

#### Conversions for Length to Weight ####
#convert FishMIP length thresholds to weight thresholds, using (wet weight)=0.01(length)^3
weight10 <- 10^round(log10(10^(1/3)*100),1) # using Weight (g) = 0.01 * length^3, length is in cm
weight30 <- 10^round(log10(30^(1/3)*100),1)

#### Mixed Layer Depth ####
MLD <- 60

#### Load ZooMSS Matrix Data ####
enviro_data <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20201016_CMIP5_Matrix/enviro_CMIP5_Matrix.RDS") %>%
  mutate(cellID = 1:n()) # Create a cellID
res <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20201016_CMIP5_Matrix/Output/res_20201016_CMIP5_Matrix.RDS" )
mdl <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20201016_CMIP5_Matrix/Output/model_20201016_CMIP5_Matrix.RDS")

w <- mdl$param$w
w_lim <- 10^c(-1, 0, 1, 2, 3, 4, 5)

carbon <- mdl$param$Groups$Carbon
Bio <- fZooMSS_CarbonBiomass(res, w, carbon) # Convert to carbon biomass
BioSumSp <- fZooMSS_SumSpecies(Bio) # Sum the species
rm(res, Bio)

Bio_df <- as_tibble(matrix(unlist(BioSumSp), nrow=length(BioSumSp), byrow=T)) %>%
  mutate(cellID = 1:n()) %>% # Create a cellID
  pivot_longer(cols = starts_with("V"), names_to = "SizeClass", values_to = "Biomass") %>%
  mutate(Biomass = Biomass * MLD, # Convert biomass to m-2 by * MLD
         Weight = rep(w, times = length(BioSumSp))) %>% # Make sure weight class is on every row
  # select(-SizeClass) %>%
  filter(Weight <= 100001) %>% # Remove very large stuff (100 kg)
  # mutate(BiomassC = Biomass * 0.1) %>% # convert to carbon biomass
  add_column(tcb = 1, tpb = 1, tsb = 1, b10cm = 0, b30cm = 0) %>% # Create column of ones
  mutate(tcb = tcb * Biomass, # All consumers is simply biomass
         tpb = tpb * Biomass, # All pelagic consumers is simply biomass
         b10cm = replace(b10cm, Weight >= weight10, 1), # Replace 0 with 1 for rows inside weight range
         b30cm = replace(b30cm, Weight >= weight30, 1), # Replace 1 with zero for rows outside weight range
         b10cm = b10cm * Biomass, # Multiply weight class switch (0,1) by Biomass
         b30cm = b30cm * Biomass)  # Multiply weight class switch (0,1) by Biomass

BioSum_df <- Bio_df %>%
  group_by(cellID) %>%
  summarise(tcb = sum(tcb),
            tpb = sum(tpb),
            b10cm = sum(b10cm),
            b30cm = sum(b30cm),
            .groups = "keep") %>%
  ungroup() %>%
  left_join(select(enviro_data, cellID, chlo, sst), by = "cellID") %>%
  rename(SST = sst) %>%
  mutate(Chl_log10 = log10(chlo),
       Chl_C_mg_m3 = 10^(0.89 * Chl_log10 + 1.79), #Convert Chl to Carbon using Maranon et al. 2014
       Chl_C_g_m2 = Chl_C_mg_m3/1000/MLD,
       tsb = tcb + Chl_C_g_m2)

rm(BioSumSp, Bio_df, enviro_data)

files = list.files(path = paste0(base_dir, "Input"), pattern = "*.rds", full.names = FALSE)

for (f in 1:length(files)){

  nc <- read_rds(paste0(base_dir, "Input", .Platform$file.sep, files[f]))
  out <- ann(as.matrix(BioSum_df[,c("SST", "Chl_log10")]),
             as.matrix(nc[,c("SST", "Chl_log10")]),
             k = 1, verbose = FALSE)

  nc <- nc %>%
    mutate(cellID = out$knnIndexDist[,1],
           EuclideanDist = out$knnIndexDist[,2],
           tsb = BioSum_df$tsb[cellID],
           tcb = BioSum_df$tcb[cellID],
           tpb = BioSum_df$tpb[cellID],
           b10cm = BioSum_df$b10cm[cellID],
           b30cm = BioSum_df$b30cm[cellID],
           Chl_log10_ZooMSS = BioSum_df$Chl_log10[cellID],
           SST_ZooMSS = BioSum_df$SST[cellID])

  write_rds(nc, paste0(base_dir, "Output", .Platform$file.sep, str_replace(files[f],".rds", "_withZooMSS.rds"))) # Save to RDM
  rm(nc, out)
}

