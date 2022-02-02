# S01-01 Setup #########################################################################################################

library(tidyverse)
library(gridExtra)
select <- dplyr::select

library(rgdal)
library(raster)
library(reticulate)
library(princurve)

library(cluster)
library(factoextra)
library(dendextend)
library(reshape2)

library(minpack.lm)

working_folder <- 'D:/Project/Research/Paper 4 Parameters'
output_folder <- 'D:/Project/Research/Paper 4 Parameters/Outputs/'
functions_folder <- 'D:/Project/Research/Scripts/R Codes/'
gis_folder <- 'D:/Project/Research/GlobalStorage/GIS/DataGlobal/Temp/arcpyTemp'
gis_gdb <- 'D:/Project/Research/GlobalStorage/GIS/DataGlobal/Temp/arcpyTemp.gdb'
arcpy_exe <- 'C:/Python27/ArcGIS10.5/python.exe'

output_pptx <- 0
pptx_file <- paste0(output_folder, 'Plots.pptx')
pptx_base_file <- paste0(output_folder, 'PlotsBase.pptx')

setwd(working_folder)

source(paste0(functions_folder, 'functions_hypso.R'))
source(paste0(functions_folder, 'functions_wf.R'))
source(paste0(functions_folder, 'functions_wf_divergence.R'))
source(paste0(functions_folder, 'functions_supplementary.R'))
source(paste0(functions_folder, 'functions_cross_plot.R'))
source(paste0(functions_folder, 'DecomposablePlots.R'))

basins_VS <- "D:/Project/Research/GlobalStorage/GIS/DataNarmada/Stream/Basins.shp"
flowLength_RS <- "D:/Project/Research/GlobalStorage/GIS/DataNarmada/DEMPra/flowL_clip.tif"
fil_RS <- "D:/Project/Research/GlobalStorage/GIS/DataNarmada/DEMPra/fil_clip.tif"
fac_RS <- "D:/Project/Research/GlobalStorage/GIS/DataNarmada/DEMPra/fac_clip.tif"
slope_RS <- "D:/Project/Research/GlobalStorage/GIS/DataNarmada/DEMPra/slope_d_clip.tif"

watersheds <- 1
basin_ids <- list(1:72)
basin_ids_string <- list(as.character(basin_ids[[1]]))
df_watersheds <- data.frame(watershed = watersheds,
                            watershed_name = c('Narmada'))

watershed_current <- 1
watershed_name_current <- df_watersheds %>% 
  filter(watershed == watershed_current) %>% 
  pull(watershed_name)

n_mix_current <- 2

# S01-02 Data ##########################################################################################################

# df_basin_data <- readRDS('df_basin_data.RDS')
# df_basin_data_summary <- readRDS('df_basin_data_summary2.RDS')
# 
# list_hypso_nls_formulations <- readRDS('list_hypso_nls_formulations_05.RDS')
# df_hypso_formulations <- readRDS('df_hypso_formulations_05.RDS')
# df_hypso_summary_formulations <- readRDS('df_hypso_summary_formulations_05.RDS')
# df_hypso_summary_formulations_current <- readRDS('df_hypso_summary_formulations_current.RDS')
# df_hypso <- readRDS('df_hypso.RDS')
# 
# df_di_pairs <- readRDS('df_di_pairs.RDS')
# clusGap_di <- readRDS('clusGap_di.RDS')
# df_di_lowest <- readRDS('df_di_lowest.RDS')
# basin_id_outliers_di_original <- readRDS('basin_id_outliers_di_original.RDS')
# basin_id_outliers_di <- readRDS('basin_id_outliers_di.RDS')
# df_hypso_cluster_outliers <- readRDS('df_hypso_cluster_outliers.RDS')
# clusters_di <- readRDS('clusters_di2.RDS')
# df_cluster_di <- readRDS('df_cluster_di.RDS')
# df_hypso_cluster <- readRDS('df_hypso_cluster2.RDS')
# df_hypso_mean <- readRDS('df_hypso_mean.RDS')
# df_hypso_median <- readRDS('df_hypso_median.RDS')
# df_cluster_hypso_wf <- readRDS('df_cluster_hypso_wf_02.RDS')
# 
# n_mix_current <- 2
# if (n_mix_current == 2)
# {
#   snorm_list <- readRDS('snorm_list_03.RDS')
#   df_sn_params <- readRDS('snorm_params_df_02.RDS')
#   df_sn_values <- readRDS('snorm_values_df_02.RDS')
#   df_sn_values_main <- readRDS('df_sn_values_main_02.RDS')
#   
#   df_wf_pairs <- readRDS('df_wf_pairs_02.RDS')
#   clusGap_l2_original <- readRDS('clusGap_l2_original.RDS')
#   basin_id_outliers_l2 <- readRDS('basin_id_outliers_l2.RDS')
#   df_wf_l2_lowest <- readRDS('df_wf_l2_lowest.RDS')
#   clusters_l2 <- readRDS('clusters_l2_03.RDS')
#   df_cluster_l2 <- readRDS('df_cluster_l2.RDS')
#   df_clusters_wf <- readRDS('df_clusters_wf_03.RDS')
#   clusGap_l2 <- readRDS('clusGap_l2.RDS')
#   df_wf_mean <- readRDS('df_wf_mean.RDS')
#   df_wf_median <- readRDS('df_wf_median.RDS')
#   
#   df_cluster_hypso_wf <- readRDS('df_cluster_hypso_wf_02.RDS')
# } else if (n_mix_current == 3)
# {
#   snorm_list <- readRDS('snorm_list_03.RDS')
#   df_sn_params <- readRDS('snorm_params_df_02.RDS')
#   df_sn_values <- readRDS('snorm_values_df_nmix3_02.RDS')
#   df_sn_values_main <- readRDS('df_sn_values_main_nmix3_02.RDS')
#   
#   df_wf_pairs <- readRDS('df_wf_pairs_nmix3_02.RDS')
#   
#   m_l2_pairs <- readRDS('m_l2_pairs_nmix3.RDS')
#   clusters_l2 <- readRDS('clusters_l2_nmix3_02.RDS')
#   clusters_agnes_l2 <- readRDS('clusters_agnes_l2_nmix3_02.RDS')
#   
#   basin_id_outliers_l2 <- readRDS('basin_id_outliers_l2_nmix3.RDS')
#   df_cluster_wf <- readRDS('df_cluster_wf_02.RDS')
#   df_wf_representative <- readRDS('df_wf_representative_02.RDS')
#   df_cluster_representative_wf <- readRDS('df_cluster_representative_wf_02.RDS')
# } else if (n_mix_current == 5)
# {
#   snorm_list <- readRDS('snorm_list_nmix5_01.RDS')
#   df_sn_params <- readRDS('snorm_params_df_nmix5_02.RDS')
#   df_sn_values <- readRDS('snorm_values_df_nmix5_02.RDS')
#   df_sn_values_main <- readRDS('df_sn_values_main_nmix5_02.RDS')
#   df_wf_pairs <- readRDS('df_wf_pairs_nmix5_02.RDS')
#   m_l2_pairs <- readRDS('m_l2_pairs_nmix5.RDS')
#   clusters_l2 <- readRDS('clusters_l2_nmix5_02.RDS')
#   clusters_agnes_l2 <- readRDS('clusters_agnes_l2_nmix5_02.RDS')
# }
#
# S02-01 Read GIS data and convert to R data #######################################################################

library(raster)
library(reticulate)

use_python(arcpy_exe, required = TRUE)

py_run_string('import arcpy')
py_run_string('import numpy')

py_run_string('arcpy.env.overwriteOutput = True')

py_run_string('print (arcpy.CheckExtension("Spatial"))')
py_run_string('print (arcpy.CheckOutExtension("Spatial"))')

basins_f <- "basins_lyr"

py_run_string(paste0('arcpy.MakeFeatureLayer_management("', 
                     basins_VS,
                     '", "',
                     basins_f,
                     '")'))

py_run_string(paste0(basins_f,
                     ' = arcpy.da.FeatureClassToNumPyArray("',
                     basins_VS,
                     '", ["gridcode"])'))

flowLength_list <- list()
elevation_list <- list()
slope_list <- list()
fac_list <- list()

basins <- 1:126

for(basin in basins)
{
  # basin <- basins[1]
  print (basin)
  
  # Field with basin_id
  # py_run_string('field = ["basin_id"]')
  
  # Filter basin_id
  py_run_string(paste0('arcpy.SelectLayerByAttribute_management("',
                       basins_f,
                       '", "NEW_SELECTION", ',
                       "'",
                       '"basin_id" = ',
                       basin,
                       "')"))
  
  # Flow length
  py_run_string(paste0('temp_extracted = arcpy.sa.ExtractByMask("',
                       flowLength_RS,
                       '", "',
                       basins_f,
                       '")'))
  py_run_string(paste0('temp_extracted.save("', tempFolder, '/try1.tif")'))
  r <- raster(paste0(tempFolder, '/try1.tif'))
  r_values <- getValues(r)
  r_values <- r_values[!is.na(r_values)]
  flowLength_list[[as.character(basin)]] <- r_values
  
  # Elevation
  py_run_string(paste0('temp_extracted = arcpy.sa.ExtractByMask("',
                       fil_RS,
                       '", "',
                       basins_f,
                       '")'))
  py_run_string(paste0('temp_extracted.save("', tempFolder, '/try1.tif")'))
  r <- raster(paste0(tempFolder, '/try1.tif'))
  r_values <- getValues(r)
  r_values <- r_values[!is.na(r_values)]
  elevation_list[[as.character(basin)]] <- r_values
  
  # fac
  py_run_string(paste0('temp_extracted = arcpy.sa.ExtractByMask("',
                       fac_RS,
                       '", "',
                       basins_f,
                       '")'))
  py_run_string(paste0('temp_extracted.save("', tempFolder, '/try1.tif")'))
  r <- raster(paste0(tempFolder, '/try1.tif'))
  r_values <- getValues(r)
  r_values <- r_values[!is.na(r_values)]
  fac_list[[as.character(basin)]] <- r_values
  
  # Slope
  py_run_string(paste0('temp_extracted = arcpy.sa.ExtractByMask("',
                       slope_RS,
                       '", "',
                       basins_f,
                       '")'))
  py_run_string(paste0('temp_extracted.save("', tempFolder, '/try1.tif")'))
  r <- raster(paste0(tempFolder, '/try1.tif'))
  r_values <- getValues(r)
  r_values <- r_values[!is.na(r_values)]
  slope_list[[as.character(basin)]] <- r_values
}

py_run_string('print (arcpy.CheckInExtension("Spatial"))')

feature_subbasin <- "D:/Project/Research/GlobalStorage/GIS/DataNarmada/Stream/Basins.shp"
shape_subbasin <- shapefile(basins_VS)
temp_df_basin_data_summary_additional <- shape_subbasin@data %>% 
  mutate(watershed = 2,
         perimeter_gis_km = perim_km,
         area_gis_km2 = area_km2) %>% 
  select(watershed, basin_id, perimeter_gis_km, area_gis_km2)

elevation_list %>%
  saveRDS('raw_elevation.RDS')
fac_list %>%
  saveRDS('raw_fac.RDS')
slope_list %>%
  saveRDS('raw_slope.RDS')
flowLength_list %>%
  saveRDS('raw_flowLength.RDS')

temp_df_basin_data_summary_additional_gis %>% 
  saveRDS('temp_df_basin_data_summary_additional_gis.RDS')

# S02-02 Create dataframe ##########################################################################################################

df_basin_data <- data.frame()

for (basin_id in basin_ids)
{
  # basin_id <- basin_ids[1]
  
  temp_df <- data.frame(basin_id = as.numeric(basin_id),
                        elevation_m = elevation_list[[basin_id]],
                        fac_m = fac_list[[basin_id]],
                        slope_deg = slope_list[[basin_id]],
                        chainage_m_global = flowLength_list[[basin_id]])
  
  df_basin_data <- rbind(df_basin_data, temp_df)
}

df_basin_data <- df_basin_data %>% 
  group_by(basin_id) %>%
  mutate(elevation_m_min = min(elevation_m),
         elevation_m_max = max(elevation_m)) %>%
  ungroup() %>%
  mutate(elevation_scaled = (elevation_m-elevation_m_min)/(elevation_m_max-elevation_m_min))

df_basin_data <- df_basin_data %>% 
  group_by(basin_id) %>%
  mutate(chainage_m_global_start = min(chainage_m_global)) %>%
  ungroup() %>%
  mutate(chainage_m = chainage_m_global - chainage_m_global_start) %>%
  group_by(basin_id) %>%
  mutate(chainage_m_max = max(chainage_m)) %>%
  ungroup() %>%
  mutate(chainage_scaled = chainage_m/chainage_m_max) %>%
  arrange(basin_id, chainage_m) %>% 
  mutate(watershed = 1) %>% 
  select(watershed, everything())

df_basin_data %>%
  saveRDS('df_basin_data.RDS', version = 2)

# S02-03 Basin data summary ##########################################################################################################

df_basin_data_summary <- df_basin_data %>%
  group_by(basin_id) %>%
  summarize(area_km2 = n(),
            elevation_m_min = first(elevation_m_min),
            elevation_m_max = first(elevation_m_max),
            chainage_m_global_start = first(chainage_m_global_start),
            chainage_m_max = first(chainage_m_max)) %>% 
  mutate(watershed = 1) %>% 
  select(watershed, everything())

df_basin_data_summary %>%
  saveRDS('df_basin_data_summary.RDS', version = 2)

# S02-04 Additional basin summary data #############################################################################################################

temp_df_basin_data_summary_additional <- df_basin_data %>% 
  group_by(watershed, basin_id) %>% 
  summarize(elevation_m_mean = mean(elevation_m),
            elevation_m_median = median(elevation_m),
            elevation_m_sd = sd(elevation_m),
            chainage_m_mean = mean(chainage_m),
            chainage_m_median = median(chainage_m),
            chainage_m_sd = sd(chainage_m),
            slope_deg_min = min(slope_deg),
            slope_deg_max = max(slope_deg),
            slope_deg_mean = mean(slope_deg),
            slope_deg_median = median(slope_deg),
            slope_deg_sd = sd(slope_deg))

df_basin_data_summary <- df_basin_data_summary %>% 
  left_join(temp_df_basin_data_summary_additional_gis,
            by = c('watershed', 'basin_id')) %>% 
  left_join(temp_df_basin_data_summary_additional,
            by = c('watershed', 'basin_id'))

df_basin_data_summary %>% 
  colnames()

df_basin_data_summary <- df_basin_data_summary %>% 
  select(watershed, basin_id, area_km2, area_gis_km2, perimeter_gis_km,
         elevation_m_min, elevation_m_max, elevation_m_mean, elevation_m_median, elevation_m_sd,
         slope_deg_min, slope_deg_max, slope_deg_mean, slope_deg_median, slope_deg_sd,
         chainage_m_global_start, chainage_m_max, chainage_m_mean, chainage_m_median, chainage_m_sd)

# df_basin_data_summary %>% 
#   # filter(watershed %in% c(1,2)) %>% 
#   ggplot(aes(x = area_km2, y = area_gis_km2)) +
#   geom_point()
# df_basin_data_summary %>% 
#   filter(watershed %in% c(1,2)) %>%
#   ggplot(aes(x = area_km2, y = area_gis_km2)) +
#   geom_point()
# df_basin_data_summary %>% 
#   filter(watershed %in% c(3)) %>%
#   ggplot(aes(x = area_km2, y = area_gis_km2)) +
#   geom_point()

df_basin_data_summary <- df_basin_data_summary %>% 
  mutate(rr_mean = (elevation_m_mean - elevation_m_min)/(elevation_m_max - elevation_m_min),
         rr_median = (elevation_m_median - elevation_m_min)/(elevation_m_max - elevation_m_min),
         compactness_ratio = perimeter_gis_km/(2*sqrt(pi*area_gis_km2)),
         basin_relief = elevation_m_max - elevation_m_min,
         relative_relief = basin_relief/perimeter_gis_km,
         basin_length = chainage_m_max,
         mean_basin_slope = slope_deg_mean)

df_basin_data_summary %>% 
  saveRDS('df_basin_data_summary2.RDS')

# S03-01 Hypsometric formulations parameter estimation (SKIP IF DONE) ###########################################################################################

list_hypso_nls_formulations <- list()
nls_formula <- y ~ ((r*(1-x^m)/((1-r)*x^m+r))^z)
nls_formula_start <- list(r=0.5, z=0.5, m=0.5)
nls_formula_lower <- c(r=0, z=0, m=0)
nls_formula_upper <- c(r=1, z=Inf, m=Inf)
temp_hypso_nls_list <- hypso_nls(nls_formula, nls_formula_start, nls_formula_lower, nls_formula_upper)
list_hypso_nls_formulations[[1]] <- temp_hypso_nls_list$list_hypso_nls_fit
df_hypso_formulations$relative_elevation_predicted_m2 <- temp_hypso_nls_list$df_hypso$relative_elevation_predicted
temp_hypso_nls_list$df_hypso_summary$formulation <- 1
df_hypso_summary_formulations <- rbind(df_hypso_summary_formulations, temp_hypso_nls_list$df_hypso_summary)

df_hypso_summary_formulations <- df_hypso_summary_formulations %>% 
  mutate(formulation_name = NA)
df_hypso_summary_formulations[df_hypso_summary_formulations$formulation == 1,]$formulation_name = 'Generalized'
# If you have multiple hypsometric function forms, this allows you to use them

df_hypso_summary_formulations <- df_hypso_summary_formulations %>% 
  mutate(watershed_name = NA)
df_hypso_summary_formulations[df_hypso_summary_formulations$watershed == 1,]$watershed_name = 'Narmada'
# If you have multiple watersheds, this allows you to use them

list_hypso_nls_formulations %>% 
  saveRDS('list_hypso_nls_formulations.RDS')
df_hypso_formulations %>% 
  saveRDS('df_hypso_formulations.RDS')
df_hypso_summary_formulations %>% 
  saveRDS('df_hypso_summary_formulations.RDS')

# S03-02 Hypsometric tables and data ###################################################################################

# Only generalized hypsometric formulation used in this study (can be changed to another formulation if desired)
df_hypso_summary_formulations_current <- df_hypso_summary_formulations %>% 
  filter(formulation == 1) %>% 
  select(watershed, basin_id, coeff1:coeff3, residual_sum_of_square, rmse, watershed_name) %>% 
  rename(r = coeff1,
         z = coeff2,
         m = coeff3) %>% 
  mutate(b = 1/r-1) %>% 
  select(watershed, watershed_name, basin_id, r, z, m, b, everything())
df_hypso_summary_formulations_current %>% 
  saveRDS('df_hypso_summary_formulations_current.RDS')

# Hyspometric data table from parameters

df_hypso <- data.frame()
for (watershed_current in watersheds)
{
  for (basin_id_current in basin_ids[[watershed_current]])
  {
    temp_df_hypso <- params_to_hypso(df_hypso_summary_formulations_current %>% 
                                       filter(watershed == watershed_current) %>% 
                                       filter(basin_id == basin_id_current), count = 0.001) %>% 
      mutate(watershed = watershed_current,
             basin_id = basin_id_current) %>% 
      select(watershed, basin_id, x, y)
    
    df_hypso <- rbind(df_hypso, temp_df_hypso)
  }
}
df_hypso %>% 
  saveRDS('df_hypso.RDS')  

# S03-03 Hypsometry distance matrix ####################################################################################

# DI pairs for distance matrix

df_di_pairs <- data.frame()
for (watershed_current in watersheds)
{
  for (basin_id_1 in basin_ids[[watershed_current]])
  {
    for (basin_id_2 in basin_ids[[watershed_current]])
    {
      df_di_pairs <- rbind(df_di_pairs, 
                           data.frame(watershed = watershed_current, 
                                      basin_id_1, 
                                      basin_id_2, 
                                      di = find_di(watershed = watershed_current, basin_id_1 = basin_id_1, basin_id_2 = basin_id_2)))
    }
  }
}
df_di_pairs %>% 
  saveRDS('df_di_pairs.RDS')

# S03-04 Clustering with hypsometric function ###########################################################################

# Clustering

m_di_pairs <- list()
clusters_di <- list()
clusters_agnes_di <- list()

for (watershed_current in watersheds)
{
  m_di_pairs[[watershed_current]] <- acast(df_di_pairs %>% 
                                             filter(watershed == watershed_current), basin_id_1 ~ basin_id_2, value.var = 'di')
  distance_matrix <- as.dist(m_di_pairs[[watershed_current]])
  clusters_di[[watershed_current]] <- hclust(distance_matrix, method = 'ward.D2')
  clusters_agnes_di[[watershed_current]] <- agnes(distance_matrix, diss = TRUE, method = 'ward')
}

m_di_pairs %>% 
  saveRDS('m_di_pairs_02.RDS')
clusters_di %>% 
  saveRDS('clusters_di_02.RDS')
clusters_agnes_di %>% 
  saveRDS('clusters_agnes_di_02.RDS')

# Best Clustering

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
ac <- function(x, watershed) 
{
  agnes(m_l2_pairs[[watershed]], diss = TRUE, method = x)$ac
}
sapply(m, ac, 1)
sapply(m, ac, 2)
sapply(m, ac, 3)

# S03-05 Clustering groups and outliers analysis #######################################################################

watershed_current <- 2
watershed_name_current <- df_watersheds %>% 
  filter(watershed == watershed_current) %>% 
  pull(watershed_name)

df_di_pairs %>% 
  filter(watershed == watershed_current) %>% 
  # filter(!(basin_id_1 %in% c(3, 24, 44))) %>% 
  # filter(!(basin_id_2 %in% c(3, 24, 44))) %>% 
  acast(basin_id_1 ~ basin_id_2, value.var = 'di') %>% 
  as.dist() %>% 
  hclust(method = 'complete') %>% 
  plot(main = paste0('Hypsometry Cluster Dendrogram for ', watershed_name_current))
df_di_pairs %>% 
  filter(watershed == watershed_current) %>% 
  # filter(!(basin_id_1 %in% c(3, 24, 44))) %>% 
  # filter(!(basin_id_2 %in% c(3, 24, 44))) %>% 
  acast(basin_id_1 ~ basin_id_2, value.var = 'di') %>% 
  as.dist() %>% 
  hclust(method = 'ward.D2') %>% 
  plot(main = paste0('Hypsometry Cluster Dendrogram for ', watershed_name_current, ' (Ward Method)'))

# Gap statistics

clusGap_di <- df_di_pairs %>%
  filter(watershed == watershed_current) %>%
  acast(basin_id_1 ~ basin_id_2, value.var = 'di') %>%
  clusGap(FUN = hcut, K.max = 15, B = 500) 
clusGap_di %>%
  saveRDS('clusGap_di.RDS')
clusGap_di %>%
  fviz_gap_stat() +
  labs(title = '')
clusGap_di %>%
  fviz_gap_stat_pra(paste0('Gap statistic for hypsometric clusters')) +
  geom_vline(xintercept = 8, linetype = 'dashed')

# # Silhouette analysis
# 
# plot(2:20, sapply(2:20, function(i) { 
#   mean(silhouette(cutree(clusters_di[[watershed_current]], i), dmatrix=m_l2_pairs[[watershed_current]])[,"sil_width"]) }),
#   main = paste0('Silhouette analysis for Hypsometry for ', watershed_name_current),
#   xlab="Number of clusters", 
#   ylab="Average Silhouette", 
#   type="b", 
#   pch=20)
# abline(v = 7, col = 'red')
# dev.copy(png,
#          foldered_png(paste0('Silhouette analysis for Width Function (L2) for ', watershed_name_current)),
#          width = 800,
#          height = 600); dev.off()

# Outliers

n_max <- df_di_pairs %>% 
  filter(watershed == watershed_current) %>% 
  pull(basin_id_1) %>% 
  max()
n_optimal <- n_max %>% sqrt() %>% ceiling()
n_lowest <- 1
df_di_lowest <- data.frame()
for (n_lowest in 1:n_optimal)
{
  df_di_lowest <- rbind(df_di_lowest,
                           df_di_pairs %>% 
                             filter(watershed == watershed_current) %>% 
                             filter(di > 0) %>% 
                             arrange(basin_id_1, di) %>% 
                             group_by(basin_id_1) %>% 
                             slice(1:n_lowest) %>% 
                             mutate(di_log = log(di)) %>% 
                             summarise(di_geometric_mean = sum(di_log)) %>% 
                             mutate(di_geometric_mean = (exp(di_geometric_mean))^(1/n_lowest)) %>% 
                             mutate(n_lowest = n_lowest))
}
df_di_lowest %>% 
  saveRDS('df_di_lowest.RDS')
df_di_lowest %>% 
  ggplot() +
  geom_line(aes(x = n_lowest,
                y = di_geometric_mean,
                group = basin_id_1),
            alpha = 0.3) +
  scale_x_continuous(breaks = 1:n_optimal) +
  labs(title = paste0('Geometric mean DI of Closest Neighbors'),
       x = 'Number of closest neighbors',
       y = 'Geometric mean of DI for neighbors') +
  theme_pra_bw +
  theme(panel.grid.minor.x = element_blank())
df_di_pairs %>% 
  filter(watershed == watershed_current) %>% 
  filter(di > 0) %>% 
  arrange(basin_id_1, di) %>% 
  group_by(basin_id_1) %>% 
  slice(1:n_lowest) %>% 
  ggplot(aes(x = basin_id_1,
             y = di)) +
  geom_point() +
  geom_line(aes(group = basin_id_1)) +
  scale_x_continuous(breaks = 1:n_max) +
  labs(x = 'Basin id',
       y = 'DI',
       title = 'DI for closest neighbors') +
  theme_pra_bw +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5))

n_lowest <- 15
basin_id_outliers_di_original <- df_di_pairs %>% 
  filter(watershed == watershed_current) %>% 
  filter(di > 0) %>% 
  arrange(basin_id_1, di) %>% 
  group_by(basin_id_1) %>% 
  slice(1:n_lowest) %>% 
  filter(di > 0.15) %>% 
  pull(basin_id_1) %>% 
  unique()
basin_id_outliers_di_original %>% 
  saveRDS('basin_id_outliers_di_original.RDS')

temp_best_k <- 8

temp_clusters <- df_di_pairs %>% 
  filter(watershed == watershed_current) %>% 
  acast(basin_id_1 ~ basin_id_2, value.var = 'di') %>% 
  as.dist() %>% 
  # hclust(method = 'complete') %>%
  hclust(method = 'ward.D2') %>%
  cutree(k = temp_best_k)
df_hypso_cluster_outliers <- data.frame(watershed = watershed_current,
                              basin_id = basin_ids[[watershed_current]],
                              cluster = temp_clusters)
df_hypso_cluster_outliers %>% 
  saveRDS('df_hypso_cluster_outliers.RDS')
df_hypso %>%
  left_join(df_hypso_cluster_outliers,
            by = c('watershed', 'basin_id')) %>%
  filter(watershed == watershed_current) %>%
  mutate(basin_id = factor(basin_id)) %>%
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                group = basin_id),
            size = 0.1) +
  labs(x = 'Relative Area above elevation',
       y = 'Relative Elevation',
       title = paste0('Hypsometric Cluster for ', watershed_name_current)) +
  theme_pra_bw +
  facet_wrap(~cluster, nrow = 3)
# ggsave(filename = foldered_png(paste0('Clusters Hypso for ', watershed_name_current)), width = 8, height = 6)

df_hypso %>%
  left_join(df_hypso_cluster_outliers,
            by = c('watershed', 'basin_id')) %>% 
  filter(watershed == watershed_current) %>% 
  mutate(outlier = ifelse(basin_id %in% basin_id_outliers_di_original
                          , 'Outlier', 'Not Outlier')) %>% 
  mutate(basin_id = factor(basin_id)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                group = basin_id,
                color = outlier),
            size = 0.1) +
  scale_color_manual(values = c('Outlier' = 'red',
                                'Not Outlier' = 'black')) +
  labs(x = 'Relative Area above elevation',
       y = 'Relative Elevation',
       title = paste0('Hypsometric Cluster for ', watershed_name_current)) +
  theme_pra_bw +
  facet_wrap(~cluster, nrow = 3)
# ggsave(filename = foldered_png(paste0('Clusters Outliers for ', watershed_name_current)), width = 8, height = 6)

# Outliers removed

basin_id_outliers_di <- NA
basin_id_outliers_di %>% 
  saveRDS('basin_id_outliers_di.RDS')

temp_basin_ids <- (basin_ids[[watershed_current]])[!(basin_ids[[watershed_current]] %in% basin_id_outliers_di)]
clusters_di <- df_di_pairs %>% 
  filter(watershed == watershed_current) %>% 
  filter(!(basin_id_1 %in% basin_id_outliers_di)) %>%
  filter(!(basin_id_2 %in% basin_id_outliers_di)) %>%
  acast(basin_id_1 ~ basin_id_2, value.var = 'di') %>% 
  as.dist() %>% 
  # hclust(method = 'complete') %>%
  hclust(method = 'ward.D2')
df_cluster_di <- data.frame(watershed = watershed_current,
                                 basin_id = temp_basin_ids,
                                 cluster = cutree(clusters_di, k = temp_best_k))
df_hypso_cluster <- df_hypso %>%
  left_join(df_cluster_di,
            by = c('watershed', 'basin_id'))

clusters_di %>% 
  saveRDS('clusters_di2.RDS')
df_cluster_di %>% 
  saveRDS('df_cluster_di.RDS')
df_hypso_cluster %>% 
  saveRDS('df_hypso_cluster2.RDS')

clusters_di %>% 
  plot(main = paste0('Hypsometry Cluster Dendrogram'))

df_hypso_cluster %>% 
  filter(watershed == watershed_current) %>%
  mutate(basin_id = factor(basin_id)) %>%
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                group = basin_id),
            size = 0.1) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Relative Area above elevation',
       y = 'Relative Elevation',
       title = paste0('Hypsometric Clusters')) +
  theme_pra_bw +
  facet_wrap(~cluster, nrow = 3)

df_cluster_di %>% 
  write_csv(foldered_csv('df_cluster_di'))

# Mean representative Hypsometric Curve

df_hypso_mean <- df_hypso_cluster %>% 
  group_by(watershed, cluster, x) %>% 
  summarise(y_mean = mean(y)) %>% 
  ungroup()
df_hypso_mean %>% 
  saveRDS('df_hypso_mean.RDS')

# Median representative Hypsometric Curve

df_hypso_median <- data.frame()
for (watershed_current in c(2))#watersheds)
{
  # watershed_current <- 2
  
  for (cluster_current in df_hypso_cluster %>% filter(watershed == watershed_current) %>% pull(cluster) %>% unique())
  {
    # cluster_current <- 1
    
    temp_basin_ids <- df_hypso_cluster %>%
      filter(watershed == watershed_current) %>%
      filter(cluster == cluster_current) %>% 
      pull(basin_id) %>% 
      unique()
    
    temp_lowest_di <- Inf
    temp_current_basin_id <- NA
    for (basin_id_current in temp_basin_ids)
    {
      temp_current_di <- find_di_from_hypso_y(df_hypso_mean %>% 
                                                filter(watershed == watershed_current) %>%
                                                filter(cluster == cluster_current) %>% 
                                                pull(y_mean),
                                              df_hypso %>%
                                                filter(watershed == watershed_current) %>%
                                                filter(basin_id == basin_id_current) %>% 
                                                pull(y))
      
      if (temp_current_di < temp_lowest_di)
      {
        temp_lowest_di = temp_current_di
        temp_current_basin_id <- basin_id_current
      }
    }
    
    df_hypso_median <- rbind(df_hypso_median, data.frame(watershed = watershed_current,
                                                                             cluster = cluster_current,
                                                                             basin_id = temp_current_basin_id))
  }
}
df_hypso_median %>% 
  saveRDS('df_hypso_median.RDS')

# Update hyspometric data table from parameters

df_hypso_cluster <- df_hypso_cluster %>% 
  left_join(df_hypso_median %>% 
              mutate(representative = 1),
            by = c('watershed', 'cluster', 'basin_id'))
df_hypso_cluster <- df_hypso_cluster %>% 
  mutate(representative = ifelse(is.na(representative), 0, representative))

df_hypso_cluster %>% 
  saveRDS('df_hypso_cluster2.RDS')

# S04-01 Plotting Hypsometry clusters ###################################################################################

clusGap_di %>%
  fviz_gap_stat_pra(paste0('Gap statistic for hypsometric clusters')) +
  geom_vline(xintercept = 8, linetype = 'dashed') +
  theme_pra_bw
dev.copy(png,
         foldered_png(paste0('Hypso - Gap statistics for Hypsometry for ', watershed_name_current)),
         width = 800,
         height = 600); dev.off()
if (output_pptx == 1)
{
  last_plot() %>% 
    create_pptx(pptx_file, width = 4, height = 3.5)
}

df_hypso_cluster %>% 
  filter(watershed == watershed_current) %>%
  mutate(basin_id = factor(basin_id)) %>%
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                group = basin_id),
            size = 0.1) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Relative Area above elevation',
       y = 'Relative Elevation',
       title = paste0('Hypsometric Clusters')) +
  theme_pra_bw +
  facet_wrap(~cluster, nrow = 2)
  # facet_wrap(~cluster, nrow = 3)
ggsave(filename = foldered_png(paste0('Hypso - Clusters Hypso for ', watershed_name_current)), width = 8, height = 6)
if (output_pptx == 1)
{
  # last_plot() %>% 
  #   create_pptx(pptx_file, width = 8, height = 6)
  last_plot() %>% 
    create_pptx(pptx_file, width = 9, height = 4)
}

df_di_lowest %>% 
  ggplot() +
  geom_line(aes(x = n_lowest,
                y = di_geometric_mean,
                group = basin_id_1),
            alpha = 0.3) +
  scale_x_continuous(breaks = 1:n_optimal) +
  labs(title = paste0('Geometric mean DI of Closest Neighbors'),
       x = 'Number of closest neighbors',
       y = 'Geometric mean of DI for neighbors') +
  theme_pra_bw +
  theme(panel.grid.minor.x = element_blank())
ggsave(foldered_png(paste0('Hypso - Geometric mean DI of Closest Neighbors for ', watershed_name_current)), width = 6, height = 4.5)
df_di_pairs %>% 
  filter(watershed == watershed_current) %>% 
  filter(di > 0) %>% 
  arrange(basin_id_1, di) %>% 
  group_by(basin_id_1) %>% 
  slice(1:n_lowest) %>% 
  ggplot(aes(x = basin_id_1,
             y = di)) +
  geom_point() +
  geom_line(aes(group = basin_id_1)) +
  scale_x_continuous(breaks = 1:n_max) +
  labs(x = 'Basin id',
       y = 'DI',
       title = 'DI for closest neighbors') +
  theme_pra_bw +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5))
ggsave(foldered_png(paste0('Hypso - DI for Closest Neighbors for ', watershed_name_current)), width = 12, height = 5.5)

df_hypso %>%
  left_join(df_hypso_cluster_outliers,
            by = c('watershed', 'basin_id')) %>% 
  filter(watershed == watershed_current) %>% 
  mutate(outlier = ifelse(basin_id %in% basin_id_outliers_di_original
                          , 'Outlier', 'Not Outlier')) %>% 
  mutate(basin_id = factor(basin_id)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                group = basin_id,
                color = outlier),
            size = 0.1) +
  scale_color_manual(name = '',
                     values = c('Outlier' = 'red',
                                'Not Outlier' = 'black')) +
  labs(x = 'Relative Area above elevation',
       y = 'Relative Elevation',
       title = paste0('Hypsometric Cluster for ', watershed_name_current)) +
  theme_pra_bw +
  facet_wrap(~cluster, nrow = 3)
ggsave(filename = foldered_png(paste0('Hypso - Clusters Outliers for ', watershed_name_current)), width = 8, height = 6)

df_cluster_di %>% 
  filter(basin_id == 16)
temp_clusters <- c(3, 1, 6, 8, 2, 4, 7, 5)
temp_colors <- c('#556b2f', '#191970', '#ff4500', '#ffd700', '#00ff7f', '#00bfff', '#0000ff', '#ff1493')
temp_color_df1 <- colorname_to_values(temp_colors, 1:8)
temp_color_df1
temp_color_dendro <- data.frame(temp_clusters) %>% 
  left_join(temp_color_df1,
            by = c('temp_clusters' = 'v_labels')) %>% 
  pull(v_colors) %>% 
  as.character()
clusters_di %>% 
  plot(main = paste0('Hypsometry Cluster Dendrogram'),
       sub = NA,
       xlab = NA)
rh <- clusters_di %>% 
  rect.hclust(k = 8, border = temp_color_dendro)
beg_clus <- head(cumsum(c(1, lengths(rh))), -1)
# y_clus <- weighted.mean(rev(clusters_di$height)[2:3], c(4, 1))
y_clus <- .25
text(x=beg_clus, y=y_clus, labels=temp_clusters, font=2)
# dev.copy(png,
#          foldered_png(paste0('Hypso - Dendrogram for Hypsometry for ', watershed_name_current)),
#          width = 850,
#          height = 400); dev.off()
if (output_pptx == 1)
{
  create_pptx_base(filename = pptx_base_file, width = 8.5*1.15, height = 4*1.15)
}

# df_cluster_di %>% 
#   ggplot() +
#   geom_bar(aes(x = cluster), color = 'black', fill = 'white') +
#   scale_x_continuous(breaks = 1:8) +
#   labs(x = 'Cluster',
#        y = 'Number of members') +
#   theme_pra_bw
df_cluster_di %>% 
  group_by(cluster) %>% 
  summarize(count = n())
df_cluster_di %>% 
  ggplot() +
  geom_bar(aes(x = cluster, color = as.factor(cluster)), fill = 'white', size = 0.75) +
  scale_x_continuous(breaks = 1:8) +
  scale_color_manual(values = temp_colors) +
  labs(x = 'Cluster',
       y = 'Number of members') +
  theme_pra_bw +
  theme(legend.position = 'none')
if (output_pptx == 1)
{
  last_plot() %>% 
    create_pptx(pptx_file, width = 3.5, height = 2.5)
}

df_hypso_mean %>% 
  filter(watershed == watershed_current) %>% 
  # mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y_mean, 
                color = cluster,
                group = cluster),
            size = 0.6) +
  scale_color_manual(values = temp_colors,
                     name = 'Cluster') +
  labs(x = 'Relative Area above Elevation',
       y = 'Relative Elevation',
       title = 'Mean Hypsometric Curve') +
  theme_pra_bw
ggsave(filename = foldered_png('Hypso - Clusters Mean Hypso'), width = 6, height = 4)
if (output_pptx == 1)
{
  last_plot() %>% 
    create_pptx(pptx_file, width = 4.5, height = 3)
}

df_hypso_cluster %>% 
  filter(watershed == watershed_current) %>% 
  mutate(representative = ifelse(representative == 1, 'representative', 'other')) %>% 
  mutate(representative = factor(representative, levels = c('representative', 'other'))) %>%
  mutate(cluster = factor(cluster)) %>% 
  arrange(desc(representative)) %>%
  unite('representative_basin_id' , c(representative, basin_id), remove = FALSE) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y, 
                color = representative,
                size = representative,
                group = representative_basin_id)) +
  scale_color_manual(name = '', 
                     values = c('representative' = 'black',
                                'other' = 'grey40')) +
  scale_size_manual(name = '', 
                    values = c('representative' = 1,
                               'other' = .1)) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Relative Area above Elevation',
       y = 'Relative Elevation',
       title = 'Representative Hypsometric Curve') +
  theme_pra_bw +
  theme(legend.position = 'none') +
  facet_wrap(~cluster, nrow = 2)
  # facet_wrap(~cluster, nrow = 3)
ggsave(filename = foldered_png('Hypso - Clusters Median Hypso'), width = 12, height = 8)
if (output_pptx == 1)
{
  # last_plot() %>% 
  #   create_pptx(pptx_file, width = 8, height = 6)
  last_plot() %>% 
    create_pptx(pptx_file, width = 9, height = 4)
}

df_hypso_cluster %>% 
  filter(watershed == watershed_current) %>% 
  filter(cluster == 7)
  

df_hypso_cluster %>% 
  filter(watershed == watershed_current) %>% 
  mutate(representative = ifelse(representative == 1, 'representative', 'other')) %>% 
  mutate(representative = factor(representative, levels = c('other', 'representative'))) %>%
  # mutate(representative = factor(representative, levels = c('representative', 'other'))) %>%
  mutate(cluster = factor(cluster)) %>% 
  arrange(desc(representative)) %>%
  unite('representative_basin_id' , c(representative, basin_id), remove = FALSE) %>% 
  mutate(representative_basin_id = factor(representative_basin_id, levels = unique(.$representative_basin_id))) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y, 
                color = representative,
                size = representative,
                group = representative_basin_id)) +
  scale_color_manual(name = '', 
                     values = c('representative' = 'grey70',
                                'other' = 'black')) +
  scale_size_manual(name = '', 
                    values = c('representative' = 4,
                               'other' = .2)) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Relative Area above Elevation',
       y = 'Relative Elevation',
       title = 'Representative Hypsometric Curve') +
  theme_pra_bw +
  theme(legend.position = 'none') +
  facet_wrap(~cluster, nrow = 2)
if (output_pptx == 1)
{
  # last_plot() %>% 
  #   create_pptx(pptx_file, width = 8, height = 6)
  last_plot() %>% 
    create_pptx(pptx_file, width = 9, height = 4)
}

df_hypso_cluster %>%
  filter(watershed == watershed_current) %>%
  mutate(representative = ifelse(representative == 1, 'representative', 'other')) %>%
  mutate(representative = factor(representative, levels = c('other', 'representative'))) %>%
  # mutate(representative = factor(representative, levels = c('representative', 'other'))) %>%
  mutate(cluster = factor(cluster)) %>%
  arrange(desc(representative)) %>%
  unite('representative_basin_id' , c(representative, basin_id), remove = FALSE) %>%
  mutate(representative_basin_id = factor(representative_basin_id, levels = unique(.$representative_basin_id))) %>%
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                color = representative,
                size = representative,
                group = representative_basin_id)) +
  scale_color_manual(name = '',
                     values = c('representative' = 'grey70',
                                'other' = 'black')) +
  scale_size_manual(name = '',
                    values = c('representative' = 4,
                               'other' = .2)) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Relative Area above Elevation',
       y = 'Relative Elevation',
       title = 'Representative Hypsometric Curve') +
  theme_pra_bw +
  # theme(legend.position = 'none') +
  facet_wrap(~cluster, nrow = 2)
if (output_pptx == 1)
{
  # last_plot() %>%
  #   create_pptx(pptx_file, width = 8, height = 6)
  last_plot() %>%
    create_pptx(pptx_file, width = 9, height = 4)
}

#
# S04-02 Width Function Parameters #####################################################################################

snorm_list <- list()
for (watershed_current in watersheds)
{
  for (basin_id_selected in basin_ids[[watershed_current]])
  {
    # basin_id_selected <- 1
    print (paste0('Working with Watershed  ', watershed_current, ' Basin ', basin_id_selected))
    
    data_unit <- df_basin_data %>%
      filter(watershed == watershed_current) %>% 
      filter(basin_id == basin_id_selected) %>%
      pull(chainage_scaled)
    
    for (mix_n in c(5))
    {
      print (paste0('Mix: ', mix_n))
      # snorm <- fit_sn(data_unit, n_mix = mix_n)
      
      snorm <- fit_sn(data_unit, n_mix = mix_n, domain_lower = 0, domain_upper = 1)
      snorm_list[[paste0(watershed_current, '_', basin_id_selected, '_', mix_n)]] = snorm
      print (paste0('Done! Iters: ', snorm$iter))
    }
  }
}

snorm_list %>%
  saveRDS('snorm_list_nmix5_01.RDS')

# S04-03 Width Function parameters to dataframe ########################################################################

df_sn_params <- list()
for (mix_n in c(1:3))
{
  df_sn_params[[mix_n]] <- data.frame()
}

# watershed_current <- 2
for (watershed_current in watersheds)
{
  for (basin_id_selected in basin_ids[[watershed_current]])
  {
    # basin_id_selected <- 1
    print (paste0('Working with Watershed  ', watershed_current, ' Basin ', basin_id_selected))
    
    for (mix_n in 1:3)
    {
      df_sn_params_temp <- cbind(data.frame(watershed = watershed_current,
                                            basin_id = basin_id_selected),
                                 get_sn_params_criteria(snorm_list[[paste0(watershed_current, '_', basin_id_selected,'_',mix_n)]]),
                                 data.frame(nontruncated = get_sn_prob(snorm_list[[paste0(watershed_current, '_', basin_id_selected,'_',mix_n)]])))
      df_sn_params[[mix_n]] <- rbind(df_sn_params[[mix_n]], df_sn_params_temp)
    }
  }
}

df_sn_params %>%
  saveRDS('snorm_params_df_nmix5_02.RDS')

# for (mix_n in 1:3)
# {
#   df_sn_params[[mix_n]] %>%
#     write_csv(paste0('SN',mix_n ,'_params.csv'))
# }

# S04-04 WF data table ##################################################################################################

df_sn_values <- list()
for (watershed_current in watersheds)
{
  for (basin_id_selected in basin_ids[[watershed_current]])
  {
    print (paste0('Working with Watershed ', watershed_current, ' basin ', basin_id_selected))
    
    data_unit <- df_basin_data %>%
      filter(watershed == watershed_current) %>% 
      filter(basin_id == basin_id_selected) %>%
      pull(chainage_scaled)
    
    mix_n <- 3
    df_sn_values[[paste0(watershed_current, '_', basin_id_selected,'_',mix_n)]] <- get_sn_xytable(snorm_list[[paste0(watershed_current, '_', basin_id_selected,'_',mix_n)]], min_x = -0.01, max_x = 1.01)
  }
}
df_sn_values %>% 
  saveRDS('snorm_values_df_nmix3_02.RDS')

mix_n <- 3
df_sn_values_main <- data.frame()
for (watershed_current in watersheds)
{
  for (basin_id_selected in basin_ids[[watershed_current]])
  {
    df_sn_values_main <- rbind(df_sn_values_main,
                               df_sn_values[[paste0(watershed_current, '_', basin_id_selected, '_', mix_n)]] %>% 
                                 select(x, y) %>% 
                                 mutate(watershed = watershed_current,
                                        basin_id = basin_id_selected))
  }
}
df_sn_values_main %>% 
  saveRDS('df_sn_values_main_nmix3_02.RDS')

# S04-05 Width Function distance matrix #################################################################################

# DI pairs for distance matrix

df_wf_pairs <- data.frame()
for (watershed_current in watersheds)
{
  for (basin_id_1 in basin_ids[[watershed_current]])
  {
    for (basin_id_2 in basin_ids[[watershed_current]])
    {
      print (paste0('Watershed ', watershed_current, ' Basins: ', basin_id_1, '-', basin_id_2))
      
      df_wf_pairs <- rbind(df_wf_pairs, 
                           data.frame(watershed = watershed_current, 
                                      basin_id_1, 
                                      basin_id_2, 
                                      l1 = get_pd_index_basin(basin_id_1, basin_id_2, watershed = watershed_current, index = 2),
                                      l2 = get_pd_index_basin(basin_id_1, basin_id_2, watershed = watershed_current)))
    }
  }
}
df_wf_pairs %>% 
  saveRDS('df_wf_pairs_nmix3_02.RDS')

# S04-06 Clustering with width function #################################################################################

# Clustering

m_l2_pairs <- list()
clusters_l2 <- list()
clusters_agnes_l2 <- list()

for (watershed_current in watersheds)
{
  m_l2_pairs[[watershed_current]] <- acast(df_wf_pairs %>% 
                                             filter(watershed == watershed_current), basin_id_1 ~ basin_id_2, value.var = 'l2')
  distance_matrix <- as.dist(m_l2_pairs[[watershed_current]])
  clusters_l2[[watershed_current]] <- hclust(distance_matrix, method = 'ward.D')
  clusters_agnes_l2[[watershed_current]] <- agnes(distance_matrix, diss = TRUE, method = 'ward')
}

m_l2_pairs  %>% 
  saveRDS('m_l2_pairs.RDS')
clusters_l2 %>% 
  saveRDS('clusters_l2_02.RDS')
clusters_agnes_l2 %>% 
  saveRDS('clusters_agnes_l2_02.RDS')

# Best Clustering

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
ac <- function(x, watershed) 
{
  agnes(m_l2_pairs[[watershed]], diss = TRUE, method = x)$ac
}
sapply(m, ac, 1)
sapply(m, ac, 2)
sapply(m, ac, 3)

# S04-07 Clustering groups and outliers analysis ########################################################################

watershed_current <- 1

watershed_name_current <- df_watersheds %>% 
  filter(watershed == watershed_current) %>% 
  pull(watershed_name)

df_wf_pairs %>% 
  filter(watershed == watershed_current) %>% 
  acast(basin_id_1 ~ basin_id_2, value.var = 'l2') %>% 
  as.dist() %>% 
  hclust(method = 'complete') %>% 
  plot(main = paste0('Cluster Dendrogram for ', watershed_name_current))
# dev.copy(png,
#          foldered_png(paste0('Dendrogram for Width Function (L2) for ', watershed_name_current)),
#          width = 1200,
#          height = 700); dev.off()
df_wf_pairs %>% 
  filter(watershed == watershed_current) %>% 
  acast(basin_id_1 ~ basin_id_2, value.var = 'l2') %>% 
  as.dist() %>% 
  hclust(method = 'ward.D2') %>% 
  plot(main = paste0('Cluster Dendrogram for ', watershed_name_current, ' (Ward Method)'))
# dev.copy(png,
#          foldered_png(paste0('Dendrogram for Width Function (L2) for ', watershed_name_current)),
#          width = 1200,
#          height = 700); dev.off()

# Gap statistics

clusGap_l2_original <- df_wf_pairs %>%
  filter(watershed == watershed_current) %>%
  acast(basin_id_1 ~ basin_id_2, value.var = 'l2') %>%
  clusGap(FUN = hcut, K.max = 15, B = 500)
clusGap_l2_original %>% 
  saveRDS('clusGap_l2_original.RDS')
clusGap_l2_original %>% 
  fviz_gap_stat()

# # Silhouette analysis
# 
# plot(2:20, sapply(2:20, function(i) { 
#   mean(silhouette(cutree(clusters_l2[[watershed_current]], i), dmatrix=m_l2_pairs[[watershed_current]])[,"sil_width"]) }),
#   main = paste0('Silhouette analysis for Width Function (L2) for ', watershed_name_current),
#   xlab="Number of clusters", 
#   ylab="Average Silhouette", 
#   type="b", 
#   pch=20)
# abline(v = 7, col = 'red')
# dev.copy(png,
#          foldered_png(paste0('Silhouette analysis for Width Function (L2) for ', watershed_name_current)),
#          width = 800,
#          height = 600); dev.off()

# Outliers

n_max <- df_wf_pairs %>% 
  filter(watershed == watershed_current) %>% 
  pull(basin_id_1) %>% 
  max()
n_optimal <- n_max %>% sqrt() %>% ceiling()
n_lowest <- 1
df_wf_l2_lowest <- data.frame()
for (n_lowest in 1:n_optimal)
{
  df_wf_l2_lowest <- rbind(df_wf_l2_lowest,
                           df_wf_pairs %>% 
                             filter(watershed == watershed_current) %>% 
                             filter(l2 > 0) %>% 
                             arrange(basin_id_1, l2) %>% 
                             group_by(basin_id_1) %>% 
                             slice(1:n_lowest) %>% 
                             mutate(l2_log = log(l2)) %>% 
                             summarise(l2_geometric_mean = sum(l2_log)) %>% 
                             mutate(l2_geometric_mean = (exp(l2_geometric_mean))^(1/n_lowest)) %>% 
                             mutate(n_lowest = n_lowest))
}
df_wf_l2_lowest %>% 
  saveRDS('df_wf_l2_lowest.RDS')
df_wf_l2_lowest %>% 
  ggplot() +
  geom_line(aes(x = n_lowest,
                y = l2_geometric_mean,
                group = basin_id_1),
            alpha = 0.3) +
  scale_x_continuous(breaks = 1:n_optimal) +
  labs(title = paste0('Geometric mean L2 of Closest Neighbors for ', watershed_name_current),
       x = 'Number of closest neighbors',
       y = 'Geometric mean of L2 distances for neighbors') +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank())
# ggsave(foldered_png(paste0('Geometric mean L2 of Closest Neighbors for ', watershed_name_current)), width = 6, height = 4.5)
df_wf_pairs %>% 
  filter(watershed == watershed_current) %>% 
  filter(l2 > 0) %>% 
  arrange(basin_id_1, l2) %>% 
  group_by(basin_id_1) %>% 
  slice(1:n_lowest) %>% 
  ggplot(aes(x = basin_id_1,
             y = l2)) +
  geom_point() +
  geom_line(aes(group = basin_id_1)) +
  scale_x_continuous(breaks = 1:n_max) +
  labs(x = 'Basin id',
       y = 'L2 distance',
       title = 'L2 distances to closest neighbors') +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5))
# ggsave(foldered_png(paste0('L2 for Closest Neighbors for ', watershed_name_current)), width = 12, height = 5.5)

n_lowest <- 15
basin_id_outliers_l2 <- df_wf_pairs %>% 
  filter(watershed == watershed_current) %>% 
  filter(l2 > 0) %>% 
  arrange(basin_id_1, l2) %>% 
  group_by(basin_id_1) %>% 
  slice(1:n_lowest) %>% 
  filter(l2 > 0.45) %>% 
  pull(basin_id_1) %>% 
  unique()
basin_id_outliers_l2 %>% 
  saveRDS('basin_id_outliers_l2.RDS')

temp_best_k <- 6

temp_clusters <- df_wf_pairs %>% 
  filter(watershed == watershed_current) %>% 
  # filter(!(basin_id_1 %in% temp_basin_ids_outliers)) %>%
  # filter(!(basin_id_2 %in% temp_basin_ids_outliers)) %>%
  acast(basin_id_1 ~ basin_id_2, value.var = 'l2') %>% 
  as.dist() %>% 
  # hclust(method = 'complete') %>%
  hclust(method = 'ward.D2') %>%
  cutree(k = temp_best_k)
df_clusters_wf_outliers <- data.frame(watershed = watershed_current,
                              basin_id = basin_ids[[watershed_current]],
                              cluster = temp_clusters)

df_clusters_wf_outliers %>% 
  saveRDS('df_clusters_wf_outliers.RDS')

df_sn_values_main %>%
  left_join(df_clusters_wf_outliers,
            by = c('watershed', 'basin_id')) %>%
  filter(watershed == watershed_current) %>%
  mutate(basin_id = factor(basin_id)) %>%
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                group = basin_id),
            size = 0.1) +
  labs(x = 'Hydrological Distance',
       y = '',
       title = paste0('Width Function L2 Cluster for ', watershed_name_current)) +
  theme_bw() +
  facet_wrap(~cluster, nrow = 3)
# ggsave(filename = foldered_png(paste0('Clusters WF1 L2 for ', watershed_name_current)), width = 8, height = 6)

df_sn_values_main %>%
  left_join(df_clusters_wf_outliers,
            by = c('watershed', 'basin_id')) %>% 
  filter(watershed == watershed_current) %>% 
  mutate(outlier = ifelse(basin_id %in% basin_id_outliers_l2
                         , 'Outlier', 'Not Outlier')) %>% 
  mutate(basin_id = factor(basin_id)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                group = basin_id,
                color = outlier),
            size = 0.1) +
  scale_color_manual(values = c('Outlier' = 'red',
                                'Not Outlier' = 'black')) +
  labs(x = 'Hydrological Distance',
       y = '',
       title = paste0('Width Function L2 Cluster for ', watershed_name_current)) +
  theme_bw() +
  facet_wrap(~cluster, nrow = 3)
# ggsave(filename = foldered_png(paste0('Clusters WF L2 for ', watershed_name_current)), width = 8, height = 6)

# Outliers removed

temp_basin_ids <- (basin_ids[[watershed_current]])[!(basin_ids[[watershed_current]] %in% basin_id_outliers_l2)]
clusters_l2 <- df_wf_pairs %>% 
  filter(watershed == watershed_current) %>% 
  filter(!(basin_id_1 %in% basin_id_outliers_l2
           )) %>%
  filter(!(basin_id_2 %in% basin_id_outliers_l2
           )) %>%
  acast(basin_id_1 ~ basin_id_2, value.var = 'l2') %>% 
  as.dist() %>% 
  # hclust(method = 'complete') %>%
  hclust(method = 'ward.D2')

df_cluster_l2 <- data.frame(watershed = watershed_current,
                              basin_id = temp_basin_ids,
                              cluster = cutree(clusters_l2, k = temp_best_k))

df_clusters_wf <- df_sn_values_main %>%
  # filter(!(basin_id %in% temp_basin_ids_outliers)) %>% 
  left_join(df_cluster_l2,
            by = c('watershed', 'basin_id'))

clusters_l2 %>% 
  saveRDS('clusters_l2_03.RDS')
df_cluster_l2 %>% 
  saveRDS('df_cluster_l2.RDS')
df_clusters_wf %>% 
  saveRDS('df_clusters_wf_03.RDS')

df_cluster_l2 %>% 
  filter(watershed == watershed_current) %>% 
  select(watershed:cluster) %>% 
  write_csv(foldered_csv('df_cluster_wf'))

df_clusters_wf %>% 
  filter(!(basin_id %in% basin_id_outliers_l2)) %>% 
  filter(watershed == watershed_current) %>% 
  mutate(basin_id = factor(basin_id)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                group = basin_id),
            size = 0.1) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Hydrological Distance',
       y = '',
       title = paste0('Width Function L2 Cluster (outlier remmoved) for ', watershed_name_current)) +
  theme_bw() +
  facet_wrap(~cluster, nrow = 3)
# ggsave(filename = foldered_png(paste0('Clusters WF L2 (outlier remmoved) for ', watershed_name_current)), width = 8, height = 6)

df_clusters_wf %>% 
  filter(watershed == watershed_current) %>% 
  mutate(basin_id = factor(basin_id)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                group = basin_id),
            size = 0.1) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Hydrological Distance',
       y = '',
       title = paste0('Width Function L2 Cluster (outlier remmoved) for ', watershed_name_current)) +
  theme_bw() +
  facet_wrap(~cluster, nrow = 3)
# ggsave(filename = foldered_png(paste0('Clusters WF L2 (outlier remmoved) for ', watershed_name_current)), width = 8, height = 6)

# Gap statistics

clusGap_l2 <- df_wf_pairs %>%
  filter(watershed == watershed_current) %>%
  filter(!(basin_id_1 %in% basin_id_outliers_l2)) %>%
  filter(!(basin_id_2 %in% basin_id_outliers_l2)) %>%
  acast(basin_id_1 ~ basin_id_2, value.var = 'l2') %>%
  clusGap(FUN = hcut, K.max = 15, B = 500)
clusGap_l2 %>% 
  saveRDS('clusGap_l2.RDS')
clusGap_l2 %>% 
  fviz_gap_stat()

clusGap_l2 %>%
  fviz_gap_stat_pra()
# dev.copy(png,
#          foldered_png(paste0('Gap statistics for Width Function (L2) (outlier removed) for ', watershed_name_current)),
#          width = 800,
#          height = 600); dev.off()

# # Silhouette analysis
# 
# plot(2:20, sapply(2:20, function(i) { 
#   mean(silhouette(df_wf_pairs %>% 
#                     filter(watershed == watershed_current) %>% 
#                     filter(!(basin_id_1 %in% basin_id_outliers_l2
#                              )) %>%
#                     filter(!(basin_id_2 %in% basin_id_outliers_l2
#                              )) %>%
#                     acast(basin_id_1 ~ basin_id_2, value.var = 'l2') %>% 
#                     as.dist() %>% 
#                     hclust(method = 'complete') %>%
#                     cutree(k = i), 
#                   dmatrix = df_wf_pairs %>% 
#                     filter(watershed == watershed_current) %>% 
#                     filter(!(basin_id_1 %in% basin_id_outliers_l2
#                              )) %>%
#                     filter(!(basin_id_2 %in% basin_id_outliers_l2
#                              )) %>%
#                     acast(basin_id_1 ~ basin_id_2, value.var = 'l2'))[,"sil_width"]) }),
#   main = paste0('Silhouette analysis for Width Function (L2) (outliers removed) for ', watershed_name_current),
#   xlab="Number of clusters", 
#   ylab="Average Silhouette", 
#   type="b", 
#   pch=20)
# abline(v = 7, col = 'red')
# dev.copy(png,
#          foldered_png(paste0('Silhouette analysis for Width Function (L2) (outliers removed) for ', watershed_name_current)),
#          width = 800,
#          height = 600); dev.off()

# Mean representative Hypsometric Curve

df_wf_mean <- df_clusters_wf %>% 
  filter(!is.na(cluster)) %>% 
  group_by(watershed, cluster, x) %>% 
  summarise(y_mean = mean(y)) %>% 
  ungroup()
df_wf_mean %>% 
  saveRDS('df_wf_mean.RDS')

df_wf_mean %>% 
  filter(watershed == watershed_current) %>% 
  # mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y_mean,
                group = cluster),
            size = 0.6) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Hydrological Distance',
       y = '',
       title = 'Mean Width Function') +
  theme_bw() +
  facet_wrap(~cluster)
# ggsave(filename = foldered_png('Clusters Mean WF'), width = 7, height = 4.8)

# Median representative Hypsometric Curve

df_wf_median <- data.frame()
for (watershed_current in c(2))#watersheds)
{
  # watershed_current <- 2
  
  for (cluster_current in df_wf_mean %>% filter(watershed == watershed_current) %>% pull(cluster) %>% unique())
  {
    # cluster_current <- 1
    
    temp_basin_ids <- df_cluster_wf %>%
      filter(watershed == watershed_current) %>%
      filter(cluster == cluster_current) %>% 
      pull(basin_id) %>% 
      unique()
    
    temp_lowest_l2 <- Inf
    temp_current_basin_id <- NA
    for (basin_id_current in temp_basin_ids)
    {
      # basin_id_current <- 1
      
      temp_delx <- df_clusters_wf %>%
        filter(watershed == watershed_current) %>%
        filter(basin_id == basin_id_current) %>% 
        mutate(delx = x - lag(x)) %>% 
        slice(2) %>% 
        pull(delx)
      
      temp_current_l2 <- get_pd_index_y(df_wf_mean %>% 
                                                filter(watershed == watershed_current) %>%
                                                filter(cluster == cluster_current) %>% 
                                                pull(y_mean),
                                        df_clusters_wf %>%
                                                filter(watershed == watershed_current) %>%
                                                filter(basin_id == basin_id_current) %>% 
                                                pull(y), 
                                        delx = temp_delx)
      
      if (temp_current_l2 < temp_lowest_l2)
      {
        temp_lowest_l2 = temp_current_l2
        temp_current_basin_id <- basin_id_current
      }
    }
    
    df_wf_median <- rbind(df_wf_median, data.frame(watershed = watershed_current,
                                                                             cluster = cluster_current,
                                                                             basin_id = temp_current_basin_id))
  }
}
df_wf_median %>% 
  saveRDS('df_wf_median.RDS')

# Update hyspometric data table from parameters

df_clusters_wf <- df_clusters_wf %>% 
  left_join(df_wf_median %>% 
              mutate(representative = 1),
            by = c('watershed', 'cluster', 'basin_id'))
df_clusters_wf <- df_clusters_wf %>% 
  mutate(representative = ifelse(is.na(representative), 0, representative))

df_clusters_wf %>% 
  saveRDS('df_clusters_wf_03.RDS')

df_clusters_wf %>% 
  filter(watershed == watershed_current) %>% 
  # mutate(representative = as.character(representative)) %>% 
  mutate(representative = ifelse(representative == 1, 'representative', 'other')) %>% 
  # filter(representative == 1) %>% 
  # mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y, 
                color = representative,
                size = representative,
                group = basin_id)) +
  scale_color_manual(name = '', 
                     values = c('representative' = 'black',
                                'other' = 'grey60')) +
  scale_size_manual(name = '', 
                     values = c('representative' = 1,
                                'other' = 0.1)) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Relative Area above Elevation',
       y = 'Relative Elevation',
       title = 'Representative Hypsometric Curve') +
  theme_bw() +
  facet_wrap(~cluster, nrow = 3)
# ggsave(filename = foldered_png('Clusters Representative WF'), width = 7, height = 4.8)

# S04-08 Plotting Width Function clusters ##############################################################################

df_clusters_wf_outliers <- readRDS('df_clusters_wf_outliers.RDS')

clusGap_l2_original %>%
  fviz_gap_stat_pra(paste0('Gap statistic for width functions')) +
  geom_vline(xintercept = 6, linetype = 'dashed')
dev.copy(png,
         foldered_png(paste0('WF - Gap statistics for WF for ', watershed_name_current)),
         width = 800,
         height = 600); dev.off()
if (output_pptx == 1)
{
  last_plot() %>% 
    create_pptx(pptx_file, width = 4, height = 3.5)
}

n_lowest <- 15
df_wf_l2_lowest %>% 
  ggplot() +
  geom_line(aes(x = n_lowest,
                y = l2_geometric_mean,
                group = basin_id_1),
            alpha = 0.3) +
  scale_x_continuous(breaks = 1:n_optimal) +
  labs(title = paste0('Geometric mean L2 of Closest Neighbors'),
       x = 'Number of closest neighbors',
       y = 'Geometric mean of L2 distances for neighbors') +
  theme_pra_bw +
  theme(panel.grid.minor.x = element_blank())
ggsave(foldered_png(paste0('WF - Geometric mean L2 of Closest Neighbors for ', watershed_name_current)), width = 6, height = 4.5)
df_wf_pairs %>% 
  filter(watershed == watershed_current) %>% 
  filter(l2 > 0) %>% 
  arrange(basin_id_1, l2) %>% 
  group_by(basin_id_1) %>% 
  slice(1:n_lowest) %>% 
  ggplot(aes(x = basin_id_1,
             y = l2)) +
  geom_point() +
  geom_line(aes(group = basin_id_1)) +
  scale_x_continuous(breaks = 1:n_max) +
  labs(x = 'Basin id',
       y = expression(paste(L[2],' distance'))) +#,
       # title = 'L2 distances to closest neighbors') +
  theme_pra_bw +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5))
ggsave(foldered_png(paste0('WF - L2 for Closest Neighbors for ', watershed_name_current)), width = 12, height = 5.5)
ggsave(foldered_png(paste0('WF - L2 for Closest Neighbors for ', watershed_name_current, ' 2')), width = 11, height = 3.9)

df_sn_values_main %>%
  left_join(df_clusters_wf_outliers,
            by = c('watershed', 'basin_id')) %>%
  filter(watershed == watershed_current) %>%
  mutate(basin_id = factor(basin_id)) %>%
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                group = basin_id),
            size = 0.1) +
  labs(x = 'Hydrological Distance',
       y = '',
       title = paste0('Width Function Clusters')) +
  theme_pra_bw +
  facet_wrap(~cluster, nrow = 2)
# facet_wrap(~cluster, nrow = 3)
ggsave(filename = foldered_png(paste0('WF - Clusters WF Original for ', watershed_name_current)), width = 8, height = 6)
if (output_pptx == 1)
{
  # last_plot() %>% 
  #   create_pptx(pptx_file, width = 6, height = 6)
  last_plot() %>% 
    create_pptx(pptx_file, width = 9, height = 4)
}

df_sn_values_main %>%
  left_join(df_clusters_wf_outliers,
            by = c('watershed', 'basin_id')) %>% 
  filter(watershed == watershed_current) %>% 
  mutate(outlier = ifelse(basin_id %in% basin_id_outliers_l2
                          , 'Outlier', 'Not Outlier')) %>% 
  mutate(basin_id = factor(basin_id)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                group = basin_id,
                color = outlier),
            size = 0.1) +
  scale_color_manual(name = '',
                     values = c('Outlier' = 'red',
                                'Not Outlier' = 'black')) +
  labs(x = 'Hydrological Distance',
       y = '',
       title = paste0('Width Function Cluster Outliers')) +
  theme_pra_bw +
  theme(legend.position = 'none') +
  facet_wrap(~cluster, nrow = 3)
ggsave(filename = foldered_png(paste0('WF - Clusters WF with outliers for ', watershed_name_current)), width = 8, height = 6)
if (output_pptx == 1)
{
  last_plot() %>% 
    create_pptx(pptx_file, width = 6, height = 6)
}

df_clusters_wf %>% 
  filter(!(basin_id %in% basin_id_outliers_l2)) %>% 
  filter(watershed == watershed_current) %>% 
  mutate(basin_id = factor(basin_id)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                group = basin_id),
            size = 0.1) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Hydrological Distance',
       y = '',
       title = paste0('Width Function Clusters (outlier removed)')) +
  theme_pra_bw +
  facet_wrap(~cluster, nrow = 2)
# facet_wrap(~cluster, nrow = 3)
ggsave(filename = foldered_png(paste0('WF - Clusters WF (outlier remmoved) for ', watershed_name_current)), width = 8, height = 6)
if (output_pptx == 1)
{
  # last_plot() %>% 
  #   create_pptx(pptx_file, width = 6, height = 6)
  last_plot() %>% 
    create_pptx(pptx_file, width = 9, height = 4)
}

df_cluster_l2 %>% 
  filter(basin_id == 26)
temp_clusters <- c(6, 2, 5, 1, 4, 3)
temp_colors <- c('#66cdaa', '#ff8c00', '#00ff00', '#0000ff', '#1e90ff', '#ff1493')
temp_color_df1 <- colorname_to_values(temp_colors, 1:6)
temp_color_df1
temp_color_dendro <- data.frame(temp_clusters) %>% 
  left_join(temp_color_df1,
            by = c('temp_clusters' = 'v_labels')) %>% 
  pull(v_colors) %>% 
  as.character()
clusters_l2 %>% 
  plot(main = paste0('Width Function Cluster Dendrogram'),
       sub = NA,
       xlab = NA)
temp_rh <- clusters_l2 %>% 
  rect.hclust(k = 6, border = temp_color_dendro)
beg_clus <- head(cumsum(c(1, lengths(temp_rh))), -1)
# y_clus <- weighted.mean(rev(clusters_di$height)[2:3], c(4, 1))
y_clus <- 1.0
text(x=beg_clus, y=y_clus, labels=temp_clusters, font=2)
# dev.copy(png,
#          foldered_png(paste0('WF - Dendrogram for WF for ', watershed_name_current)),
#          width = 850,
#          height = 400); dev.off()
if (output_pptx == 1)
{
  create_pptx_base(filename = pptx_base_file, width = 8.5*1.15, height = 4*1.15)
}

# df_cluster_l2 %>% 
#   ggplot() +
#   geom_bar(aes(x = cluster), color = 'black', fill = 'white') +
#   scale_x_continuous(breaks = 1:6) +
#   labs(x = 'Cluster',
#        y = 'Number of members') +
#   theme_pra_bw
df_cluster_l2 %>% 
  group_by(cluster) %>% 
  summarize(count = n())
df_cluster_l2 %>% 
  ggplot() +
  geom_bar(aes(x = cluster, color = as.factor(cluster)), fill = 'white', size = 0.75) +
  scale_x_continuous(breaks = 1:6) +
  scale_color_manual(values = temp_colors) +
  labs(x = 'Cluster',
       y = 'Number of members') +
  theme_pra_bw +
  theme(legend.position = 'none')
if (output_pptx == 1)
{
  last_plot() %>% 
    create_pptx(pptx_file, width = 2.8, height = 2.5)
}

clusGap_l2 %>%
  fviz_gap_stat_pra(paste0('Gap statistic for width functions (outliers removed)')) +
  geom_vline(xintercept = 6, linetype = 'dashed')
dev.copy(png,
         foldered_png(paste0('WF - Gap statistics for WF (outliers removed) for ', watershed_name_current)),
         width = 800,
         height = 600); dev.off()
if (output_pptx == 1)
{
  last_plot() %>% 
    create_pptx(pptx_file, width = 4, height = 3.5)
}

df_wf_mean %>% 
  filter(watershed == watershed_current) %>% 
  # mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y_mean,
                group = cluster,
                color = cluster),
            size = 0.6) +
  scale_color_manual(values = temp_colors,
                     name = 'Cluster') +
  labs(x = 'Hydrological Distance',
       y = '',
       title = 'Mean Width Function') +
  theme_pra_bw
  # facet_wrap(~cluster)
ggsave(filename = foldered_png('WF - Clusters Mean WF'), width = 7, height = 4.8)
if (output_pptx == 1)
{
  last_plot() %>% 
    create_pptx(pptx_file, width = 4.5, height = 3)
}

df_clusters_wf %>% 
  filter(!(basin_id %in% basin_id_outliers_l2)) %>% 
  filter(watershed == watershed_current) %>% 
  mutate(representative = ifelse(representative == 1, 'representative', 'other')) %>% 
  mutate(representative = factor(representative, levels = c('representative', 'other'))) %>%
  mutate(cluster = factor(cluster)) %>% 
  arrange(desc(representative)) %>%
  unite('representative_basin_id' , c(representative, basin_id), remove = FALSE) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y, 
                color = representative,
                size = representative,
                group = representative_basin_id)) +
  scale_color_manual(name = '', 
                     values = c('representative' = 'black',
                                'other' = 'grey60')) +
  scale_size_manual(name = '', 
                    values = c('representative' = 1,
                               'other' = 0.1)) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Hydrological Distance',
       y = '',
       title = 'Representative Width Function') +
  theme_pra_bw +
  theme(legend.position = 'none') +
  facet_wrap(~cluster, nrow = 2)
# facet_wrap(~cluster, nrow = 3)
ggsave(filename = foldered_png('WF - Clusters Median WF'), width = 7, height = 4.8)
if (output_pptx == 1)
{
  # last_plot() %>% 
  #   create_pptx(pptx_file, width = 6, height = 6)
  last_plot() %>% 
    create_pptx(pptx_file, width = 9, height = 4)
}

temp <- df_clusters_wf %>% 
  # filter(cluster == 1) %>%
  filter(!(basin_id %in% basin_id_outliers_l2)) %>% 
  filter(watershed == watershed_current) %>% 
  mutate(cluster = factor(cluster)) %>%
  # arrange(representative) %>%
  arrange(desc(representative), basin_id) %>%
  unite('representative_basin_id' , c(representative, basin_id), remove = FALSE) %>%
  mutate(representative = ifelse(representative == 1, 'representative', 'other')) %>% 
  # mutate(representative = factor(representative, levels = c('other', 'representative'))) %>%
  mutate(representative = factor(representative, levels = c('representative', 'other')))
temp <- df_clusters_wf %>% 
  # filter(cluster == 1) %>%
  filter(!(basin_id %in% basin_id_outliers_l2)) %>% 
  filter(watershed == watershed_current) %>% 
  mutate(cluster = factor(cluster)) %>%
  # arrange(representative) %>%
  arrange(desc(representative), basin_id) %>%
  unite('representative_basin_id' , c(representative, basin_id), remove = FALSE) %>%
  mutate(representative = ifelse(representative == 1, 'representative', 'other')) %>% 
  mutate(representative = factor(representative, levels = c('representative', 'other'))) %>%
  mutate(representative_basin_id = factor(representative_basin_id, levels = unique(.$representative_basin_id)))
temp$representative_basin_id
temp$representative

df_clusters_wf %>% 
  # filter(cluster == 1) %>%
  filter(!(basin_id %in% basin_id_outliers_l2)) %>% 
  filter(watershed == watershed_current) %>% 
  mutate(cluster = factor(cluster)) %>%
  # arrange(representative) %>%
  arrange(desc(representative), basin_id) %>%
  unite('representative_basin_id' , c(representative, basin_id), remove = FALSE) %>%
  mutate(representative = ifelse(representative == 1, 'representative', 'other')) %>% 
  mutate(representative = factor(representative, levels = c('representative', 'other'))) %>%
  mutate(representative_basin_id = factor(representative_basin_id, levels = unique(.$representative_basin_id))) %>% 
  # mutate(representative = factor(representative, levels = c('other', 'representative'))) %>%
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                group = representative_basin_id,
                # group = basin_id,
                color = representative,
                size = representative)) +
  scale_color_manual(name = '',
                     values = c('representative' = 'grey70',
                                'other' = 'black')) +
                     # breaks = c('other','representative')) +
  scale_size_manual(name = '',
                    values = c('representative' = 4,
                               'other' = 0.2)) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Hydrological Distance',
       y = '',
       title = 'Representative Width Function') +
  theme_pra_bw +
  theme(legend.position = 'none') +
  facet_wrap(~cluster, nrow = 2)
# facet_wrap(~cluster, nrow = 3)
ggsave(filename = foldered_png('WF - Clusters Median WF'), width = 7, height = 4.8)
if (output_pptx == 1)
{
  # last_plot() %>% 
  #   create_pptx(pptx_file, width = 6, height = 6)
  last_plot() %>% 
    create_pptx(pptx_file, width = 9, height = 4)
}

# df_clusters_wf %>% 
#   # filter(cluster == 1) %>%
#   filter(!(basin_id %in% basin_id_outliers_l2)) %>% 
#   filter(watershed == watershed_current) %>% 
#   mutate(representative = ifelse(representative == 1, 'representative', 'other')) %>% 
#   mutate(representative = factor(representative, levels = c('representative', 'other'))) %>%
#   # mutate(representative = factor(representative, levels = c('other', 'representative'))) %>%
#   mutate(cluster = factor(cluster)) %>%
#   # arrange(representative) %>%
#   arrange(desc(representative)) %>%
#   # unite('representative_basin_id' , c(representative, basin_id), remove = FALSE) %>%
#   ggplot() +
#   geom_line(data = subset(., representative == 'other'),
#             mapping = aes(x = x,
#                 y = y,
#                 # group = representative_basin_id, 
#                 group = basin_id,
#                 color = representative,
#                 size = representative)) +
#   scale_color_manual(name = '',
#                      values = c('representative' = 'grey60',
#                                 'other' = 'black')) +
#   # breaks = c('other','representative')) +
#   scale_size_manual(name = '',
#                     values = c('representative' = 4,
#                                'other' = 0.2)) +
#   # scale_color_brewer(palette = 'Set1',
#   #                    name = 'Basin id') +
#   labs(x = 'Hydrological Distance',
#        y = '',
#        title = 'Representative Width Function') +
#   theme_pra_bw +
#   theme(legend.position = 'none') +
#   facet_wrap(~cluster, nrow = 2)
# # facet_wrap(~cluster, nrow = 3)
# ggsave(filename = foldered_png('WF - Clusters Median WF'), width = 7, height = 4.8)
# if (output_pptx == 1)
# {
#   # last_plot() %>% 
#   #   create_pptx(pptx_file, width = 6, height = 6)
#   last_plot() %>% 
#     create_pptx(pptx_file, width = 9, height = 4)
# }

#
# S05-01 Combine Hypso Cluster and WF Cluster ##########################################################################

watershed_current <- 1
df_cluster_hypso_wf <- df_cluster_di %>% 
  filter(watershed == watershed_current) %>% 
  rename(cluster_hypso = cluster) %>% 
  left_join(df_cluster_l2 %>% 
              filter(watershed == watershed_current) %>% 
              rename(cluster_wf_l2 = cluster),
            by = c('watershed', 'basin_id')) %>% 
  unite('cluster_hypso_wf_l2', c(cluster_hypso, cluster_wf_l2), remove = FALSE)
df_cluster_hypso_wf %>% 
  saveRDS('df_cluster_hypso_wf_02.RDS')

# S05-02 Entanglement between DI and L2 clusters #######################################################################

# Entanglement Plots

watershed_current <- 1

watershed_name_current <- df_watersheds %>% 
  filter(watershed == watershed_current) %>% 
  pull(watershed_name)

dend_di <- clusters_di %>% as.dendrogram()
dend_l2 <- clusters_l2 %>% as.dendrogram()

dl <- dendlist(dend_di, dend_l2)
# png(paste0('Entanglement_', watershed_name_current, '_di_l2_untangled.png'), width = 1200, height = 1200)
tanglegram(dl)
# dev.off()
# png(paste0('Entanglement_', watershed_name_current, '_di_l2_step1side.png'), width = 1200, height = 1200)
dendlist(dend_di, dend_l2) %>%
  untangle(method = "step1side") %>% # Find the best alignment layout
  tanglegram()  
# dev.off()

# Entangle spreadsheet

df_cluster_hypso_wf %>% 
  write_csv(foldered_csv('cluster_hypso_wf'))

# Entangled group member count

df_cluster_hypso_wf %>% 
  group_by(cluster_hypso_wf_l2) %>% 
  summarise(total = n()) %>% 
  arrange(total) %>% 
  mutate(cluster_hypso_wf_l2 = factor(cluster_hypso_wf_l2, ordered = TRUE, levels = .$cluster_hypso_wf_l2)) %>% 
  ggplot() +
  geom_bar(aes(x = cluster_hypso_wf_l2, y = total), stat = 'identity') +
  scale_y_continuous(breaks = seq(0,10,1)) +
  labs(x = 'Cluster: Hypso_Width Function (L2)',
       y = 'Number of Members',
       title = 'Clustering Intersection between Hypso and Width Function (L2)') +
  coord_flip() +
  theme_pra_bw
# ggsave(filename = foldered_png('Cluster Count Hypso WF L2'), width = 6, height = 6)

# Entanglement cross plot

temp_df_cluster_hypso_wf <- df_cluster_hypso_wf %>% 
  filter(!is.na(cluster_wf_l2)) %>% 
  mutate(x_jitter = cluster_hypso,
         y_jitter = cluster_wf_l2)
g_cross_cluster <- temp_df_cluster_hypso_wf %>% 
  create_jitter_from_df2() %>% 
  left_join(df_basin_data_summary %>% select(watershed:area_gis_km2),
            by = c('watershed', 'basin_id')) %>% 
  ggplot(aes(x = x_jittered, 
             y = y_jittered)) +
  geom_point(aes(size = area_gis_km2),
             color = 'red',
             alpha = 0.25) +
  geom_text(aes(label = basin_id),
            size = 4,
            color = 'black') +
  scale_x_continuous(breaks = seq(min(temp_df_cluster_hypso_wf$x_jitter), max(temp_df_cluster_hypso_wf$x_jitter), 1)) +
  scale_y_continuous(breaks = seq(min(temp_df_cluster_hypso_wf$y_jitter), max(temp_df_cluster_hypso_wf$y_jitter), 1)) +
  scale_size_continuous(name = 'Area (km2)',
                        range = c(2.5, 7)) +
  coord_fixed(xlim = c(min(temp_df_cluster_hypso_wf$x_jitter)-.5, max(temp_df_cluster_hypso_wf$x_jitter)+.5),
              ylim = c(min(temp_df_cluster_hypso_wf$y_jitter)-.5, max(temp_df_cluster_hypso_wf$y_jitter)+.5)) +
  # coord_cartesian(xlim = c(0, 10)) +
  labs(x = 'Hypsometry cluster',
       y = 'Width function cluster') +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = 'grey70'),
        legend.position = 'bottom'); g_cross_cluster
g_cross_cluster %>% 
  ggsave(filename = foldered_png('X_Hypso_WF - Crossplot'), width = 8, height = 6)

g_cross_cluster <- temp_df_cluster_hypso_wf %>% 
  create_jitter_from_df2() %>% 
  left_join(df_basin_data_summary %>% select(watershed:area_gis_km2),
            by = c('watershed', 'basin_id')) %>% 
  ggplot(aes(x = x_jittered, 
             y = y_jittered)) +
  geom_point(aes(size = area_gis_km2),
             color = 'red',
             alpha = 0.25) +
  geom_text(aes(size = area_gis_km2,
                label = basin_id),
            color = 'black') +
  scale_x_continuous(breaks = seq(min(temp_df_cluster_hypso_wf$x_jitter), max(temp_df_cluster_hypso_wf$x_jitter), 1)) +
  scale_y_continuous(breaks = seq(min(temp_df_cluster_hypso_wf$y_jitter), max(temp_df_cluster_hypso_wf$y_jitter), 1)) +
  scale_size_continuous(name = 'Area (km2)',
                        range = c(2.5, 7)) +
  coord_fixed(xlim = c(min(temp_df_cluster_hypso_wf$x_jitter)-.5, max(temp_df_cluster_hypso_wf$x_jitter)+.5),
              ylim = c(min(temp_df_cluster_hypso_wf$y_jitter)-.5, max(temp_df_cluster_hypso_wf$y_jitter)+.5)) +
  # coord_cartesian(xlim = c(0, 10)) +
  labs(x = 'Hypsometry cluster',
       y = 'Width function cluster') +
  # theme_bw() +
  theme_pra_bw +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = 'grey70'),
        legend.position = 'bottom'); g_cross_cluster
g_cross_cluster %>% 
  create_pptx(pptx_file, width = 6.5, height = 6.5)
g_cross_cluster %>% 
  ggsave(filename = foldered_png('X_Hypso_WF - Crossplot2'), width = 8, height = 6)

temp_g <- df_hypso_mean %>% 
  filter(watershed == watershed_current) %>% 
  # mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y_mean, 
                # color = cluster,
                group = cluster),
            size = 0.6) + 
  labs(x = '',
       y = '',
       title = '') +
  facet_wrap(~cluster, nrow = 1) +
  # theme_bw(); temp_g
  theme_skeleton; temp_g
temp_g %>% 
  create_pptx(pptx_file, width = 6.5, height = 2)

temp_g <- df_wf_mean %>% 
  filter(watershed == watershed_current) %>% 
  # mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y_mean,
                # color = cluster,
                group = cluster),
            size = 0.6) +
  labs(x = '',
       y = '',
       title = '') +
  facet_wrap(~cluster, ncol = 1) +
  # theme_bw(); temp_g
  theme_skeleton; temp_g
temp_g %>% 
  create_pptx(pptx_file, width = 1.6, height = 5.05)



df_cluster_hypso_wf %>% 
  group_by(cluster_hypso_wf_l2) %>% 
  summarize(count = n()) %>% 
  arrange(count)


# Entanglement cross plot 2

temp_df_cluster_hypso_wf2 <- df_cluster_hypso_wf %>% 
  filter(!is.na(cluster_wf_l2)) %>% 
  mutate(x_jitter = cluster_hypso,
         y_jitter = cluster_wf_l2) %>% 
  left_join(df_basin_data_summary %>% select(watershed:area_gis_km2),
            by = c('watershed', 'basin_id'))

temp_df_cluster_hypso_wf2 %>% 
  head()

g_cross_cluster2 <- temp_df_cluster_hypso_wf2 %>% 
  group_by(cluster_hypso_wf_l2) %>%
  mutate(x_jittered = x_jitter + (area_gis_km2-min(area_gis_km2))/(max(area_gis_km2)-min(area_gis_km2))*0.8 - 0.4) %>% 
  mutate(y_jittered = y_jitter + (area_gis_km2-min(area_gis_km2))/(max(area_gis_km2)-min(area_gis_km2))*0.8 - 0.4) %>%
  mutate(x_jittered = ifelse(is.nan(x_jittered), x_jitter, x_jittered)) %>% 
  mutate(y_jittered = ifelse(is.nan(y_jittered), y_jitter, y_jittered)) %>% 
  ggplot(aes(x = x_jittered, 
             y = y_jittered)) +
  # geom_point(aes(size = area_gis_km2),
  #            color = 'red',
  #            alpha = 0.25) +
  geom_text(aes(label = basin_id),
            size = 3,
            color = 'black') +
  scale_x_continuous(breaks = seq(min(temp_df_cluster_hypso_wf$x_jitter), max(temp_df_cluster_hypso_wf$x_jitter), 1)) +
  scale_y_continuous(breaks = seq(min(temp_df_cluster_hypso_wf$y_jitter), max(temp_df_cluster_hypso_wf$y_jitter), 1)) +
  # scale_size_continuous(name = 'Area (km2)',
  #                       range = c(2, 5)) +
  coord_fixed(xlim = c(min(temp_df_cluster_hypso_wf$x_jitter)-.5, max(temp_df_cluster_hypso_wf$x_jitter)+.5),
              ylim = c(min(temp_df_cluster_hypso_wf$y_jitter)-.5, max(temp_df_cluster_hypso_wf$y_jitter)+.5)) +
  # coord_cartesian(xlim = c(0, 10)) +
  labs(x = 'Hypsometry cluster',
       y = 'Width function cluster') +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = 'grey70'),
        legend.position = 'bottom'); g_cross_cluster2

# g_cross_cluster2 %>% 
#   ggsave(filename = foldered_png('X_Hypso_WF - Crossplot (Diag 1)'), width = 8, height = 6)
g_cross_cluster2 %>% 
  ggsave(filename = foldered_png('X_Hypso_WF - Crossplot (Diag 2)'), width = 8, height = 6)

### STOP HERE FOR NOW

g_cross_cluster %>% 
  create_pptx(pptx_file, width = 6.5, height = 6.5)

temp_g <- df_hypso_cluster %>% 
  filter(watershed == watershed_current) %>% 
  filter(representative == 1) %>% 
  # mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y),
            size = 0.01) +
  labs(x = '',
       y = '',
       title = '') +
  facet_wrap(~cluster, nrow = 1) +
  theme_skeleton; temp_g
temp_g %>% 
  create_pptx(pptx_file, width = 6.5, height = 2)
temp_g <- df_sn_values_main %>%
  filter(watershed == watershed_current) %>% 
  filter(representative_l2 == 1) %>% 
  # mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster_l2, ordered = TRUE, levels = df_sn_values_main$cluster_l2 %>% unique() %>% rev())) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y),
            size = .5) +
  labs(x = '',
       y = '',
       title = '') +
  facet_wrap(~cluster, ncol = 1) +
  # theme_bw(); temp_g
  theme_skeleton; temp_g
temp_g %>% 
  create_pptx(pptx_file, width = 1.6, height = 5.05)
temp_g <- df_sn_values_main %>%
  filter(watershed == watershed_current) %>% 
  filter(representative_l1 == 1) %>% 
  # mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster_l1, ordered = TRUE, levels = df_sn_values_main$cluster_l1 %>% unique())) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y),
            size = 0.5) +
  labs(x = '',
       y = '',
       title = '') +
  facet_wrap(~cluster, nrow = 1) +
  theme_skeleton; temp_g
temp_g %>% 
  create_pptx(pptx_file, width = 5.05, height = 1.6)
