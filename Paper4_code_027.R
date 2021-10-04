# Setup #########################################################################################################

library(tidyverse)
library(gridExtra)
select <- dplyr::select

library(raster)
library(reticulate)
# library(princurve, lib = 'D:\\Unmanaged\\Rlib')
library(princurve)

library(cluster)
library(factoextra)
library(dendextend)
library(reshape2)

source("D:/Project/Research/Scripts/R Codes/ggplot_themes.R")
source("D:/Project/Research/Scripts/R Codes/DecomposablePlots.R")
source("D:/Project/Research/Scripts/R Codes/SMSN_MIX_Pra.R")
source("D:/Project/Research/Scripts/R Codes/functions_supplementary.R")

library(minpack.lm)

setwd("D:/Project/Research/Paper 4 Parameters")
output_folder <- "D:/Project/Research/Paper 4 Parameters/Outputs/"

pptx_file <- paste0(output_folder, 'Plots.pptx')
pptx_base_file <- paste0(output_folder, 'PlotsBase.pptx')
output_pptx <- 1

# Functions  ####################################################################################################

scaleFun <- function(x)
{
  y <- vector()
  for (x_each in x)
  {
    y <- c(y, as.character(x_each))
  }
  return (y)
}

# Folder for saving
foldered <- function(filename, output_foldername = output_folder)
{
  paste0(output_foldername, filename)
}

# Folder for saving png
foldered_png <- function(filename, output_foldername = output_folder)
{
  paste0(output_foldername, filename, '.png')
}

# Folder for saving pdf
foldered_pdf <- function(filename, output_foldername = output_folder)
{
  paste0(output_foldername, filename, '.pdf')
}

# Folder for saving csv
foldered_csv <- function(filename, output_foldername = output_folder)
{
  paste0(output_foldername, filename, '.csv')
}

# Change elevation vector to hypsometric dataframe
elevation_to_hypso <- function(data_elevation_raw)
{
  df_hypso <- tibble(elevation = data_elevation_raw %>% unique)
  df_hypso <- df_hypso %>% 
    arrange(desc(elevation))
  df_hypso$area_above_elevation <- NA
  for (elev in df_hypso$elevation)
  {
    # elev <- df_hypso$elevation[2]
    df_hypso[df_hypso$elevation == elev,]$area_above_elevation = sum(elev < data_elevation_raw)
  }
  df_hypso <- df_hypso %>% 
    mutate(relative_elevation = (elevation-min(elevation))/(max(elevation)-min(elevation)),
           relative_area_above_elevation = area_above_elevation/max(area_above_elevation))
  return(df_hypso)
}

# Get hypsometric dataframe for given basin
elevation_to_hypso_basin <- function(basin_id_, watershed_ = 1)
{
  data_elevation_raw <- df_basin_data %>% 
    filter(watershed == watershed_) %>% 
    filter(basin_id == basin_id_) %>% 
    pull(elevation_m)
  
  return(elevation_to_hypso(data_elevation_raw))
}

# Try different formulations for hypsometric curve fitting
hypso_nls <- function(nls_formula, nls_formula_start, nls_formula_lower = NA, nls_formula_upper = NA, max_iter = 100)
{
  if (is.na(nls_formula_lower))
  {
    nls_formula_lower <- rep(-Inf, length(nls_formula_start))
  }
  if (is.na(nls_formula_upper))
  {
    nls_formula_upper <- rep(Inf, length(nls_formula_start))
  }
  
  list_hypso_nls_fit <- list()
  df_hypso <- data.frame()
  df_hypso_summary <- data.frame()
  
  set.seed(20160227)
  for(watershed_current in watersheds)
  {
    # watershed_current <- watersheds[2]
    for (basin_id_current in basin_ids[[watershed_current]])
    {
      # basin_id_current <- basin_ids[1]
      print (paste0(watershed_current, '-', basin_id_current))
      
      temp_df_hypso <- elevation_to_hypso_basin(basin_id_current, watershed_current)
      temp_df_hypso$watershed <- watershed_current
      temp_df_hypso$basin_id <- basin_id_current
      
      x <- temp_df_hypso$relative_area_above_elevation
      y <- temp_df_hypso$relative_elevation
      
      temp_df <- data.frame(x, y)
      
      # nls_fit <- nlsLM(formula = as.formula(nls_formula),
      #                  data = temp_df,
      #                  start = nls_formula_start,
      #                  lower = nls_formula_lower,
      #                  upper = nls_formula_upper,
      #                  control = nls.lm.control(maxiter = max_iter))
      nls_fit <- tryCatch(nlsLM(formula = nls_formula,
                                data = temp_df,
                                start = nls_formula_start,
                                lower = nls_formula_lower,
                                upper = nls_formula_upper,
                                control = nls.lm.control(maxiter = max_iter)),
                          error = function(e) NA)
      # print (nls_fit)
      
      list_hypso_nls_fit[[paste0(watershed_current, '-', basin_id_current)]] <- nls_fit
      
      if (!is.na(nls_fit))
      {
        temp_df_hypso$relative_elevation_predicted <- predict(nls_fit, newdata = data.frame(R = x))
        
        coeff1 <- ifelse(length(nls_formula_start) >= 1, summary(nls_fit)$coefficients[1], NA)
        coeff2 <- ifelse(length(nls_formula_start) >= 2, summary(nls_fit)$coefficients[2], NA)
        coeff3 <- ifelse(length(nls_formula_start) >= 3, summary(nls_fit)$coefficients[3], NA)
        coeff4 <- ifelse(length(nls_formula_start) >= 4, summary(nls_fit)$coefficients[4], NA)
        coeff5 <- ifelse(length(nls_formula_start) >= 5, summary(nls_fit)$coefficients[5], NA)
        
        convergence_check <- summary(nls_fit)$convInfo$isConv
        convergence_tolerance_achieved <- summary(nls_fit)$convInfo$finTol
        residual_sum_of_square <- sum((summary(nls_fit)$residuals)^2)
        rmse <- sqrt(residual_sum_of_square/length(summary(nls_fit)$residuals))
      } else
      {
        temp_df_hypso$relative_elevation_predicted <- NA
        
        coeff1 <- NA
        coeff2 <- NA
        coeff3 <- NA
        coeff4 <- NA
        coeff5 <- NA
        
        convergence_check <- NA
        convergence_tolerance_achieved <- NA
        residual_sum_of_square <- NA
        rmse <- NA
      }
      df_hypso <- rbind(df_hypso, temp_df_hypso)
      df_hypso_summary <- rbind(df_hypso_summary, data.frame(watershed = watershed_current,
                                                             basin_id = basin_id_current, 
                                                             coeff1, coeff2, coeff3, coeff4, coeff5,
                                                             convergence_check, convergence_tolerance_achieved, residual_sum_of_square, rmse))
    }
  }
  
  return(list(list_hypso_nls_fit = list_hypso_nls_fit, 
              df_hypso = df_hypso, 
              df_hypso_summary = df_hypso_summary))
}

find_di <- function(watershed = 1, basin_id_1, basin_id_2, count = 0.01)
{
  temp_hypso_x <- seq(0, 1, count)
  
  # watershed_current <- 1
  # basin_id_current_1 <- 1
  # basin_id_current_2 <- 2
  
  watershed_current <- watershed
  basin_id_current_1 <- basin_id_1
  basin_id_current_2 <- basin_id_2
  
  temp_df_1 <- df_basin_data_summary %>% 
    filter(watershed == watershed_current) %>% 
    filter(basin_id == basin_id_current_1)
  temp_df_2 <- df_basin_data_summary %>% 
    filter(watershed == watershed_current) %>% 
    filter(basin_id == basin_id_current_2)
  
  df_hypso_formulations %>%
    filter(watershed == watershed_current) %>%
    filter(basin_id == basin_id_current_1) %>%
    select(relative_area_above_elevation, relative_elevation_predicted_m2)
  df_hypso_formulations %>%
    filter(watershed == watershed_current) %>%
    filter(basin_id == basin_id_current_2) %>%
    select(relative_area_above_elevation, relative_elevation_predicted_m2)
  
  temp_nls_1 <- list_hypso_nls_formulations[[2]][[paste0(watershed_current, '-', basin_id_current_1)]]
  temp_nls_2 <- list_hypso_nls_formulations[[2]][[paste0(watershed_current, '-', basin_id_current_2)]]
  
  temp_hypso_x_1 <- stats::predict(temp_nls_1, newdata = data.frame(x = temp_hypso_x))
  temp_hypso_x_2 <- stats::predict(temp_nls_2, newdata = data.frame(x = temp_hypso_x))
  
  di = mean(abs(temp_hypso_x_1 - temp_hypso_x_2))
  
  return (di)
}

find_di_from_hypso_y <- function(df_hyspo_y_1, df_hypso_y_2)
{
  di = mean(abs(df_hyspo_y_1 - df_hypso_y_2))
  
  return (di)
}

find_di_from_hypso <- function(df_hyspo_1, df_hypso_2)
{
  return (find_di_from_hypso_y(df_hyspo_1$y, df_hypso_2$y))
}

find_di_from_params <- function(df_params_1, df_params_2, count = 0.01)
{
  return (find_di_from_hypso(df_params_1, count), find_di_from_hypso(df_params_2, count))
}

params_to_hypso <- function(df_parameters, count = 0.01)
{
  # r <- df_parameters$r
  z <- df_parameters$z
  m <- df_parameters$m
  b <- df_parameters$b

  temp_hypso_x <- seq(0, 1, count)
  temp_hypso_y <- ((1-temp_hypso_x^m)/(1+b*temp_hypso_x^m))^z
  # temp_hypso_y2 <- (r*(1-temp_hypso_x^m)/(r+(1-r)*temp_hypso_x^m))^z
  # sum(abs(temp_hypso_y-temp_hypso_y2))
  
  return(data.frame(x = temp_hypso_x, y = temp_hypso_y))
}

get_pd_index <- function(snorm1, snorm2, min_x = 0, max_x = 1, delx = 0.01, index = 3)
{
  PDI <- 0
  
  for (x_value in seq(min_x,max_x,delx))
  {
    # p <- get_sn_prob(snorm_selected1, x_value, x_value+delx)
    # q <- get_sn_prob(snorm_selected2, x_value, x_value+delx)
    # print ("start")
    # print (snorm1)
    # print (x_value)
    p <- get_sn_y(snorm1, x_value)
    q <- get_sn_y(snorm2, x_value)
    
    if (index == 2)
    {
      PDI_ind = abs(p-q)
    }else if (index == 3)
    {
      PDI_ind = (p-q)^2
    }else if (index == 4)
    {
      PDI_ind = (sqrt(p)-sqrt(q))^2
    }else if (index == 8)
    {
      PDI_ind = sqrt(p*q)
    }
    
    PDI = PDI + PDI_ind
    
    # i = i+1
  }
  PDI = PDI*delx
  if (index %in% c(3,4))
  {
    PDI = sqrt(PDI)
  }
  return (PDI)
}

get_pd_index_y <- function(y1, y2, delx, index = 3)
{
  PDI <- 0
  
  # delx <- 0.01
  
  for (iter in 1:length(y1))
  {
    p <- y1[iter]
    q <- y2[iter]
    
    if (index == 2)
    {
      PDI_ind = abs(p-q)
    }else if (index == 3)
    {
      PDI_ind = (p-q)^2
    }else if (index == 4)
    {
      PDI_ind = (sqrt(p)-sqrt(q))^2
    }else if (index == 8)
    {
      PDI_ind = sqrt(p*q)
    }
    
    PDI = PDI + PDI_ind
    
    # i = i+1
  }
  PDI = PDI*delx
  if (index %in% c(3,4))
  {
    PDI = sqrt(PDI)
  }
  return (PDI)
}

get_pd_index_basin <- function(basin_id_1, basin_id_2, watershed = 2, nmix = 2, nmix1 = NA, nmix2 = NA, delx = 0.01, index = 3)
{
  if (!is.na(nmix))
  {
    nmix1 = nmix
    nmix2 = nmix
  }
  if (is.na(watershed))
  {
    snorm_selected1 <- snorm_list[[paste0(basin_id_1,'_',nmix1)]]
    snorm_selected2 <- snorm_list[[paste0(basin_id_2,'_',nmix2)]]
  } else
  {
    snorm_selected1 <- snorm_list[[paste0(watershed, '_', basin_id_1,'_',nmix1)]]
    snorm_selected2 <- snorm_list[[paste0(watershed, '_', basin_id_2,'_',nmix2)]]
  }
  
  get_pd_index(snorm_selected1, snorm_selected2, delx = delx, index = index)
}
get_pd_index_basin <- Vectorize(get_pd_index_basin)

indices_plot_basin <- function(basin_id_1, basin_id_2, watershed = 2, mix_n = 2)
{
  # basin_id_1 <- 1
  # basin_id_2 <- 10
  
  if (is.na(watershed_current))
  {
    df_sn_values_1 <- get_sn_xytable(snorm_list[[paste0(basin_id_1,'_',mix_n)]], min_x = -0.01, max_x = 1.01)
    df_sn_values_2 <- get_sn_xytable(snorm_list[[paste0(basin_id_2,'_',mix_n)]], min_x = -0.01, max_x = 1.01)
  } else
  {
    df_sn_values_1 <- get_sn_xytable(snorm_list[[paste0(watershed, '_', basin_id_1,'_',mix_n)]], min_x = -0.01, max_x = 1.01)
    df_sn_values_2 <- get_sn_xytable(snorm_list[[paste0(watershed, '_', basin_id_2,'_',mix_n)]], min_x = -0.01, max_x = 1.01)
  }
  
  ggplot() +
    geom_line(data = df_sn_values_1,
              mapping = aes(x = x,
                            y = y),
              size = 0.3) +
    geom_line(data = df_sn_values_2,
              mapping = aes(x = x,
                            y = y),
              size = 0.3) +
    labs(x = 'Scaled Distance from outlet',
         y = '',
         title = paste0('Width functions for basins ', basin_id_1, ' and ', basin_id_2)) +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = 'bottom',
          legend.direction = 'vertical')
}

indices_plot_skeleton_basin <- function(basin_id_1, basin_id_2, watershed = 2, mix_n = 2)
{
  if (is.na(watershed_current))
  {
    df_sn_values_1 <- get_sn_xytable(snorm_list[[paste0(basin_id_1,'_',mix_n)]], min_x = -0.01, max_x = 1.01)
    df_sn_values_2 <- get_sn_xytable(snorm_list[[paste0(basin_id_2,'_',mix_n)]], min_x = -0.01, max_x = 1.01)
  } else
  {
    df_sn_values_1 <- get_sn_xytable(snorm_list[[paste0(watershed, '_', basin_id_1,'_',mix_n)]], min_x = -0.01, max_x = 1.01)
    df_sn_values_2 <- get_sn_xytable(snorm_list[[paste0(watershed, '_', basin_id_2,'_',mix_n)]], min_x = -0.01, max_x = 1.01)
  }
  
  ggplot() +
    geom_line(data = df_sn_values_1,
              mapping = aes(x = x,
                            y = y),
              size = 0.3) +
    geom_line(data = df_sn_values_2,
              mapping = aes(x = x,
                            y = y),
              size = 0.3) +
    labs(x = '',
         y = '',
         title = '') +
    theme_transparent +
    theme(legend.title = element_blank(),
          legend.position = 'right',
          legend.direction = 'vertical',
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    theme_transparent
}

generate_spacing_coord <- function(n_points, height = 1, width = 1)
{
  # height <- 1
  # width <- 1
  # n_points <- 4
  n_points_square <- ceiling(sqrt(n_points))^2
  
  total_area <- width*height
  point_area <- total_area/n_points_square
  point_length <- sqrt(point_area)
  
  df_xy <- data.frame()
  iter_x <- point_length/2
  while (iter_x < width)
  {
    iter_y <- point_length/2
    while (iter_y < height)
    {
      df_xy <- rbind(df_xy, data.frame(x = iter_x, y = iter_y))
      
      iter_y <- iter_y + point_length
    }
    iter_x <- iter_x + point_length
  }
  
  df_xy <- df_xy %>% 
    sample_n(n_points)
  df_xy$x <- df_xy$x-height/2
  df_xy$y <- df_xy$y-height/2
  
  df_xy %>% 
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    coord_cartesian(xlim = c(-.5, .5),
                    ylim = c(-.5, .5))
  
  return(df_xy)
}

create_jitter_from_df2 <- function(df_x_y_jitter)
{
  df_x_y_jitter <- df_x_y_jitter %>% 
    mutate(x_jittered = NA,
           y_jittered = NA)
  for (temp_x_iter in df_x_y_jitter %>% pull(x_jitter) %>% unique())
  {
    # temp_x_iter <- 1
    for (temp_y_iter in df_x_y_jitter %>% pull(y_jitter) %>% unique())
    {
      # temp_y_iter <- 1
      temp_n <- df_x_y_jitter %>% 
        filter(x_jitter == temp_x_iter) %>% 
        filter(y_jitter == temp_y_iter) %>% 
        nrow()
      
      df_spacing_coord <- generate_spacing_coord(temp_n)
      
      temp_x <- temp_x_iter + df_spacing_coord %>% 
        pull(x)
      temp_y <- temp_y_iter + df_spacing_coord %>% 
        pull(y)
      df_x_y_jitter$x_jittered[df_x_y_jitter$x_jitter == temp_x_iter & 
                                 df_x_y_jitter$y_jitter == temp_y_iter] <- temp_x
      df_x_y_jitter$y_jittered[df_x_y_jitter$x_jitter == temp_x_iter & 
                                 df_x_y_jitter$y_jitter == temp_y_iter] <- temp_y
    }
  }
  return(df_x_y_jitter)
  # return (df_x_y_jitter %>% 
  #           select(-x_jitter, -y_jitter) %>%
  #           rename(x_jitter = x_jittered,
  #                  y_jitter = y_jittered))
}

create_jitter_from_df <- function(df_x_y_jitter)
{
  # df_spacing_coord <- data.frame(x = c(0, -.5, .5,  0,   0, -.5, .5,  .5, -.5, -.25, .25,  .25, -.25, -.25, .25, .75,  .75,  .25, -.25, -.75, -.75, -.75, .75,  .75, -.75),
  #                                y = c(0,   0,  0, .5, -.5,  .5, .5, -.5, -.5,  .25, .25, -.25, -.25,  .75, .75, .25, -.25, -.75, -.75, -.25,  .25,  .75, .75, -.75, -.75))
  df_spacing_coord <- data.frame(x = c(0, -.5, .5,  0,   0, -.5, .5,  .5, -.5, -.25, .25,  .25, -.25, -.25, .25, .50,  .50,  .25, -.25, -.50, -.50, .00,  .00, .25, -.25),
                                 y = c(0,   0,  0, .5, -.5,  .5, .5, -.5, -.5,  .25, .25, -.25, -.25,  .50, .50, .25, -.25, -.50, -.50, -.25,  .25, .25, -.25, .00,  .00))
  # nrow(df_spacing_coord)
  # df_spacing_coord %>%
  #   ggplot(aes(x = x, y = y)) +
  #   geom_point()
  # df_x_y_jitter <- df_cluster_hypso_wf %>% 
  #   mutate(x_jitter = cluster_hypso,
  #          y_jitter = cluster_wf)
  df_x_y_jitter <- df_x_y_jitter %>% 
    mutate(x_jittered = NA,
           y_jittered = NA)
  for (temp_x_iter in df_x_y_jitter %>% pull(x_jitter) %>% unique())
  {
    # temp_x_iter <- 1
    for (temp_y_iter in df_x_y_jitter %>% pull(y_jitter) %>% unique())
    {
      # temp_y_iter <- 1
      temp_n <- df_x_y_jitter %>% 
        filter(x_jitter == temp_x_iter) %>% 
        filter(y_jitter == temp_y_iter) %>% 
        nrow()
      temp_x <- temp_x_iter + .7*df_spacing_coord %>% 
        slice(1:temp_n) %>% 
        pull(x)
      temp_y <- temp_y_iter + .7*df_spacing_coord %>% 
        slice(1:temp_n) %>% 
        pull(y)
      df_x_y_jitter$x_jittered[df_x_y_jitter$x_jitter == temp_x_iter & 
                                 df_x_y_jitter$y_jitter == temp_y_iter] <- temp_x
      df_x_y_jitter$y_jittered[df_x_y_jitter$x_jitter == temp_x_iter & 
                                 df_x_y_jitter$y_jitter == temp_y_iter] <- temp_y
    }
  }
  return(df_x_y_jitter)
  # return (df_x_y_jitter %>% 
  #           select(-x_jitter, -y_jitter) %>%
  #           rename(x_jitter = x_jittered,
  #                  y_jitter = y_jittered))
}

fviz_gap_stat_pra <- function(gap_stat, title_text = 'Optimal number of clusters') 
{
  gap <- gap_stat$Tab[, "gap"]
  se <- gap_stat$Tab[, "SE.sim"]
  df <- as.data.frame(gap_stat$Tab, stringsAsFactors = TRUE)
  df$clusters <- as.factor(1:nrow(df))
  df$ymin <- gap - se
  df$ymax <- gap + se
  p <- df %>% 
    ggplot(aes(x = clusters, y = gap, group = 1)) +
    geom_line() +
    geom_errorbar(aes_string(ymin = "ymin", 
                             ymax = "ymax"), width = 0.2) +
    labs(y = "Gap statistic", 
         x = "Number of clusters", 
         title = title_text) +
    theme_pra_bw
  p
}

colorname_to_values <- function(v_colors, v_labels = NA)
{
  v_colors_values <- c()
  for (i in 1:length(v_colors))
  {
    v_colors_values <- c(v_colors_values, 
                         paste0(temp_colors[i] %>% 
                                  str_sub(2, 3) %>% 
                                  base::strtoi(16) %>% 
                                  str_pad(4),
                                temp_colors[i] %>% 
                                  str_sub(4, 5) %>% 
                                  base::strtoi(16) %>% 
                                  str_pad(4),
                                temp_colors[i] %>% 
                                  str_sub(6, 7) %>% 
                                  base::strtoi(16) %>% 
                                  str_pad(4)))
  }
  data.frame(v_labels, v_colors, v_colors_values)
}

# Data ##########################################################################################################

df_basin_data <- readRDS('df_basin_data.RDS')
df_basin_data_summary <- readRDS('df_basin_data_summary2.RDS')

basin_ids <- list(1:221, 1:72, 1:126)
basin_ids_string <- list(as.character(basin_ids[[1]]), 
                         as.character(basin_ids[[2]]), 
                         as.character(basin_ids[[3]]))
watersheds <- 1:3
df_watersheds <- data.frame(watershed = watersheds,
                            watershed_name = c('Congo', 'Narmada', 'Yukon'))

list_hypso_nls_formulations <- readRDS('list_hypso_nls_formulations_05.RDS')
df_hypso_formulations <- readRDS('df_hypso_formulations_05.RDS')
df_hypso_summary_formulations <- readRDS('df_hypso_summary_formulations_05.RDS')
df_hypso_summary_formulations_current <- readRDS('df_hypso_summary_formulations_current.RDS')
df_hypso <- readRDS('df_hypso.RDS')

df_di_pairs <- readRDS('df_di_pairs.RDS')
clusGap_di <- readRDS('clusGap_di.RDS')
df_di_lowest <- readRDS('df_di_lowest.RDS')
basin_id_outliers_di_original <- readRDS('basin_id_outliers_di_original.RDS')
basin_id_outliers_di <- readRDS('basin_id_outliers_di.RDS')
df_hypso_cluster_outliers <- readRDS('df_hypso_cluster_outliers.RDS')
clusters_di <- readRDS('clusters_di2.RDS')
df_cluster_di <- readRDS('df_cluster_di.RDS')
df_hypso_cluster <- readRDS('df_hypso_cluster2.RDS')
df_hypso_mean <- readRDS('df_hypso_mean.RDS')
df_hypso_median <- readRDS('df_hypso_median.RDS')
df_cluster_hypso_wf <- readRDS('df_cluster_hypso_wf_02.RDS')

n_mix_current <- 2
if (n_mix_current == 2)
{
  snorm_list <- readRDS('snorm_list_03.RDS')
  df_sn_params <- readRDS('snorm_params_df_02.RDS')
  df_sn_values <- readRDS('snorm_values_df_02.RDS')
  df_sn_values_main <- readRDS('df_sn_values_main_02.RDS')
  
  df_wf_pairs <- readRDS('df_wf_pairs_02.RDS')
  clusGap_l2_original <- readRDS('clusGap_l2_original.RDS')
  basin_id_outliers_l2 <- readRDS('basin_id_outliers_l2.RDS')
  df_wf_l2_lowest <- readRDS('df_wf_l2_lowest.RDS')
  clusters_l2 <- readRDS('clusters_l2_03.RDS')
  df_cluster_l2 <- readRDS('df_cluster_l2.RDS')
  df_clusters_wf <- readRDS('df_clusters_wf_03.RDS')
  clusGap_l2 <- readRDS('clusGap_l2.RDS')
  df_wf_mean <- readRDS('df_wf_mean.RDS')
  df_wf_median <- readRDS('df_wf_median.RDS')
  
  df_cluster_hypso_wf <- readRDS('df_cluster_hypso_wf_02.RDS')
} else if (n_mix_current == 3)
{
  snorm_list <- readRDS('snorm_list_03.RDS')
  df_sn_params <- readRDS('snorm_params_df_02.RDS')
  df_sn_values <- readRDS('snorm_values_df_nmix3_02.RDS')
  df_sn_values_main <- readRDS('df_sn_values_main_nmix3_02.RDS')
  
  df_wf_pairs <- readRDS('df_wf_pairs_nmix3_02.RDS')
  
  m_l2_pairs <- readRDS('m_l2_pairs_nmix3.RDS')
  clusters_l2 <- readRDS('clusters_l2_nmix3_02.RDS')
  clusters_agnes_l2 <- readRDS('clusters_agnes_l2_nmix3_02.RDS')
  
  basin_id_outliers_l2 <- readRDS('basin_id_outliers_l2_nmix3.RDS')
  df_cluster_wf <- readRDS('df_cluster_wf_02.RDS')
  df_wf_representative <- readRDS('df_wf_representative_02.RDS')
  df_cluster_representative_wf <- readRDS('df_cluster_representative_wf_02.RDS')
} else if (n_mix_current == 5)
{
  snorm_list <- readRDS('snorm_list_nmix5_01.RDS')
  df_sn_params <- readRDS('snorm_params_df_nmix5_02.RDS')
  df_sn_values <- readRDS('snorm_values_df_nmix5_02.RDS')
  df_sn_values_main <- readRDS('df_sn_values_main_nmix5_02.RDS')
  df_wf_pairs <- readRDS('df_wf_pairs_nmix5_02.RDS')
  m_l2_pairs <- readRDS('m_l2_pairs_nmix5.RDS')
  clusters_l2 <- readRDS('clusters_l2_nmix5_02.RDS')
  clusters_agnes_l2 <- readRDS('clusters_agnes_l2_nmix5_02.RDS')
}

watershed_current <- 2
watershed_name_current <- df_watersheds %>% 
  filter(watershed == watershed_current) %>% 
  pull(watershed_name)
#
# Hypsometric tables and data ###################################################################################

# Only second hypso formulation

df_hypso_summary_formulations_current <- df_hypso_summary_formulations %>% 
  filter(formulation == 2) %>% 
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
    # watershed_current <- 3
    # basin_id_current <- 105
    
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

# Hypsometry distance matrix ####################################################################################

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

# Clustering with hypsometric function ###########################################################################

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

# Clustering groups and outliers analysis #######################################################################

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

# Plotting Hypsometry clusters ###################################################################################

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
# Plotting Hypsometry clusters (old) #############################################################################

watershed_current <- 2
watershed_name_current <- df_watersheds %>% 
  filter(watershed == watershed_current) %>% 
  pull(watershed_name)

clusters_di[[watershed_current]] %>% 
  plot(hang = -1)
clusters_di[[watershed_current]] %>% 
  plot(hang = -1,
       main = 'Hypsoetry Cluster Dendogram',
       sub = '',
       xlab = 'Basins',
       ylab = 'DI')
abline(h = 0.4, col = 'red')
# clusters_di[[watershed_current]] %>%
#   as.dendrogram() %>% 
#   rect.dendrogram(h = 0.4)

plot(2:20, sapply(2:20, function(i) { 
  mean(silhouette(cutree(clusters_di[[watershed_current]], i), dmatrix=m_di_pairs[[watershed_current]])[,"sil_width"]) }),
  main = 'Optimal Number of DI clusters',
  xlab="Number of clusters", 
  ylab="Average Silhouette", 
  type="b", 
  pch=20)
dev.copy(png, 
         foldered_png('Optimal Clustering for Hypsometry'),
         width = 700,
         height = 400); dev.off()

df_cluster %>% 
  filter(watershed == watershed_current) %>%  
  group_by(watershed, cluster) %>% 
  summarise(n = n()) %>% 
  left_join(df_watersheds, by = 'watershed') %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = n), stat = 'identity') +
  labs(x = 'Cluster',
       y = 'Members',
       title = 'Number of members in Hypsometric Cluster') +
  # facet_wrap(~watershed_name, nrow = 1, scales = 'free_x') +
  theme_bw()
ggsave(filename = foldered_png('Cluster Count Hypso'), width = 8, height = 4)

watershed_current <- 2
df_hypso_formulations %>%
  filter(watershed == watershed_current) %>% 
  left_join(df_cluster,
            by = c('watershed', 'basin_id')) %>% 
  mutate(basin_id = factor(basin_id)) %>% 
  ggplot() +
  geom_line(aes(x = relative_area_above_elevation,
                y = relative_elevation_predicted_m2, 
                group = basin_id),
            size = 0.1) +
  labs(x = 'Relative Area above Elevation',
       y = 'Relative Elevation',
       title = paste0('Hypsometric Clusters in ', watershed_name_current)) +
  theme_bw() +
  facet_wrap(~cluster, nrow = 2)
ggsave(filename = foldered_png(paste0('Clusters Hypso1 - ',watershed_name_current)), width = 8, height = 6)
df_hypso_formulations %>%
  filter(watershed == watershed_current) %>% 
  left_join(df_cluster,
            by = c('watershed', 'basin_id')) %>% 
  mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster)) %>% 
  ggplot() +
  geom_line(aes(x = relative_area_above_elevation,
                y = relative_elevation_predicted_m2, 
                color = cluster,
                group = basin_id),
            size = 0.1) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Relative Area above Elevation',
       y = 'Relative Elevation',
       title = paste0('Hypsometric Clusters in ', watershed_name_current)) +
  theme_bw()
ggsave(filename = foldered_png('Clusters Hypso2'), width = 6, height = 4)

watershed_current <- 2
df_hypso %>% 
  filter(watershed == watershed_current) %>% 
  mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y, 
                color = cluster,
                group = basin_id),
            size = 0.1) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Relative Area above Elevation',
       y = 'Relative Elevation',
       title = 'Clusters') +
  theme_bw()
ggsave(filename = foldered_png('Hypso clusters'), width = 6, height = 4)

df_hypso_representative %>% 
  filter(watershed == watershed_current) %>% 
  # mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y_mean, 
                color = cluster,
                group = cluster),
            size = 0.6) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Relative Area above Elevation',
       y = 'Relative Elevation',
       title = 'Mean Hypsometric Curve') +
  theme_bw()
ggsave(filename = foldered_png('Clusters Mean Hypso'), width = 6, height = 4)

df_hypso %>% 
  filter(watershed == watershed_current) %>% 
  filter(representative == 1) %>% 
  # mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y, 
                color = cluster,
                group = cluster),
            size = 0.6) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Relative Area above Elevation',
       y = 'Relative Elevation',
       title = 'Representative Hypsometric Curve') +
  theme_bw()
ggsave(filename = foldered_png('Clusters Representative Hypso'), width = 6, height = 4)



  
  
df_hypso_summary_formulations_current %>% 
  filter(watershed == watershed_current) %>%
  mutate(cluster = factor(cluster)) %>% 
  # filter(basin_id %in% basin_ids_cluster[[cluster_current]]) %>% 
  ggplot(aes(x = cluster, y = r)) +
  # geom_violin(fill = 'grey90') +
  geom_boxplot(width = 0.2, fill = 'grey70') +
  # stat_boxplot(geom = 'errorbar', width = 0.2) +
  theme_bw()
df_hypso_summary_formulations_current %>% 
  filter(watershed == watershed_current) %>%
  mutate(cluster = factor(cluster)) %>% 
  # filter(basin_id %in% basin_ids_cluster[[cluster_current]]) %>% 
  ggplot(aes(x = cluster, y = 1/b)) +
  # geom_violin(fill = 'grey90') +
  geom_boxplot(width = 0.2, fill = 'grey70') +
  # stat_boxplot(geom = 'errorbar', width = 0.2) +
  labs(x = 'Cluster',
       y = '1/B',
       title = 'Hypsmetric parameter: B') +
  theme_bw()
ggsave(filename = paste0('Param B - Cluster ', cluster_current, '.png'), width = 6, height = 4)
df_hypso_summary_formulations_current %>% 
  filter(watershed == watershed_current) %>%
  mutate(cluster = factor(cluster)) %>% 
  # filter(basin_id %in% basin_ids_cluster[[cluster_current]]) %>% 
  ggplot(aes(x = cluster, y = z)) +
  geom_violin(fill = 'grey90') +
  geom_boxplot(width = 0.2, fill = 'grey70') +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  scale_y_log10() +
  labs(x = 'Cluster',
       y = 'z',
       title = 'Hypsmetric parameter: z') +
  theme_bw()
ggsave(filename = paste0('Param z - Cluster ', cluster_current, '.png'), width = 6, height = 4)
df_hypso_summary_formulations_current %>% 
  filter(watershed == watershed_current) %>%
  mutate(cluster = factor(cluster)) %>% 
  # filter(basin_id %in% basin_ids_cluster[[cluster_current]]) %>% 
  ggplot(aes(x = cluster, y = m)) +
  geom_violin(fill = 'grey90') +
  geom_boxplot(width = 0.2, fill = 'grey70') +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  scale_y_log10() +
  labs(x = 'Cluster',
       y = 'm',
       title = 'Hypsmetric parameter: m') +
  theme_bw()
ggsave(filename = paste0('Param m - Cluster ', cluster_current, '.png'), width = 6, height = 4)



distance_matrix <- df_hypso_summary_formulations_current %>% 
  filter(watershed == watershed_current) %>%
  select(b, z, m)
distance_matrix <- df_hypso_summary_formulations_current %>% 
  filter(watershed == watershed_current) %>%
  select(b, z, m) %>% 
  mutate(b = b,
         z = log10(z),
         m = log10(m))
row.names(distance_matrix) <- df_hypso_summary_formulations_current %>% 
  filter(watershed == watershed_current) %>%
  pull(basin_id) %>% 
  as.character()
distance_matrix <- dist(distance_matrix)  
clusters_rmz_euclidean <- hclust(distance_matrix)
plot(clusters_rmz_euclidean)


plot(clusters_di)
plot(clusters_rmz_euclidean)
plot(clusters_di %>% as.dendrogram())
plot(clusters_rmz_euclidean %>% as.dendrogram())

dend_di <- clusters_di %>% as.dendrogram()
dend_rmz_euclidean <- clusters_rmz_euclidean %>% as.dendrogram()

dl <- dendlist(dend_di, dend_rmz_euclidean)
png('Entanglement.png', width = 1200, height = 1200)
tanglegram(dl)
dev.off()

png('Entanglement2.png', width = 1200, height = 1200)
dendlist(dend_di, dend_rmz_euclidean) %>%
  untangle(method = "step1side") %>% # Find the best alignment layout
  tanglegram()  
dev.off()

dendlist(dend_di, dend_rmz_euclidean) %>%
  untangle(method = "step1side") %>% # Find the best alignment layout
  entanglement()  



df_hypso_summary_formulations_current %>% 
  filter(watershed == watershed_current) %>% 
  ggplot() +
  geom_point(aes(x = b, y = z, color = factor(cluster))) +
  theme_bw()
ggsave(filename = foldered_png('Param b vs z'), width = 6, height = 4)
df_hypso_summary_formulations_current %>% 
  # filter(watershed == watershed_current) %>% 
  ggplot() +
  geom_point(aes(x = b, y = z, color = factor(cluster))) +
  scale_y_log10() +
  scale_x_log10() +
  facet_wrap(~watershed_name, nrow = 1, scales = 'free') +
  theme_bw()
ggsave(filename = foldered_png('Param b vs z transformed'), width = 12, height = 4)

# watershed_current <- 2
df_hypso_summary_formulations_current %>% 
  left_join(df_basin_data_summary,
            by = c('watershed', 'basin_id')) %>% 
  # filter(watershed == watershed_current) %>%
  ggplot(aes(x = area_gis_km2, y = 1/b)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = 'Area (sq.km.)',
       y = '1/B',
       title = 'Area vs B') +
  facet_wrap(~watershed_name, nrow = 1) +
  theme_bw()
ggsave(filename = foldered_png('Basin area vs B'), width = 12, height = 4)
df_hypso_summary_formulations_current %>% 
  left_join(df_basin_data_summary,
            by = c('watershed', 'basin_id')) %>% 
  # filter(watershed == watershed_current) %>%
  ggplot(aes(x = area_gis_km2, y = z)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = 'Area (sq.km.)',
       y = 'z',
       title = 'Area vs z') +
  facet_wrap(~watershed_name, nrow = 1) +
  theme_bw()
ggsave(filename = foldered_png('Basin area vs z'), width = 12, height = 4)
df_hypso_summary_formulations_current %>% 
  left_join(df_basin_data_summary,
            by = c('watershed', 'basin_id')) %>% 
  # filter(watershed == watershed_current) %>%
  ggplot(aes(x = area_gis_km2, y = m)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = 'Area (sq.km.)',
       y = 'm',
       title = 'Area vs m') +
  facet_wrap(~watershed_name, nrow = 1) +
  theme_bw()
ggsave(filename = foldered_png('Basin area vs m'), width = 12, height = 4)

df_hypso_summary_formulations_current %>% 
  left_join(df_basin_data_summary,
            by = c('watershed', 'basin_id')) %>% 
  # filter(watershed == watershed_current) %>%
  ggplot(aes(x = area_gis_km2, 
             y = r,
             color = watershed_name)) +
  geom_point() +
  scale_x_log10() +
  # scale_y_log10() +
  labs(x = 'Area (sq.km.)',
       y = 'r',
       title = 'Area vs r') +
  # facet_wrap(~watershed_name, nrow = 1) +
  theme_bw()
ggsave(filename = foldered_png('Basin area vs r (Combined, semilog)'), width = 7, height = 4)
df_hypso_summary_formulations_current %>% 
  left_join(df_basin_data_summary,
            by = c('watershed', 'basin_id')) %>% 
  # filter(watershed == watershed_current) %>%
  ggplot(aes(x = area_gis_km2, 
             y = z,
             color = watershed_name)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = 'Area (sq.km.)',
       y = 'z',
       title = 'Area vs m') +
  # facet_wrap(~watershed_name, nrow = 1) +
  theme_bw()
ggsave(filename = foldered_png('Basin area vs z (Combined, loglog)'), width = 7, height = 4)
df_hypso_summary_formulations_current %>% 
  left_join(df_basin_data_summary,
            by = c('watershed', 'basin_id')) %>% 
  # filter(watershed == watershed_current) %>%
  ggplot(aes(x = area_gis_km2, 
             y = m,
             color = watershed_name)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = 'Area (sq.km.)',
       y = 'm',
       title = 'Area vs m') +
  # facet_wrap(~watershed_name, nrow = 1) +
  theme_bw()
ggsave(filename = foldered_png('Basin area vs m (Combined, loglog)'), width = 7, height = 4)
# df_hypso_summary_formulations_current %>% 
#   left_join(df_basin_data_summary,
#             by = c('watershed', 'basin_id')) %>% 
#   # filter(watershed == watershed_current) %>%
#   ggplot(aes(x = area_gis_km2, 
#              y = r,
#              color = watershed_name)) +
#   geom_point() +
#   scale_x_log10() +
#   scale_y_log10() +
#   labs(x = 'Area (sq.km.)',
#        y = 'r',
#        title = 'Area vs r') +
#   # facet_wrap(~watershed_name, nrow = 1) +
#   theme_bw()
# ggsave(filename = foldered_png('Basin area vs r (Combined, loglog)'), width = 7, height = 4)
# df_hypso_summary_formulations_current %>% 
#   left_join(df_basin_data_summary,
#             by = c('watershed', 'basin_id')) %>% 
#   # filter(watershed == watershed_current) %>%
#   ggplot(aes(x = area_gis_km2, 
#              y = z,
#              color = watershed_name)) +
#   geom_point() +
#   scale_x_log10() +
#   # scale_y_log10() +
#   labs(x = 'Area (sq.km.)',
#        y = 'z',
#        title = 'Area vs m') +
#   # facet_wrap(~watershed_name, nrow = 1) +
#   theme_bw()
# ggsave(filename = foldered_png('Basin area vs z (Combined, semilog)'), width = 7, height = 4)
# df_hypso_summary_formulations_current %>% 
#   left_join(df_basin_data_summary,
#             by = c('watershed', 'basin_id')) %>% 
#   # filter(watershed == watershed_current) %>%
#   ggplot(aes(x = area_gis_km2, 
#              y = m,
#              color = watershed_name)) +
#   geom_point() +
#   scale_x_log10() +
#   # scale_y_log10() +
#   labs(x = 'Area (sq.km.)',
#        y = 'm',
#        title = 'Area vs m') +
#   # facet_wrap(~watershed_name, nrow = 1) +
#   theme_bw()
# ggsave(filename = foldered_png('Basin area vs m (Combined, semilog)'), width = 7, height = 4)

df_hypso_summary_formulations_current %>% 
  left_join(df_basin_data_summary,
            by = c('watershed', 'basin_id')) %>% 
  # filter(watershed == watershed_current) %>%
  ggplot(aes(x = elevation_m_mean, 
             y = r,
             color = watershed_name)) +
  geom_point() +
  # scale_x_log10() +
  # scale_y_log10() +
  labs(x = 'Mean elevation (m)',
       y = 'r',
       title = 'Area vs r') +
  # facet_wrap(~watershed_name, nrow = 1) +
  theme_bw()
ggsave(filename = foldered_png('Mean Elevation vs r (Combined)'), width = 7, height = 4)
df_hypso_summary_formulations_current %>% 
  left_join(df_basin_data_summary,
            by = c('watershed', 'basin_id')) %>% 
  # filter(watershed == watershed_current) %>%
  ggplot(aes(x = elevation_m_mean, 
             y = z,
             color = watershed_name)) +
  geom_point() +
  # scale_x_log10() +
  scale_y_log10() +
  labs(x = 'Mean elevation (m)',
       y = 'z',
       title = 'Area vs m') +
  # facet_wrap(~watershed_name, nrow = 1) +
  theme_bw()
ggsave(filename = foldered_png('Mean Elevation vs z (Combined)'), width = 7, height = 4)
df_hypso_summary_formulations_current %>% 
  left_join(df_basin_data_summary,
            by = c('watershed', 'basin_id')) %>% 
  # filter(watershed == watershed_current) %>%
  ggplot(aes(x = elevation_m_mean, 
             y = m,
             color = watershed_name)) +
  geom_point() +
  # scale_x_log10() +
  scale_y_log10() +
  labs(x = 'Mean elevation (m)',
       y = 'm',
       title = 'Area vs m') +
  # facet_wrap(~watershed_name, nrow = 1) +
  theme_bw()
ggsave(filename = foldered_png('Mean Elevation vs m (Combined)'), width = 7, height = 4)

df_hypso_summary_formulations_current %>% 
  left_join(df_basin_data_summary,
            by = c('watershed', 'basin_id')) %>% 
  # filter(watershed == watershed_current) %>%
  ggplot(aes(x = area_gis_km2)) +
  geom_histogram() +
  # scale_x_log10() +
  # scale_y_log10() +
  labs(x = 'Area (sq.km.)',
       title = 'Area distribution') +
  facet_wrap(~watershed_name, nrow = 1, scales = 'free') +
  theme_bw()
ggsave(filename = foldered_png('Area Distribution'), width = 9, height = 4)

df_basin_data_summary %>% 
  filter(watershed == 1) %>% 
  summary()

#
# Width Function Parameters (estimated in supercomputer) ########################################################

# df_basin_data <- readRDS('df_basin_data.RDS')
# df_basin_data  %>% saveRDS('df_basin_data_compatible.RDS', version = 2)

# watershed_current <- 2

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

# Width Function parameters to dataframe ########################################################################

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

# WF data table ##################################################################################################

# WF data table

# watershed_current <- 2
df_sn_values <- list()
for (watershed_current in watersheds)
{
  for (basin_id_selected in basin_ids[[watershed_current]])
  {
    # basin_id_selected <- basin_ids[1]
    # basin_id_selected <- 1
    
    print (paste0('Working with Watershed ', watershed_current, ' basin ', basin_id_selected))
    
    data_unit <- df_basin_data %>%
      filter(watershed == watershed_current) %>% 
      filter(basin_id == basin_id_selected) %>%
      pull(chainage_scaled)
    
    # for (mix_n in 1:3) 
    # {
    mix_n <- 3
    df_sn_values[[paste0(watershed_current, '_', basin_id_selected,'_',mix_n)]] <- get_sn_xytable(snorm_list[[paste0(watershed_current, '_', basin_id_selected,'_',mix_n)]], min_x = -0.01, max_x = 1.01)
    # temp_df_sn_values <- get_sn_xytable(snorm_list[[paste0(watershed_current, '_', basin_id_selected,'_',mix_n)]], min_x = -0.01, max_x = 1.01)
    # }
  }
}
df_sn_values %>% 
  saveRDS('snorm_values_df_nmix3_02.RDS')
# df_sn_values <- readRDS('snorm_values_df_02.RDS')

# watershed_current <- 2
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
# df_sn_values_main <- df_sn_values_main %>%
#   left_join(df_cluster_wf,
#             by = c('watershed', 'basin_id')) 
#   select(watershed, basin_id, x, y, cluster_l1, cluster_l2)
df_sn_values_main %>% 
  saveRDS('df_sn_values_main_nmix3_02.RDS')

# L1 or L2 distance #############################################################################################

indices_plot_skeleton_basin(40, 72, 2, mix_n = n_mix_current)

indices_plot_basin(40, 72, 2, mix_n = n_mix_current)
get_pd_index_basin(40, 72, 2, index = 2, nmix = n_mix_current)
get_pd_index_basin(40, 72, 2, index = 3, nmix = n_mix_current)

# Width Function plots ###########################################################################################

{
  # watershed_current <- 2
  basin_id_selected <- 22
  mix_n <- n_mix_current
  data_unit <- df_basin_data %>%
    filter(watershed == watershed_current) %>% 
    filter(basin_id == basin_id_selected) %>% 
    pull(chainage_scaled)
  plot_sn(snorm_list[[paste0(watershed_current, '_', basin_id_selected, '_', mix_n)]], y_data = data_unit)
}

# Width Function distance matrix #################################################################################

# DI pairs for distance matrix

# watershed_current <- 2
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

# Clustering with width function #################################################################################

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
# m_l2_pairs %>%
#   saveRDS('m_l2_pairs_nmix3.RDS')
# clusters_l2 %>%
#   saveRDS('clusters_l2_nmix3_02.RDS')
# clusters_agnes_l2 %>%
#   saveRDS('clusters_agnes_l2_nmix3_02.RDS')

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

# Clustering groups and outliers analysis ########################################################################

watershed_current <- 2
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

# Plotting Width Function clusters ##############################################################################

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
# Combine Hypso Cluster and WF Cluster ##########################################################################

watershed_current <- 2
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

# watershed_current <- 2
# df_cluster_hypso_wf <- df_cluster %>% 
#   filter(watershed == watershed_current) %>% 
#   rename(cluster_hypso = cluster) %>% 
#   left_join(df_cluster_l1 %>% 
#               filter(watershed == watershed_current) %>% 
#               rename(cluster_wf_l1 = cluster),
#             by = c('watershed', 'basin_id')) %>% 
#   left_join(df_cluster_l2 %>% 
#               filter(watershed == watershed_current) %>% 
#               rename(cluster_wf_l2 = cluster),
#             by = c('watershed', 'basin_id')) %>% 
#   unite('cluster_hypso_wf_l1', c(cluster_hypso, cluster_wf_l1), remove = FALSE) %>% 
#   unite('cluster_hypso_wf_l2', c(cluster_hypso, cluster_wf_l2), remove = FALSE)
# df_cluster_hypso_wf %>% 
#   saveRDS('df_cluster_hypso_wf.RDS')

# Entanglement between DI and L2 clusters #######################################################################

# Entanglement Plots

watershed_current <- 2
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

#
# Width Function plots full ######################################################################################

# data_widthfunction <- df_basin_data %>% 
#   filter(watershed == watershed_current) %>% 
#   dplyr::select(basin_id, chainage_m_global, chainage_m_global_start:chainage_scaled)
# 
# df_sn_values <- list()
# g_wf_sn <- list()
# g_wf_sn_grid <- list()
# g_wf_sn_scaled <- list()
# g_wf_sn_scaled_grid <- list()
# g_wf_sn_cum <- list()
# 
# for (mix_n in 1:3)
# {
#   df_sn_values[[mix_n]] <- list()
#   g_wf_sn[[mix_n]] <- list()
#   g_wf_sn_scaled[[mix_n]] <- list()
#   g_wf_sn_cum[[mix_n]] <- list()
# }
# 
# for (basin_id_selected in basin_ids)
# {
#   # basin_id_selected <- basin_ids[1]
#   
#   print (paste0('Working with ', basin_id_selected))
#   
#   data_unit <- data_widthfunction %>%
#     filter(basin_id == basin_id_selected) %>% 
#     pull(chainage_scaled)
#   
#   for (mix_n in 1:3) {
#     df_sn_values[[mix_n]][[basin_id_selected]] <- get_sn_xytable(snorm_list[[paste0(basin_id_selected,'_',mix_n)]], min_x = -0.01, max_x = 1.01)
#     
#     df_sn_params_selected <- df_sn_params[[mix_n]] %>% 
#       filter(basin_id == basin_id_selected)
#     
#     snorm_selected <- snorm_list[[paste0(basin_id_selected,'_',mix_n)]]
#     
#     g_wf_sn[[mix_n]][[basin_id_selected]] <- plot_sn(snorm_selected, df_sn_values[[mix_n]][[basin_id_selected]], data_unit, scale_group = FALSE) +
#       labs(x = 'Scaled distance from outlet',
#            y = 'Density',
#            title = paste0('Width Function and Skew Normal Fit for Basin ', basin_id_selected)) +
#       theme_pra_classic +
#       theme(legend.title = element_blank(),
#             legend.position = 'right',
#             legend.direction = 'vertical')
#     
#     g_wf_sn_scaled[[mix_n]][[basin_id_selected]] <- plot_sn(snorm_selected, df_sn_values[[mix_n]][[basin_id_selected]], data_unit, scale_group = TRUE) +
#       labs(x = 'Scaled distance from outlet',
#            y = 'Density',
#            title = paste0('Width Function and Skew Normal Fit for Basin ', basin_id_selected)) +
#       theme_pra_classic +
#       theme(legend.title = element_blank(),
#             legend.position = 'right',
#             legend.direction = 'vertical')
#     
#     g_wf_sn_cum[[mix_n]][[basin_id_selected]] <- plot_sn_cum(snorm_selected, df_sn_values[[mix_n]][[basin_id_selected]], data_unit, plot_sn_fit = TRUE) +
#       scale_color_manual(name = '',
#                          values = c('Observed' = 'blue',
#                                     'Modelled' = 'red')) +
#       labs(x = 'Scaled distance from outlet',
#            y = 'Cumulative Density',
#            title = paste0('Cumulative Density Comparison for Basin ', basin_id_selected, ', mix = ', mix_n)) +
#       theme_pra_classic +
#       theme(legend.title = element_blank(),
#             legend.position = 'right',
#             legend.direction = 'vertical')
#   }
#   
#   g_wf_sn_grid[[basin_id_selected]] <- grid.arrange(g_wf_sn[[1]][[basin_id_selected]],
#                                                     g_wf_sn[[2]][[basin_id_selected]],
#                                                     g_wf_sn[[3]][[basin_id_selected]])
#   
#   g_wf_sn_scaled_grid[[basin_id_selected]] <- grid.arrange(g_wf_sn_scaled[[1]][[basin_id_selected]], 
#                                                            g_wf_sn_scaled[[2]][[basin_id_selected]], 
#                                                            g_wf_sn_scaled[[3]][[basin_id_selected]])
#   
#   # ggsave(width = 8, height = 8, filename = foldered_png(paste0('Unscaled Width Function and Skew Normal Fit for Basin ', str_pad(basin_id_selected, 3, pad = '0'))), plot = g_wf_sn_grid[[basin_id_selected]])
#   ggsave(width = 8, height = 8, filename = foldered_png(paste0('Width Function and Skew Normal Fit for Basin ', str_pad(basin_id_selected, 3, pad = '0'))), plot = g_wf_sn_scaled_grid[[basin_id_selected]])
#   # ggsave(width = 6, height = 4, filename = foldered_png(paste0('Cumulative Density Plot for Basin ', str_pad(basin_id_selected, 3, pad = '0'), ' mix ', mix_n)), plot = g_wf_sn_cum[[mix_n]][[basin_id_selected]])
# }
# 
# df_sn_values %>% 
#   saveRDS('snorm_values_df_01.RDS')
# g_wf_sn %>% 
#   saveRDS('snorm_plot_01.RDS')
# g_wf_sn_grid %>% 
#   saveRDS('snorm_plot_grid_01.RDS')
# g_wf_sn_scaled %>% 
#   saveRDS('snorm_plot_scaled_01.RDS')
# g_wf_sn_scaled_grid %>% 
#   saveRDS('snorm_plot_scaled_grid_01.RDS')
# g_wf_sn_cum %>% 
#   saveRDS('snorm_plot_cum_01.RDS')
# 
# plot_sn(snorm_list[[2]], y_data = data_unit)
# snorm_list %>% 
#   str()

# Kmeans Clustering #################################################################################################################

# n_x_values <- 100
# y_matrix_list <- list()
# for (watershed_current in watersheds)
# {
#   temp_y_matrix_watershed <- c()
#   for (basin_id_current in basin_ids[[watershed_current]])
#   {
#     # print (head(temp_y_matrix_watershed))
#     # watershed_current <- 1
#     # basin_id_current <- 3
#     # print (paste0('Watershed: ', watershed_current, '; Basin: ', basin_id_current))
#     
#     temp_current_df_hypso_formulations <- df_hypso_formulations %>% 
#       filter(watershed == watershed_current) %>% 
#       filter(basin_id == basin_id_current) %>% 
#       select(watershed, basin_id, relative_area_above_elevation, relative_elevation)
#     temp_current_df_hypso_formulations
#     
#     temp_y_matrix_basin <- c()    
#     for (x_temp in seq(0, 1, length.out = n_x_values))
#     {
#       temp_y_matrix_basin <- c(temp_y_matrix_basin, 
#                                approx(x = temp_current_df_hypso_formulations$relative_area_above_elevation, 
#                                       y = temp_current_df_hypso_formulations$relative_elevation, 
#                                       xout = x_temp)$y)
#     }
#     temp_y_matrix_watershed <- cbind(temp_y_matrix_watershed, temp_y_matrix_basin)
#   }
#   y_matrix_list[[watershed_current]] <- temp_y_matrix_watershed
# }
# 
# y_matrix_list %>% 
#   saveRDS('kmeans_matrix.RDS')
# 
# n_clusters <- 5
# watershed_current <- 1
# k <- list()
# df_cluster <- data.frame()
# for(watershed_current in watersheds)
# {
#   k[[watershed_current]] <- kmeans(t(y_matrix_list[[watershed_current]]), n_clusters)
#   for (temp_cluster in 1:n_clusters)
#   {
#     temp_df_cluster <- data.frame(watershed = watershed_current, cluster = temp_cluster, x = seq(0, 1, length.out = 100), y = k[[watershed_current]]$centers[temp_cluster, ])
#     df_cluster <- rbind(df_cluster, temp_df_cluster)
#   }
# }
# 
# watershed_current <- 1
# df_cluster_count <- data.frame(cluster = k[[watershed_current]]$cluster %>% unname()) %>% 
#   group_by(cluster) %>% 
#   summarize(count = n())
# df_cluster %>% 
#   filter(watershed == watershed_current) %>% 
#   left_join(df_cluster_count, by = c('cluster' = 'cluster')) %>% 
#   ggplot(aes(x = x)) +
#   geom_line(aes(y = y, group = cluster, color = count))
# 
# watershed_current <- 2
# df_cluster_count <- data.frame(cluster = k[[watershed_current]]$cluster %>% unname()) %>% 
#   group_by(cluster) %>% 
#   summarize(count = n())
# df_cluster %>% 
#   filter(watershed == watershed_current) %>% 
#   left_join(df_cluster_count, by = c('cluster' = 'cluster')) %>% 
#   ggplot(aes(x = x)) +
#   geom_line(aes(y = y, group = cluster, color = count))
# 
# watershed_current <- 3
# df_cluster_count <- data.frame(cluster = k[[watershed_current]]$cluster %>% unname()) %>% 
#   group_by(cluster) %>% 
#   summarize(count = n())
# df_cluster %>% 
#   filter(watershed == watershed_current) %>% 
#   left_join(df_cluster_count, by = c('cluster' = 'cluster')) %>% 
#   ggplot(aes(x = x)) +
#   geom_line(aes(y = y, group = cluster, color = count))


# Schematic plots ###################################################################################################################

# synthetic_hypsometric <- 
  
watershed_current <- 2
basin_id_current <- 16
temp_position <- 100
temp_x <- df_hypso_formulations %>%
  filter(watershed == watershed_current) %>% 
  filter(basin_id == basin_id_current) %>%
  slice(temp_position) %>% 
  pull(relative_area_above_elevation)
temp_y <- df_hypso_formulations %>%
  filter(watershed == watershed_current) %>% 
  filter(basin_id == basin_id_current) %>%
  slice(temp_position) %>% 
  pull(relative_elevation_predicted)
df_hypso_formulations %>%
  filter(watershed == watershed_current) %>% 
  filter(basin_id == basin_id_current) %>% 
  ggplot() +
  # geom_line(aes(x = relative_area_above_elevation,
  #               y = relative_elevation,
  #               linetype = 'Empirical')) +
  geom_line(aes(x = relative_area_above_elevation,
                y = relative_elevation_predicted)) +
  labs(x = 'Relative area above elevation (a/A)',
       y = 'Relative elevation (h/H)') +
  geom_point(aes(x = 0, y = 1)) +
  geom_point(aes(x = 1, y = 0)) +
  geom_point(aes(x = temp_x, y = temp_y)) +
  # geom_text(aes(x = 0.06, y = 1, label = '(0, 1)')) +
  # geom_text(aes(x = 0.94, y = 0, label = '(1, 0)')) +
  # geom_text(aes(x = temp_x + 0.06, y = temp_y + 0.025, label = '(x, y)')) +
  theme_pra_bw +
  theme(legend.title = element_text(size = 0))
if (output_pptx == 1)
{
  last_plot() %>% 
    create_pptx(pptx_file, width = 4.6, height = 3.8)
}

data_widthfunction <- readRDS('D:/Project/Research/Storage/Sheepscot 10m/Outputs01/data_widthfunction001.RDS')

basin_id_selected <- 32
data_unit <- data_widthfunction %>%
  filter(basin_id == basin_id_selected) %>% 
  mutate(chainage_km = chainage_m/1000) %>% 
  mutate(group = chainage_km%/%1) %>% 
  mutate(group = as.character(group))
data_unit %>%
  ggplot() +
  geom_histogram(mapping = aes(x = chainage_km,
                               y = ..density..,
                               fill = '0'))
# data_unit <- df_basin_data %>%
#   filter(watershed == watershed_current) %>% 
#   filter(basin_id == basin_id_selected) %>% 
#   mutate(chainage_km = chainage_m/1000) %>%
#   mutate(group = chainage_km%/%0.3) %>%
#   mutate(group = as.character(group))
data_unit %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = chainage_scaled,
                               y = ..count../sum(..count..),
                               fill = 'group'),                 # fill = 'white',
                 color = 'grey80') +
  labs(x = 'Scaled distance from outlet',
       y = 'Density',
       title = 'Width Function') +
  theme_pra_bw +
  theme_classic() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size = text_size),
        text = element_text(size = text_size),
        axis.ticks.length = unit(-0.1, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.2,0.2,0.2,0.2), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.2,0.2,0.2,0.2), "cm")))

data_unit %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = chainage_km,
                               y = ..count../sum(..count..),
                               fill = group),                 # fill = 'white',
                 color = 'grey80',
                 # binwidth = .3,
                 breaks = seq(0,5.7,0.2)) +
  scale_x_continuous(breaks = seq(0,7,1)) +
  scale_fill_manual(values = c('0' = '#305CC2',
                               '1' = '#29C22B',
                               '2' = '#C7532C',
                               '3' = '#BD2CC7',
                               '4' = '#29A3A6',
                               '5' = '#C7C044')) +
  labs(x = 'Distance from outlet (km)',
       y = 'Density',
       title = 'Width Function') +
  theme_pra_bw +
  theme_classic() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size = text_size),
        text = element_text(size = text_size),
        axis.ticks.length = unit(-0.1, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.2,0.2,0.2,0.2), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.2,0.2,0.2,0.2), "cm")))

if (output_pptx == 1)
{
  last_plot() %>%
    create_pptx(pptx_file, width = 3.3*1.3, height = 2.5*1.3)
}


# Other similarity comparison #######################################################################################################

scale_fac_to_m2 <- 7.9*10^5
scale_chainage_to_m <- 10^5

# df_basin_data %>% 
#   filter(watershed == 2) %>%
#   filter(fac_m < 5) %>%
#   ggplot() +
#   geom_histogram(aes(x = fac_m))
#   # scale_x_log10()

df_basin_data %>% 
  filter(watershed == 2) %>% 
  select(basin_id:chainage_m_global) %>% 
  arrange(chainage_m_global)



# CTI

df_basin_data_cti <- df_basin_data %>% 
  filter(watershed == 2) %>% 
  select(basin_id, fac_m, slope_deg) %>% 
  filter(slope_deg > 0) %>% 
  mutate(cti = log((fac_m + 1)/tan(slope_deg*pi/180)))

df_basin_data_cti %>% 
  # filter(basin_id < 9) %>% 
  mutate(basin_id = as.factor(basin_id)) %>% 
  ggplot() +
  geom_histogram(aes(x = cti)) + 
  facet_wrap(~basin_id, ncol = 10)

df_basin_data_cti %>% 
  filter(basin_id == 2)

df_basin_data_cti %>% 
  filter(basin_id == 2) %>% 
  ggplot() +
  geom_histogram(aes(x = cti), binwidth = 0.5) 

bin_width <- 0.5
basin_id_1 <- 1
basin_id_2 <- 5
# find_jsd(basin_id_1, basin_id_2, bin_width)
df_basin_data_cti %>% 
  filter(basin_id %in% c(basin_id_1, basin_id_2)) %>% 
  mutate(basin_id = as.factor(basin_id)) %>% 
  ggplot() +
  geom_freqpoly(aes(x = cti, color = basin_id, y = ..density..), binwidth = 0.5) +
  labs(title = paste0('Basins: ', basin_id_1, ' and ', basin_id_2, '; JSD = ', find_jsd(basin_id_1, basin_id_2, bin_width) %>% round(4)))

find_jsd <- function(basin_id_1, basin_id_2, bin_width)
{
  temp_1 <- df_basin_data_cti %>% 
    filter(basin_id == basin_id_1) %>% 
    mutate(bin = cut(cti, seq(0, cti %>% max() %>% ceiling(), bin_width))) %>% 
    group_by(bin) %>% 
    summarize(cti_count = n())
  temp_2 <- df_basin_data_cti %>% 
    filter(basin_id == basin_id_2) %>% 
    mutate(bin = cut(cti, seq(0, cti %>% max() %>% ceiling(), bin_width))) %>% 
    group_by(bin) %>% 
    summarize(cti_count = n())
  temp_joined <- temp_1 %>% 
    left_join(temp_2, by = 'bin') %>% 
    mutate(cti_count.x = ifelse(is.na(cti_count.x), 0, cti_count.x)) %>% 
    mutate(cti_count.y = ifelse(is.na(cti_count.y), 0, cti_count.y)) %>% 
    mutate(cti_count.x = cti_count.x/sum(cti_count.x)) %>% 
    mutate(cti_count.y = cti_count.y/sum(cti_count.y))
  jsd_data <- rbind(temp_joined %>% pull(2), 
                    temp_joined %>% pull(3))
  return (JSD(jsd_data))
}

# CTI pairs for distance matrix

df_cti_pairs <- data.frame()
watershed_current <- 2

for (basin_id_1 in basin_ids[[watershed_current]])
{
  for (basin_id_2 in basin_ids[[watershed_current]])
  {
    print (paste0(basin_id_1, '-', basin_id_2))
    df_cti_pairs <- rbind(df_cti_pairs, 
                         data.frame(watershed = watershed_current, 
                                    basin_id_1, 
                                    basin_id_2, 
                                    jsd = find_jsd(basin_id_1, basin_id_2, bin_width)))
  }
}
df_cti_pairs %>% 
  saveRDS('df_cti_pairs.RDS')

# Clustering

m_cti_pairs <- list()
clusters_cti <- list()
clusters_agnes_cti <- list()

# for (watershed_current in watersheds)
# {
m_cti_pairs[[watershed_current]] <- acast(df_cti_pairs %>% 
                                           filter(watershed == watershed_current), 
                                          basin_id_1 ~ basin_id_2, value.var = 'jsd')
distance_matrix <- as.dist(m_cti_pairs[[watershed_current]])
clusters_cti[[watershed_current]] <- hclust(distance_matrix, method = 'ward.D2')
clusters_agnes_cti[[watershed_current]] <- agnes(distance_matrix, diss = TRUE, method = 'ward')
# }

m_cti_pairs %>% 
  saveRDS('m_cti_pairs_02.RDS')
clusters_cti %>% 
  saveRDS('clusters_cti_02.RDS')
clusters_agnes_cti %>% 
  saveRDS('clusters_agnes_cti_02.RDS')

watershed_name_current <- df_watersheds %>% 
  filter(watershed == watershed_current) %>% 
  pull(watershed_name)

df_cti_pairs %>% 
  filter(watershed == watershed_current) %>% 
  # filter(!(basin_id_1 %in% c(3, 24, 44))) %>% 
  # filter(!(basin_id_2 %in% c(3, 24, 44))) %>% 
  acast(basin_id_1 ~ basin_id_2, value.var = 'jsd') %>% 
  as.dist() %>% 
  hclust(method = 'complete') %>% 
  plot(main = paste0('Hypsometry Cluster Dendrogram for ', watershed_name_current))
df_cti_pairs %>% 
  filter(watershed == watershed_current) %>% 
  # filter(!(basin_id_1 %in% c(3, 24, 44))) %>% 
  # filter(!(basin_id_2 %in% c(3, 24, 44))) %>% 
  acast(basin_id_1 ~ basin_id_2, value.var = 'jsd') %>% 
  as.dist() %>% 
  hclust(method = 'ward.D2') %>% 
  plot(main = paste0('Hypsometry Cluster Dendrogram for ', watershed_name_current, ' (Ward Method)'))

# Gap statistics

clusGap_cti <- df_cti_pairs %>%
  filter(watershed == watershed_current) %>%
  acast(basin_id_1 ~ basin_id_2, value.var = 'jsd') %>%
  clusGap(FUN = hcut, K.max = 15, B = 500) 
clusGap_cti %>%
  saveRDS('clusGap_cti.RDS')
clusGap_cti %>%
  fviz_gap_stat() +
  labs(title = '')
temp_best_k <- 6
clusGap_cti %>%
  fviz_gap_stat_pra(paste0('Gap statistic for hypsometric clusters')) +
  geom_vline(xintercept = temp_best_k, linetype = 'dashed')

# Cluster grouping

clusters_cti <- df_cti_pairs %>% 
  filter(watershed == watershed_current) %>% 
  acast(basin_id_1 ~ basin_id_2, value.var = 'jsd') %>% 
  as.dist() %>% 
  # hclust(method = 'complete') %>%
  hclust(method = 'ward.D2')
df_cluster_cti <- data.frame(watershed = watershed_current,
                            basin_id = cutree(clusters_cti, k = temp_best_k) %>% names() %>% as.numeric(),
                            cluster = cutree(clusters_cti, k = temp_best_k))
df_basin_data_cti_cluster <- df_basin_data_cti %>% 
  left_join(df_cluster_cti %>% filter(watershed == watershed_current),
            by = c('basin_id'))

clusters_cti %>% 
  saveRDS('clusters_cti2.RDS')
df_cluster_cti %>% 
  saveRDS('df_cluster_cti.RDS')
df_basin_data_cti_cluster %>%
  saveRDS('df_basin_data_cti_cluster2.RDS')

clusters_cti %>% 
  plot(main = paste0('Hypsometry Cluster Dendrogram'))

df_basin_data_cti_cluster %>% 
  # filter(basin_id < 9) %>% 
  mutate(basin_id = as.factor(basin_id)) %>% 
  ggplot() +
  geom_freqpoly(aes(x = cti, group = basin_id, y = ..density..), binwidth = 0.5, size = 0.1) + 
  facet_wrap(~cluster, ncol = 3) +
  labs(title = 'CTI clusters') +
  theme_bw()
ggsave(filename = foldered_png(paste0('CTI - Clusters CTI for ', watershed_name_current)), width = 8, height = 6)

temp_1 <- df_cluster_hypso_wf %>% 
  group_by(cluster_hypso_wf_l2) %>% 
  mutate(count = n()) %>% 
  filter(count >= 4) %>% 
  ungroup()

df_cluster_cti %>% 
  left_join(temp_1 %>% select(basin_id, cluster_hypso_wf_l2),
            by = 'basin_id') %>% 
  mutate(cluster_hypso_wf_l2 = ifelse(is.na(cluster_hypso_wf_l2), 0, cluster_hypso_wf_l2)) %>% 
  group_by(cluster) %>% 
  mutate(sn = row_number()) %>% 
  ggplot() +
  geom_text(aes(x = 0, y = 100-sn, label = basin_id, color = cluster_hypso_wf_l2)) +
  scale_color_manual(name = 'Bivariate Group',
                     values = c('0' = 'black',
                                '2_3' = 'green',
                                '4_1' = 'purple',
                                '4_5' = 'red',
                                '5_2' = 'blue')) +
  labs(title = 'CTI cluster') +
  facet_wrap(~cluster, nrow = 1) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave(filename = foldered_png(paste0('CTI - Clusters CTI basins for ', watershed_name_current)), width = 6, height = 4)



# rDUNE

df_basin_data_rDUNE <- df_basin_data %>% 
  filter(watershed == 2) %>% 
  select(basin_id, fac_m, elevation_m, chainage_m_global) %>% 
  


#         
# Temp ##############################################################################################################################

# Old data
# df_cluster_l2 <- readRDS('df_cluster_l2_02.RDS')
# df_sn_values <- readRDS('snorm_values_df_01.RDS')
# g_wf_sn <- readRDS('snorm_plot_01.RDS')
# g_wf_sn_grid <- readRDS('snorm_plot_grid_01.RDS')
# g_wf_sn_scaled <- readRDS('snorm_plot_scaled_01.RDS')
# g_wf_sn_scaled_grid <- readRDS('snorm_plot_scaled_grid_01.RDS')
# g_wf_sn_cum <- readRDS('snorm_plot_cum_01.RDS')
# 
# df_cluster_wf <- readRDS('df_cluster_wf.RDS')
# df_wf_representative_l1 <- readRDS('df_wf_representative_l1_02.RDS')
# df_wf_representative_l2 <- readRDS('df_wf_representative_l2_02.RDS')
# df_cluster_representative_l1 <- readRDS('df_cluster_representative_l1_02.RDS')
# df_cluster_representative_l2 <- readRDS('df_cluster_representative_l2_02.RDS')
# df_sn_values_main <- readRDS('df_sn_values_main_03.RDS')
# df_cluster_hypso_wf <- readRDS('df_cluster_hypso_wf.RDS')

# m_l1_pairs <- readRDS('m_l1_pairs.RDS')
# clusters_l1 <- readRDS('clusters_l1_02.RDS')
# df_cluster_l1 <- readRDS('df_cluster_l1_02.RDS')

# m_di_pairs <- readRDS('m_di_pairs_02.RDS')
# basin_id_outliers_di <- readRDS('basin_id_outliers_di.RDS')
# clusters_di <- readRDS('clusters_di_02.RDS')
# # clusters_agnes_di <- readRDS('clusters_agnes_di_02.RDS')
# df_cluster <- readRDS('df_cluster.RDS')
# df_hypso_summary_formulations_current <- readRDS('df_hypso_summary_formulations_current.RDS')
# df_hypso <- readRDS('df_hypso.RDS')  
# df_hypso_cluster <- readRDS('df_hypso_cluster.RDS')  
# df_hypso_representative <- readRDS('df_hypso_representative.RDS')
# df_cluster_representative <- readRDS('df_cluster_representative.RDS')





# # devtools::install_github("slowkow/ggrepel", lib = 'D:\\Unmanaged\\Rlib')
# # library(ggrepel, lib = 'D:\\Unmanaged\\Rlib')
# library(ggrepel)
# 
# df_cluster_hypso_wf %>% 
#   left_join(df_basin_data_summary %>% select(watershed:area_gis_km2),
#             by = c('watershed', 'basin_id')) %>% 
#   mutate(area_gis_km2_scaled = 1 + 5*(area_gis_km2-min(.$area_gis_km2))/(max(.$area_gis_km2)-min(.$area_gis_km2))) %>%
#   ggplot() +
#   geom_jitter(aes(x = cluster_hypso, 
#                   y = cluster_wf,
#                   size = area_gis_km2_scaled),
#               color = 'red',
#               alpha = 0.5,
#               width = 0.4, 
#               height = 0.4) +
#   scale_x_continuous(breaks = seq(min(df_cluster_hypso_wf$cluster_hypso), max(df_cluster_hypso_wf$cluster_hypso), 1)) +
#   scale_y_continuous(breaks = seq(min(df_cluster_hypso_wf$cluster_wf), max(df_cluster_hypso_wf$cluster_wf), 1)) +
#   coord_fixed() +
#   labs(x = 'Hypsometry cluster',
#        y = 'Width function cluster') +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_line(color = 'grey70'))
# 
# temp_pos <- position_jitter(width = 0.4, seed = 13)
# df_cluster_hypso_wf %>% 
#   left_join(df_basin_data_summary %>% select(watershed:area_gis_km2),
#             by = c('watershed', 'basin_id')) %>% 
#   # mutate(area_gis_km2_scaled = 100 + 50*(area_gis_km2-min(.$area_gis_km2))/(max(.$area_gis_km2)-min(.$area_gis_km2))) %>%
#   ggplot(aes(x = cluster_hypso, 
#              y = cluster_wf)) +
#   geom_point(aes(size = area_gis_km2),
#              color = 'red',
#              alpha = 0.5,
#              position = temp_pos) +
#   geom_text(aes(label = basin_id),
#             position = temp_pos) +
#   scale_x_continuous(breaks = seq(min(df_cluster_hypso_wf$cluster_hypso), max(df_cluster_hypso_wf$cluster_hypso), 1)) +
#   scale_y_continuous(breaks = seq(min(df_cluster_hypso_wf$cluster_wf), max(df_cluster_hypso_wf$cluster_wf), 1)) +
#   scale_size_continuous(name = 'Area (km2)',
#                         range = c(4, 12)) +
#   coord_fixed() +
#   labs(x = 'Hypsometry cluster',
#        y = 'Width function cluster') +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_line(color = 'grey70'))
# 
# temp_df_cluster_hypso_wf <- df_cluster_hypso_wf %>% 
#   mutate(x_jitter = NA,
#          y_jitter = NA)
# for (cluster_current in temp_df_cluster_hypso_wf %>% pull(cluster_hypso) %>% unique())
# {
#   # cluster_current <- 1
#   temp_n <- temp_df_cluster_hypso_wf %>%
#     filter(cluster_hypso == cluster_current) %>%
#     nrow()
#   # runif(temp_n, cluster_current - 0.4, cluster_current + 0.4)
#   temp_df_cluster_hypso_wf$x_jitter[temp_df_cluster_hypso_wf$cluster_hypso == cluster_current] <- seq(cluster_current - 0.4, cluster_current + 0.4, length.out = temp_n) %>% sample()
# }
# for (cluster_current in temp_df_cluster_hypso_wf %>% pull(cluster_wf) %>% unique())
# {
#   # cluster_current <- 1
#   temp_n <- temp_df_cluster_hypso_wf %>%
#     filter(cluster_wf == cluster_current) %>%
#     nrow()
#   # runif(temp_n, cluster_current - 0.4, cluster_current + 0.4)
#   temp_df_cluster_hypso_wf$y_jitter[temp_df_cluster_hypso_wf$cluster_wf == cluster_current] <- seq(cluster_current - 0.4, cluster_current + 0.4, length.out = temp_n) %>% sample()
# }
# g_cross_cluster <- temp_df_cluster_hypso_wf %>% 
#   left_join(df_basin_data_summary %>% select(watershed:area_gis_km2),
#             by = c('watershed', 'basin_id')) %>% 
#   ggplot(aes(x = x_jitter, 
#              y = y_jitter)) +
#   geom_point(aes(size = area_gis_km2),
#              color = 'red',
#              alpha = 0.5) +
#   geom_text(aes(label = basin_id)) +
#   scale_x_continuous(breaks = seq(min(df_cluster_hypso_wf$cluster_hypso), max(df_cluster_hypso_wf$cluster_hypso), 1)) +
#   scale_y_continuous(breaks = seq(min(df_cluster_hypso_wf$cluster_wf), max(df_cluster_hypso_wf$cluster_wf), 1)) +
#   scale_size_continuous(name = 'Area (km2)',
#                         range = c(4, 10)) +
#   coord_fixed() +
#   labs(x = 'Hypsometry cluster',
#        y = 'Width function cluster') +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_line(color = 'grey70'),
#         legend.position = 'bottom'); g_cross_cluster

#

watershed_current <- 2
watershed_name_current <- df_watersheds %>% 
  filter(watershed == watershed_current) %>% 
  pull(watershed_name)

temp_h1 <- 0.3
temp_h2 <- 0.7

temp_df_cluster <- data.frame(watershed = watershed_current,
                              basin_id = basin_ids[[watershed_current]],
                              cluster = cutree(clusters_agnes_l2[[watershed_current]], h = temp_h1))
temp_n_clusters <- temp_df_cluster %>% 
  select(cluster) %>% 
  max()
df_sn_values_main %>%
  left_join(temp_df_cluster,
            by = c('watershed', 'basin_id')) %>% 
  filter(watershed == watershed_current) %>% 
  # mutate(outlier = ifelse(basin_id %in% temp_basin_ids_outliers, 'Outlier', 'Not Outlier')) %>% 
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
  facet_wrap(~cluster, nrow = temp_n_clusters %>% sqrt() %>% floor())
ggsave(filename = foldered_png(paste0('Clusters WF L2 for ', watershed_name_current)), width = 14, height = 7)

basin_id_outliers_l2 <- temp_df_cluster %>% 
  group_by(cluster) %>% 
  mutate(n_members = n()) %>% 
  filter(n_members <= 1) %>% 
  pull(basin_id)
temp_basin_ids <- (basin_ids[[watershed_current]])[!(basin_ids[[watershed_current]] %in% basin_id_outliers_l2
)]
temp_clusters <- df_wf_pairs %>% 
  filter(watershed == watershed_current) %>% 
  filter(!(basin_id_1 %in% basin_id_outliers_l2
  )) %>%
  filter(!(basin_id_2 %in% basin_id_outliers_l2
  )) %>%
  acast(basin_id_1 ~ basin_id_2, value.var = 'l2') %>% 
  as.dist() %>% 
  # hclust(method = 'complete') %>%
  hclust(method = 'ward.D2') %>%
  cutree(h = temp_h2)
temp_df_cluster_no <- data.frame(watershed = watershed_current,
                                 basin_id = temp_basin_ids,
                                 cluster = temp_clusters)
temp_n_clusters <- temp_df_cluster_no %>% 
  pull(cluster) %>% 
  unique() %>% 
  length()
df_sn_values_main %>%
  filter(watershed == watershed_current) %>% 
  # filter(!(basin_id %in% temp_basin_ids_outliers)) %>% 
  left_join(temp_df_cluster_no,
            by = c('watershed', 'basin_id')) %>%
  # mutate(cluster = as.character(cluster)) %>% 
  # mutate(cluster = ifelse(is.na(cluster), 'Outliers', cluster)) %>% 
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
       title = paste0('Width Function L2 Cluster (outlier removed) for ', watershed_name_current)) +
  theme_bw() +
  # facet_wrap(~cluster, nrow = 3)
  facet_wrap(~cluster, nrow = temp_n_clusters %>% sqrt() %>% floor())
# ggsave(filename = foldered_png(paste0('Clusters WF L2 (outlier removed) for ', watershed_name_current)), width = 7, height = 6)
ggsave(filename = foldered_png(paste0('Clusters WF L2 (outlier removed) for ', watershed_name_current)), width = 14, height = 7)

#





temp_df_cluster <- data.frame(watershed = watershed_current,
                              basin_id = basin_ids[[watershed_current]],
                              cluster = cutree(clusters_l2[[watershed_current]], k = 7))
df_sn_values_main %>%
  left_join(temp_df_cluster,
            by = c('watershed', 'basin_id')) %>% 
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
       title = paste0('Width Function L2 Cluster for ', watershed_name_current)) +
  theme_bw() +
  facet_wrap(~cluster, nrow = 3)
# ggsave(filename = foldered_png(paste0('Clusters WF1 L2 for ', watershed_name_current)), width = 14, height = 7)

df_sn_values_main %>%
  left_join(temp_df_cluster,
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
# ggsave(filename = foldered_png(paste0('Clusters WF1 L2 for ', watershed_name_current)), width = 14, height = 7)

#








clusters_l2[[watershed_current]] %>% 
  plot(hang = -1,
       main = paste0('Width Function (L2) Cluster Dendogram for ', watershed_name_current),
       sub = '',
       xlab = 'Basins',
       ylab = 'L2')
clusters_l2[[watershed_current]] %>%
  as.dendrogram() %>%
  rect.dendrogram(k = 11)
dev.copy(png, 
         foldered_png(paste0('Width Function (L2) Cluster Dendogram for ', watershed_name_current)),
         width = 1600,
         height = 700); dev.off()

temp_df_cluster <- data.frame(watershed = watershed_current,
                              basin_id = basin_ids[[watershed_current]],
                              cluster = cutree(clusters_l2[[watershed_current]], k = 11))
df_sn_values_main %>%
  left_join(temp_df_cluster,
            by = c('watershed', 'basin_id')) %>% 
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
       title = paste0('Width Function L2 Cluster for ', watershed_name_current)) +
  theme_bw() +
  facet_wrap(~cluster, nrow = 3)
ggsave(filename = foldered_png(paste0('Clusters WF1 L2 for ', watershed_name_current)), width = 12, height = 7)












# Clustering and Cutree (L2)

clusters_l2 <- list()
m_l2_pairs <- list()

watershed_current <- 1
m_l2_pairs[[watershed_current]] <- acast(df_wf_pairs %>% 
                                           filter(watershed == watershed_current), basin_id_1 ~ basin_id_2, value.var = 'l2')
distance_matrix <- as.dist(m_l2_pairs[[watershed_current]])
clusters_l2[[watershed_current]] <- hclust(distance_matrix)
plot(clusters_l2[[watershed_current]])
abline(h = 0.7, col = 'red')
df_cluster_l2_1 <- data.frame(watershed = watershed_current,
                              basin_id = basin_ids[[watershed_current]],
                              cluster = cutree(clusters_l2[[watershed_current]], h = 0.7))

watershed_current <- 2
m_l2_pairs[[watershed_current]] <- acast(df_wf_pairs %>% 
                                           filter(watershed == watershed_current), basin_id_1 ~ basin_id_2, value.var = 'l2')
distance_matrix <- as.dist(m_l2_pairs[[watershed_current]])
clusters_l2[[watershed_current]] <- hclust(distance_matrix)
plot(clusters_l2[[watershed_current]])
abline(h = 0.55, col = 'red')
df_cluster_l2_2 <- data.frame(watershed = watershed_current,
                              basin_id = basin_ids[[watershed_current]],
                              cluster = cutree(clusters_l2[[watershed_current]], h = 0.55))

watershed_current <- 3
m_l2_pairs[[watershed_current]] <- acast(df_wf_pairs %>% 
                                           filter(watershed == watershed_current), basin_id_1 ~ basin_id_2, value.var = 'l2')
distance_matrix <- as.dist(m_l2_pairs[[watershed_current]])
clusters_l2[[watershed_current]] <- hclust(distance_matrix)
plot(clusters_l2[[watershed_current]])
abline(h = 0.81, col = 'red')
df_cluster_l2_3 <- data.frame(watershed = watershed_current,
                              basin_id = basin_ids[[watershed_current]],
                              cluster = cutree(clusters_l2[[watershed_current]], h = 0.81))

df_cluster_l2 <- rbind(df_cluster_l2_1,
                       df_cluster_l2_2,
                       df_cluster_l2_3)

m_l2_pairs %>% 
  saveRDS('m_l2_pairs.RDS')
clusters_l2 %>% 
  saveRDS('clusters_l2_02.RDS')
df_cluster_l2 %>% 
  saveRDS('df_cluster_l2_02.RDS')

# Clustering and Cutree (L1)

clusters_l1 <- list()
m_l1_pairs <- list()

watershed_current <- 1
m_l1_pairs[[watershed_current]] <- acast(df_wf_pairs %>% 
                                           filter(watershed == watershed_current), basin_id_1 ~ basin_id_2, value.var = 'l1')
distance_matrix <- as.dist(m_l1_pairs[[watershed_current]])
clusters_l1[[watershed_current]] <- hclust(distance_matrix)
plot(clusters_l1[[watershed_current]])
abline(h = 0.57, col = 'red')
df_cluster_l1_1 <- data.frame(watershed = watershed_current,
                              basin_id = basin_ids[[watershed_current]],
                              cluster = cutree(clusters_l1[[watershed_current]], h = 0.57))

watershed_current <- 2
m_l1_pairs[[watershed_current]] <- acast(df_wf_pairs %>% 
                                           filter(watershed == watershed_current), basin_id_1 ~ basin_id_2, value.var = 'l1')
distance_matrix <- as.dist(m_l1_pairs[[watershed_current]])
clusters_l1[[watershed_current]] <- hclust(distance_matrix)
plot(clusters_l1[[watershed_current]])
abline(h = 0.5, col = 'red')
df_cluster_l1_2 <- data.frame(watershed = watershed_current,
                              basin_id = basin_ids[[watershed_current]],
                              cluster = cutree(clusters_l1[[watershed_current]], h = 0.5))

watershed_current <- 3
m_l1_pairs[[watershed_current]] <- acast(df_wf_pairs %>% 
                                           filter(watershed == watershed_current), basin_id_1 ~ basin_id_2, value.var = 'l1')
distance_matrix <- as.dist(m_l1_pairs[[watershed_current]])
clusters_l1[[watershed_current]] <- hclust(distance_matrix)
plot(clusters_l1[[watershed_current]])
abline(h = 0.68, col = 'red')
df_cluster_l1_3 <- data.frame(watershed = watershed_current,
                              basin_id = basin_ids[[watershed_current]],
                              cluster = cutree(clusters_l1[[watershed_current]], h = 0.68))

df_cluster_l1 <- rbind(df_cluster_l1_1,
                       df_cluster_l1_2,
                       df_cluster_l1_3)

m_l1_pairs %>% 
  saveRDS('m_l1_pairs.RDS')
clusters_l1 %>% 
  saveRDS('clusters_l1_02.RDS')
df_cluster_l1 %>% 
  saveRDS('df_cluster_l1_02.RDS')

df_cluster_wf <- df_cluster_l1 %>% 
  rename(cluster_l1 = cluster) %>% 
  left_join(df_cluster_l2 %>% rename(cluster_l2 = cluster),
            by = c('watershed', 'basin_id'))

df_cluster_wf %>% 
  saveRDS('df_cluster_wf.RDS')

# Mean representative Hypsometric Curve (L1)

df_wf_representative_l1 <- df_sn_values_main %>% 
  group_by(watershed, cluster_l1, x) %>% 
  summarise(y_mean = mean(y)) %>% 
  ungroup()
df_wf_representative_l1 %>% 
  saveRDS('df_wf_representative_l1_02.RDS')

# Mean representative Hypsometric Curve (L2)

df_wf_representative_l2 <- df_sn_values_main %>% 
  group_by(watershed, cluster_l2, x) %>% 
  summarise(y_mean = mean(y)) %>% 
  ungroup()
df_wf_representative_l2 %>% 
  saveRDS('df_wf_representative_l2_02.RDS')

# Median representative Width Function (L1)

df_cluster_representative_l1 <- data.frame()
# watershed_current <- 2
mix_n <- 2
for (watershed_current in watersheds)
{
  for (cluster_current in df_sn_values_main %>% filter(watershed == watershed_current) %>% pull(cluster_l1) %>% unique())
  {
    # cluster_current <- 1
    
    temp_basin_ids <- df_sn_values_main %>%
      filter(watershed == watershed_current) %>%
      filter(cluster_l1 == cluster_current) %>%
      pull(basin_id) %>%
      unique()
    
    temp_lowest_l1 <- Inf
    temp_current_basin_id <- NA
    for (basin_id_current in temp_basin_ids)
    {
      # basin_id_current <- temp_basin_ids[1]
      
      del_x <- df_sn_values_main %>%
        filter(watershed == watershed_current) %>%
        filter(basin_id == basin_id_current) %>% 
        mutate(del_y = x - lag(x)) %>% 
        slice(2) %>% 
        pull(del_y)
      
      temp_y <- df_sn_values_main %>%
        filter(watershed == watershed_current) %>%
        filter(basin_id == basin_id_current) %>% 
        pull(y)
      temp_y_mean <- df_wf_representative_l1 %>%
        filter(watershed == watershed_current) %>%
        filter(cluster_l1 == cluster_current) %>% 
        pull(y_mean)
      
      temp_current_l1 <- get_pd_index_y(temp_y, temp_y_mean, del_x, index = 2)
      
      if (temp_current_l1 < temp_lowest_l1)
      {
        temp_lowest_l1 = temp_current_l1
        temp_current_basin_id <- basin_id_current
      }
    }
    
    df_cluster_representative_l1 <- rbind(df_cluster_representative_l1, data.frame(watershed = watershed_current,
                                                                                   cluster_l1 = cluster_current,
                                                                                   basin_id = temp_current_basin_id))
  }
}
df_cluster_representative_l1 %>%
  saveRDS('df_cluster_representative_l1_02.RDS')

# Median representative Width Function (L2)

df_cluster_representative_l2 <- data.frame()
# watershed_current <- 2
mix_n <- 2
for (watershed_current in watersheds)
{
  for (cluster_current in df_sn_values_main %>% filter(watershed == watershed_current) %>% pull(cluster_l2) %>% unique())
  {
    # cluster_current <- 1
    
    temp_basin_ids <- df_sn_values_main %>%
      filter(watershed == watershed_current) %>%
      filter(cluster_l2 == cluster_current) %>%
      pull(basin_id) %>%
      unique()
    
    temp_lowest_l2 <- Inf
    temp_current_basin_id <- NA
    for (basin_id_current in temp_basin_ids)
    {
      # basin_id_current <- temp_basin_ids[1]
      
      del_x <- df_sn_values_main %>%
        filter(watershed == watershed_current) %>%
        filter(basin_id == basin_id_current) %>% 
        mutate(del_y = x - lag(x)) %>% 
        slice(2) %>% 
        pull(del_y)
      
      temp_y <- df_sn_values_main %>%
        filter(watershed == watershed_current) %>%
        filter(basin_id == basin_id_current) %>% 
        pull(y)
      temp_y_mean <- df_wf_representative_l2 %>%
        filter(watershed == watershed_current) %>%
        filter(cluster_l2 == cluster_current) %>% 
        pull(y_mean)
      
      temp_current_l2 <- get_pd_index_y(temp_y, temp_y_mean, del_x)
      
      if (temp_current_l2 < temp_lowest_l2)
      {
        temp_lowest_l2 = temp_current_l2
        temp_current_basin_id <- basin_id_current
      }
    }
    
    df_cluster_representative_l2 <- rbind(df_cluster_representative_l2, data.frame(watershed = watershed_current,
                                                                                   cluster_l2 = cluster_current,
                                                                                   basin_id = temp_current_basin_id))
  }
}
df_cluster_representative_l2 %>%
  saveRDS('df_cluster_representative_l2_02.RDS')

# Update WF data table from parameters

df_sn_values_main <- df_sn_values_main %>%
  left_join(df_cluster_representative_l1 %>%
              mutate(representative_l1 = 1),
            by = c('watershed', 'cluster_l1', 'basin_id')) %>% 
  left_join(df_cluster_representative_l2 %>%
              mutate(representative_l2 = 1),
            by = c('watershed', 'cluster_l2', 'basin_id')) %>% 
  mutate(representative_l1 = ifelse(is.na(representative_l1), 0, representative_l1)) %>% 
  mutate(representative_l2 = ifelse(is.na(representative_l2), 0, representative_l2))

df_sn_values_main %>% 
  saveRDS('df_sn_values_main_03.RDS')





# Clustering with width function (optimal number of clusters)

# Congo 

watershed_current <- 1
watershed_name_current <- df_watersheds %>% 
  filter(watershed == watershed_current) %>% 
  pull(watershed_name)

plot(2:20, sapply(2:20, function(i) { 
  mean(silhouette(cutree(clusters_l1[[watershed_current]], i), dmatrix=m_l1_pairs[[watershed_current]])[,"sil_width"]) }),
  main = paste0('Optimal Number of L1 clusters for ', watershed_name_current),
  xlab="Number of clusters", 
  ylab="Average Silhouette", 
  type="b", 
  pch=20)
abline(v = 7, col = 'red')
dev.copy(png, 
         foldered_png(paste0('Optimal Clustering for Width Function (L1) for ', watershed_name_current)),
         width = 700,
         height = 400); dev.off()
plot(2:20, sapply(2:20, function(i) { 
  mean(silhouette(cutree(clusters_l2[[watershed_current]], i), dmatrix=m_l2_pairs[[watershed_current]])[,"sil_width"]) }),
  main = paste0('Optimal Number of L1 clusters for ', watershed_name_current),
  xlab="Number of clusters", 
  ylab="Average Silhouette", 
  type="b", 
  pch=20)
abline(v = 13, col = 'red')
dev.copy(png, 
         foldered_png(paste0('Optimal Clustering for Width Function (L2) for ', watershed_name_current)),
         width = 700,
         height = 400); dev.off()

clusters_l1[[watershed_current]] %>% 
  plot(hang = -1,
       main = paste0('Width Function (L1) Cluster Dendogram for ', watershed_name_current),
       sub = '',
       xlab = 'Basins',
       ylab = 'L1')
clusters_l1[[watershed_current]] %>%
  as.dendrogram() %>%
  rect.dendrogram(k = 7)
dev.copy(png, 
         foldered_png(paste0('Width Function (L1) Cluster Dendogram for ', watershed_name_current)),
         width = 1600,
         height = 700); dev.off()
clusters_l2[[watershed_current]] %>% 
  plot(hang = -1,
       main = paste0('Width Function (L2) Cluster Dendogram for ', watershed_name_current),
       sub = '',
       xlab = 'Basins',
       ylab = 'L2')
clusters_l2[[watershed_current]] %>%
  as.dendrogram() %>%
  rect.dendrogram(k = 13)
dev.copy(png, 
         foldered_png(paste0('Width Function (L2) Cluster Dendogram for ', watershed_name_current)),
         width = 1600,
         height = 700); dev.off()

temp_df_cluster <- data.frame(watershed = watershed_current,
                              basin_id = basin_ids[[watershed_current]],
                              cluster = cutree(clusters_l1[[watershed_current]], k = 7))
df_sn_values_main %>%
  left_join(temp_df_cluster,
            by = c('watershed', 'basin_id')) %>% 
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
       title = paste0('Width Function L1 Cluster for ', watershed_name_current)) +
  theme_bw() +
  facet_wrap(~cluster, nrow = 3)
ggsave(filename = foldered_png(paste0('Clusters WF1 L1 for ', watershed_name_current)), width = 10, height = 7)
temp_df_cluster <- data.frame(watershed = watershed_current,
                              basin_id = basin_ids[[watershed_current]],
                              cluster = cutree(clusters_l2[[watershed_current]], k = 13))
df_sn_values_main %>%
  left_join(temp_df_cluster,
            by = c('watershed', 'basin_id')) %>% 
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
       title = paste0('Width Function L2 Cluster for ', watershed_name_current)) +
  theme_bw() +
  facet_wrap(~cluster, nrow = 3)
ggsave(filename = foldered_png(paste0('Clusters WF1 L2 for ', watershed_name_current)), width = 14, height = 7)

# Narmada

watershed_current <- 2
watershed_name_current <- df_watersheds %>% 
  filter(watershed == watershed_current) %>% 
  pull(watershed_name)

plot(2:20, sapply(2:20, function(i) { 
  mean(silhouette(cutree(clusters_l1[[watershed_current]], i), dmatrix=m_l1_pairs[[watershed_current]])[,"sil_width"]) }),
  main = paste0('Optimal Number of L1 clusters for ', watershed_name_current),
  xlab="Number of clusters", 
  ylab="Average Silhouette", 
  type="b", 
  pch=20)
abline(v = 12, col = 'red')
dev.copy(png, 
         foldered_png(paste0('Optimal Clustering for Width Function (L1) for ', watershed_name_current)),
         width = 700,
         height = 400); dev.off()
plot(2:20, sapply(2:20, function(i) { 
  mean(silhouette(cutree(clusters_l2[[watershed_current]], i), dmatrix=m_l2_pairs[[watershed_current]])[,"sil_width"]) }),
  main = paste0('Optimal Number of L1 clusters for ', watershed_name_current),
  xlab="Number of clusters", 
  ylab="Average Silhouette", 
  type="b", 
  pch=20)
abline(v = 11, col = 'red')
dev.copy(png, 
         foldered_png(paste0('Optimal Clustering for Width Function (L2) for ', watershed_name_current)),
         width = 700,
         height = 400); dev.off()

clusters_l1[[watershed_current]] %>% 
  plot(hang = -1,
       main = paste0('Width Function (L1) Cluster Dendogram for ', watershed_name_current),
       sub = '',
       xlab = 'Basins',
       ylab = 'L1')
clusters_l1[[watershed_current]] %>%
  as.dendrogram() %>%
  rect.dendrogram(k = 12)
dev.copy(png, 
         foldered_png(paste0('Width Function (L1) Cluster Dendogram for ', watershed_name_current)),
         width = 1600,
         height = 700); dev.off()
clusters_l2[[watershed_current]] %>% 
  plot(hang = -1,
       main = paste0('Width Function (L2) Cluster Dendogram for ', watershed_name_current),
       sub = '',
       xlab = 'Basins',
       ylab = 'L2')
clusters_l2[[watershed_current]] %>%
  as.dendrogram() %>%
  rect.dendrogram(k = 11)
dev.copy(png, 
         foldered_png(paste0('Width Function (L2) Cluster Dendogram for ', watershed_name_current)),
         width = 1600,
         height = 700); dev.off()

temp_df_cluster <- data.frame(watershed = watershed_current,
                              basin_id = basin_ids[[watershed_current]],
                              cluster = cutree(clusters_l1[[watershed_current]], k = 12))
df_sn_values_main %>%
  left_join(temp_df_cluster,
            by = c('watershed', 'basin_id')) %>% 
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
       title = paste0('Width Function L1 Cluster for ', watershed_name_current)) +
  theme_bw() +
  facet_wrap(~cluster, nrow = 3)
ggsave(filename = foldered_png(paste0('Clusters WF1 L1 for ', watershed_name_current)), width = 12, height = 7)
temp_df_cluster <- data.frame(watershed = watershed_current,
                              basin_id = basin_ids[[watershed_current]],
                              cluster = cutree(clusters_l2[[watershed_current]], k = 11))
df_sn_values_main %>%
  left_join(temp_df_cluster,
            by = c('watershed', 'basin_id')) %>% 
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
       title = paste0('Width Function L2 Cluster for ', watershed_name_current)) +
  theme_bw() +
  facet_wrap(~cluster, nrow = 3)
ggsave(filename = foldered_png(paste0('Clusters WF1 L2 for ', watershed_name_current)), width = 12, height = 7)

# Yukon

watershed_current <- 3
watershed_name_current <- df_watersheds %>% 
  filter(watershed == watershed_current) %>% 
  pull(watershed_name)

plot(2:20, sapply(2:20, function(i) { 
  mean(silhouette(cutree(clusters_l1[[watershed_current]], i), dmatrix=m_l1_pairs[[watershed_current]])[,"sil_width"]) }),
  main = paste0('Optimal Number of L1 clusters for ', watershed_name_current),
  xlab="Number of clusters", 
  ylab="Average Silhouette", 
  type="b", 
  pch=20)
abline(v = 11, col = 'red')
dev.copy(png, 
         foldered_png(paste0('Optimal Clustering for Width Function (L1) for ', watershed_name_current)),
         width = 700,
         height = 400); dev.off()
plot(2:20, sapply(2:20, function(i) { 
  mean(silhouette(cutree(clusters_l2[[watershed_current]], i), dmatrix=m_l2_pairs[[watershed_current]])[,"sil_width"]) }),
  main = paste0('Optimal Number of L1 clusters for ', watershed_name_current),
  xlab="Number of clusters", 
  ylab="Average Silhouette", 
  type="b", 
  pch=20)
abline(v = 7, col = 'red')
dev.copy(png, 
         foldered_png(paste0('Optimal Clustering for Width Function (L2) for ', watershed_name_current)),
         width = 700,
         height = 400); dev.off()

clusters_l1[[watershed_current]] %>% 
  plot(hang = -1,
       main = paste0('Width Function (L1) Cluster Dendogram for ', watershed_name_current),
       sub = '',
       xlab = 'Basins',
       ylab = 'L1')
clusters_l1[[watershed_current]] %>%
  as.dendrogram() %>%
  rect.dendrogram(k = 11)
dev.copy(png, 
         foldered_png(paste0('Width Function (L1) Cluster Dendogram for ', watershed_name_current)),
         width = 1600,
         height = 700); dev.off()
clusters_l2[[watershed_current]] %>% 
  plot(hang = -1,
       main = paste0('Width Function (L2) Cluster Dendogram for ', watershed_name_current),
       sub = '',
       xlab = 'Basins',
       ylab = 'L2')
clusters_l2[[watershed_current]] %>%
  as.dendrogram() %>%
  rect.dendrogram(k = 7)
dev.copy(png, 
         foldered_png(paste0('Width Function (L2) Cluster Dendogram for ', watershed_name_current)),
         width = 1600,
         height = 700); dev.off()

temp_df_cluster <- data.frame(watershed = watershed_current,
                              basin_id = basin_ids[[watershed_current]],
                              cluster = cutree(clusters_l1[[watershed_current]], k = 11))
df_sn_values_main %>%
  left_join(temp_df_cluster,
            by = c('watershed', 'basin_id')) %>% 
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
       title = paste0('Width Function L1 Cluster for ', watershed_name_current)) +
  theme_bw() +
  facet_wrap(~cluster, nrow = 3)
ggsave(filename = foldered_png(paste0('Clusters WF1 L1 for ', watershed_name_current)), width = 12, height = 7)
temp_df_cluster <- data.frame(watershed = watershed_current,
                              basin_id = basin_ids[[watershed_current]],
                              cluster = cutree(clusters_l2[[watershed_current]], k = 7))
df_sn_values_main %>%
  left_join(temp_df_cluster,
            by = c('watershed', 'basin_id')) %>% 
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
       title = paste0('Width Function L2 Cluster for ', watershed_name_current)) +
  theme_bw() +
  facet_wrap(~cluster, nrow = 3)
ggsave(filename = foldered_png(paste0('Clusters WF1 L2 for ', watershed_name_current)), width = 10, height = 7)








# Plotting width function clusters

watershed_current <- 2
watershed_name_current <- df_watersheds %>% 
  filter(watershed == watershed_current) %>% 
  pull(watershed_name)

clusters_l1[[watershed_current]] %>% 
  plot(hang = -1)
clusters_l1[[watershed_current]] %>% 
  plot(hang = -1,
       main = 'Width Function Cluster Dendogram',
       sub = '',
       xlab = 'Basins',
       ylab = 'L1')
abline(h = 0.35, col = 'red')
clusters_l1[[watershed_current]] %>%
  as.dendrogram() %>%
  rect.dendrogram(k = 12)
clusters_l2[[watershed_current]] %>% 
  plot(hang = -1,
       main = 'Width Function Cluster Dendogram',
       sub = '',
       xlab = 'Basins',
       ylab = 'L2')
abline(h = 0.45, col = 'red')
clusters_l2[[watershed_current]] %>%
  as.dendrogram() %>%
  rect.dendrogram(k = 11)
# clusters_l1[[watershed_current]] %>% as.dendrogram() %>% plot()
# clusters_l2[[watershed_current]] %>% plot()
# clusters_l2[[watershed_current]] %>% as.dendrogram() %>% plot()

plot(2:20, sapply(2:20, function(i) { 
  mean(silhouette(cutree(clusters_l1[[watershed_current]], i), dmatrix=m_l1_pairs[[watershed_current]])[,"sil_width"]) }),
  main = 'Optimal Number of L1 clusters',
  xlab="Number of clusters", 
  ylab="Average Silhouette", 
  type="b", 
  pch=20)
dev.copy(png, 
         foldered_png('Optimal Clustering for Width Function (L1)'),
         width = 700,
         height = 400); dev.off()
plot(2:20, sapply(2:20, function(i) { 
  mean(silhouette(cutree(clusters_l2[[watershed_current]], i), dmatrix=m_l2_pairs[[watershed_current]])[,"sil_width"]) }),
  main = 'Optimal Number of L2 clusters',
  xlab="Number of clusters", 
  ylab="Average Silhouette", 
  type="b", 
  pch=20)
dev.copy(png, 
         foldered_png('Optimal Clustering for Width Function (L2)'),
         width = 700,
         height = 400); dev.off()

df_cluster_l1 %>% 
  filter(watershed == watershed_current) %>%  
  group_by(watershed, cluster) %>% 
  summarise(n = n()) %>% 
  left_join(df_watersheds, by = 'watershed') %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = n), stat = 'identity') +
  labs(x = 'Cluster',
       y = 'Members',
       title = 'Number of members in Width Function L1 clusters') +
  # facet_wrap(~watershed_name, nrow = 1, scales = 'free_x') +
  theme_bw()
ggsave(filename = foldered_png('Cluster Count WF L1'), width = 8, height = 4)
df_cluster_l2 %>% 
  filter(watershed == watershed_current) %>%  
  group_by(watershed, cluster) %>% 
  summarise(n = n()) %>% 
  left_join(df_watersheds, by = 'watershed') %>% 
  ggplot() +
  geom_bar(aes(x = factor(cluster), y = n), stat = 'identity') +
  labs(x = 'Cluster',
       y = 'Members',
       title = 'Number of members in Width Function L2 clusters') +
  # facet_wrap(~watershed_name, nrow = 1, scales = 'free_x') +
  theme_bw()
ggsave(filename = foldered_png('Cluster Count WF L2'), width = 8, height = 4)

df_sn_values_main %>%
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
       title = 'Width Function L1 Cluster') +
  theme_bw() +
  facet_wrap(~cluster_l1, nrow = 3)
ggsave(filename = foldered_png('Clusters WF1 L1'), width = 8, height = 7)
df_sn_values_main %>%
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
       title = 'Width Function L2 Cluster') +
  theme_bw() +
  facet_wrap(~cluster_l2, nrow = 3)
ggsave(filename = foldered_png('Clusters WF1 L2'), width = 11, height = 7)

df_sn_values_main %>%
  filter(watershed == watershed_current) %>% 
  mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster_l1 = factor(cluster_l1)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y, 
                color = cluster_l1,
                group = basin_id),
            size = 0.1) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Hydrological Distance',
       y = '',
       title = 'Width Function L1 Clusters') +
  theme_bw()
ggsave(filename = foldered_png('Clusters WF2 L1'), width = 6, height = 4)
df_sn_values_main %>%
  filter(watershed == watershed_current) %>% 
  mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster_l2 = factor(cluster_l2)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y, 
                color = cluster_l2,
                group = basin_id),
            size = 0.1) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Hydrological Distance',
       y = '',
       title = 'Width Function L2 Clusters') +
  theme_bw()
ggsave(filename = foldered_png('Clusters WF2 L2'), width = 6, height = 4)

df_wf_representative_l1 %>% 
  filter(watershed == watershed_current) %>% 
  # mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster_l1)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y_mean, 
                color = cluster,
                group = cluster),
            size = 0.6) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Hydrological Distance',
       y = '',
       title = 'Mean Width Function (L1)') +
  theme_bw()
ggsave(filename = foldered_png('Clusters Mean WF L1'), width = 6, height = 4)
df_wf_representative_l2 %>% 
  filter(watershed == watershed_current) %>% 
  # mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster_l2)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y_mean, 
                color = cluster,
                group = cluster),
            size = 0.6) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Hydrological Distance',
       y = '',
       title = 'Mean Width Function (L2)') +
  theme_bw()
ggsave(filename = foldered_png('Clusters Mean WF L2'), width = 6, height = 4)

df_wf_representative_l1 %>% 
  filter(watershed == watershed_current) %>% 
  # mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster_l1)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y_mean, 
                # color = cluster,
                group = cluster),
            size = 0.6) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Hydrological Distance',
       y = '',
       title = 'Mean Width Function (L1)') +
  facet_wrap(~cluster, nrow = 2) +
  theme_bw()
ggsave(filename = foldered_png('Clusters Mean WF2 L1'), width = 6, height = 3)
df_wf_representative_l2 %>% 
  filter(watershed == watershed_current) %>% 
  # mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster_l2)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y_mean, 
                # color = cluster,
                group = cluster),
            size = 0.6) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Hydrological Distance',
       y = '',
       title = 'Mean Width Function (L2)') +
  facet_wrap(~cluster, nrow = 3) +
  theme_bw()
ggsave(filename = foldered_png('Clusters Mean WF2 L2'), width = 6, height = 4)

df_wf_representative_l1 %>% 
  filter(watershed == watershed_current) %>% 
  rename(cluster = cluster_l1) %>% 
  mutate(metric = 'L1') %>% 
  rbind(df_wf_representative_l2 %>% 
          filter(watershed == watershed_current) %>% 
          rename(cluster = cluster_l2) %>% 
          mutate(metric = 'L2')) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y_mean, 
                group = cluster),
            size = 0.6) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Hydrological Distance',
       y = '',
       title = 'Mean Width Function (L1 vs L2)') +
  facet_grid(cluster ~ metric) + 
  theme_bw()
ggsave(filename = foldered_png('Clusters Mean WF L1 vs L2'), width = 6, height = 8)

df_sn_values_main %>%
  filter(watershed == watershed_current) %>% 
  filter(representative_l1 == 1) %>% 
  # mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster_l1)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                color = cluster,
                group = cluster),
            size = 0.6) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Hydrological Distance',
       y = '',
       title = 'Representative Width Function (L1)') +
  theme_bw()
ggsave(filename = foldered_png('Clusters Representative WF L1'), width = 6, height = 4)
df_sn_values_main %>%
  filter(watershed == watershed_current) %>% 
  filter(representative_l2 == 1) %>% 
  # mutate(basin_id = factor(basin_id)) %>% 
  mutate(cluster = factor(cluster_l2)) %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                color = cluster,
                group = cluster),
            size = 0.6) +
  # scale_color_brewer(palette = 'Set1',
  #                    name = 'Basin id') +
  labs(x = 'Hydrological Distance',
       y = '',
       title = 'Representative Width Function (L2)') +
  theme_bw()
ggsave(filename = foldered_png('Clusters Representative WF L2'), width = 6, height = 4)

#
