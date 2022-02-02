# This file contains functions for divergenge metrics for width function

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

