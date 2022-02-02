# This file contains functions for cross hypso-wf plot

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
