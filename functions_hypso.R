# This file contains files for Hypsometric estimation

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
