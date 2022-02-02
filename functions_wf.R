# This file contains functions for the estimation of width function (credit to mixsmsn package: https://cran.r-project.org/web/packages/mixsmsn/index.html)
# Formerly named: SMSN_MIX_Pra.R

library(sn)
library(mixsmsn)
library(tidyverse)

dsn_trunc <- function(x, xi = 0, omega = 1, alpha = 0, trunc = NA, domain_lower = -Inf,  domain_upper = Inf, ...)
{
  if (!is.na(trunc))
  {
    dens <- dsn(x, xi = xi, omega = omega, alpha = alpha, ...) / trunc
  }else
  {
    dens <- dsn(x, xi = xi, omega = omega, alpha = alpha, ...)
    # dens <- dens / get_sn_prob(list(mu = xi, sigma2 = omega^2, shape = alpha, pii = 1, trunc = 1), x_lower = domain_lower, x_upper = domain_upper)
    dens <- dens / get_sn_prob_hidden(list(mu = xi, sigma2 = omega^2, shape = alpha, pii = 1, trunc = 1), x_lower = domain_lower, x_upper = domain_upper)
  }
  
  if (domain_lower != -Inf)
  {
    i = 1
    for (y_ind in x)
    {
      if (y_ind < domain_lower)
      {
        dens[i] <- 0
      }
      i = i + 1
    }
  }
  if (domain_upper != Inf)
  {
    i = 1
    for (y_ind in x)
    {
      if (y_ind > domain_upper)
      {
        dens[i] <- 0
      }
      i = i + 1
    }
  }
  return(dens)
}

psn_trunc <- function(x, xi = 0, omega = 1, alpha = 0, trunc = NA, trunc_lower = NA, domain_lower = -Inf,  domain_upper = Inf, ...)
{
  if (!is.na(trunc_lower))
  {
    dens <- (psn(x, xi = xi, omega = omega, alpha = alpha, ...) - trunc_lower)/trunc
  }
  else
  {
    dens <- psn(x, xi = xi, omega = omega, alpha = alpha, ...)
    dens <- (dens - get_sn_prob_hidden(list(mu = xi, sigma2 = omega^2, shape = alpha, pii = 1, trunc = 1), x_lower = -Inf, x_upper = domain_lower)) / get_sn_prob_hidden(list(mu = xi, sigma2 = omega^2, shape = alpha, pii = 1, trunc = 1), x_lower = domain_lower, x_upper = domain_upper)
  }
  if (domain_lower != -Inf)
  {
    i = 1
    for (y_ind in x)
    {
      if (y_ind < domain_lower)
      {
        dens[i] <- 0
      }
      i = i + 1
    }
  }
  if (domain_upper != Inf)
  {
    i = 1
    for (y_ind in x)
    {
      if (y_ind > domain_upper)
      {
        dens[i] <- 1
      }
      i = i + 1
    }
  }
  # print (dens)
  return(dens)
}

fit_sn_old <- function(x_vector, n_mix = NULL, family = "Skew.normal", criteria = TRUE, iter.max = 300, calc.im = TRUE, group = FALSE, nu = 3, ...)
{
  # Deprecated
  # snorm <- smsn.mix(x_vector, nu = nu, g = n_mix, criteria = criteria, group = group, family = family, calc.im = calc.im, iter.max = iter.max, ...)
  snorm <- smsn.mix(x_vector, nu = nu, g = n_mix, criteria = criteria, group = group, family = family, calc.im = calc.im, iter.max = iter.max, ...)
  return (snorm)
}

fit_sn <- function(x_vector, n_mix = NULL, domain_lower = -Inf, domain_upper = Inf, family = "Skew.normal", criteria = TRUE, iter.max = 1000, calc.im = TRUE, group = FALSE, nu = 3, ...)
{
  # snorm <- smsn.mix(x_vector, nu = nu, g = n_mix, criteria = criteria, group = group, family = family, calc.im = calc.im, iter.max = iter.max, ...)
  snorm <- smsn.mix_trunc(x_vector, nu = nu, g = n_mix, criteria = criteria, group = group, family = family, domain_lower = domain_lower, domain_upper = domain_upper, calc.im = calc.im, iter.max = iter.max, ...)
  return (snorm)
}

get_sn_params <- function(snorm)
{
  sn_params <- data.frame(temp = 0)
  n_mix <- length(snorm$mu)
  for (i in 1:n_mix)
  {
    sn_params[,paste0('mix',i)] <- snorm$pii[i]
    sn_params[,paste0('location',i)] <- snorm$mu[i]
    sn_params[,paste0('scale',i)] <- sqrt(snorm$sigma2[i])
    sn_params[,paste0('shape',i)] <- snorm$shape[i]
    sn_params[,paste0('trunc',i)] <- snorm$trunc[i]
    sn_params[,paste0('trunc_lower',i)] <- snorm$trunc_lower[i]
  }
  sn_params <- sn_params[,2:ncol(sn_params)]
  return (sn_params)
}

get_sn_criteria <- function(snorm)
{
  sn_criteria <- data.frame(nmix = length(snorm$mu),
                            iter = snorm$iter,
                            aic = snorm$aic,
                            bic = snorm$bic,
                            edc = snorm$edc,
                            icl = snorm$icl)
  return (sn_criteria)
}

get_sn_params_criteria <- function(snorm)
{
  return (cbind(get_sn_params(snorm), get_sn_criteria(snorm)))
}

get_sn_prob <- function(snorm, x_lower = 0, x_upper = 1)
{
  if (x_lower > x_upper)
  {
    return (NA)
  }
  n_mix <- length(snorm$mu)
  psn_total <- 0
  for (i in 1:n_mix)
  {
    if (x_lower == Inf)
    {
      psn_lessthan = 1
    }else
    {
      psn_lessthan = psn_trunc(x_lower,
                               xi = snorm$mu[i],
                               omega = sqrt(snorm$sigma2[i]),
                               alpha = snorm$shape[i],
                               snorm$trunc[i],
                               snorm$trunc_lower[i],
                               domain_lower = snorm$domain[1],
                               domain_upper = snorm$domain[2])
    }
    if (x_upper == Inf)
    {
      psn_morethan = 0
    }else
    {
      psn_morethan = 1 - psn_trunc(x_upper,
                                   xi = snorm$mu[i],
                                   omega = sqrt(snorm$sigma2[i]),
                                   alpha = snorm$shape[i],
                                   snorm$trunc[i],
                                   snorm$trunc_lower[i],
                                   domain_lower = snorm$domain[1],
                                   domain_upper = snorm$domain[2])
    }      
    psn_total <- psn_total + snorm$pii[i]*(psn_lessthan + psn_morethan)
  }
  return (1 - psn_total)  
}

get_sn_y <- function(snorm, x, get_individual = FALSE)
{
  n_mix <- length(snorm$mu)
  y <- 0
  y_ind <- vector()
  y_ind2 <- vector()
  for (i in 1:n_mix)
  {
    temp <- dsn_trunc(x,
                      snorm$mu[i],
                      sqrt(snorm$sigma2[i]),
                      snorm$shape[i],
                      snorm$trunc[i],
                      domain_lower = snorm$domain[1],
                      domain_upper = snorm$domain[2])
    y_ind <- c(y_ind, temp)
    temp2 <-temp * snorm$pii[i]
    y_ind2 <- c(y_ind2, temp2)
    y <- y + temp * snorm$pii[i]
  }
  if (get_individual == FALSE)
  {
    return (y)
  }else
  {
    return(list(y = y, 
                y_individual = y_ind,
                y_individual2 = y_ind2))
  }
}

get_sn_y_cum <- function(snorm, x)#, get_individual = FALSE)
{
  n_mix <- length(snorm$mu)
  y <- 0
  y_ind <- vector()
  y_ind2 <- vector()
  for (i in 1:n_mix)
  {
    temp <- psn_trunc(x,
                      snorm$mu[i],
                      sqrt(snorm$sigma2[i]),
                      snorm$shape[i],
                      snorm$trunc[i],
                      domain_lower = snorm$domain[1],
                      domain_upper = snorm$domain[2])
    y_ind <- c(y_ind, temp)
    temp2 <-temp * snorm$pii[i]
    y_ind2 <- c(y_ind2, temp2)
    y <- y + temp * snorm$pii[i]
  }
  if(y < 0) # Can happen for very very small values of x (<=E-16)
  {
    y = 0
  }
  
  return (y)
  
  # if (get_individual == FALSE)
  # {
  #   return (y)
  # }else
  # {
  #   return(list(y = y, 
  #               y_individual = y_ind,
  #               y_individual2 = y_ind2))
  # }
}

get_sn_xytable <- function(snorm, min_x = 0, max_x = 1, count_x = 1000, x_vector = NA)
{
  if(is.numeric(x_vector))
  {
    dsn_data <- data.frame(x = x_vector)
  }else
  {
    dsn_data <- data.frame(x = seq(min_x,max_x,length.out = count_x))
  }
  dsn_data
  dsn_data$y <- 0
  n_mix <- length(snorm$mu)
  for (i in 1:n_mix)
  {
    dsn_data[,paste0('y', i)] <- dsn_trunc(dsn_data$x,
                                           snorm$mu[i],
                                           sqrt(snorm$sigma2[i]),
                                           snorm$shape[i],
                                           snorm$trunc[i],
                                           domain_lower = snorm$domain[1],
                                           domain_upper = snorm$domain[2])
    dsn_data[,paste0('ys', i)] <- dsn_data[,paste0('y', i)] * snorm$pii[i]
    dsn_data$y <- dsn_data$y + dsn_data[,paste0('y', i)] * snorm$pii[i]
  }
  
  # dsn_data$y_check <- 0
  # for (i in 1:nrow(dsn_data))
  # {
  #   dsn_data$y_check[i] = get_sn_y(snorm, dsn_data$x[i])
  # }

  dsn_data$y_cum <- 0
  for (i in 1:nrow(dsn_data))
  {
    dsn_data$y_cum[i] = get_sn_y_cum(snorm, dsn_data$x[i])
  }
  
  return (dsn_data)
}

plot_sn <- function(snorm, df_sn_xytable = NA, y_data = NA, plot_groups = TRUE, scale_group = TRUE, plot_sn_fit = TRUE, min_x = 0, max_x = 1, count_x = 1000, x_vector = NA)
{
  if (plot_sn_fit == FALSE)
  {
    g_wf <- ggplot() +
      geom_histogram(data = data.frame(y_data = y_data),
                     mapping = aes(x = y_data,
                                   y = ..density..),
                     fill = 'white',
                     color = 'black')
  }else
  {
    if (is.na(df_sn_xytable))
    {
      df_sn_xytable = get_sn_xytable(snorm, min_x = min_x, max_x = max_x, count_x = count_x, x_vector = x_vector)
    }
    g_wf <- df_sn_xytable %>%
      ggplot()
    if (!is.na(y_data))
    {
      g_wf <- g_wf + 
        geom_histogram(data = data.frame(y_data = y_data),
                       mapping = aes(x = y_data,
                                     y = ..density..),
                       fill = 'white',
                       color = 'black')
    }
    
    if (plot_groups == TRUE)
    {
      text_mix <- character(0)
      n_mix <- length(snorm$mu)
      for (i in 1:n_mix)
      {
        text_mix[i] <- paste0('SN(', 
                              round(snorm$mu[i],3),', ',
                              round(sqrt(snorm$sigma2[i]),3),', ',
                              round(snorm$shape[i],3),'), w=',
                              round(snorm$pii[i],3))
        if (scale_group == TRUE)
        {
          loop_input <- paste0("geom_line(mapping = aes(x = x, y = !!sym(paste0('ys',",i,")), color = text_mix[",i,"]))")
        }else
        {
          loop_input <- paste0("geom_line(mapping = aes(x = x, y = !!sym(paste0('y',",i,")), color = text_mix[",i,"]))")
        }
        g_wf <- g_wf +
          eval(parse(text=loop_input))
      }
    }
    g_wf <- g_wf +
      geom_line(data = df_sn_xytable,
                mapping = aes(x = x,
                              y = y),
                color = 'black',
                size = 1.3) +
      theme(legend.title = element_blank(),
            legend.position = 'bottom',
            legend.direction = 'vertical')
  }
  g_wf
  return (g_wf)  
}

plot_sn_cum <- function(snorm, df_sn_xytable = NA, y_data = NA, plot_sn_fit = TRUE, 
                        data_name = 'Observed', modelled_name = 'Modelled',
                        min_x = 0, max_x = 1, count_x = 1000, x_vector = NA)
{
  if (plot_sn_fit == FALSE)
  {
    g_wf <- ggplot() +
      stat_ecdf(data = data.frame(y_data = y_data),
                mapping = aes(x = y_data),
                geom = 'step',
                color = 'black',
                pad = FALSE)
  }else
  {
    if (is.na(df_sn_xytable))
    {
      df_sn_xytable = get_sn_xytable(snorm,min_x = min_x, max_x = max_x, count_x = count_x, x_vector = x_vector)
    }
    g_wf <- df_sn_xytable %>%
      ggplot()
    if (!is.na(y_data))
    {
      g_wf <- g_wf +
        stat_ecdf(data = data.frame(y_data = y_data),
                  mapping = aes(x = y_data,
                                color = data_name),
                  geom = 'step',
                  pad = FALSE)
    }
    g_wf <- g_wf +
      geom_line(data = df_sn_xytable,
                mapping = aes(x = x,
                              y = y_cum,
                              color = modelled_name),
                size = 1.3) +
      theme(legend.title = element_blank(),
            legend.position = 'bottom',
            legend.direction = 'vertical')
  }
  g_wf
  return (g_wf)  
}

##################################################################

## SN Density / CDF with scale rental #######
dsn_trunc_hidden <- function(y, mu = 0, sigma2 = 1, shape=1, domain_lower = -Inf, domain_upper = Inf)
  {
  dens <- 2*dnorm(y, mu, sqrt(sigma2))*pnorm(shape*((y - mu)/sqrt(sigma2)))

  # snorm <- list(mu = mu, sigma2 = sigma2, shape = shape, pii = 1, trunc = 1)
  # print (str(snorm))
  # print ("First")
  # print (str(snorm))
  # print (get_sn_prob_hidden(snorm, x_lower = domain_lower, x_upper = domain_upper))
  
  dens <- dens / get_sn_prob_hidden(list(mu = mu, sigma2 = sigma2, shape = shape, pii = 1, trunc = 1), x_lower = domain_lower, x_upper = domain_upper)

  if (domain_lower != -Inf)
  {
    i = 1
    for (y_ind in y)
    {
      if (y_ind < domain_lower)
      {
        dens[i] <- 0
      }
      i = i + 1
    }
  }
  if (domain_upper != Inf)
  {
    i = 1
    for (y_ind in y)
    {
      if (y_ind > domain_upper)
      {
        dens[i] <- 0
      }
      i = i + 1
    }
  }
  return(dens)
}

get_sn_prob_hidden <- function(snorm, x_lower = 0, x_upper = 1)
{
  # print ("WHY")
  # print (str(snorm))
  # print (x_lower)
  # print (x_upper)
  if (x_lower > x_upper)
  {
    return (NA)
  }
  n_mix <- length(snorm$mu)
  psn_total <- 0
  for (i in 1:n_mix)
  {
    if (x_lower == Inf)
    {
      psn_lessthan = 1
    }else
    {
      psn_lessthan = psn(x_lower,
                         xi = snorm$mu[i],
                         omega = sqrt(snorm$sigma2[i]),
                         alpha = snorm$shape[i])
    }
    if (x_upper == Inf)
    {
      psn_morethan = 0
    }else
    {
      psn_morethan = 1 - psn(x_upper,
                             xi = snorm$mu[i],
                             omega = sqrt(snorm$sigma2[i]),
                             alpha = snorm$shape[i])
    }
    psn_total <- psn_total + snorm$pii[i]*(psn_lessthan + psn_morethan)
  }
  # print (psn_total)
  return (1 - psn_total)  
}

########### Densities of SNI Blends ##################
dsn_mixed_trunc <- function(x, pi1, mu, sigma2, shape, domain_lower = -Inf, domain_upper = Inf)
{
  # x: eh o vetor de dados
  # outros parametros devem ser do tipo vetor c() de dimensao g (qtd de misturas)
  g <- length(pi1)
  dens <- 0
  for (j in 1:g)
  {
    dens <- dens + pi1[j]*dsn_trunc_hidden(x, mu[j], sigma2[j], shape[j], domain_lower, domain_upper)
  }
  return(dens)
}

smsn.mix_trunc <- function (y, nu, mu = NULL, sigma2 = NULL, shape = NULL, pii = NULL, 
          g = NULL, domain_lower = -Inf, domain_upper = Inf, get.init = TRUE, criteria = TRUE, group = FALSE, 
          family = "Skew.normal", error = 1e-05, iter.max = 100, calc.im = TRUE, 
          obs.prob = FALSE, kmeans.param = NULL) 
  {
  if (ncol(as.matrix(y)) > 1) 
    stop("This function is only for univariate response y!")
  if (family != "Skew.normal") 
      stop(paste("Family", family, "not recognized.\n", sep = " "))
  if ((length(g) == 0) && ((length(mu) == 0) || (length(sigma2) == 0) || (length(shape) == 0) || (length(pii) == 0))) 
    stop("The model is not specified correctly.\n")
  if (get.init == FALSE) {
    g <- length(mu)
    if ((length(mu) != length(sigma2)) || (length(mu) != length(pii))) 
      stop("The size of the initial values are not compatibles.\n")
    if ((family == "Skew.t" || family == "Skew.cn" || family == "Skew.slash" || family == "Skew.normal") & (length(mu) != length(shape))) 
      stop("The size of the initial values are not compatibles.\n")
    if (sum(pii) != 1) 
      stop("probability of pii does not sum to 1.\n")
  }
  if ((length(g) != 0) && (g < 1)) 
    stop("g must be greater than 0.\n")
  if (get.init == TRUE) {
    key.mu <- key.sig <- key.shp <- TRUE
    if (length(g) == 0) 
      stop("g is not specified correctly.\n")
    if (length(mu) > 0) 
      key.mu <- FALSE
    if (length(sigma2) > 0) 
      key.sig <- FALSE
    if (length(shape) > 0) 
      key.shp <- FALSE
    if (((!key.mu) & (length(mu) != g)) | ((!key.sig) & (length(sigma2) != g)) | ((!key.shp) & (length(shape) != g))) 
      stop("The size of the initial values are not compatibles.\n")
    k.iter.max <- 50
    n.start <- 1
    algorithm <- "Hartigan-Wong"
    if (length(kmeans.param) > 0) {
      if (length(kmeans.param$iter.max) > 0) 
        k.iter.max <- kmeans.param$iter.max
      if (length(kmeans.param$n.start) > 0) 
        n.start <- kmeans.param$n.start
      if (length(kmeans.param$algorithm) > 0) 
        algorithm <- kmeans.param$algorithm
    }
    if (g > 1) {
      if (key.mu) 
        init <- kmeans(y, g, k.iter.max, n.start, algorithm)
      else init <- kmeans(y, mu, k.iter.max, n.start, algorithm)
      pii <- init$size/length(y)
      if (key.mu) 
        mu <- as.vector(init$centers)
      if (key.sig) 
        sigma2 <- init$withinss/init$size
      if (key.shp) {
        shape <- c()
        for (j in 1:g) {
          m3 <- (1/init$size[j]) * sum((y[init$cluster == j] - mu[j])^3)
          shape[j] <- sign(m3/sigma2[j]^(3/2))
        }
      }
    }
    else {
      if (key.mu) 
        mu <- mean(y)
      if (key.sig) 
        sigma2 <- var(y)
      pii <- 1
      if (key.shp) {
        m3 <- (1/length(y)) * sum((y - mu)^3)
        shape <- sign(m3/sigma2^(3/2))
      }
    }
  }
  if (family == "Skew.normal") {
    # lk <- sum(log(dsn_mixed(y, pii, mu, sigma2, shape)))
    lk <- sum(log(dsn_mixed_trunc(y, pii, mu, sigma2, shape, domain_lower, domain_upper)))
    n <- length(y)
    delta <- Delta <- Gama <- rep(0, g)
    for (k in 1:g) {
      delta[k] <- shape[k]/(sqrt(1 + shape[k]^2))
      Delta[k] <- sqrt(sigma2[k]) * delta[k]
      Gama[k] <- sigma2[k] - Delta[k]^2
    }
    teta <- c(mu, Delta, Gama, pii)
    mu.old <- mu
    Delta.old <- Delta
    Gama.old <- Gama
    criterio <- 1
    count <- 0
    
    pb = txtProgressBar(min = 0, max = iter.max, initial = 0) 

    while ((criterio > error) && (count <= iter.max)) {
      setTxtProgressBar(pb,count)
      
      count <- count + 1
      tal <- matrix(0, n, g)
      S1 <- matrix(0, n, g)
      S2 <- matrix(0, n, g)
      S3 <- matrix(0, n, g)
      for (j in 1:g) {
        Mtij2 <- 1/(1 + (Delta[j]^2) * (Gama[j]^(-1)))
        Mtij <- sqrt(Mtij2)
        mutij <- Mtij2 * Delta[j] * (Gama[j]^(-1)) * 
          (y - mu[j])
        prob <- pnorm(mutij/Mtij)
        if (length(which(prob == 0)) > 0) 
          prob[which(prob == 0)] <- .Machine$double.xmin
        E = dnorm(mutij/Mtij)/prob
        u = rep(1, n)
        d1 <- dsn_trunc_hidden(y, mu[j], sigma2[j], shape[j], domain_lower, domain_upper)
        if (length(which(d1 == 0)) > 0) 
          d1[which(d1 == 0)] <- .Machine$double.xmin
        d2 <- dsn_mixed_trunc(y, pii, mu, sigma2, shape, domain_lower, domain_upper)
        if (length(which(d2 == 0)) > 0) 
          d2[which(d2 == 0)] <- .Machine$double.xmin
        tal[, j] <- d1 * pii[j]/d2
        S1[, j] <- tal[, j] * u
        S2[, j] <- tal[, j] * (mutij * u + Mtij * E)
        S3[, j] <- tal[, j] * (mutij^2 * u + Mtij2 + Mtij * mutij * E)
        pii[j] <- (1/n) * sum(tal[, j])
        mu[j] <- sum(S1[, j] * y - Delta.old[j] * S2[, j])/sum(S1[, j])
        Delta[j] <- sum(S2[, j] * (y - mu[j]))/sum(S3[, j])
        Gama[j] <- sum(S1[, j] * (y - mu[j])^2 - 2 * (y - mu[j]) * Delta[j] * S2[, j] + Delta[j]^2 * S3[, j])/sum(tal[, j])
        sigma2[j] <- Gama[j] + Delta[j]^2
        shape[j] <- ((sigma2[j]^(-1/2)) * Delta[j])/(sqrt(1 - (Delta[j]^2) * (sigma2[j]^(-1))))
      }
      pii[g] <- 1 - (sum(pii) - pii[g])
      zero.pos <- NULL
      zero.pos <- which(pii == 0)
      if (length(zero.pos) != 0) {
        pii[zero.pos] <- 1e-10
        pii[which(pii == max(pii))] <- max(pii) - sum(pii[zero.pos])
      }
      param <- teta
      teta <- c(mu, Delta, Gama, pii)
      lk1 <- sum(log(dsn_mixed_trunc(y, pii, mu, sigma2, shape, domain_lower, domain_upper)))
      criterio <- abs(lk1/lk - 1)
      mu.old <- mu
      Delta.old <- Delta
      Gama.old <- Gama
      lk <- lk1
    }
    if (criteria == TRUE) {
      cl <- apply(tal, 1, which.max)
      icl <- 0
      for (j in 1:g) icl <- icl + sum(log(pii[j] * dsn_trunc_hidden(y[cl == j], mu[j], sigma2[j], shape[j], domain_lower, domain_upper)))
    }
  }
  if (criteria == TRUE) {
    if ((family == "t") | (family == "Normal")) 
      d <- g * 2 + (g - 1)
    else d <- g * 3 + (g - 1)
    aic <- -2 * lk + 2 * d
    bic <- -2 * lk + log(n) * d
    edc <- -2 * lk + 0.2 * sqrt(n) * d
    icl <- -2 * icl + log(n) * d
    obj.out <- list(mu = mu, sigma2 = sigma2, shape = shape, 
                    pii = pii, nu = nu, aic = aic, bic = bic, edc = edc, 
                    icl = icl, iter = count, n = length(y), group = cl)
  }
  else obj.out <- list(mu = mu, sigma2 = sigma2, shape = shape, 
                       pii = pii, nu = nu, iter = count, n = length(y), group = apply(tal, 1, which.max))
  if (group == FALSE) 
    obj.out <- obj.out[-(length(obj.out))]
  if (obs.prob == TRUE) {
    nam <- c()
    for (i in 1:ncol(tal)) nam <- c(nam, paste("Group ", i, sep = ""))
    if (ncol(tal) == 1) 
      dimnames(tal)[[2]] <- list(nam)
    if (ncol(tal) > 1) 
      dimnames(tal)[[2]] <- nam
    obj.out$obs.prob <- tal
    if ((ncol(tal) - 1) > 1) 
      obj.out$obs.prob[, ncol(tal)] <- 1 - rowSums(obj.out$obs.prob[, 1:(ncol(tal) - 1)])
    else obj.out$obs.prob[, ncol(tal)] <- 1 - obj.out$obs.prob[, 1]
    obj.out$obs.prob[which(obj.out$obs.prob[, ncol(tal)] < 0), ncol(tal)] <- 0
    obj.out$obs.prob <- round(obj.out$obs.prob, 10)
  }
  class(obj.out) <- family
  # if (calc.im) {
  #   IM <- im.smsn(as.matrix(y), obj.out)
  #   sdev <- sqrt(diag(solve(IM[[1]])))
  #   obj.out$im.sdev = sdev
  # }
  class(obj.out) <- family
  obj.out
  
  obj.out$domain <- c(domain_lower, domain_upper)
  
  obj.out$trunc <- obj.out$mu * 0 + 1 + 0.0055
  obj.out$trunc_lower <- obj.out$mu * 0 + 0.0077
  for (i in 1:g)
  {
    # obj.out$trunc[i] <- get_sn_prob_hidden(list(mu = obj.out$mu[i], sigma2 = obj.out$sigma2[i], shape = obj.out$shape[i], pii = 1, trunc = 1), x_lower = domain_lower, x_upper = domain_upper)
    obj.out$trunc[i] <- get_sn_prob_hidden(list(mu = obj.out$mu[i], sigma2 = obj.out$sigma2[i], shape = obj.out$shape[i], pii = 1, trunc = 1), x_lower = domain_lower, x_upper = domain_upper)
    obj.out$trunc_lower[i] <- get_sn_prob_hidden(list(mu = obj.out$mu[i], sigma2 = obj.out$sigma2[i], shape = obj.out$shape[i], pii = 1, trunc = 1), x_lower = -Inf, x_upper = domain_lower)
  }
  
  obj.out
}

# rmix_pra <- function(n, pii, family, arg, cluster=FALSE) {
#   #Funcao para gerar misturas de g populacoes
#   #n: numero de amostras geradas
#   #p: vetor de proporcoes das misturas (tamanho g)
#   #arg: deve ser um tipo list com cada entrada contendo um vetor de tamanho g de agrumentos a ser passado para rF1
#   if(family != "Skew.normal")
#   {
#     stop(paste("Family",family,"not recognized.",sep=" "))
#   }
#   
#   if(family == "Skew.normal") {
#     rF1 <- gen.Skew.normal_pra
#     for (i in 1:length(arg)) if(length(arg[[i]]) != 4 && length(arg[[i]]) != 3) stop(paste("Number of arguments is not comformidable for argument ",i,".\n",sep=" "))
# 
#   x1 <- vector(mode = "numeric", length = n)
#   clu <- vector(mode = "numeric", length = n)
#   g <- length(pii)
#   interval <- c(0)
#   for (j in 1:g-1) interval <- cbind(interval, interval[j] + pii[j])
#   interval <- cbind(interval, 1)
#   for(i in 1:n) {
#     u <- runif(1)
#     clu[i] <- findInterval(u, interval)
#     x1[i] <- do.call("rF1", c(list(1), arg[[clu[i]]]))
#   }
#   if(cluster) return(list(y=x1, cluster=clu))
#   else return(x1)
#   }
# }
# 
# gen.Skew.normal_pra <- function(n, mu, sigma2, shape, nu=NULL){
#   #Funcao para gerar valores aleatorios de uma Skew-Normal
#   #n: qtd de valores a ser gerado
#   #mu, sigma2, shape: locacao, escala e assimetria, respectivamente
#   delta <- shape / sqrt(1 + shape^2)
#   y <- mu*rep(1,n) + sqrt(sigma2)*(delta*abs(rnorm(n)) + (1 - delta^2)^(1/2)*rnorm(n))
#   return(y)
# }