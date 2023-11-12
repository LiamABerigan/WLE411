library(renv)
library(tidyverse)
library(here)
library(amt)
library(sf)
library(terra)
library(readxl)
library(AICcmodavg)

ssf_td_3 <- function(time_difference, selected_season) {
  
  #read in a landcov raster so we can use its projection
  landcov_forest_binary <- rast(here("predictive_layers", "na_landcover", "forest_binary.tif"))
  
  amwo_frequent <- readRDS(here("woodcock_locations_18_23.rds")) %>%
    rename(step_state = migration_stage,
           point_state = behavioural_classification) %>% 
    st_transform(st_crs(landcov_forest_binary))
  
  capture_sheet <- read_excel(here("capture_sheet.xlsx"), col_types = c("text", "text", "date", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text")) %>% 
    rename(local_identifier = Movebank_ID,
           capture_date = Date,
           capture_state = State,
           sex = Sex,
           age = Age) %>% 
    dplyr::select(local_identifier, capture_state, sex, age, capture_date) %>% 
    filter(!(local_identifier %in% c("NA", "Nocheckin"))) %>% 
    filter(!is.na(local_identifier)) %>% 
    group_by(local_identifier) %>% 
    distinct(local_identifier, .keep_all = T)
  
  amwo_frequent <- amwo_frequent %>% 
    left_join(capture_sheet)
  
  ## Next, let's delineate our two scales (2nd/3rd order) and figure out how to delineate available steps seperately for each scale
  
  # Starting by putting things in amt format
  freq_nest <- bind_cols(st_drop_geometry(amwo_frequent), st_coordinates(amwo_frequent)) %>%
    nest(.by = "local_identifier", .key = "tbl")
  
  freq_nest$track <- map(freq_nest$tbl, function(x){
    x %>% 
      mk_track(.x = X, .y = Y, .t = timestamp, 
               crs = st_crs(landcov_forest_binary), #4326
               all_cols = T)
  })
  
  # Let's ensure that there are at least 5 locations in each track
  freq_nest$n <- map(freq_nest$track, function(x){
    nrow(x)
  })
  
  freq_nest <- freq_nest %>% 
    filter(n >= 5)
  
  # Check sampling rate
  freq_nest$sample_rate <- map(freq_nest$track, function(x){
    rt <- summarize_sampling_rate(x, time_unit = "hour")
    
    rt$mean
  })
  
  freq_nest$steps <- map(freq_nest$track, function(x){
    x %>% 
      steps(lonlat = F, keep_cols = "both", diff_time_units = "hour")
  })
  
  # Delineating steps > 16.1 km (2nd order) and < 16.1 km (3rd order)
  freq_nest$steps <- map(freq_nest$steps, function(x){
    x$order_2_burst <- NA
    x$order_3_burst <- NA
    
    i <- 1
    for (k in 1:nrow(x)) {
      if(x$sl_[k] >= 16100){
        x$order_2_burst[k] <- TRUE
        i <- i + 1 #progress to the next order 3 burst
        
      } else {
        x$order_3_burst[k] <- i
      }
    }
    
    return(x)
  })
  
  # summer vector
  summer_states <- c("NS", "ONT", "PA", "QUE", "VT", "ME", "NY", "RI", "WI", "WV")
  # winter vector
  winter_states <- c("AL", "FL", "GA", "LA", "NC", "SC")
  # both seasons vector
  both_states <- c("MD", "NJ", "VA")
  
  stationary_options <- c("Step state: Stationary", "Step state: Foray loop", "Step state: Dispersal", "Step state: Migratory (summer)")
  
  # Determine whether a given step is summering, mig, or wintering
  freq_nest$steps <- map(freq_nest$steps, function(x){
    
    # standardize day of year
    x <- x %>% 
      mutate(standard_date = mdy(paste0(month(t1_), "/", day(t1_), "/2020")))
    
    x$season <- NA
    
    # Initialize the first state
    x$season[1] <- case_when(
      x$step_state_start[1] == "Step state: Migratory (spring)" ~ "Migratory (spring)", #begins as a spring mig step
      x$step_state_start[1] == "Step state: Migratory (fall)" ~ "Migratory (fall)", #begins as a fall mig step
      x[1,]$capture_state_start %in% summer_states ~ "Summer", # If caught in a "summer" area and first movement point state is stationary, initial state summer
      x[1,]$capture_state_start %in% winter_states ~ "Winter", # likewise winter
      x[1,]$capture_state_start %in% both_states & x[1,]$standard_date >= mdy("11/1/2020") ~ "Winter", # If in an overlap state (ex. VA), refine by time of year captured
      x[1,]$capture_state_start %in% both_states & x[1,]$standard_date < mdy("4/1/2020") ~ "Winter",
      x[1,]$capture_state_start %in% both_states & (x[1,]$standard_date >= mdy("4/1/2020") & x[1,]$standard_date < mdy("11/1/2020")) ~ "Summer",
      .default = "Oops"
    )
    
    for (k in 2:nrow(x)){
      x$season[k] <- case_when(
        x$step_state_start[k] %in% stationary_options & x$step_state_start[k-1] %in% stationary_options ~ x$season[k-1], #if the current and prior step states are stationary, return the prior season
        x$step_state_start[k] == "Step state: Migratory (spring)" & x$step_state_start[k-1] %in% stationary_options ~ "Migratory (spring)", 
        x$step_state_start[k] == "Step state: Migratory (fall)" & x$step_state_start[k-1] %in% stationary_options ~ "Migratory (fall)", 
        x$point_state_start[k] == "Point state: Migratory (spring)" ~ "Migratory (spring)", #if the current point state is mig spring fall, return mig spring fall
        x$point_state_start[k] == "Point state: Migratory (fall)" ~ "Migratory (fall)", 
        x$step_state_start[k-1] == "Step state: Migratory (spring)" & x$step_state_start[k] %in% stationary_options ~ "Summer", #if the prior step state is mig spring and the current step state is stationary, return summer
        x$step_state_start[k-1] == "Step state: Migratory (fall)" & x$step_state_start[k] %in% stationary_options ~ "Winter", #if the prior step state is mig fall and the current step state is stationary, return winter
        .default = "Oops" #else oops
      )
    }
    
    # late summer can be defined using three methods:
    # 1) "Core breeding period" from literature- Hicks 1933, Pitelka 1943, Montague et al. 2005
    # June 1
    # Hicks, L. E. 1933. The breeding birds of Ashtabula County. The Wilson Bulletin 45:168–195.
    # Pitelka, F. A. 1943. Territoriality, display, and certain ecological relations of the American woodcock. The Wilson Bulletin 55.
    # Montague, W. R., D. A. Haukos, and L. M. Smith. 2005. Factors affecting January reproduction of American woodcock in Texas. Southeastern Naturalist 4:639–646.
    
    # 2) After the date on which 95% of spring migratory movements have concluded
    # June 4 (calculated from fac-classification)
    
    # 3) The date on which 95% of nesting attempts have concluded
    # July 25 (calculated from Colby's data using delineate_nesting_period.Rmd)
    
    # Using #3 for now
    
    for (k in 1:nrow(x)) {
      if(x$season[k] == "Summer"){
        x$season[k] <- case_when(
          x$standard_date[k] < mdy("7/25/2020") ~ "Summer (early)", # Currently using the "core breeding period" definition
          .default = "Summer (late)"
        )
      }
    }
    return(x)
  })
  
  ## Determine seasonality of 2nd order steps
  freq_nest$steps <- map(freq_nest$steps, function(x){
    for (k in 1:nrow(x)){
      if(!is.na(x$order_2_burst[k])){
        x$order_2_burst[k] <- case_when(
          x$step_state_start[k+1] == "Step state: Stationary" ~ x$season[k+1], #If the next state is stationary, select the next state as the season of selection
          x$step_state_start[k+1] != "Step state: Stationary" ~ x$season[k], #Otherwise, use the current season (should be mig spring or fall)
          .default = "Oops"
        )
      }
    }
    return(x)
  }) 
  
  ## Delineate random steps
  
  # Delineating sl and ta distributions at a population scale, so that I can only include steps below a certain time threshold
  
  freq_spring_mig_3 <- freq_nest
  
  # freq_spring_mig_3$num_steps <- map(freq_spring_mig_3$steps, function(x){
  #   x %>% 
  #     filter(season == selected_season & !is.na(order_3_burst)) %>% 
  #     nrow()
  # })
  
  # Need at least 5 steps in this season for us to examine further
  ## With population distributions this may no longer be necessary
  
  # freq_spring_mig_3 <- freq_spring_mig_3 %>% 
  #   filter(num_steps >= 5)
  
  # Create a distribution of stopover step lengths and turn angles for the population (allowing only steps within the time difference threshold)
  pop_sl_distr <- freq_spring_mig_3 %>% 
    dplyr::select(local_identifier, steps) %>% 
    unnest(cols = c(steps)) %>% 
    mutate(dt_ = as.numeric(dt_)) %>% 
    filter(dt_ <= time_difference) %>% 
    filter(season == selected_season & !is.na(order_3_burst)) %>%
    pull(sl_) %>% 
    fit_distr("gamma")
  
  pop_ta_distr <- freq_spring_mig_3 %>% 
    dplyr::select(local_identifier, steps) %>% 
    unnest(cols = c(steps)) %>% 
    mutate(dt_ = as.numeric(dt_)) %>% 
    filter(dt_ <= time_difference) %>% 
    filter(season == selected_season & !is.na(order_3_burst)) %>%
    pull(ta_) %>% 
    fit_distr("vonmises")
  
  # saving these distributions for later analysis
  results_sl <- freq_spring_mig_3 %>% 
    dplyr::select(local_identifier, steps) %>% 
    unnest(cols = c(steps)) %>% 
    mutate(dt_ = as.numeric(dt_)) %>% 
    filter(dt_ <= time_difference) %>% 
    filter(season == selected_season & !is.na(order_3_burst)) %>%
    pull(sl_) #%>% 
    #saveRDS(paste0("sl_", season_abbrv, "_", time_difference, "hr_3.rds"))
  
  results_ta <- freq_spring_mig_3 %>% 
    dplyr::select(local_identifier, steps) %>% 
    unnest(cols = c(steps)) %>% 
    mutate(dt_ = as.numeric(dt_)) %>% 
    filter(dt_ <= time_difference) %>% 
    filter(season == selected_season & !is.na(order_3_burst)) %>%
    pull(ta_) #%>% 
    #saveRDS(paste0("ta_", season_abbrv, "_", time_difference, "hr_3.rds"))
  
  
  freq_spring_mig_3$ua <- map(freq_spring_mig_3$steps, function(x){
    # Create a distribution of stopover step lengths and turn angles for each individual
    
    # indiv_sl_distr <- x %>% 
    #   filter(seasonal_class_3 == "Migration (spring)") %>%
    #   pull(sl_) %>% 
    #   fit_distr("gamma")
    # 
    # indiv_ta_distr <- x %>% 
    #   filter(seasonal_class_3 == "Migration (spring)") %>%
    #   pull(ta_) %>% 
    #   fit_distr("vonmises")
    
    x %>% #create avail steps for the entire track, and keep only those for the season and order in question
      random_steps(n_control = 10,
                   sl_distr = pop_sl_distr,
                   ta_distr = pop_ta_distr) %>% 
      filter(season == selected_season & !is.na(order_3_burst)) %>%
      mutate(dt_ = as.numeric(dt_)) %>% #also only keep those within the allowable time difference
      filter(dt_ <= time_difference)
  })
  
  # Count the number of steps in ua, now that we've selected down to the season/time scale of interest. 
  # If we've filtered out all steps, remove the row from consideration
  freq_spring_mig_3$nrow_ua <- map(freq_spring_mig_3$ua, function(x){
    nrow(x)
  })
  
  freq_spring_mig_3 <- freq_spring_mig_3 %>% 
    filter(nrow_ua > 0)
  
  ## Extracting covariates at the end of each step (smallest possible scale of selection)
  landcov_general <- rast(here("predictive_layers", "na_landcover", "landcov_general.tif"))
  
  freq_spring_mig_3$ua_cov <- map(freq_spring_mig_3$ua, function(x){
    x %>% 
      extract_covariates(landcov_general, where = "end") %>% 
      rename("lndcv_general" = "landcover_na_2020_102008")
  })
  
  flat_spring_mig_3 <- freq_spring_mig_3 %>% 
    dplyr::select(local_identifier, ua_cov) %>% 
    unnest(ua_cov) %>% 
    mutate(lndcv_general = as.factor(lndcv_general))
  
  # Manually create dummy variables
  flat_spring_mig_3 <- flat_spring_mig_3 %>% 
    mutate(needleleaf = if_else(lndcv_general == 1, 1, 0),
           broadleaf = if_else(lndcv_general == 2, 1, 0),
           mixed = if_else(lndcv_general == 3, 1, 0),
           shrub = if_else(lndcv_general == 4, 1, 0),
           grass = if_else(lndcv_general == 5, 1, 0),
           lichen = if_else(lndcv_general == 6, 1, 0),
           wetland = if_else(lndcv_general == 7, 1, 0),
           crop = if_else(lndcv_general == 8, 1, 0),
           barren = if_else(lndcv_general == 9, 1, 0),
           urban = if_else(lndcv_general == 10, 1, 0),
           water = if_else(lndcv_general == 11, 1, 0),
           snow = if_else(lndcv_general == 12, 1, 0)
    ) %>% 
    dplyr::select(-lndcv_general)
  
  ## Running a conditional logistic regression
  # Don't forget to fit an individual random effect
  
  m <- list()
  
  m[[1]] <- flat_spring_mig_3 %>% 
    fit_clogit(case_ ~ strata(step_id_))
  
  m[[2]] <- flat_spring_mig_3 %>% 
    fit_clogit(case_ ~ needleleaf + strata(step_id_))
  
  m[[3]] <- flat_spring_mig_3 %>% 
    fit_clogit(case_ ~ broadleaf + strata(step_id_))
  
  m[[4]] <- flat_spring_mig_3 %>% 
    fit_clogit(case_ ~ mixed + strata(step_id_))
  
  m[[5]] <- flat_spring_mig_3 %>% 
    fit_clogit(case_ ~ shrub + strata(step_id_))
  
  m[[6]] <- flat_spring_mig_3 %>% 
    fit_clogit(case_ ~ grass + strata(step_id_))
  
  m[[7]] <- flat_spring_mig_3 %>% 
    fit_clogit(case_ ~ lichen + strata(step_id_))
  
  m[[8]] <- flat_spring_mig_3 %>% 
    fit_clogit(case_ ~ wetland + strata(step_id_))
  
  m[[9]] <- flat_spring_mig_3 %>% 
    fit_clogit(case_ ~ crop + strata(step_id_))
  
  m[[10]] <- flat_spring_mig_3 %>% 
    fit_clogit(case_ ~ barren + strata(step_id_))
  
  m[[11]] <- flat_spring_mig_3 %>% 
    fit_clogit(case_ ~ urban + strata(step_id_))
  
  m[[12]] <- flat_spring_mig_3 %>% 
    fit_clogit(case_ ~ water + strata(step_id_))
  
  m[[13]] <- flat_spring_mig_3 %>% 
    fit_clogit(case_ ~ snow + strata(step_id_))
  
  for (i in 1:length(m)) {
    m[[i]] <- m[[i]]$model #include just the clogit output and not the reported sl and ta
  }
  
  mnames <- c("null", "needleleaf", "broadleaf", "mixed", "shrub", "grass", "lichen", "wetland", "crop", "barren", "urban", "water", "snow")
  
  aic_results <- aictab(cand.set = m, modnames = mnames)
  aic_results
  
  return(list(sl = results_sl, ta = results_ta, aic = aic_results)) #m = m,
  
  #saveRDS(m, paste0("m_", season_abbrv, "_", time_difference, "hr_3.rds"))
  #saveRDS(aic_results, paste0("aic_", season_abbrv, "_", time_difference, "hr_3.rds"))
}