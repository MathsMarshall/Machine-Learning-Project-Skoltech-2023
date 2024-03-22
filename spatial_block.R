  ##Library imports
  install.packages('spThin')
  install.packages('automap')
  install.packages('tmap')
  install.packages('blockCV')
  install.packages('dplyr')
  install.packages('terra')
  install.packages('dismo')
  install.packages('raster')
  install.packages('randomForest')
  install.packages('stringr')
  install.packages('tidyr')
  install.packages('caret')
  install.packages('stars')
  install.packages('pROC')
  install.packages('sf')
  install.packages('ggplot2')
  install.packages('ROCR')
  install.packages('mapview')
  install.packages('data.table')
  install.packages('ROCR')
  install.packages("precrec")
  
  library(precrec)
  library(spThin)
  library(mapview)
  library(dismo)
  library(raster)
  library(terra)
  library(dplyr)
  library(blockCV)
  library(tmap) 
  library(automap)
  library(randomForest)
  library(stringr)
  library(tidyr)
  library(caret)
  library(pROC)
  library(maps)
  library(stars)
  library(sf)
  library(ggplot2)
  library(ROCR)
  library(data.table)
  
  
  
  setwd('C:/Users/Holdings/Documents/data/curr_env')
  
  #Shape files of Norway and Sweden
  no_sh <- shapefile("C:/Users/Holdings/Documents/data/NOR_adm/NOR_adm0.shp")
  sw_sh <- shapefile("C:/Users/Holdings/Documents/data/SWE_adm/SWE_adm0.shp")
  sh <- rbind(no_sh, sw_sh, makeUniqueIDs = TRUE)
  all_rasters <- list.files(path = "C:/Users/Holdings/Documents/data/curr_env", pattern='.asc', 
                            all.files=TRUE, full.names=FALSE)
  
  
  
  env_data <- function(lst_rasters){
    bioclim <- lapply(lst_rasters, raster)
    stack1 <- stack(bioclim)
    change_names <- function(stack1){
      names(stack1[[1]]) <- 'bio15'
      names(stack1[[2]]) <- 'bio19'
      names(stack1[[3]]) <- 'bio2'
      names(stack1[[4]]) <- 'bio3'
      names(stack1[[5]]) <- 'bio5'
      names(stack1[[6]]) <- 'bio6'
      names(stack1[[7]]) <- 'bio8'
      return(stack1)
    }
    stack1 <- change_names(stack1)
    masked <- mask(x = stack1, mask = sh)
    cropped_env <- crop(x = masked, y = extent(sh))
    crs(cropped_env) <- '+proj=longlat +datum=WGS84'
    return(cropped_env)
  }
  
  
  cropped_env <- env_data(all_rasters)
  
  #Species data
  gen_cam_occ <- read.csv('C:/Users/Holdings/Documents/data/species_data/curr_occ.csv')
  gen_cam <- read.csv('C:/Users/Holdings/Documents/data/species_data/curr_abs.csv')
  
  #Pre process data 
  preprocess_data <- function(occ_df, abs_df){
    #species data
    occ_df$occurrenceStatus <- 1
    abs_df <- abs_df[abs_df$occurrenceStatus == 0, ]
    occ_df <- occ_df[,c('occurrenceStatus', 'decimalLongitude', 'decimalLatitude')]
    abs_df <- abs_df[,c('occurrenceStatus', 'decimalLongitude', 'decimalLatitude')]
    train <- rbind(occ_df, abs_df)
    pa_data <- st_as_sf(train, coords = c("decimalLongitude", 'decimalLatitude'), crs = crs(cropped_env))
    
    return(pa_data)
  }
  
  pa_data <- preprocess_data(gen_cam_occ, gen_cam)
  table(pa_data$occurrenceStatus)
  ##Look at the data
  #mapview(train, xcol = "decimalLongitude", ycol = "decimalLatitude", crs = 4269, grid = FALSE)
  mapview(pa_data)
  
  
  ## Plot presence and absence data on map of study area
  tm_shape(cropped_env[[1]]) +
    tm_raster(legend.show = FALSE, n = 10, palette = gray.colors(10)) +
    tm_shape(pa_data) +
    tm_dots(col = "occurrenceStatus", style = "cat", size = 0.15, alpha=0.4)
  
  
  ## PAST DATA
  df_abs_past <- read.csv('C:/Users/Holdings/Documents/data/species_data/past_abs.csv')
  df_occ_past <- read.csv("C:/Users/Holdings/Documents/data/species_data/past_occ.csv")
  
  
  
  setwd("C:/Users/Holdings/Documents/data/past_env")
  all_rasters_test <- list.files(path = "C:/Users/Holdings/Documents/data/past_env", pattern='.asc', 
                                 all.files=TRUE, full.names=FALSE)
  cropped_env_test <- env_data(all_rasters_test)
  
  
  preprocess_past_data <- function(df_occ, df_abs){
    df_abs <- df_abs[df_abs$species=='Gentianella campestris',]
    test <- df_abs[df_abs$year >=1994,]
    test <- df_abs[df_abs$year <=2009,]
    test <- test[,c('occurrenceStatus', 'decimalLongitude', 'decimalLatitude')]
    test$occurrenceStatus<- ifelse(test$occurrenceStatus=="PRESENT",1,0)
    test <- test[test$occurrenceStatus==0, ]
    df_occ$occurrenceStatus <- 1
    df_occ <- df_occ[,c('occurrenceStatus', 'decimalLongitude', 'decimalLatitude')]
    test <- rbind(df_occ,test)
    pa_data_test<- st_as_sf(test, coords = c("decimalLongitude", 'decimalLatitude'), crs = crs(cropped_env_test))
    return(pa_data_test)
  }
  
  pa_data_test <- preprocess_past_data(df_occ_past,df_abs_past)
  
  
  ##FUTURE data
  setwd("C:/Users/Holdings/Documents/data/fut_env")
  all_rasters_fut <- list.files(path = "C:/Users/Holdings/Documents/data/fut_env", pattern='.asc', 
                                all.files=TRUE, full.names=FALSE)
  cropped_env_fut <- env_data(all_rasters_fut)
  
  #Spatial CV
  spatial_cv <- function(pa_data, cropped_env, 
                         cropped_env_test, pa_data_test, 
                         cropped_env_fut){
    sizes <- c(60000, 120000, 240000, 480000)
    
    df_pred <- list()
    df_pred_past <- list()
    bio_curr_df_lst = list()
    bio_past_df_lst = list()
    bio_fut_df_lst = list()
    
    for (val in sizes) {
      set.seed(2)
      sb <- spatialBlock(speciesData = pa_data,
                         species = "occurrenceStatus",
                         rasterLayer = cropped_env,
                         theRange = val, # size of the blocks
                         k = 5,
                         selection = "random",
                         iteration = 100, # find evenly dispersed folds
                         biomod2Format = TRUE,
                         xOffset = 0, # shift the blocks horizontally
                         yOffset = 0)
      folds <- sb$folds
      testTable <- pa_data
      testTable$pred <- NA
      mydata <- raster::extract(cropped_env, pa_data, df = TRUE)
      mydata <- raster::extract(cropped_env, pa_data, df = TRUE)
      mydata$occurrenceStatus <- as.factor(pa_data$occurrenceStatus)
      mydata <- mydata %>% 
        mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))
      mydata <- mydata[-1]
      
      
      set.seed(3)
      cross <- for(k in seq_len(length(folds))){
        trainSet <- unlist(folds[[k]][1]) # training set indices
        testSet <- unlist(folds[[k]][2]) # testing set indices
        rf <- randomForest(occurrenceStatus~., mydata[trainSet, ], ntree = 1000, importance=TRUE) # model fitting on training set
        testTable$pred[testSet] <- predict(rf, mydata[testSet, ], type = "prob")[,2] # predict the test set
      }
      df_pred <- append(df_pred,testTable)
      
      #preprocess_current prediction
      bio_curr_df <- data.frame(rasterToPoints(cropped_env))
      bio_curr_df$pred <- predict(rf, bio_curr_df,type = "prob")[,2]
      bio_curr_df <- bio_curr_df[,c('x', 'y', 'pred')]
      bio_curr_df_lst <- append(bio_curr_df_lst, bio_curr_df)
      
      
      ##Test on the past data
      mydata_test <- raster::extract(cropped_env_test, pa_data_test, df = TRUE)
      
      mydata_test$occurrenceStatus <- as.factor(pa_data_test$occurrenceStatus)
      mydata_test <- mydata_test %>% 
        mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))
      mydata_test <- mydata_test[-1]
      
      testTable_past <- pa_data_test
      testTable_past$pred <- NA
      testTable_past$pred <- predict(rf, mydata_test, type = "prob")[,2]
      df_pred_past <- append(df_pred_past,testTable_past)
      
      bio_test_df <- data.frame(rasterToPoints(cropped_env_test))
      bio_test_df$pred <- predict(rf, bio_test_df,type = "prob")[,2]
      bio_test_df <- bio_test_df[,c('x', 'y', 'pred')]
      bio_past_df_lst <- append(bio_past_df_lst, bio_test_df)
      
      bio_fut_df <- data.frame(rasterToPoints(cropped_env_fut))
      bio_fut_df$pred <- predict(rf, bio_fut_df,type = "prob")[,2]
      bio_fut_df <- bio_fut_df[,c('x', 'y', 'pred')]
      bio_fut_df_lst <- append(bio_fut_df_lst, bio_fut_df)
    }
    out = list(df_pred, bio_curr_df_lst, df_pred_past, bio_past_df_lst, bio_fut_df_lst)
    return(out)
  }
  
  out <- spatial_cv(pa_data, cropped_env, 
                    cropped_env_test, pa_data_test, cropped_env_fut)
  
  df_pred <- as.data.frame(out[1])
  bio_curr_df_lst <- as.data.frame(out[2])
  
  df_pred_past <- as.data.frame(out[3])
  bio_past_df_lst <- as.data.frame(out[4])
  bio_fut_df_lst <- as.data.frame(out[5])
  
  plot_prediction <- function(bio_curr_df_lst){
    
    bio_spBlock60 <- as.data.frame(bio_curr_df_lst[1:3])
    bio_spBlock120 <- as.data.frame(bio_curr_df_lst[4:6])
    bio_spBlock240 <- as.data.frame(bio_curr_df_lst[7:9])
    bio_spBlock480 <- as.data.frame(bio_curr_df_lst[10:12])
    
    my_colors_3 <- c( "#008fbf", "#47b0df",
                      "#dae695", '#e6ae95', 
                      "#df7b7b", "#a00000" )
    
    lst_curr_map <- list(bio_spBlock60=bio_spBlock60,bio_spBlock120=bio_spBlock120,bio_spBlock240=bio_spBlock240,bio_spBlock480=bio_spBlock480)
    ID <- names(lst_curr_map)
    ID_curr <- c(bio_spBlock60='spBlock60', 
                 bio_spBlock120='spBlock120', 
                 bio_spBlock240='spBlock240', 
                 bio_spBlock480='spBlock480')
    change_names <- function(lst_curr_map){
      for (i in seq_along(lst_curr_map )) {
        names(lst_curr_map [[i]]) <- c("longitude", "latitude", "pred")
      }
      return(lst_curr_map)
    }
    
    lst_curr_map <- change_names(lst_curr_map)
    lst_curr_maps <- mapply(cbind, lst_curr_map, "dataset"=ID, SIMPLIFY=F)
    curr_maps_df <- as.data.frame(rbindlist(l = lst_curr_maps))
    table(curr_maps_df$dataset)
    
    ggplot() +
      geom_raster(data = curr_maps_df , aes(x = longitude, y = latitude, fill = pred)) +
      #scale_fill_distiller(palette = "Spectral", direction = -1, na.value = "white") +
      scale_fill_gradientn (colours = my_colors_3, na.value = "white", limits = c(0,1))+
      coord_quickmap()+
      theme_classic(base_size = 12, base_family = "Georgia")+theme(legend.position = "bottom")+
      facet_wrap(~dataset,  labeller=labeller(dataset=ID_curr)) +
      theme(text=element_text(size=12))+
      theme(plot.title = element_text(hjust = 0.5))
    
  }
  
  curr_plot <- plot_prediction(bio_curr_df_lst)
  past_plot <- plot_prediction(bio_past_df_lst)
  fut_plot <- plot_prediction(bio_fut_df_lst)
  
  png("C:/Users/Holdings/Documents/figures/spatial_cv_map_fut.png")
  print(fut_plot)
  dev.off()
  
  save_prediction <- function(df_pred){
    spBlock60 <- as.data.frame(df_pred[1:3])
    spBlock120 <- as.data.frame(df_pred[4:6])
    spBlock240 <- as.data.frame(df_pred[7:9])
    spBlock480 <- as.data.frame(df_pred[10:12])
    
    data_names <- c("spBlock60", "spBlock120", "spBlock240", 'spBlock480')  
    
    for(i in 1:length(data_names)) {                              # Head of for-loop
      write.csv2(get(data_names[i]),                              # Write CSV files to folder
                 paste0("C:/Users/Holdings/Documents/results/",
                        data_names[i],
                        ".csv"),
                 row.names = FALSE)
    }
    
  }
  
  save_prediction(df_pred)
  
  
  
  ## To check ROC AUC scores
  precrec_obj <- evalmod(scores = df_pred_past$pred.2, labels = df_pred_past$occurrenceStatus.2)
  autoplot(precrec_obj)
  
  
  
