## Fucntion for B-LOO CV. 
buff_cv <- function(pa_data, cropped_env, 
                    cropped_env_test, pa_data_test){
  
  set.seed(1)
  bf <- buffering(speciesData = pa_data,
                  theRange = 480000,
                  species = "occurrenceStatus", # to count the number of presences and absences/backgrounds
                  spDataType = "PA", # presence-absence  data type
                  progress = TRUE)
  
  folds <- bf$folds
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
  df_pred <- testTable
  
  #preprocess_current prediction
  bio_curr_df <- data.frame(rasterToPoints(cropped_env))
  bio_curr_df$pred <- predict(rf, bio_curr_df,type = "prob")[,2]
  bio_curr_df <- bio_curr_df[,c('x', 'y', 'pred')]
  bio_curr_df_lst <- bio_curr_df
  
  
  ##Test on the past data
  mydata_test <- raster::extract(cropped_env_test, pa_data_test, df = TRUE)
  
  mydata_test$occurrenceStatus <- as.factor(pa_data_test$occurrenceStatus)
  mydata_test <- mydata_test %>% 
    mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))
  mydata_test <- mydata_test[-1]
  
  
  
  testTable_past <- pa_data_test
  testTable_past$pred <- NA
  testTable_past$pred <- predict(rf, mydata_test, type = "prob")[,2]
  df_pred_past <- testTable_past
  
  bio_test_df <- data.frame(rasterToPoints(cropped_env_test))
  bio_test_df$pred <- predict(rf, bio_test_df,type = "prob")[,2]
  bio_test_df <- bio_test_df[,c('x', 'y', 'pred')]
  bio_past_df_lst <- bio_test_df
  
  bio_fut_df <- data.frame(rasterToPoints(cropped_env_fut))
  bio_fut_df$pred <- predict(rf, bio_fut_df,type = "prob")[,2]
  bio_fut_df <- bio_fut_df[,c('x', 'y', 'pred')]
  bio_fut_df_lst <- bio_fut_df
  
  out = list(df_pred, bio_curr_df_lst, df_pred_past, bio_past_df_lst, bio_fut_df_lst)
  return(out)
}

buff <- buff_cv(pa_data, cropped_env, 
                cropped_env_test, pa_data_test)

##Save preds

buff_cur_pred <- as.data.frame(buff[1])
buff_past_pred <- as.data.frame(buff[3])

write.csv(buff_cur_pred , 'buff_cur_pred.csv')
write.csv(buff_past_pred, 'buff_past_pred.csv')

##To check ROC AUC scores you can run it
precrec_obj <- evalmod(scores = buff_past_pred$pred, labels = buff_past_pred$occurrenceStatus)
autoplot(precrec_obj)


##Plot maps of predictions
buff_cur_map <- as.data.frame(buff[2])
buff_past_map <- as.data.frame(buff[4])
buff_fut_map <- as.data.frame(buff[5])


lst_buff_maps <- list(buff_cur_map, buff_past_map, buff_fut_map)
change_names <- function(lst_buff_maps){
  for (i in seq_along(lst_buff_maps )) {
    names(lst_buff_maps [[i]]) <- c("longitude", "latitude", "pred")
  }
  return(lst_buff_maps)
}

buff_cur_map <- as.data.frame(change_names(lst_buff_maps)[1])
buff_past_map <- as.data.frame(change_names(lst_buff_maps)[2])
buff_fut_map <- as.data.frame(change_names(lst_buff_maps)[3])


my_colors_3 <- c( "#008fbf", "#47b0df",
                  "#dae695", '#e6ae95', 
                  "#df7b7b", "#a00000" )

cur_plot <- ggplot() +
  geom_raster(data = buff_cur_map , aes(x = longitude, y = latitude, fill = pred)) +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value = "white") +
  scale_fill_gradientn (colours = my_colors_3, na.value = "white", limits = c(0,1))+
  coord_quickmap()+
  theme_classic(base_size = 12, base_family = "Georgia")+theme(legend.position = "bottom") +
  ggtitle('B_LOO') +
  theme(text=element_text(size=12))+
  theme(plot.title = element_text(hjust = 0.5))

##Save map of current prediction
#png("C:/Users/Holdings/Documents/figures/buff_cv_map_curr.png")
print(cur_plot)
dev.off()


past_plot <- ggplot() +
  geom_raster(data = buff_past_map , aes(x = longitude, y = latitude, fill = pred)) +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value = "white") +
  scale_fill_gradientn (colours = my_colors_3, na.value = "white", limits = c(0,1))+
  coord_quickmap()+
  theme_classic(base_size = 12, base_family = "Georgia")+theme(legend.position = "bottom") +
  ggtitle('B_LOO 1994-2009') +
  theme(text=element_text(size=12))+
  theme(plot.title = element_text(hjust = 0.5))


##Save map of past prediction
png("C:/Users/Holdings/Documents/figures/buff_cv_map_past.png")
print(past_plot)
dev.off()

fut_plot <- ggplot() +
  geom_raster(data = buff_fut_map , aes(x = longitude, y = latitude, fill = pred)) +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value = "white") +
  scale_fill_gradientn (colours = my_colors_3, na.value = "white", limits = c(0,1))+
  coord_quickmap()+
  theme_classic(base_size = 12, base_family = "Georgia")+theme(legend.position = "bottom") +
  ggtitle('B_LOO CanESM_126 2040-2060') +
  theme(text=element_text(size=12))+
  theme(plot.title = element_text(hjust = 0.5))


##Save map of future prediction
png("C:/Users/Holdings/Documents/figures/buff_cv_map_fut.png")
print(fut_plot)
dev.off()
