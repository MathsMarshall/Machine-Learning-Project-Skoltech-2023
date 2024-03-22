nndm_loo_cv <- function(pa_data, cropped_env, 
                        cropped_env_test, pa_data_test){
  
  set.seed(1) # Ensuring reproducibility
  
  # Preprocessing the data similar to B-LOO for fair comparison
  testTable <- pa_data
  testTable$pred <- NA
  mydata <- raster::extract(cropped_env, pa_data, df = TRUE)
  mydata$occurrenceStatus <- as.factor(pa_data$occurrenceStatus)
  mydata <- mydata %>% 
    mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))
  
  # Assuming mydata[-1] meant to remove row ID or similar identifier from data
  mydata <- mydata[-1] 
  
  # Using Leave-One-Out approach for NNDM
  set.seed(3)
  for(i in 1:nrow(mydata)){
    # Separating one sample for testing
    testSet <- mydata[i, ]
    # Using the rest for training
    trainSet <- mydata[-i, ]
    
    # Fit the model on the training set
    rf <- randomForest(occurrenceStatus~., trainSet, ntree = 1000, importance = TRUE)
    
    # Predict the left-out observation
    testTable$pred[i] <- predict(rf, testSet, type = "prob")[,2]
  }
  df_pred <- testTable
  
  # Preprocess current prediction
  bio_curr_df <- data.frame(rasterToPoints(cropped_env))
  bio_curr_df$pred <- predict(rf, bio_curr_df, type = "prob")[,2]
  bio_curr_df <- bio_curr_df[, c('x', 'y', 'pred')]
  bio_curr_df_lst <- bio_curr_df
  
  # Test on the past data
  mydata_test <- raster::extract(cropped_env_test, pa_data_test, df = TRUE)
  mydata_test$occurrenceStatus <- as.factor(pa_data_test$occurrenceStatus)
  mydata_test <- mydata_test %>% 
    mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))
  mydata_test <- mydata_test[-1]
  
  testTable_past <- pa_data_test
  testTable_past$pred <- NA
  testTable_past$pred <- predict(rf, mydata_test, type = "prob")[,2]
  df_pred_past <- testTable_past
  
  bio_test_df <- data.frame(rasterToPoints(cropped_env_test))
  bio_test_df$pred <- predict(rf, bio_test_df, type = "prob")[,2]
  bio_test_df <- bio_test_df[, c('x', 'y', 'pred')]
  bio_past_df_lst <- bio_test_df
  
  # Assuming future data handling is similar to past data prediction
  bio_fut_df <- data.frame(rasterToPoints(cropped_env_fut)) # cropped_env_fut needs to be defined or passed as a function argument
  bio_fut_df$pred <- predict(rf, bio_fut_df, type = "prob")[,2]
  bio_fut_df <- bio_fut_df[, c('x', 'y', 'pred')]
  bio_fut_df_lst <- bio_fut_df
  
  out = list(df_pred, bio_curr_df_lst, df_pred_past, bio_past_df_lst, bio_fut_df_lst)
  return(out)
}

# Usage would be similar to your B-LOO function:
nndm <- nndm_loo_cv(pa_data, cropped_env, cropped_env_test, pa_data_test)

## Save predictions

nndm_cur_pred <- as.data.frame(nndm[1])
nndm_past_pred <- as.data.frame(nndm[3])

write.csv(nndm_cur_pred, 'nndm_cur_pred.csv')
write.csv(nndm_past_pred, 'nndm_past_pred.csv')

## To check ROC AUC scores you can run it
precrec_obj <- evalmod(scores = nndm_past_pred$pred, labels = nndm_past_pred$occurrenceStatus)
autoplot(precrec_obj)

## Plot maps of predictions
nndm_cur_map <- as.data.frame(nndm[2])
nndm_past_map <- as.data.frame(nndm[4])
nndm_fut_map <- as.data.frame(nndm[5])

lst_nndm_maps <- list(nndm_cur_map, nndm_past_map, nndm_fut_map)
change_names <- function(lst_nndm_maps){
  for (i in seq_along(lst_nndm_maps)) {
    names(lst_nndm_maps[[i]]) <- c("longitude", "latitude", "pred")
  }
  return(lst_nndm_maps)
}

nndm_cur_map <- as.data.frame(change_names(lst_nndm_maps)[1])
nndm_past_map <- as.data.frame(change_names(lst_nndm_maps)[2])
nndm_fut_map <- as.data.frame(change_names(lst_nndm_maps)[3])

## Define color palette
my_colors_3 <- c("#008fbf", "#47b0df", "#dae695", '#e6ae95', "#df7b7b", "#a00000")

## Current predictions map
cur_plot <- ggplot() +
  geom_raster(data = nndm_cur_map, aes(x = longitude, y = latitude, fill = pred)) +
  scale_fill_gradientn(colours = my_colors_3, na.value = "white", limits = c(0, 1)) +
  coord_quickmap() +
  theme_classic(base_size = 12, base_family = "Georgia") + theme(legend.position = "bottom") +
  ggtitle('NNDM_LOO Current Predictions') +
  theme(text = element_text(size = 12)) +
  theme(plot.title = element_text(hjust = 0.5))

## Save map of current prediction
ggsave("C:/Users/Holdings/Documents/figures/nndm_cv_map_curr.png", cur_plot)

## Past predictions map
past_plot <- ggplot() +
  geom_raster(data = nndm_past_map, aes(x = longitude, y = latitude, fill = pred)) +
  scale_fill_gradientn(colours = my_colors_3, na.value = "white", limits = c(0, 1)) +
  coord_quickmap() +
  theme_classic(base_size = 12, base_family = "Georgia") + theme(legend.position = "bottom") +
  ggtitle('NNDM_LOO Past Predictions') +
  theme(text = element_text(size = 12)) +
  theme(plot.title = element_text(hjust = 0.5))

## Save map of past prediction
ggsave("C:/Users/Holdings/Documents/figures/nndm_cv_map_past.png", past_plot)

## Future predictions map
fut_plot <- ggplot() +
  geom_raster(data = nndm_fut_map, aes(x = longitude, y = latitude, fill = pred)) +
  scale_fill_gradientn(colours = my_colors_3, na.value = "white", limits = c(0, 1)) +
  coord_quickmap() +
  theme_classic(base_size = 12, base_family = "Georgia") + theme(legend.position = "bottom") +
  ggtitle('NNDM_LOO Future Predictions') +
  theme(text = element_text(size = 12)) +
  theme(plot.title = element_text(hjust = 0.5))

## Save map of future prediction
ggsave("C:/Users/Holdings/Documents/figures/nndm_cv_map_fut.png", fut_plot)
