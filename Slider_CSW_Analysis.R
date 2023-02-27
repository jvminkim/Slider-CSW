library(tidyverse)
library(lubridate)
library(baseballr)
library(RMySQL)
library(pca3d)
library(rgl)
library(bio3d)
library(caret)
library(xgboost)
library(pdp)
library(SHAPforxgboost)
library(iml)
library(pdp)
library(xgboostExplainer)

#Getting pitches from MySQL Server
#############################################################################################################################################################

conn <- dbConnect(MySQL(), dbname = "Statcast",
                  user = "root", password = ":)")

query <- "
SELECT pitcher_name, game_date, release_speed, release_pos_x, release_pos_y, release_pos_z,
description, pfx_x, pfx_z, plate_x, plate_z, pitch_type, balls, strikes
FROM pitches
"

query_height <- "
SELECT full_name, height, primary_position_abbreviation, pitch_hand_code
FROM height
"

all_pitches <- dbGetQuery(conn, query) %>%
  unique()

all_height <- dbGetQuery(conn, query_height) %>%
  unique()

all_height$height <- sapply(strsplit(as.character(all_height$height),"'|\""),
       function(x){as.numeric(x[1]) + (round(as.numeric(x[2])/12,3))})

#2. Gathering pitches, creating CSW stat for all pitches. Creating arm angle variables as well as difference variables.
#CREATED:
  #Datasets :
              #pitcher_csw : creating csw stats as well as arm angle variables for every pitch
              #four_seam_fastball_difference_peripherals : creating a four-seam fastball profile to use for difference statistics
              #other_fb_difference_peripherals : same as before but if pitcher throws a cutter or sinker as their primary fastball
              #offspeed_difference_peripherals : creates the difference profiles between slider and their fastball
              #breakingball_peripherals : calculating the variance of their arm angles/release position when throwing a slider
#############################################################################################################################################################

#Creating CSW Stat

pitcher_csw <- all_pitches %>%
  drop_na(pitch_type, release_pos_x,release_pos_y, release_pos_z) %>%
  filter((pitch_type == "FF" | pitch_type == "FC" | pitch_type == "SI" |
          pitch_type == "SL" | pitch_type == "CH" | pitch_type == "CU")) %>%
  mutate(year = year(game_date)) %>%
  group_by(pitcher_name, year) %>%
  mutate(pitch_year_totals = n()) %>%
  group_by(pitcher_name, year, pitch_type) %>%
  mutate(pitch_type_totals = n(),
         avg_release_x = mean(release_pos_x),
         avg_release_y = mean(release_pos_y),
         avg_release_z = mean(release_pos_z)) %>%
  left_join(all_height, by = c("pitcher_name" = "full_name")) %>%
  drop_na(primary_position_abbreviation , height) %>%
  mutate(adjacent = avg_release_z - (as.numeric(height) * .7),
         opposite = abs(avg_release_x),
         hypotenuse = sqrt(adjacent^2 + opposite^2),
         angle = (acos((adjacent^2 + hypotenuse^2 - opposite^2)/ (2*(adjacent*hypotenuse))) * (180/pi)),
         arm_slot = ifelse(angle >= 0 & angle < 30 , "Overhand", ifelse(
           angle >= 30 & angle < 70, "Three-Quarters", ifelse(
             angle >= 70 & angle <= 90, "Sidearm", ifelse(angle > 90, "Submarine", "Invalid")))),
         called_strike_whiff = ifelse(description == "called_strike", 1, ifelse(description == "swinging_strike" , 1 , 0))) %>%
  unique()

rm(all_pitches)
rm(all_height)

#Used to create difference variables

four_seam_fastball_difference_peripherals <- pitcher_csw %>%
  filter((pitch_type == "FF")) %>%
  group_by(pitcher_name, year) %>%
  mutate(avg_fb_velocity = mean(release_speed, na.rm = T),
         avg_fb_hmov = mean(pfx_x, na.rm = T),
         avg_fb_vmov = mean(pfx_z, na.rm = T)) %>%
  select(pitcher_name, year,avg_fb_velocity, avg_fb_hmov, avg_fb_vmov) %>%
  unique()

#Filtering which fastball is their primary fastball if a pitcher doesn't throw a cutter or sinker.

other_fb_difference_peripherals <- pitcher_csw %>%
  filter((pitch_type == "SI") | (pitch_type == "FC")) %>%
  group_by(pitcher_name, year, pitch_type) %>%
  mutate(si_count = ifelse(pitch_type == "SI", 1,0),
         fc_count = ifelse(pitch_type == "FC", 1,0)) %>%
  mutate(avg_other_fb_velocity = mean(release_speed, na.rm = T),
         avg_other_fb_vmov = mean(pfx_x, na.rm = T),
         avg_other_fb_hmov = mean(pfx_z, na.rm = T)) %>%
  select(pitcher_name, year, pitch_type, pitch_type_totals, avg_other_fb_velocity, avg_other_fb_vmov, avg_other_fb_hmov, si_count, fc_count) %>%
  unique() %>%
  group_by(pitcher_name, year) %>%
  mutate(si_throw = ifelse(pitch_type == "SI", 1,0),
         fc_throw = ifelse(pitch_type == "FC", 1,0),
         fb_sums = sum(si_throw + fc_throw)) %>%
  mutate(dominant_fb = ifelse(fb_sums == 2, ifelse(sum(si_count) >= sum(fc_count), "SI", "FC"), "SI")) %>%
  mutate(keep_fb = ifelse(dominant_fb == pitch_type, 1, ifelse(dominant_fb == "Either", 1,0))) %>%
  filter(keep_fb == 1) %>%
  mutate(pitch_type = dominant_fb) %>%
  select(pitcher_name, year, avg_other_fb_velocity, avg_other_fb_vmov, avg_other_fb_hmov)
  
#Creating difference variables

offspeed_difference_peripherals <- pitcher_csw %>% 
  select(pitcher_name, year,release_speed, pfx_x, pfx_z, pitch_type) %>%
  filter((pitch_type == "SL") ) %>%
  mutate(avg_os_velocity = mean(release_speed),
         avg_os_hmov = mean(pfx_x),
         avg_os_vmov = mean(pfx_z)) %>%
  left_join(four_seam_fastball_difference_peripherals, by = c("pitcher_name", "year")) %>%
  left_join(other_fb_difference_peripherals, by = c("pitcher_name", "year")) %>%
  group_by(pitcher_name, year, pitch_type) %>%
  mutate(velocity_difference = ifelse(!is.na(avg_fb_velocity - avg_os_velocity),avg_fb_velocity - avg_os_velocity,
                                      avg_other_fb_velocity - avg_os_velocity),
         vmov_difference = ifelse(!is.na(avg_fb_vmov - avg_os_vmov),avg_fb_vmov - avg_os_vmov,
                                  avg_other_fb_vmov - avg_os_vmov),
         hmov_difference = ifelse(!is.na(avg_fb_hmov - avg_os_hmov),avg_fb_hmov - avg_os_hmov,
                                  avg_other_fb_hmov - avg_os_hmov)) %>%
  select(pitcher_name, year, pitch_type, velocity_difference, vmov_difference, hmov_difference) %>%
  unique()

#Measuring Variance

breakingball_peripherals <- pitcher_csw %>%
  filter((pitch_type == "SL")) %>%
  group_by(pitcher_name, pitch_type, year) %>%
  mutate(bb_release_variance = (sum(abs(release_pos_x) - abs(avg_release_x))^2) +
           sum((release_pos_y - avg_release_y)^2) +
           sum((release_pos_z - avg_release_z)^2)/pitch_type_totals, 
         avg_bb_release_x = mean(release_pos_x),
         avg_bb_release_y = mean(release_pos_y),
         avg_bb_release_z = mean(release_pos_z)) 
  #select(pitcher_name, pitch_type, year, avg_bb_release_x, avg_bb_release_y, avg_bb_release_z, bb_release_variance)


gc()

#Modeling : Used an XGBoost model

###########################################################################################################################################################

sl_pitches <- sl_pitches %>%
  mutate(release_pos_x = ifelse(release_pos_x > 0, release_pos_x * -1, 0))


#Sliders
set.seed(42)

  sl_smp_size <- floor(.7 * nrow(sl_pitches))
  sl_train_indexes <- sample(seq_len(nrow(sl_pitches)), size = sl_smp_size)
  sl_train <- sl_pitches[sl_train_indexes,]
  sl_test <- head(sl_pitches[-sl_train_indexes,], sl_smp_size)
  
  sl_train_components <- sl_train %>%
    ungroup() %>%
    dplyr::select(called_strike_whiff, release_speed, pfx_x, pfx_z, plate_x, plate_z,
                  balls, strikes, velocity_difference, vmov_difference, hmov_difference,
                  angle, bb_release_variance)
  
  sl_test_components <- sl_test %>%
    ungroup() %>%
    dplyr::select(called_strike_whiff, release_speed, pfx_x, pfx_z, plate_x, plate_z,
                  balls, strikes, velocity_difference, vmov_difference, hmov_difference,
                  angle, bb_release_variance)
  
  sl_full_components <- sl_pitches %>%
    ungroup() %>%
    dplyr::select(called_strike_whiff, release_speed, pfx_x, pfx_z, plate_x, plate_z,
                  balls, strikes, velocity_difference, vmov_difference, hmov_difference,
                  angle, bb_release_variance)
    
  
  sl_xgb_train = xgb.DMatrix(data = as.matrix(sl_train_components[,-1]), label = sl_train_components$called_strike_whiff)
  sl_xgb_test = xgb.DMatrix(data = as.matrix(sl_test_components[,-1]), label = sl_test_components$called_strike_whiff)
  
  sl_xgb_total = xgb.DMatrix(data = as.matrix(sl_full_components[,-1]), label = sl_full_components$called_strike_whiff)
  
  sl_params = list(booster = "gbtree",
                   eta = .01,
                   max_depth = 5,
                   objective="binary:logistic",
                   eval_metric = "rmse"
                   )
  
  sl_watchlist = list(train = sl_xgb_train, test = sl_xgb_test)
  
  sl_xgb_model = xgb.train(data = sl_xgb_train,
                           params = sl_params,
                           watchlist = sl_watchlist, 
                           nrounds = 10000,
                           early_stopping_rounds = 10,
                           verbose = 0
                           )
  
  nround_xgb = which.min(sl_xgb_model$evaluation_log$test_rmse)
  
  final_sl_xgb = xgboost(data = sl_xgb_test, params = sl_params, nrounds = nround_xgb , verbose = 0)
  
  predicted_sl_xCSW = predict(final_sl_xgb, sl_xgb_test)
  
  sl_pitches$called_strike_whiff = as.numeric(sl_pitches$called_strike_whiff)
  
  
  #Creating the datasets based on models:
    #CREATED :
              # sl_info_xCSW : Assigns a probability to each pitch for it to be a CSW
              # sl_info_summaries : Creates a summary of probabilities, with the mean of a pitcher's CSW on pitches in a year.
  ###########################################################################################################################################################  
  
  #Creates the probability prediction for each pitch to be a CSW
  
  sl_info_xCSW <- sl_pitches %>%
    ungroup() %>%
    mutate(xCSW = predict(final_sl_xgb, sl_xgb_total))
  
  sl_info_summaries <- sl_info_xCSW %>%
    left_join(all_height %>% dplyr::select("full_name", "pitch_hand_code"), by = c("pitcher_name" = "full_name"))%>%
    group_by(pitcher_name, year) %>%
    mutate(xCSW = mean(xCSW),
           CSW = mean(called_strike_whiff),
           CSW_diff = xCSW - CSW,
           avg_horizontal_movement = mean(pfx_x) * 12,
           avg_vertical_movement = mean(pfx_z) * 12,
           avg_arm_angle = mean(angle)) %>%
    filter(pitch_type_totals >= 200) %>%
    rename(hand = pitch_hand_code) %>%
    select(pitcher_name, year, xCSW, CSW, CSW_diff, pitch_type_totals, pitch_year_totals, avg_horizontal_movement, avg_vertical_movement, hand, arm_slot, avg_arm_angle) %>%
    unique() %>%
    mutate(`Slider Type` = ifelse((abs(avg_horizontal_movement) > 6.5 && avg_vertical_movement > -2),"Sweeper", "Non-Sweeper"))

  sl_xgb_rmse <- caret::RMSE(mean(sl_test_components$called_strike_whiff),  mean(predicted_sl_xCSW))
  
  #Creating visualizations for SHAP values in the model and pdp plots
  ###########################################################################################################################################################  
 
  X <- data.matrix(sl_train_components[, -1])
  
  sl_train_model <- xgboost(data = sl_xgb_train, params = sl_params, nrounds = nround_xgb , verbose = 0)

  shap <- shap.prep(sl_xgb_model, X_train = X)

  shap.plot.summary(shap)
  
  shap_values <- shap.values(xgb_model = sl_xgb_model, X_train = as.matrix(sl_test_components[,-1]))
  shap_values$mean_shap_score
  shap_values_sl <- shap_values$shap_score
  
  shap_long_csw <- shap.prep(shap_contrib = shap_values_sl, X_train = as.matrix(sl_test_components[,-1]))
  
  Shap_int <- predict(sl_train_model, as.matrix(sl_test_components[,-1]), predinteraction = TRUE)
  
  
  shap_long
  
  plate_x_z <- shap.plot.dependence(data_long = shap_long_csw,
                                    data_int = Shap_int,
                                    x= "plate_x", y = "plate_z",
                                    color_feature = "value", 
                                    mapping = aes(color = "value"),
                                    xlab = "Plate X", ylab = "Plate Z")
  
  #IML
  pdp <- pdp::partial(object = sl_train_model, train = as.matrix(sl_train_components[,-1]),
                      pred.var = "plate_x", type = "classification")
  
  pdp_z <- pdp::partial(object = sl_train_model, train = as.matrix(sl_train_components[,-1]),
                        pred.var = "plate_z", type = "classification")
  
  par(mfrow = c(1,2))
  
  plate_x_csw <- ggplot(data = pdp, aes(x = plate_x, y = yhat)) +
    geom_smooth(se = FALSE) +
    labs(x = "Plate X",  y = "Predicted Values of CSW") +
    xlim(-1,1) +
    ylim(-1,0) + 
    geom_vline(xintercept = .71, colour = "red", linetype = "dashed") + 
    geom_vline(xintercept = -.71, colour = "red", linetype = "dashed") +
    ggtitle("CSW Predictions by Plate Location(Vertically)") + 
    annotate("text", x = .37, y = -0.23, label = "Right Side of Strike Zone") +
    annotate("text", x = -.36, y = -0.23, label = "Left Side of Strike Zone") + 
    geom_segment(
      x = -.59, y = -0.235, xend = -.69, yend = -0.235,
      arrow = arrow(length = unit(0.3, "cm"), type = "closed")
    ) + 
    geom_segment(
      x = .61, y = -0.235, xend = .69, yend = -.235,
      arrow = arrow(length = unit(0.3, "cm"), type = "closed")
    ) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "gray"))
  
  plate_z_csw <- ggplot(data = pdp_z, aes(x = plate_z, y = -yhat)) +
    geom_smooth(se = FALSE) +
    labs(x = "Plate Z",  y = "Predicted Values of CSW") +
    xlim(1,4) +
    ylim(-2.5, 0) + 
    geom_vline(xintercept = 3.5, colour = "red", linetype = "dashed") + 
    geom_vline(xintercept = 1.5, colour = "red", linetype = "dashed") + 
    ggtitle("CSW Predictions by Plate Location(Horizontally)") + 
    annotate("text", x = 3.07, y = -0.19, label = "Upper Strike Zone") +
    annotate("text", x = 1.9, y = -0.19, label = "Lower Strike Zone") + 
    geom_segment(
      x = 1.63, y = -0.20, xend = 1.52, yend = -.20,
      arrow = arrow(length = unit(0.3, "cm"), type = "closed")
    ) + 
    geom_segment(
      x = 3.37, y = -0.20, xend = 3.48, yend = -.20,
      arrow = arrow(length = unit(0.3, "cm"), type = "closed")
    ) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "gray"))
    
  sweeper_plot_xCSW <- ggplot(sl_info_summaries, aes(xCSW, `Slider Type`)) +
    geom_violin(aes(fill=`Slider Type`), trim = FALSE, draw_quantiles = c(0.25,0.75), linetype = "dashed") +
    geom_violin(fill="transparent",draw_quantiles = 0.5) +
    scale_fill_manual(values = c("#46ACC8", "#E58601")) +
    ggtitle(label = "xCSW distribution by slider type")
  
  sweeper_plot_CSW <- ggplot(sl_info_summaries, aes(CSW, `Slider Type`)) +
    geom_violin(aes(fill=`Slider Type`), trim = FALSE, draw_quantiles = c(0.25,0.75), linetype = "dashed") +
    geom_violin(fill="transparent",draw_quantiles = 0.5) +
    scale_fill_manual(values = c("#46ACC8", "#E58601")) +
    ggtitle(label = "CSW distribution by slider type")

  sweeper_plot_xCSW
  sweeper_plot_CSW
 

 
