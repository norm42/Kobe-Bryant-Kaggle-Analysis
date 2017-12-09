# 
# Copyright https://github.com/norm42/License-Repository/blob/master/normzeck_mit_license%202017.md
# (Mit license)
#
# Code Flow
#  1.  Remove variables that are not used in the model
#  2.  Set up loop for running multiple iterations of the model.
#  3.  Split the dataset into the Training set and Test set
#  4.  Run the model capturing the best model and associated data
#  5.  Predict the test set using the best model
#  6.  print and plot the results (screen and file)


kobe_filter <- cbind(kobe_no_na)  # make a copy we can modify
# xgboost uses a matrix of data as independent variables vs a formula like other algs.
# So we need to delete the independent variables we do not want to use in the model
# as well as copy the dependent variable (shot_made_flag) into a separate vector
#
# team_id and team_name have no information content - always the same
kobe_filter$team_id  <- NULL
kobe_filter$team_name  <- NULL


#x_loc and y_loc are sufficient to describe the shot location, lat/lon are expressing
#the same information. Redundent (may also need some scaling if used).  
kobe_filter$lat  <- NULL
kobe_filter$lon  <- NULL


#there is a pattern related to quarters in the game.  period, minutes, seconds do not describe
#this well.  A new variable game_time that is seconds in game covers normal and overtime periods
kobe_filter$period <- NULL
kobe_filter$minutes_remaining <- NULL
kobe_filter$seconds_remaining <- NULL
kobe_filter$matchup <- NULL
kobe_filter$shot_id <- NULL
kobe_filter$combined_shot_type <- NULL




# analyzing games, I found that there is a wide percent of successful shots.  I created
# a variable game_pct that covers the variable game_id but adds a ranking difference
# Caveat:  I need to test this as game_id is unique to each game, there maybe games with
# same percent which lumps them together.  I am making an assumption that this new
# information will be more important.  However, I will test with/without game_id
# Also game_date does have a unique ID per game which maybe sufficient

# game_event_id is a sequential id assigned to all events in the game, all players.  
# So there is a lot of randomness to that varaible that is not useful 
# for predicting Kobe Bryant's performance
#kobe_filter$game_id <- NULL
kobe_filter$game_event_id <- NULL

kobe_filter$opponent <- NULL
kobe_filter$playoffs <- NULL
kobe_filter$game_date <- NULL

# These are commented out for a demo of variable importance and ROC curves
#kobe_filter$action_type <- NULL
#kobe_filter$shots_notmade_by_second <- NULL
#kobe_filter$shots_made_by_second <- NULL
#kobe_filter$game_pct <- NULL



k_xg_param <- list(objective  = "binary:logistic", eval_metric = "logloss", eta = 0.04,
                max_depth = 6,  subsample = 0.40, colsample_bytree  = 0.80)  

#-------------------
# This initalizes the ability to run iterations of the model and select the best one
# in addition the range of accuracy with multiple iteraitons
# can give an indication of over fitting
#
k_xg_iteration <- 1          # allow multiple iterations
model_xg <- list()           # store models
cm_model <- list()            # store confustion matrix

# setup recording of accuracy for iterations
accuracy <- vector(mode = "double", length = k_xg_iteration)
most_accurate <- 0
accurate_idx <- 1
set.seed(73)  #sheldon :)

k_rfstart <- proc.time()    # start timer

for(i in 1:k_xg_iteration) {
  # Create train and test sets
  in_train <- createDataPartition(y=kobe_filter$shot_made_flag, 
                                  p=0.75, list = FALSE)
  
  kobe_train <- kobe_filter[in_train,]
  kobe_test <- kobe_filter[-in_train,]
  # we need to save the dependent variable for training and testing
  k_train_shot <- as.integer(kobe_train$shot_made_flag) - 1
  k_test_shot <- as.integer(kobe_test$shot_made_flag) - 1
  
  # Then remove them from the variable data frame
  kobe_train$shot_made_flag <- NULL
  kobe_test$shot_made_flag <- NULL
  
  #------------------
  # xgboost uses a matrix of variables vs data frame and formula
  #
  num_train <-data.matrix(kobe_train, rownames.force = NA)
  
  model_xg[[i]] <- xgboost(data = as.matrix(num_train),
                      print_every_n = 10,
                      params = k_xg_param, 
                      label = k_train_shot, 
                      nrounds = 100)
  
  # Predict the test set based on the model
  # predict generates a vector of probabilities that we threshold at 0.5
  num_test <-data.matrix(kobe_test, rownames.force = NA)
  preds <- predict(model_xg[[i]], num_test, type="prob")
  preds_th <- ifelse(preds > 0.5,1,0)
  
  # Making the Confusion Matrix
  cm <- table(k_test_shot, preds_th)
  accuracy[i] <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  cm_model[[i]] <- cm
  #
  # keep the most accurate iteration
  if(accuracy[i] > most_accurate) {
    most_accurate <- accuracy[i] 
    accurate_idx <- i
    k_logdf <- data.frame(kobe_test$loc_x, kobe_test$loc_y, 
                          k_test_shot, preds_th, preds)
    names(k_logdf) <- c("loc_x", "loc_y", "shot_made_flag", "pred_shot", "preds")
  }
}

k_rftime <- proc.time() - k_rfstart  # how long?

k_xg_range <- max(accuracy) - min(accuracy)  # diff in accuracy as one test of over fitting

model_xg1 <- model_xg[[accurate_idx]]   # best model

#-------------------
# Here we generate the data needed for a ROC curve, save the data in csv file

log_xgpr <- prediction(k_logdf$preds, k_test_shot)
log_xgperf <- performance(log_xgpr, measure = "tpr", x.measure = "fpr")

xg_aucdf <- data.frame(log_xgperf@x.values[[1]], log_xgperf@y.values[[1]])
names(xg_aucdf) <- c("FP", "TP")
write.csv(file = "xgboost_rf_auc.csv", xg_aucdf)
#-------------------
# Compute the area under the ROC curve
#
log_xgauc <- performance(log_xgpr, measure = "auc")
log_xgauc <- log_xgauc@y.values[[1]]

#--------------------
# plot ROC curve and save
x11()
p <- ggplot(xg_aucdf, aes(x = FP, y = TP)) + geom_line(linetype = "solid") +
  labs(x="False Positive", y="True Positive") +
  ggtitle("XGBoost Regression ROC Curve")
print(p)
savePlot(filename = "XGBoost_ROC.png", type = "png", device = dev.cur())

#------------------
# Generate importance info.  This is a list of the variable importance to the model
# ranked in order.
xgb_imp <- xgb.importance(colnames(num_test), model = model_xg1)
write.csv(file = "xgboost imp.csv", xgb_imp)


#-------------------------------------------------------------------------------
# MEASURING THE PREDICTIVE ABILITY OF THE MODEL
#1 1 tp
#0 0 tn
#1 0 fn
#0 1 fp
k_confusion <- vector(mode = "character", length = nrow(k_logdf))
for(i in 1:nrow(k_logdf)) {
  k_map <- (as.integer(k_logdf$shot_made_flag[i])) * 2
  k_map <- (as.integer(k_logdf$pred_shot[i])) + k_map + 1
  k_confusion[i] <- switch(k_map, "True Neg", "False Pos", "False Neg", "True Pos")
}
k_logdf <- data.frame(k_logdf, k_confusion)
k_logdf_tptn <- subset(k_logdf, (k_confusion %in% c("True Neg", "True Pos")))
k_logdf_fpfn <- subset(k_logdf, (k_confusion %in% c("False Neg", "False Pos")))
k_tp <- sum(k_confusion == "True Pos")
k_tn <- sum(k_confusion == "True Neg")
k_fp <- sum(k_confusion == "False Pos")
k_fn <- sum(k_confusion == "False Neg")

#----------------------------------

x11()
p <- ggplot(k_logdf, aes(x=k_confusion, y= preds)) +geom_boxplot() +
  labs(x="Confusion Matrix Axis", y="Probability") +
  ggtitle("Distribution of Confusion Matrix, Threshold at 0.5")
print(p)
savePlot(filename = "xgboost_box.png", type = "png", device = dev.cur())


#----------------------------------
# plot results
#
x11()
k_title <- sprintf("Predicted Shots.False Pos: %d; False Neg: %d; True Pos: %d; True Neg: %d",
                   k_fp, k_fn, k_tp, k_tn)
zonecolor <- c('True Neg' = 'firebrick1','False Pos' = 'orange', 'False Neg' = 'blue4', 
               'True Pos' = 'seagreen')
p <- ggplot(k_logdf, aes(x=loc_x, y=loc_y)) + geom_point(aes(color=k_confusion)) +
  ylim(-50,800) + scale_colour_manual(name='k_confusion', values=zonecolor) +
  labs(x="Location X", y="Location Y") +
  theme(plot.title = element_text(size=10)) +
  ggtitle(k_title)
print(p)
savePlot(filename = "xgboost_pred.png", type = "png", device = dev.cur())

x11()
zonecolor <- c('True Neg' = 'firebrick1', 'True Pos' = 'green4')
p <- ggplot(k_logdf_tptn, aes(x=loc_x, y=loc_y)) + geom_point(aes(color=k_confusion)) +
  ylim(-50,800) + scale_colour_manual(name='k_confusion', values=zonecolor) +
  labs(x="Location X", y="Location Y") +
  ggtitle("Predicted Shots True Pos, True Neg")
print(p)
savePlot(filename = "xgboost_true.png", type = "png", device = dev.cur())

x11()
zonecolor <- c('False Neg' = 'blue4', 'False Pos' = 'orange')
p <- ggplot(k_logdf_fpfn, aes(x=loc_x, y=loc_y)) + geom_point(aes(color=k_confusion)) +
  ylim(-50,800) + scale_colour_manual(name='k_confusion', values=zonecolor) +
  labs(x="Location X", y="Location Y") +
  ggtitle("Predicted Shots False Pos, False Neg")
print(p)
savePlot(filename = "xgboost_false.png", type = "png", device = dev.cur())


