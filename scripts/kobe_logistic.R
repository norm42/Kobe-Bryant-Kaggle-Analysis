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
#
# Remove the variables we are not using for the model
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

# analyzing games, I found that there is a wide percent of successful shots.  I created
# a variable game_pct that covers the variable game_id but adds a ranking difference
# Caveat:  I need to test this as game_id is unique to each game, there maybe games with
# same percent which lumps them together.  I am making an assumption that this new
# information will be more important.  However, I will test with/without game_id
# Also game_date does have a unique ID per game which maybe sufficient

# game_event_id is a sequential id assigned to all events in the game, all players.  
# So there is a lot of randomness to that varaible that is not useful 
# for predicting Kobe Bryant's performance
kobe_filter$game_id <- NULL
kobe_filter$game_event_id <- NULL

k_log_iteration <- 1          # allow multiple iterations
model_log <- list()           # store models
cm_model <- list()            # store confustion matrix
cm_train <- list()            # training set confusion matrix
# setup recording of accuracy for iterations
accuracy <- vector(mode = "double", length = k_log_iteration)
accuracy_train <- vector(mode = "double", length = k_log_iteration)
most_accurate <- 0
accurate_idx <- 1
set.seed(73)  #sheldon :)

k_rfstart <- proc.time()      # time the model process

for(i in 1:k_log_iteration) {
  # split into train and test sets
  in_train <- createDataPartition(y=kobe_filter$shot_made_flag, 
                                  p=0.75, list = FALSE)
  
  kobe_train <- kobe_filter[in_train,]
  kobe_test <- kobe_filter[-in_train,]
  # Logistic regression needs to have the same catagories in the training and test set
  # for prediction to work, note - may need to check more cat vars
  #
  k_intersect <- intersect(kobe_test$action_type, kobe_train$action_type)
  kobe_train <- kobe_train[kobe_train$action_type %in% k_intersect,]
  kobe_test <- kobe_test[kobe_test$action_type %in% k_intersect,]
  
  # we need to save the dependent variable for training and testing (xgboost carryover)
  k_train_shot <- as.integer(kobe_train$shot_made_flag) - 1
  k_test_shot <- as.integer(kobe_test$shot_made_flag) - 1
  
  # Logstic model
  model_log[[i]] <- glm(shot_made_flag ~ season + shot_distance + 
                            shot_type + shot_zone_basic + shot_zone_area +
                            loc_x + loc_y + game_time + action_type +
                            shots_notmade_by_second + shots_made_by_second + game_pct,
                            family=binomial(link='logit'),data=kobe_train)

  # Predict the test set based on the model
  # predict generates a vector of probabilities that we threshold at 0.5
  preds <- predict(model_log[[i]], kobe_test, type = 'response');
  preds_th <- ifelse(preds > 0.5,1,0)

  # Make the Confusion Matrix
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

k_rftime <- proc.time() - k_rfstart   # how long did it take?

#-------------------
# Here we generate the data needed for a ROC curve
log_pr <- prediction(k_logdf$preds, k_test_shot)
log_perf <- performance(log_pr, measure = "tpr", x.measure = "fpr")

aucdf <- data.frame(log_perf@x.values[[1]], log_perf@y.values[[1]])
names(aucdf) <- c("FP", "TP")
write.csv(file = "logistic_rf_auc.csv", aucdf)

#------------------------------
# Plot ROC
x11()
p <- ggplot(aucdf, aes(x = FP, y = TP)) + geom_line(linetype = "solid") +
  labs(x="False Positive", y="True Positive") +
  ggtitle("Logistic Regression ROC Curve")
print(p)
savePlot(filename = "logistic_ROC.png", type = "png", device = dev.cur())

#-------------------
# Compute the area under the ROC curve
#
log_auc <- performance(log_pr, measure = "auc")
log_auc <- log_auc@y.values[[1]]

k_log_range <- max(accuracy) - min(accuracy)    # range of accuracy for iterations

model_log1 <- model_log[[accurate_idx]]         # best model

log_conf <- cm_model[[accurate_idx]]            # confustion matrix for model

log_sum <- summary(model_log1)                  # summary of model

log_anova <- anova(model_log1, test = "Chisq")  # anova/chisq for significance of variables


# Log info to a file
#
sink("log_summary.txt")
cat("Model generation time\n")
print(k_rftime)
cat("Area under ROC \n")
print(log_auc)
cat("Model Accuracy\n")
print(accuracy)
cat("Model ------------- \n")
print(model_log1)
cat("Confusion matrix \n")
print(log_conf)
cat("Summary\n")
print(log_sum)
cat("Anova \n")
print(log_anova)
#log_confit
#log_odds
sink()


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

x11()
p <- ggplot(k_logdf, aes(x=k_confusion, y= preds)) +geom_boxplot() +
  labs(x="Confusion Matrix Axis", y="Probability") +
  ggtitle("Distribution of Confusion Matrix, Threshold at 0.5")
print(p)
savePlot(filename = "logistic_box.png", type = "png", device = dev.cur())

#----------------------------------
# plot results.  Plot title indicates the plot content.
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
savePlot(filename = "logistic_pred.png", type = "png", device = dev.cur())

x11()
zonecolor <- c('True Neg' = 'firebrick1', 'True Pos' = 'green4')
p <- ggplot(k_logdf_tptn, aes(x=loc_x, y=loc_y)) + geom_point(aes(color=k_confusion)) +
  ylim(-50,800) + scale_colour_manual(name='k_confusion', values=zonecolor) +
  labs(x="Location X", y="Location Y") +
  ggtitle("Predicted Shots True Pos, True Neg")
print(p)
savePlot(filename = "logistic_true.png", type = "png", device = dev.cur())

x11()
zonecolor <- c('False Neg' = 'blue4', 'False Pos' = 'orange')
p <- ggplot(k_logdf_fpfn, aes(x=loc_x, y=loc_y)) + geom_point(aes(color=k_confusion)) +
  ylim(-50,800) + scale_colour_manual(name='k_confusion', values=zonecolor) +
  labs(x="Location X", y="Location Y") +
  ggtitle("Predicted Shots False Pos, False Neg")
print(p)
savePlot(filename = "logistic_false.png", type = "png", device = dev.cur())


