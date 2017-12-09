# 
# Copyright https://github.com/norm42/License-Repository/blob/master/normzeck_mit_license%202017.md
# (Mit license)
#
# Code Flow
# 1. Clear environment, load libs and utility functions
# 2. Read in the data file
# 3. Rest of the file is different data manipulation cases to generate data frames
#    used in the analysis, visualization, and modeling.  Comments describe the
#    specifics.

setwd("D:/kaggle/KobeBryant_Release")

rm(list=ls())          # clear environment

source("kobe_func.R")  #utility functions

#check_pkgs()         # Run separatly to load required packages 
load_libraries()       #load libraries used

#---------------------------
# Read in full data set.  This has both labled data and test data where
# shot_mad_flag is NA
#
kobe <-  read.csv("data.csv")
kobe$action_type <- as.factor(make.names(kobe$action_type))

#---------------------------
# Compute game_time.  Period, min/sec left is not a tidy variable set.  There is overlap
# in representing time in the game.  game_time is seconds into game accounting for
# full periods (12 min) and overtime periods (5 min)
#
game_time <- vector(mode = "integer", length = nrow(kobe))

for(i in 1:nrow(kobe)) {
  game_time[i] <- kobe_get_sec(as.integer(kobe$period[i]), kobe$minutes_remaining[i], 
                                    kobe$seconds_remaining[i])
}
kobe <- data.frame(kobe, game_time)
kobe$shot_made_flag <- as.factor(kobe$shot_made_flag)
kobe$period <- as.factor(kobe$period)
kobe$playoffs <- as.factor(kobe$playoffs)
kobe$shot_zone_range <- as.factor(kobe$shot_zone_range)
kobe$game_id <- as.factor(kobe$game_id)

#---------------------------
# Reorder shot_zone_range for plotting legend - farthest to nearest
kobe$shot_zone_range <- factor(kobe$shot_zone_range, 
                        levels = c("Back Court Shot", "24+ ft.", 
                                 "16-24 ft.", "8-16 ft.", "Less Than 8 ft."))

#---------------------------
# make three convience data frames 1. shots made; 2. shots not made; 3. test set
kobe_made <- subset(kobe, shot_made_flag == 1)  # shots made
kobe_not_made <- subset(kobe, shot_made_flag == 0)  #shots not made, missed
kobe_na_shots <-  kobe[is.na(kobe$shot_made_flag),]  # shots to predict
kobe_no_na <- kobe[!is.na(kobe$shot_made_flag),]    #no na test set

#-------------------------
# See if there is some game related pattern. Express as total shots and percent made
kobe_game_id_made <- count(kobe_made$game_id)
kobe_game_id_not_made <- count(kobe_not_made$game_id)
kobe_game_intsec <- intersect(kobe_game_id_made$x, kobe_game_id_not_made$x)
k_game_made_intsec <- kobe_game_id_made[kobe_game_id_made$x %in% kobe_game_intsec,]
k_game_not_made_intsec <- kobe_game_id_not_made[kobe_game_id_not_made$x %in% kobe_game_intsec,]

kobe_game_made_count <- data.frame(k_game_made_intsec, k_game_not_made_intsec$freq,
                                   k_game_made_intsec$freq + k_game_not_made_intsec$freq,
                                   (k_game_made_intsec$freq/(k_game_made_intsec$freq + k_game_not_made_intsec$freq)) * 100)
names(kobe_game_made_count) <- c("game_id", "made", "not_made", "total_shots", "pct_made")

#---------------------------
# We now have the percent made and count for each game, we need to update that for 
# the test set (shot_made_flag = NA).  set each game_id to the value created for the labled set.
# This does not account for the NA in the test set, but there is no way to do that.
#
game_pct <- vector(mode = "integer", length = nrow(kobe_no_na))
idx <- 1
for(i in 1:nrow(kobe_no_na)) {
  game_idx <- match(kobe_no_na$game_id[i], kobe_game_made_count$game_id)
  if(!is.na(game_idx)) {
    game_pct[idx] <- kobe_game_made_count$pct_made[game_idx]
  } else {
    game_pct[idx] <- NA
  }
  idx <- idx + 1
}
kobe_no_na <- data.frame(kobe_no_na, game_pct)
kobe_no_na <- kobe_no_na[!is.na(kobe_no_na$game_pct),]

#---------------------------
# Stats on season
kobe_season <- data.frame(count(kobe_made$season), count(kobe_not_made$season))
kobe_season <- data.frame(kobe_season$x, kobe_season$freq, kobe_season$freq.1, kobe_season$freq + kobe_season$freq.1,
                         (kobe_season$freq/(kobe_season$freq + kobe_season$freq.1)) * 100)
names(kobe_season) <- c("season", "shots_made", "shots_missed", "total_shots", "pct_made")


#---------------------------
#Evaluate what happens with distance = 0 by action_type
#
kobe_zero_made <- subset(kobe_made, shot_distance == 0)
kobe_zero_not_made <- subset(kobe_not_made, shot_distance == 0)
kobe_zero_made_action <- count(kobe_zero_made$action_type)
names(kobe_zero_made_action) <- c("action_type", "made")

kobe_zero_made_action <- kobe_zero_made_action[order(-kobe_zero_made_action$made),]
kobe_zero_made_action <- subset(kobe_zero_made_action, kobe_zero_made_action$made >10)
kobe_zero_not_made_action <- count(kobe_zero_not_made$action_type)
names(kobe_zero_not_made_action) <- c("action_type", "not_made")

kobe_zero_not_made_action <- kobe_zero_not_made_action[order(-kobe_zero_not_made_action$not_made),]
kobe_zero_not_made_action <- subset(kobe_zero_not_made_action, kobe_zero_not_made_action$not_made >10)

#---------------------------
# Stats on action type
kobe_action_made <- count(kobe_made$action_type)
names(kobe_action_made) <- c("action_made", "made")
kobe_action_not_made <- count(kobe_not_made$action_type)
names(kobe_action_not_made) <- c("action_not_made", "not_made")

kobe_inter_Sec <- intersect(kobe_action_made$action_made, kobe_action_not_made$action_not_made)


k_made_intsec <- kobe_action_made[kobe_action_made$action_made %in% kobe_inter_Sec,]
k_not_made_intsec <- kobe_action_not_made[kobe_action_not_made$action_not_made %in% kobe_inter_Sec,]

kobe_action_type <- data.frame(k_made_intsec, k_not_made_intsec$not_made,
                               k_made_intsec$made + k_not_made_intsec$not_made,
                               (k_made_intsec$made/(k_made_intsec$made + k_not_made_intsec$not_made)) * 100)
names(kobe_action_type) <- c("action_type", "made", "not_made", "total_shots", "pct_made")
kobe_action_type <- arrange(kobe_action_type, desc(pct_made))
kobe_action_type$action_type <- factor(kobe_action_type$action_type, 
                levels = kobe_action_type$action_type[order(kobe_action_type$pct_made)] )
kobe_action_100 <- subset(kobe_action_type, (total_shots >= 100))


#---------------------
# Stats on distance made
kobe_shot_dist_made <- count(kobe_made$shot_distance)
kobe_shot_dist_not_made <- count(kobe_not_made$shot_distance)
kobe_dist_intsec <- intersect(kobe_shot_dist_made$x, kobe_shot_dist_not_made$x)
k_dist_made_intsec <- kobe_shot_dist_made[kobe_shot_dist_made$x %in% kobe_dist_intsec,]
k_dist_not_made_intsec <- kobe_shot_dist_not_made[kobe_shot_dist_not_made$x %in% kobe_dist_intsec,]

kobe_dist_made_count <- data.frame(k_dist_made_intsec, k_dist_not_made_intsec$freq,
                               k_dist_made_intsec$freq + k_dist_not_made_intsec$freq,
                               (k_dist_made_intsec$freq/(k_dist_made_intsec$freq + k_dist_not_made_intsec$freq)) * 100)
names(kobe_dist_made_count) <- c("distance", "made", "not_made", "total_shots", "pct_made")

#---------------------
# Stats on opponent
kobe_opp_made <- count(kobe_made$opponent)
kobe_opp_not_made <- count(kobe_not_made$opponent)
kobe_opp_count <- data.frame(kobe_opp_made$x, kobe_opp_made$freq, kobe_opp_not_made$freq,
                             kobe_opp_made$freq + kobe_opp_not_made$freq,
                             (kobe_opp_made$freq/ (kobe_opp_made$freq + kobe_opp_not_made$freq)) * 100)
names(kobe_opp_count) <- c("opponent", "made", "not_made", "total_shots", "pct_made")

#----------------------
# Stats on match up
kobe_match_made <- count(kobe_made$matchup)
kobe_match_not_made <- count(kobe_not_made$matchup)
kobe_match_count <- data.frame(kobe_match_made$x, kobe_match_made$freq, kobe_match_not_made$freq,
                             kobe_match_made$freq + kobe_match_not_made$freq,
                             (kobe_match_made$freq/ (kobe_match_made$freq + kobe_match_not_made$freq)) * 100)
names(kobe_match_count) <- c("match", "made", "not_made", "total_shots", "pct_made")

#-----------------------
# Stats on home vs away
kobe_home_made <- subset(kobe_made, grepl("@", kobe_made$matchup))
kobe_home_not_made <- subset(kobe_not_made, grepl("@", kobe_not_made$matchup))
kobe_home_made_total <- sum(as.integer(kobe_home_made$shot_made_flag))
kobe_home_not_made_total <- sum(as.integer(kobe_home_not_made$shot_made_flag))

kobe_away_made <- subset(kobe_made, grepl("vs", kobe_made$matchup))
kobe_away_not_made <- subset(kobe_not_made, grepl("vs", kobe_not_made$matchup))
kobe_away_made_total <- sum(as.integer(kobe_away_made$shot_made_flag))
kobe_away_not_made_total <- sum(as.integer(kobe_away_not_made$shot_made_flag))

kobe_away_pct <- (kobe_away_made_total/(kobe_away_made_total + kobe_away_not_made_total)) * 100
kobe_home_pct <- (kobe_home_made_total/(kobe_home_made_total + kobe_home_not_made_total)) * 100

#-----------------------
# Stats on 2pt vs 3pt
kobe_2pt_made <- subset(kobe_made, grepl("2PT", kobe_made$shot_type))
kobe_2pt_not_made <- subset(kobe_not_made, grepl("2PT", kobe_not_made$shot_type))
kobe_2pt_pct <- (nrow(kobe_2pt_made)/(nrow(kobe_2pt_made) + nrow(kobe_2pt_not_made))) *100

kobe_3pt_made <- subset(kobe_made, grepl("3PT", kobe_made$shot_type))
kobe_3pt_not_made <- subset(kobe_not_made, grepl("3PT", kobe_not_made$shot_type))
kobe_3pt_pct <- (nrow(kobe_3pt_made)/(nrow(kobe_3pt_made) + nrow(kobe_3pt_not_made))) *100

#-----------------------
# Stats on shot area
kobe_shot_area_made <- count(kobe_made$shot_zone_area)
kobe_shot_area_not_made <- count(kobe_not_made$shot_zone_area)
kobe_area_intsec <- intersect(kobe_shot_area_made$x, kobe_shot_area_not_made$x)
k_area_made_intsec <- kobe_shot_area_made[kobe_shot_area_made$x %in% kobe_area_intsec,]
k_area_not_made_intsec <- kobe_shot_area_not_made[kobe_shot_area_not_made$x %in% kobe_area_intsec,]

kobe_area_made_count <- data.frame(k_area_made_intsec, k_area_not_made_intsec$freq,
                                   k_area_made_intsec$freq + k_area_not_made_intsec$freq,
                                   (k_area_made_intsec$freq/(k_area_made_intsec$freq + k_area_not_made_intsec$freq)) * 100)
names(kobe_area_made_count) <- c("zone_area", "made", "not_made", "total_shots", "pct_made")

#------------------------
# Stats on shot zone
kobe_shot_basic_made <- count(kobe_made$shot_zone_basic)
kobe_shot_basic_not_made <- count(kobe_not_made$shot_zone_basic)
kobe_basic_intsec <- intersect(kobe_shot_basic_made$x, kobe_shot_basic_not_made$x)
k_basic_made_intsec <- kobe_shot_basic_made[kobe_shot_basic_made$x %in% kobe_basic_intsec,]
k_basic_not_made_intsec <- kobe_shot_basic_not_made[kobe_shot_basic_not_made$x %in% kobe_basic_intsec,]

kobe_basic_made_count <- data.frame(k_basic_made_intsec, k_basic_not_made_intsec$freq,
                                   k_basic_made_intsec$freq + k_basic_not_made_intsec$freq,
                                   (k_basic_made_intsec$freq/(k_basic_made_intsec$freq + k_basic_not_made_intsec$freq)) * 100)
names(kobe_basic_made_count) <- c("zone_basic", "made", "not_made", "total_shots", "pct_made")

#-------------------------
# Stats on shot_zone_range
kobe_shot_range_made <- count(kobe_made$shot_zone_range)
kobe_shot_range_not_made <- count(kobe_not_made$shot_zone_range)
kobe_range_intsec <- intersect(kobe_shot_range_made$x, kobe_shot_range_not_made$x)
k_range_made_intsec <- kobe_shot_range_made[kobe_shot_range_made$x %in% kobe_range_intsec,]
k_range_not_made_intsec <- kobe_shot_range_not_made[kobe_shot_range_not_made$x %in% kobe_range_intsec,]

kobe_range_made_count <- data.frame(k_range_made_intsec, k_range_not_made_intsec$freq,
                                    k_range_made_intsec$freq + k_range_not_made_intsec$freq,
                                    (k_range_made_intsec$freq/(k_range_made_intsec$freq + k_range_not_made_intsec$freq)) * 100)
names(kobe_range_made_count) <- c("zone_range", "made", "not_made", "total_shots", "pct_made")

#--------------------------
# Compute Simple Moving Average of shots by game time.  This
# adds a low pass filter over the data making the patterns much easier to see.
# There is some loss in detail at the 10 second level
kobe_made_game_time <- count(kobe_made$game_time)
kobe_not_made_game_time <- count(kobe_not_made$game_time)
kobe_na_game_time <- count(kobe_na_shots$game_time)

tmp <- SMA(kobe_made_game_time$freq, 10)
tmp[is.na(tmp)] <- 0
kobe_sma10_made <- data.frame(seq(1:length(tmp)), tmp)
names(kobe_sma10_made)  <- c("game_time", "sma10")

tmp <- SMA(kobe_not_made_game_time$freq, 10)
tmp[is.na(tmp)] <- 0
kobe_sma10_not_made <- data.frame(seq(1:length(tmp)), tmp)
names(kobe_sma10_not_made)  <- c("game_time", "sma10")

#--------------------------
#
# Create two variables that account for the pattern at each period
#
num_label_shots <- nrow(kobe_no_na)
shots_made_by_second <- vector(mode = "integer", length = num_label_shots)
shots_notmade_by_second <- vector(mode = "integer",  length = num_label_shots)
for(i in 1:num_label_shots) {
  idx <- match(kobe_no_na$game_time[i], kobe_made_game_time$x)
  shots_made_by_second[i] <- ifelse(is.na(idx), 0, kobe_made_game_time$freq[idx])
  idx <- match(kobe_no_na$game_time[i], kobe_not_made_game_time$x)
  shots_notmade_by_second[i] <- ifelse(is.na(idx), 0, kobe_not_made_game_time$freq[idx])
}
                                  
kobe_no_na <- data.frame(kobe_no_na, shots_made_by_second, shots_notmade_by_second)

#---------------------
# This was only for the randomForest experiment that had a limit of
# 53 catagories in a factor variable
#
kobe_action_3 <- subset(kobe_action_type, (total_shots >= 3))
kobe_inter_Sec <- intersect(kobe_no_na$action_type, kobe_action_3$action_type)
kobe_no_na_3 <- kobe_no_na[kobe_no_na$action_type %in% kobe_inter_Sec, ]
kobe_no_na_3$action_type <- as.character(kobe_no_na_3$action_type)
kobe_no_na_3$action_type <- as.factor(kobe_no_na_3$action_type)



