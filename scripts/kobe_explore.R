# 
# Copyright https://github.com/norm42/License-Repository/blob/master/normzeck_mit_license%202017.md
# (Mit license)
#
# Code Flow
# 1. Open plot file.  X11() for viewing or pdf.  Note that X11() will only display the last plot
#    unless the device is opened at each print command.  The file is really designed to
#    run separate plots or a pdf file of all the files
#------------------------------------
# This file contains a set of plots useful in the exploratory analysis
# The title of the plot indicates the type/data of plot
# either the x11 or pdf device can be used by commenting out the x11 or pdf function
# calls
#
#x11()
pdf("kobe_plots.pdf", paper="letter", width=8.5, height=11, bg="white",
    family="Courier", pointsize=10)

zonecolor <- c('Back Court Shot' = 'red','24+ ft.' = 'purple', '16-24 ft.' = 'blue4',  '8-16 ft.' = 'blue', 
               'Less Than 8 ft.' = 'deepskyblue')
p <- ggplot(kobe, aes(x=loc_x, y=loc_y)) + geom_point(aes(color=shot_zone_range)) +
  ylim(-50,800) + scale_colour_manual(name='Zone Range', values=zonecolor) +
  labs(x="Location X", y="Location Y") +
  ggtitle("All Shots by Shot Zone Range")
print(p)

zonecolor <- c('Back Court Shot' = 'red','24+ ft.' = 'purple', '16-24 ft.' = 'blue4',  '8-16 ft.' = 'blue', 
               'Less Than 8 ft.' = 'deepskyblue')
p <- ggplot(kobe_made, aes(x=loc_x, y=loc_y)) + geom_point(aes(color=shot_zone_range)) +
  ylim(-50,800) + scale_colour_manual(name='Zone Range', values=zonecolor) +
  labs(x="Location X", y="Location Y") +
  ggtitle("Shots Made by Shot Zone Range")
print(p)

zonecolor <- c('Back Court Shot' = 'red','24+ ft.' = 'purple', '16-24 ft.' = 'blue4',  '8-16 ft.' = 'blue', 
               'Less Than 8 ft.' = 'deepskyblue')
p <- ggplot(kobe_not_made, aes(x=loc_x, y=loc_y)) + geom_point(aes(color=shot_zone_range)) +
  ylim(-50,800) + scale_colour_manual(name='Zone Range', values=zonecolor) +
  labs(x="Location X", y="Location Y") +
  ggtitle("Shots Not Made by Shot Zone Range")
print(p)

zonecolor <- c('Back Court Shot' = 'red','24+ ft.' = 'purple', '16-24 ft.' = 'blue4',  '8-16 ft.' = 'blue', 
               'Less Than 8 ft.' = 'deepskyblue')
p <- ggplot(kobe_na_shots, aes(x=loc_x, y=loc_y)) + geom_point(aes(color=shot_zone_range)) +
  ylim(-50,800) + scale_colour_manual(name='Zone Range', values=zonecolor) +
  labs(x="Location X", y="Location Y") +
  ggtitle("NA Shots by Shot Zone Range")
print(p)

zonecolor <- c('Backcourt' = 'red','Above the Break 3' = 'purple', 'Mid-Range' = 'blue4',  'In The Paint (Non-RA)' = 'blue', 
               'Restricted Area' = 'deepskyblue', 'Left Corner 3' = 'gold', 'Right Corner 3' = 'gold')
p <- ggplot(kobe_made, aes(x=loc_x, y=loc_y)) + geom_point(aes(color=shot_zone_basic)) +
  ylim(-50,800) + scale_colour_manual(name='Shot Zone Basic', values=zonecolor) +
  labs(x="Location X", y="Location Y") +
  ggtitle("Shots Made by Shot Zone Basic")
print(p)

zonecolor <- c('Backcourt' = 'red','Above the Break 3' = 'purple', 'Mid-Range' = 'blue4',  'In The Paint (Non-RA)' = 'blue', 
               'Restricted Area' = 'deepskyblue', 'Left Corner 3' = 'gold', 'Right Corner 3' = 'gold')
p <- ggplot(kobe_not_made, aes(x=loc_x, y=loc_y)) + geom_point(aes(color=shot_zone_basic)) +
  ylim(-50,800) + scale_colour_manual(name='Shot Zone Basic', values=zonecolor) +
  labs(x="Location X", y="Location Y") +
  ggtitle("Shots Not Made by Shot Zone Basic")
print(p)

p <- ggplot(kobe_season, aes(y=total_shots, x=season)) + geom_bar(stat="identity") +
  geom_text(aes(label=sprintf("%0.1f%%", round(pct_made, digits = 1))), nudge_y=100.0, size=3) + 
  scale_y_continuous(breaks= seq(0, 2600, by=200))  +
  labs(x="Season", y="Total Shots, Percent Made") +
  ggtitle("Shots by Season")+
  coord_flip()
print(p)

p <- ggplot(kobe_action_type, aes(y=pct_made, x=action_type)) + geom_bar(stat="identity") +
  geom_text(aes(label=total_shots), nudge_y=5.0, size=3) + 
  scale_y_continuous(breaks= seq(0, 100, by=10))  +
  labs(x="Action Type", y="Percent Made; Total Shots") +
  ggtitle("Percent Made by Action Type, Total Shots") +
  coord_flip()
print(p)

p <- ggplot(kobe_action_100, aes(y=pct_made, x=action_type)) + geom_bar(stat="identity") +
  geom_text(aes(label=total_shots), nudge_y=5.0, size=3) + 
  scale_y_continuous(breaks= seq(0, 100, by=10))  +
  labs(x="Action Type", y="Percent Made; Total Shots") +
  ggtitle("Percent Made by Action Type, Total Shots >=100") +
  coord_flip()
print(p)

p <- ggplot(subset(kobe_shot_dist_made, freq>8), aes(y=freq, x=x)) + geom_bar(stat="identity") +
  geom_text(aes(label=freq), nudge_y=80.0, size=3) + 
  scale_y_continuous(breaks= seq(0, 3000, by=200))  +
  labs(x="Distance in Feet", y="Number Made") +
  ggtitle("Shots Made by Distance > 8 shots") +
  coord_flip()
print(p)

p <- ggplot(subset(kobe_shot_dist_not_made, freq>9), aes(y=freq, x=x)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=freq), nudge_y=80.0, size=3) + 
  scale_y_continuous(breaks= seq(0, 3000, by=200))  +
  labs(x="Distance in Feet", y="Number Not Made") +
  ggtitle("Shots Not Made by Distance > 9 shots") +
  coord_flip()
print(p)

p <- ggplot(kobe_dist_made_count, aes(y=pct_made, x=distance)) + geom_bar(stat="identity") +
  geom_text(aes(label=total_shots), nudge_y=3.0, size=3) + 
  scale_y_continuous(breaks= seq(0, 100, by=10))  +
  labs(x="Distance", y="Percent Made; Total Shots") +
  ggtitle("Percent Made by Distance") +
  coord_flip()
print(p)

p <- ggplot(kobe_opp_count, aes(y=pct_made, x=opponent)) + geom_bar(stat="identity") +
  geom_text(aes(label=total_shots), nudge_y=3.0, size=3) + 
  scale_y_continuous(breaks= seq(0, 50, by=10))  +
  labs(x="Opponent", y="Percent Made; Total Shots") +
  ggtitle("Percent Made by Opponent") +
  coord_flip()
print(p)

p <- ggplot(kobe_na_game_time, aes(x=x, y=freq)) +scale_y_continuous(breaks= seq(0, 20, by=1)) + 
  scale_x_continuous(breaks=c(720,1440, 2160,2880, 3180, 3480, 3780)) +
  labs(x="Seconds in Game", y="Shots in NA Set") +
  ggtitle("Shots in NA Set by Time in Game")+  
  geom_bar(stat="identity")
print(p)

p <- ggplot(kobe_not_made_game_time, aes(x=x, y=freq)) +scale_y_continuous(breaks= seq(0, 120, by=5)) + 
  scale_x_continuous(breaks=c(720,1440, 2160,2880, 3180, 3480, 3780)) +
  labs(x="Seconds in Game", y="Shots Not Made") +
  ggtitle("Shots Not Made by Time in Game")+  
  geom_point(stat="identity")
print(p)

p <- ggplot(kobe_made_game_time, aes(x=x, y=freq)) +
  scale_y_continuous(breaks= seq(0, 120, by=5), limits = c(0,120)) + 
  scale_x_continuous(breaks=c(720,1440, 2160,2880, 3180, 3480, 3780)) +
  labs(x="Seconds in Game", y="Shots Made") +
  ggtitle("Shots Made by Time in Game")+
  geom_point(stat="identity")
print(p)

p <- ggplot(kobe_sma10_made, aes(x=game_time, y=sma10)) +
  scale_y_continuous(breaks= seq(0, 20, by=1), limits = c(0,20)) + 
  scale_x_continuous(breaks=c(720,1440, 2160,2880, 3180, 3480, 3780)) +  
  labs(x="Seconds in Game", y="Shots Made SMA10") +
  ggtitle("Shots Made by Time in Game, SMA10")+
  geom_point(stat="identity")
print(p)


p <- ggplot(kobe_sma10_not_made, aes(x=game_time, y=sma10)) +
  scale_y_continuous(breaks= seq(0, 25, by=1), limits = c(0,20)) + 
  scale_x_continuous(breaks=c(720,1440, 2160,2880, 3180, 3480, 3780)) +  
  labs(x="Seconds in Game", y="Shots Not Made SMA10") +
  ggtitle("Shots Not Made by Time in Game, SMA10")+
  geom_point(stat="identity")
print(p)


p <- ggplot(kobe_game_made_count, aes(x=pct_made)) + 
  geom_histogram(binwidth=5, color="darkblue", fill="lightblue") +
  labs(x="Percent Made", y="Number of Games") +
  ggtitle("Histogram of Number of Games vs Percent Made")
print(p)

p <- ggplot(kobe_game_made_count, aes(x=total_shots)) + 
  geom_histogram(binwidth=3, color="darkblue", fill="lightblue") +
  labs(x="Total Shots", y="Number of Games") +
  ggtitle("Histogram of Number of Games vs Total Shots")
print(p)

dev.off()


