####Exam 4 Rough Draft.
install.packages(c("maps", "mapdata"))
library(tidyverse)
install.packages('ggmap')
install.packages('osmdata')
library(ggmap)
library(osmdata)
library(maps)
library(mapdata)
library(stringr)

load(file="sec_stad.Rdata")
ss=sec_stad
view(ss)

##Section 1, Bar Plot:
library(ggplot2)

##bar graoh showing capapcity of each stadium with 90 degree x axis text
ggbar_a= ggplot(data=ss, aes(x= Name, y=Capacity)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))

ggbar_a

##Flip the axis so that the stadium names are arranged along the left side of the plot and the values are on the bottom of the plot.
##also with 90 degree angle x axis text
ggbarflip=ggplot(data=ss, aes(x= Capacity, y=Name)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))
ggbarflip

####Section 2. Multiple Points plot

load(file="team_statistics.Rdata")
sunbelt <- subset(ts, Conference=="Sun Belt Conference",
                select=c(Team,Pass.Yard, Rush.Yard))

ggsb=ggplot(sunbelt, aes(x=Rush.Yard, y=Pass.Yard, color=Team)) +
  geom_point() 
ggsb


##Section 3: Box-and-whisker plot

big10 = subset(ts, Conference=="Big Ten Conference",
                  select=c(Team, Rush.Yard))

##For teams in the Big 10 Conference, generate a box-and-whisker plot for rushing yards. (6 points)
bb_big10 =ggplot(big10, aes(x= Team, y =Rush.Yard, fill= Team)) +
  geom_boxplot()
 bb_big10 
  
 ####Make the panel background dark blue and the fill of the box-and-whiskers bright yellow (2 points)
 ##I also removed the gridlines for easier visualization of the box and whiskers
 
  
ggbp = ggplot(big10, aes(x=Team, y=Rush.Yard)) + 
  geom_boxplot(fill = "yellow", colour = "yellow") +
  scale_y_continuous(name = "Rushing Yards") +
  ggtitle("Boxplot of Rushing Yards for Each Team") +
  theme(panel.background = element_rect(fill = "darkblue"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
ggbp

##for part b, Minesotta had the highest rushing yards
a=max(big10$Rush.Yard)
aa=big10[order(-big10$Rush.Yard),]
team_with_highest_rushing_yard=aa[1,]
team_with_highest_rushing_yard



##Section:4 Heat Map _____

load(file="football_stats.Rdata")
fs=football.stats
unique(fs$variable)

##For teams in the Southeastern conference, generate a heat map with ‘ggplot’ showing each team’s performance for each statistical variable. 
stats=fs[fs$Conference=="Southeastern Conference",] 
stats
heat_map_fs= ggplot(stats, aes(x= variable, y =stat, color =Team)) +
  geom_tile()
heat_map_fs

##Plot the log-10 transformed values for the color scale. (2 points)
heat_map_fs_2= ggplot(stats, aes(x= variable, y =stat, color =Team)) +
  geom_tile()+
  scale_y_log10()
heat_map_fs_2




##section 5 : Mapping

###Generate a ‘toner’ map figure showing the central and southeastern United States. (5 points)
bbox = c(left = min(ss$lng)-1, bottom = min(ss$lat)-1, 
         right = max(ss$lng)+1, top = max(ss$lat)+1)
map = get_stamenmap(bbox = bbox,
                    zoom = 5, maptype = 'toner')
ggmap(map)
ggsave("us_map.png")
#####Generate a second ‘toner’ map figure showing the central and southeastern United States, plus the geographic location of all the SEC stadiums (5 points).
bbox2 = c(left = min(ss$lng), bottom = min(ss$lat), 
         right = max(ss$lng), top = max(ss$lat))
map2 = get_stamenmap(bbox = bbox,
                    zoom = 6, maptype = 'toner')
ggmap(map2) + geom_point(data = ss, aes(x = lng, y = lat))
ggsave("us_map_wStadium.png")

##Generate a third ‘toner’ map figure showing the geographic location of all the SEC stadiums, this time represent these locations using the following aesthetics:

map3=ggmap(map2)+
  geom_point(data = ss, aes(x = lng, y = lat, color=Capacity, size=Capacity))

map3
ggsave("us_map_wStadium_Capacity.png")



##Section 6: Analysis
##In which state is the largest capacity stadium located? TN!!!!!
cap=max(ss$Capacity)
cap
cap_1=ss[order(-ss$Capacity),]
cap_1
State_with_largest_capacity=cap_1[1,]
State_with_largest_capacity


##What is the mean and standard deviation capacity of the stadiums in each state? (Hint: you may need to use detach(package:plyr)
library(plyr)
detach(package:plyr)
mean_cap_and_SD = ss %>%  group_by(State) %>% summarize (capMean = mean(Capacity),capSD=sd(Capacity))
mean_cap_and_SD








