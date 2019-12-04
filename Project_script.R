##Project Test run
library(tidyverse)
library(readxl)
library(plyr)
library(dplyr)

##Reading in data
d=read_excel("proj.xlsx")
e=read_excel("latlon.xlsx")

##Reshaping dataset
df=gather(d,species,Colony_Count,-1)

##set up variables for 1st loop
count=1
count2=3
##variables for 2nd loop
c=1
c2 =3

##creating blank objects and setting up for the loop/function
Concentration <- c()
Colony_count <- c()
label <- c()
Mean <- c()
percentage <- c()


rm(i)
rm(new.df)

##created a directory for plots
suppressWarnings(dir.create("new_plots"))

##for loop to make plots based on species and respective colony count
# function that generates a random file name to save the graphs
# 'lenght' can be changed to adjust the size of the generated filename
RandomString <- function(n=1, lenght=4)
{
  randomString <- c(1:n)              
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                    lenght, replace=TRUE),
                             collapse="")
  }
  return(randomString)
  
}

##indexing with for loop
for(i in 1:14){
  # creating new data frame 'new.df' and adding appropriate lists
  new.df <- data.frame(Concentration, Colony_count, label)
  Concentration <- c(df$concentration[count:count2])
  Colony_count <- c(df$Colony_Count[count:count2])
  label <- c(df$species[count:count2])
  
  ##if else statement to color line depending on colony count number
  
  if(df$Colony_Count[count] >= 80){
    new_color = "red"
  }
  else{
    new_color = "blue"
  }
  
  ##graphing inside forloop:makes a graph for each different species
  library(ggplot2)
  head(new.df)
  
  graph= ggplot(data=new.df, aes(x= Concentration, y= Colony_count)) + 
    geom_point(colour=new_color) +
    geom_smooth(color=new_color) +
    ggtitle(df$species[count]) +
    theme_bw()
  # counter that increments by 3 after each iteration (to run through each species at -4 -6 and -8 conc)
  count = count +3
  count2 = count2 + 3
  
  ##creating a table "var", using ddply, for every species along with respective colony count (also in for loop)
  var= ddply(new.df, .(Concentration, Colony_count), summarize,
             Species = (label))
  print(var)
  ##saves the graphs created for each species(from for loop) in "new_plots" folder
  png(file = paste0("new_plots/",RandomString(), ".png"), width = 8.5,
      height = 14, units = "in", res = 300)
  plot(graph)
  dev.off()
  
  
}

##mapping
library(tidyverse)
install.packages("ggmap")
install.packages("osmdata") 
library(ggmap)
library(osmdata)

bb=c(left = min(e$lon)-1.0, bottom = min(e$lat)-1.0, ##the + and minus 0.2 is so that our points are not on the edge of the map.
     right = max(e$lon)+1.0, top = max(e$lat)+1.0)
bb
data.map = get_stamenmap (bbox =bb, zoom =4, 
                          map ="toner-background")
ggmap(data.map) +
  geom_point(data = e, aes (x =lon, y =lat, color=species))

##Reshaping with melt and cast to rearrange species in alphabetical order
library(reshape2)
library(reshape)
molten.data= melt(df, id=c("species","Colony_Count"))

casted.data= cast(molten.data, species+Colony_Count~ variable, sum)
print(casted.data)


## finding the mean colony count for each species

Mean <- c(Mean, mean(df$Colony_Count[1:3]))
Mean <- c(Mean, mean(df$Colony_Count[4:6]))
Mean <- c(Mean, mean(df$Colony_Count[7:9]))
Mean <- c(Mean, mean(df$Colony_Count[10:12]))
Mean <- c(Mean, mean(df$Colony_Count[13:15]))
Mean <- c(Mean, mean(df$Colony_Count[16:18]))
Mean <- c(Mean, mean(df$Colony_Count[19:21]))
Mean <- c(Mean, mean(df$Colony_Count[22:24]))
Mean <- c(Mean, mean(df$Colony_Count[25:27]))
Mean <- c(Mean, mean(df$Colony_Count[28:30]))
Mean <- c(Mean, mean(df$Colony_Count[31:33]))
Mean <- c(Mean, mean(df$Colony_Count[34:36]))
Mean <- c(Mean, mean(df$Colony_Count[37:39]))
Mean <- c(Mean, mean(df$Colony_Count[40:42]))

library(dplyr)

##cbind
mean_cc= as.data.frame(Mean)
names= as.data.frame(unique(df$species))

mean_CC=cbind(mean_cc, names)

mean_CC2=dplyr::rename(mean_CC,mean_c_c=Mean,Species="unique(df$species)")

##plotting bar chart of mean colony count by
graph2= ggplot(data=mean_CC2, aes(x= Species, y=mean_c_c)) + 
  geom_col()

graph2

##created histogram of mean cc vs count
mean_of_colony_count = mean_CC2$mean_c_c
graph3=ggplot(data=mean_CC2, aes(mean_of_colony_count)) + geom_histogram()

graph3

# created custom operator function to find the percent colony count, relatove to the max colony count observed(1440)

`%percent%` <- function(x,y) 
{
  if (y != 0){
    percentage <- c(percentage, ((Mean[2]/y) * x)) # change number in Mean[] to get different percentage
    print(percentage)
  }
}

100 %percent% 1440 # x and y values for custom operator



