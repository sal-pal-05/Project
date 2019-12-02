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

count=1
count2=3
c=1
c2 =3

Concentration <- c()
Colony_count <- c()
label <- c()
Mean <- c()

rm(i)
rm(new.df)

##created a directory for plots
suppressWarnings(dir.create("new_plots"))

##for loop to make plots based on species and respective colony count
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
  count = count +3
  count2 = count2 + 3
  
  ##creating a table "var" for every species along with respective colony count (also in for loop)
  var= ddply(new.df, .(Concentration, Colony_count), summarize,
             Species = (label))
  print(var)
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
                          map ="terrain-background")
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

graph2= ggplot(data=mean_CC2, aes(x= Species, y=mean_c_c)) + 
  geom_col()

graph2
