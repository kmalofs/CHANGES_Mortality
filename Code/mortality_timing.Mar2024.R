#**Description:** This is the code for analysis and figures used in "Fish mass mortality events in northern temperate lakes happen later in the year than in the past"

#The data for this analysis ("mortality_timing.csv") can be downloaded from github, https://github.com/kmalofs/CHANGES_Mortality.

---
setwd("C:/Users/kmalofs/Dropbox (University of Michigan)/Current_Science/Mortality")##set working directory

###Load libraries and data###


#### load libraries #### 
library(ggplot2) #library for graphs
library(tidyverse)
library(quantreg)# for quantile regression
library(RColorBrewer)
library(ggpubr)# make a multipanel figure

### load mortality data ###
mort.data<-read.csv("mortality_timing_March.31.2024.csv") #change the .csv to your data file

### generate decades category###
mort.data$decades <- cut(mort.data$year, breaks = c(1929, 1939, 1949,1959,1969, 1979, 1989, 1999, 2009, 2019, Inf),
                         labels = c("1930s","1940s","1950s","1960s","1970s","1980s","1990s","2000s","2010s","2020s"))
### generate season category###
mort.data$season <- cut(mort.data$month, breaks = c(0, 4, 5, 9, 10, Inf),
                         labels = c("winter","other","summer","other","winter")) 

## Summarise and visualize the data
names(mort.data)
dim(mort.data)
summary(mort.data$doy)

#a tibble of the data by decades
mort.data %>%
  group_by(decades) %>%
  summarise(
    count = n(),
    mean_doy = mean(doy),
    median_doy = median(doy),
    sd_doy =sd(doy)
  )%>%
  as.data.frame(.) %>% 
  dplyr::mutate_if(is.numeric, round, 2)

#a tibble of the data by season
mort.data %>%
  group_by(season) %>%
  summarise(
    count = n(),
    mean_doy = mean(doy),
    median_doy = median(doy),
    sd_doy =sd(doy)
  )%>%
  as.data.frame(.) %>% 
  dplyr::mutate_if(is.numeric, round, 2)

#a tibble of the data by database
mort.data %>%
  group_by(database) %>%
  summarise(
    count = n(),
    mean_doy = mean(doy),
    median_doy = median(doy),
    sd_doy =sd(doy)
  )%>%
  as.data.frame(.) %>% 
  dplyr::mutate_if(is.numeric, round, 2)

###plots of months separated by decades
ggplot(data=mort.data) +
  geom_bar(mapping = aes(x=month.cat, fill=decades))+
  scale_fill_brewer(palette="Dark2")+
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~decades, scales = "free_y")


###LAKE MAPPING###
#select one point per lake 
lakes<-mort.data%>%
  distinct(Lake, New_Key, .keep_all = TRUE)

#get the Michigan basemap
MI_basemap<-map_data("state") %>%
  subset(region %in% c("michigan")) # select michigan 
p<-ggplot(data = MI_basemap) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3) 


# color points by decade and size by doy
map<-p+ geom_point(data=lakes, aes(x = long, y = lat, size = c(doy), color = c(decades))) + 
  scale_color_brewer(palette="Dark2")+ 
  labs(size="day of year",color="decades")+   
  scale_x_continuous(name="latitude") +
  scale_y_continuous(name="longitude")+
  guides(colour = guide_legend(override.aes = list(size = 4)))+
  theme_bw()+
  theme(legend.text=element_text(size=16),legend.title =element_text(size=16), legend.position = "left",axis.title= element_text(size = 20),
        axis.text = element_text(size = 16))
map
ggsave("MI.map.png", map, height = 150, width = 200, units = "mm", dpi = 600)
ggsave("MI.map.eps", map, height = 150, width = 200, units = "mm", dpi = 600)

#### Quantile Regression of day of year v year ####

#quantile regression for quantiles 0.05 to 0.95
qs<-seq(0.10, 0.90, by = 0.05)
qr<-rq(doy~year, data=mort.data, tau = qs)
coef(qr)
summary(qr)
plot(summary(qr), parm="year") 

###contingency analysis of summer and winter kill relative freq by decade
mort.data.dec<- filter(mort.data, decades!= "1980s", decades!= "1990s", decades!= "2000s", season!="other")
mort.data.dec$season <- factor(mort.data.dec$season)##drop the unused factors
mort.data.dec$decades <- factor(mort.data.dec$decades)##drop the unused factors

levels(mort.data.dec$decades)

table(mort.data.dec$decades, mort.data.dec$season) #chi-sq expected freq must be at least 5
test <- chisq.test(table(mort.data.dec$decades, mort.data.dec$season))
test

Table <- table(mort.data.dec$season, mort.data.dec$decades)
addmargins(Table)
mosaicplot( t(Table), col = c("blue","firebrick"), cex.axis = 1, sub = "decade", ylab = "relative frequency", main=NULL)
ChiTest <- chisq.test(mort.data.dec$decades, mort.data.dec$season, correct = FALSE)
ChiTest
addmargins(ChiTest$expected)

### exclude staff note reports
mort.data.noreports <- filter(mort.data, database != "Notes")

### exclude app 
mort.data.noapp <- filter(mort.data, database != "App")

#quantile regression with no reports
qr<-rq(doy~year, data=mort.data.noreports, tau = qs)
coef(qr)
summary(qr)
plot(summary(qr), parm="year") 

#quantile regression with no app data
qr<-rq(doy~year, data=mort.data.noapp, tau = qs)
coef(qr)
summary(qr)
plot(summary(qr), parm="year") 


### generate summer and winter datasets####
mort.data.summer<- filter(mort.data, month >5, month <10) ###June to September###
summary(mort.data.summer$month)
mort.data.winter<-filter(mort.data, month !=5,month!=6,month!=7,month!=8, month!=9,month!=10)  ###November to April###
summary(mort.data.winter$month)

#change obervations in november and december to negative values
mort.data[mort.data$subject_id==84932052, "doy"] <- -58
mort.data[mort.data$subject_id==84931983, "doy"] <- -35
mort.data[mort.data$subject_id==74194, "doy"] <- -31
mort.data[mort.data$subject_id==84931921, "doy"] <- -28
mort.data[mort.data$subject_id==32, "doy"] <- -19
mort.data[mort.data$subject_id==84932076, "doy"] <- -16
mort.data[mort.data$subject_id==84932051, "doy"] <- -13
mort.data[mort.data$subject_id==84931916, "doy"] <- -4

mort.data[mort.data$subject_id==84932052, "year"] <- 1942
mort.data[mort.data$subject_id==84931983, "year"] <- 1961
mort.data[mort.data$subject_id==74194, "year"] <- 2021
mort.data[mort.data$subject_id==84931921, "year"] <- 1940
mort.data[mort.data$subject_id==32, "year"] <- 2012
mort.data[mort.data$subject_id==84932076, "year"] <- 1959
mort.data[mort.data$subject_id==84932051, "year"] <- 1941
mort.data[mort.data$subject_id==84931916, "year"] <- 1940

#quantile regression after ajusting winter dates for Nov and Dec
qr<-rq(doy~year, data=mort.data, tau = qs)
coef(qr)
summary(qr)
plot(summary(qr), parm="year") 

#quantile regression for winterkills only
qr<-rq(doy~year, data=mort.data.winter, tau = qs)
coef(qr)
summary(qr)
plot(summary(qr), parm="year") 

#quantile regression for summer only
qr<-rq(doy~year, data=mort.data.summer, tau = qs)
coef(qr)
summary(qr)
plot(summary(qr), parm="year") 

####doy v year with decades colored and .25 .5 .75 IQR (Fig.3)
mort.point.plot<-ggplot(mort.data, aes(x=year,y=doy, col=decades)) + scale_color_brewer(palette="Dark2") + geom_point(size=2.5) +  
  geom_quantile(color = "darkgrey", quantiles = 0.25, lwd=0.75) + #quantreg based on the 25th percentile
  geom_quantile(color = "black", quantiles = 0.5, lwd=1.5) + # median quantile regression model with tau = 0.5
  geom_quantile(color = "darkgrey", quantiles = 0.75, lwd= 0.75) + #quantreg based on the 75th percentile
  ylab("day of year")+
  scale_x_continuous(breaks=seq(1930,2020,10))+
  theme_bw() + 
  theme(plot.title = element_text(face = "bold",hjust = 0.5), legend.position = "none", axis.title= element_text(size = 20),
        axis.text = element_text(size = 16))
mort.point.plot
ggsave("mort.point.plot.png", mort.point.plot, height = 150, width = 200, units = "mm", dpi = 600)
ggsave("mort.point.plot.eps", mort.point.plot, height = 150, width = 200, units = "mm", dpi = 600)

####doy v year with decades colored and .25 .5 .75 IQR for winter (fig. S2A)
mort.winter.point.plot<-ggplot(mort.data.winter, aes(x=year,y=doy, col=decades)) + scale_color_brewer(palette="Dark2") + geom_point(size=2.5) +  
  geom_quantile(color = "darkgrey", quantiles = 0.25, lwd=0.75) + #quantreg based on the 25th percentile
  geom_quantile(color = "black", quantiles = 0.5, lwd=1.5) + # median quantile regression model with tau = 0.5
  geom_quantile(color = "darkgrey", quantiles = 0.75, lwd= 0.75) + #quantreg based on the 75th percentile
  ylab("day of year")+
  scale_x_continuous(breaks=seq(1930,2020,10))+
  theme_bw() + 
  theme(plot.title = element_text(face = "bold",hjust = 0.5), legend.position = "none", axis.title= element_text(size = 20),
        axis.text = element_text(size = 16))
mort.winter.point.plot
ggsave("mort.winter.point.plot.png", mort.winter.point.plot, height = 150, width = 200, units = "mm", dpi = 600)
ggsave("mort.winter.point.plot.eps", mort.winter.point.plot, height = 150, width = 200, units = "mm", dpi = 600)

####doy v year with decades colored and .25 .5 .75 IQR for summer (fig. S2B)
mort.summer.point.plot<-ggplot(mort.data.summer, aes(x=year,y=doy, col=decades)) + scale_color_brewer(palette="Dark2") + geom_point(size=2.5) +  
  geom_quantile(color = "darkgrey", quantiles = 0.25, lwd=0.75) + #quantreg based on the 25th percentile
  geom_quantile(color = "black", quantiles = 0.5, lwd=1.5) + # median quantile regression model with tau = 0.5
  geom_quantile(color = "darkgrey", quantiles = 0.75, lwd= 0.75) + #quantreg based on the 75th percentile
  ylab("day of year")+
  scale_x_continuous(breaks=seq(1930,2020,10))+
  theme_bw() + 
  theme(plot.title = element_text(face = "bold",hjust = 0.5), legend.position = "none", axis.title= element_text(size = 20),
        axis.text = element_text(size = 16))
mort.summer.point.plot
ggsave("mort.summer.point.plot.png", mort.summer.point.plot, height = 150, width = 200, units = "mm", dpi = 600)
ggsave("mort.summer.point.plot.eps", mort.summer.point.plot, height = 150, width = 200, units = "mm", dpi = 600)
