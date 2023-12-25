#**Description:** This is the code for analysis and figures used in "Fish Mass Mortality Events in Northern Temperate Lakes Are Occurring Later in the Year than they Did Historically"

#The data for this analysis ("mortality_timing.csv") can be downloaded from github, https://github.com/kmalofs/CHANGES_Mortality.

---
###Load libraries and data###


#### load libraries #### 
library(ggplot2) #library for graphs
library(tidyverse)
library(quantreg)# for quantile regression
library(RColorBrewer)

### load mortality data ###
mort.data<-read.csv("mortality_timing.csv") #change the .csv to your data file

### generate decades category###
mort.data$decades <- cut(mort.data$year, breaks = c(1929, 1939, 1949,1959,1969, 1979, 1989, 1999, 2009, 2019, Inf),
                         labels = c("1930s","1940s","1950s","1960s","1970s","1980s","1990s","2000s","2010s","2020s"))

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
#FAQ(pkg="quantreg")

#quantile regression for quantiles 0.05 to 0.95
qs<-seq(0.10, 0.90, by = 0.05)
qr<-rq(doy~year, data=mort.data, tau = qs)
coef(qr)
summary(qr)
plot(summary(qr), parm="year") 

####doy v year with decades colored and .25 .5 .75 IQR
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

#make a multipanel figure

fig2 = ggarrange(map, mort.point.plot, ncol=2,nrow=1, widths = c(1, 1))
fig2
ggsave("fig2.png", fig2, height = 180, width = 220, units = "mm", dpi = 600)
ggsave("fig2.eps", fig2, height = 180, width = 220, units = "mm", dpi = 600)


