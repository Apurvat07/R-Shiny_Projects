library(wbstats) # the World Bank data downloading
library(dplyr) # data manipulation 
library(tidyr) # data manipulation 
library(purrr) # data manipulation 
library(ggplot2) # data visualization 
library(sf) # spatial data manipulation
library(rnaturalearth) # access to spatial data
library(tmap) # spatial data visualization
library(anytime)

#Read the data file
map_data<-read.csv("stay-at-home-covid.csv")
names(map_data)<-c("name_long","Code","Date","Stay_at_home")

#Data processing
map_data$Date<-anydate(map_data$Date)
map_data$name_long<-trimws(map_data$name_long)

map_data = map_data %>% 
  group_by(Date)

#Get the World data coordinates for Map
world = ne_countries(returnclass = "sf") %>% 
  select(iso_a2, name_long, continent)
head(world)

#Join the 2 data frames 
data_wide = map_data %>% spread(Date, Stay_at_home)

world_temporal = world %>%
  left_join(data_wide, by = "name_long")%>% 
  gather(Date, Stay_at_home, `2020-01-01`:`2020-05-14` )

#Case statement for classification
world_temporal = world_temporal %>%
mutate(
  Stay_type = case_when(
    Stay_at_home== 3 ~ "Required (few exceptions)",
    Stay_at_home== 2 ~ "Required (except essentials)",
    Stay_at_home== 1 ~ "Recommend not leaving house",
    Stay_at_home== 0 ~ "No measures"
  )
)

#
world_temporal$Date_1<-as.Date(anydate(world_temporal$Date),"%m %d %Y")
world_temporal$Date_1<-format(world_temporal$Date_1,"%m-%d-%Y")

#Plot
my_ani = tm_shape(world_temporal) +
  tm_fill("Stay_type", title = "Stay at Home Restriction",
          palette = "viridis",legend.is.portrait=FALSE)+
  #tm_layout(legend.position="BOTTOM",legend.stack="horizontal")+
  tm_layout(legend.outside=TRUE,legend.outside.position="bottom")+
  tm_facets(along = "Date_1")

#Generate the Gif
tmap_animation(my_ani, filename = "StayatHome.gif",
               width = 1500, height = 1000, delay = 60)
