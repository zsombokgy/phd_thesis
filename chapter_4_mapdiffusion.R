#This file contains codes for mapping diffusion in Chapter 4

#Read in libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggmap)
library(ggrepel)

#Load data
cities <- load("cities.Rda")
quebec_cities <- load("quebec_cities.Rda")
twitter_df <- load("twitter_df.Rda")
hashtag_quebec <- load("hashtag_quebec.Rda")


#Register Google key for ggmap
register_google(key = "INSERT-GOOGLE-KEY")

#Load maps
france <- get_googlemap(center = c(lon = 2, lat = 47),
                        zoom = 6, maptype = "terrain",
                        style = "feature:all|element:labels|visibility:off") %>% ggmap()

quebec <- get_googlemap(center = c(lon = -72.5, lat = 47),
                        zoom = 6, maptype = "terrain",
                        style = "feature:all|element:labels|visibility:off") %>% ggmap()

#Subset data and define diffusion time for France
hashtag_dif <- hashtag %>%
  select(borrow_type, date, city, lon, lat, citysize, population) %>%
  filter(!is.na(city), borrow_type == "prescribed",
         date > ymd_hms("2013-01-22 00:00:00"),
         date < ymd_hms("2013-01-24 00:00:00")) %>%
  group_by(city) %>%
  filter(date == min(date)) %>%
  ungroup() %>%
  mutate(time = as.numeric(difftime(date, min(date), units = "hours")),
         timecat = ifelse(time >= 3, "3 hours or more",
                          ifelse(time >= 2, "from 2 to 3 hours",
                                 ifelse(time >= 1, "from 1 to 2 hours",
                                        "in 1 hour"))),
         timecat =  as.factor(ordered(timecat, levels = c('in 1 hour',
                                                          'from 1 to 2 hours',
                                                          'from 2 to 3 hours',
                                                          '3 hours or more'))),
         citylabel = ifelse(city %in% c("Paris","Bordeaux","Toulouse","Marseille","Lyon","Nantes","Lille","Montpellier","Strasbourg","La Rochelle","Calais","Biarritz","Brest","Poitier","Vichy","Tarbes","Mulhouse"), as.character(city), ""),
         population2 = log10(population))

#Plot diffusion scatter plot and add linear regression line for France
gg_hashtagDiff <- ggplot(hashtag_dif, aes(x = time, y = population2, color = citysize)) +
  geom_point() + geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(x = "Time in hours", y = "Population", color = "") +
  scale_color_manual(values = citycolors) +
  scale_x_continuous(limits = c(1,18.5),
                     breaks = c(1,2,3,4,5,6,7,8,9,10,19)) +
  scale_y_continuous(limits = c(3.8, 6.35),
                     breaks = c(3.8, 4.39, 4.76, 5.02, 5.34, 6.35),
                     labels = c("> 10,000 people","> 20,000 people","> 50,000 people","> 100,000 people","> 200,000 people","> 2,000,000 people")) +
  geom_text_repel(label = hashtag_dif$citylabel, size = 2.5) +
  theme(text = element_text(size = 10, family = "Times"),
        legend.position = "none",
        legend.text = element_text(size = 9),
        axis.text = element_text(size = 10),
        panel.grid.major = element_line(color = "grey95"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', color = 'black'),
        panel.border = element_rect(fill = "NA"))

#Plot diffusion map for France
city_size <- c("3"=0.5, "4"=1, "5"=2, "6"=3, "7"=4, "8"=6)
citycolors <- c("3"="black", "4"="magenta", "5"="blue", "6"="orange", "7"="purple", "8"="red")

hashtag_dif_map <- france + geom_point(data = hashtag_dif,
                                       aes(x = lon, y = lat, size = citysize, color = citysize)) +
  scale_size_manual(values = city_size, name = "Population size",
                    labels = c("3"="> 10,000 people",
                               "4"="> 20,000 people",
                               "5"="> 50,000 people",
                               "6"="> 100,000 people",
                               "7"="> 200,000 people",
                               "8"="> 2,000,000 people")) +
  scale_color_manual(values = citycolors, name = "Population size",
                     labels = c("3"="> 10,000 people",
                                "4"="> 20,000 people",
                                "5"="> 50,000 people",
                                "6"="> 100,000 people",
                                "7"="> 200,000 people",
                                "8"="> 2,000,000 people")) +
  facet_grid(cols = vars(timecat)) +
  labs(x = "longitude", y = "latitude", shape = "", color = "") +
  geom_text_repel(data = hashtag_dif, aes(x = lon, y = lat, label = citylabel), size = 3) +
  theme(text = element_text(size = 10, family = "Times"),
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "grey95"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', color = 'black'),
        panel.border = element_rect(fill = "NA"))

#Subset data and define diffusion time for Quebec
hashtag_quebec_dif <- hashtag_quebec %>%
  select(borrow_type, date, city, lon, lat, citysize, population) %>%
  filter(!is.na(city), borrow_type == "prescribed",
         date > ymd_hms("2011-01-01 00:00:00")) %>%
  group_by(city) %>%
  filter(date == min(date)) %>%
  ungroup() %>%
  mutate(time = as.numeric(difftime(date, min(date), units = "weeks")),
         timecat = ifelse(time >= 52, "1 year or more",
                          ifelse(time >= 12, "from 3 to 12 months",
                                 ifelse(time >= 4, "from 1 to 3 months",
                                        "in 1 month"))),
         timecat =  as.factor(ordered(timecat, levels = c('in 1 month','from 1 to 3 months','from 3 to 12 months','1 year or more'))),
         citylabel = ifelse(city %in% c("Montréal","Québec","Sherbrooke","Baie-Comeau","Trois-Rivières","Joliette","Sept-Iles","Cap-Chat","Amos","Victoriaville"), as.character(city), ""),
         population2 = log10(population),
         month = time/4.33)

#Plot diffusion scatter plot and add linear regression line for Quebec
gg_hashtag_quebec_Diff <- ggplot(hashtag_quebec_dif, aes(x = month, y = population2, color = citysize)) +
  geom_point() + geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(x = "Time in months", y = "Population", color = "") +
  scale_color_manual(values = citycolors_QU) +
  scale_x_continuous(limits = c(0,70),
                     breaks = c(0,3,6,9,12,24,36,48,60,72)) +
  scale_y_continuous(limits = c(3.10, 6.54),
                     breaks = c(3.10, 4.57, 5.02),
                     labels = c("> 1,000 people","> 30,000 people","> 100,000 people")) +
  geom_text_repel(label = hashtag_quebec_dif$city, size = 2.5) +
  theme(text = element_text(size = 10, family = "Times"),
        legend.position = "none",
        legend.text = element_text(size = 9),
        axis.text = element_text(size = 10),
        panel.grid.major = element_line(color = "grey95"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', color = 'black'),
        panel.border = element_rect(fill = "NA"))

#Plot diffusion map for Quebec
city_size_QU <- c("small"=1, "medium"=3, "large"=5)
citycolors_QU <- c("small"="black", "medium"="blue", "large"="red")
hashtag_quebec_dif_map <- quebec + geom_point(data = hashtag_quebec_dif,
                                              aes(x = lon, y = lat, size = citysize, color = citysize)) +
  scale_size_manual(values = city_size_QU, name = "Population size",
                    labels = c("small"="> 1,000 people",
                               "medium"="> 30,000 people",
                               "large"="> 100,000 people")) +
  scale_color_manual(values = citycolors_QU, name = "Population size",
                     labels = c("small"="> 1,000 people",
                                "medium"="> 30,000 people",
                                "large"="> 100,000 people")) +
  facet_grid(cols = vars(timecat)) +
  labs(x = "longitude", y = "latitude", shape = "", color = "") +
  geom_text_repel(data = hashtag_quebec_dif, aes(x = lon, y = lat, label = citylabel), size = 3) +
  theme(text = element_text(size = 10, family = "Times"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(color = "grey95"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', color = 'black'),
        panel.border = element_rect(fill = "NA"))

#Save plots
ggsave(gg_hashtagDiff, filename = "Figure-hashtag-diffusionplot.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(hashtag_dif_map, filename = "Figure-hashtag-diffusionmap.png",
       width = 6.5, height = 3,  units = "in",
       family = "Times")

ggsave(gg_hashtag_quebec_Diff, filename = "Figure-hashtag-quebec-diffusionplot.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(hashtag_quebec_dif_map, filename = "Figure-hashtag-quebec-diffusionmap.png",
       width = 6.5, height = 3,  units = "in",
       family = "Times")
