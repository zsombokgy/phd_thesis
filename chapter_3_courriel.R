#This file contains codes for plotting 'courriel' in Chapter 3

#Read in libraries
library(tidyverse)
library(ggplot2)

#Create data frame with distribution of 'courriel' variables
courriel <- newspaper %>%
  filter(lexical == "email") %>%
  select(type, time) %>%
  group_by(type, time) %>%
  count() %>%
  spread(type, n) %>%
  mutate(english = replace_na(english, 0),
         prescribed = replace_na(prescribed, 0),
         alternative = replace_na(alternative, 0),
         percentage_en = english / (english + prescribed + alternative),
         percentage_pr = prescribed / (english + prescribed + alternative),
         percentage_alt = alternative / (english + prescribed + alternative)) %>%
  ungroup() %>%
  select(time, percentage_en, percentage_pr, percentage_alt) %>%
  gather(type, percentage, -time) %>%
  mutate(type = as.factor(type),
         type = recode(type, "percentage_en" = "email",
                       "percentage_alt" = "mail",
                       "percentage_pr" = "courriel"),
         type = relevel(as.factor(type), ref = "courriel"))

#Visualize courriel over time
gg_courriel <- courriel %>%
  ggplot(aes(x = time, y = percentage, color = type)) + geom_smooth(method = loess, se = FALSE) +
  labs(x = "Time in years", y = "Distribution", color = "Type") +
  scale_x_continuous(limits = c(-156,156),
                     breaks = c(-156,-104,-52,0,52,104,156),
                     labels = c("-3", "-2", "-1","prescription", "1", "2", "3")) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1),
                     labels = c("0",".25",".50",".75","1")) +
  geom_vline(xintercept = 0, color = "red") +
  theme(text = element_text(family = "Times"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "right",
        strip.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major = element_line(color = "grey95"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = "NA"))

#Create data frame with 'courriel' and the newspapers
courriel_source <- newspaper %>%
  filter(lexical == "email") %>%
  select(type, source, time) %>%
  group_by(type, source, time) %>%
  count() %>%
  spread(type, n) %>%
  mutate(english = replace_na(english, 0),
         prescribed = replace_na(prescribed, 0),
         alternative = replace_na(alternative, 0),
         percentage_en = english / (english + prescribed + alternative),
         percentage_pr = prescribed / (english + prescribed + alternative),
         percentage_alt = alternative / (english + prescribed + alternative)) %>%
  ungroup() %>%
  select(time, source, percentage_pr)

#Visualize the distribution of courriel in newspapers
source_colors <- c("La Croix" = "orange", "Le Figaro" = "steelblue1", "Le Monde" = "dodgerblue3",
                   "Le Parisien" = "purple", "Les Echos" = "red", "Journal du Net" = "black",
                   "01net" = "lightseagreen")

gg_courriel_sources <- courriel_source %>%
  ggplot(aes(x = time, y = percentage_pr, color = source)) + geom_smooth(method = loess, se = FALSE) +
  scale_color_manual(values = source_colors) +
  labs(x = "Time in years", y = "Distribution", color = "Newspaper") +
  scale_x_continuous(limits = c(-156,156),
                     breaks = c(-156,-104,-52,0,52,104,156),
                     labels = c("-3", "-2", "-1","prescription", "1", "2", "3")) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1),
                     labels = c("0",".25",".50",".75","1")) +
  geom_vline(xintercept = 0, color = "red") +
  theme(text = element_text(family = "Times"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "right",
        strip.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major = element_line(color = "grey95"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = "NA"))

#Save plots
ggsave(gg_courriel, filename = "Figure-courriel.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(gg_courriel_sources, filename = "Figure-courriel_source.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")