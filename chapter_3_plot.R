#This file contains codes for plotting in Chapter 3

#Read in libraries
library(tidyverse)
library(ggplot2)

#Create subsets for visualization
np_effect2 <- np_lexical$fit %>%
  full_join(np_effect, "lexical") %>%
  full_join(presctime, c("lexical","fr_name")) %>%
  select(lexical, fr_name, time, visregFit, visregLwr, visregUpr, initial, effect) %>%
  mutate(lexical = as.factor(lexical),
         fr_name = as.factor(fr_name))

np_effect_strong <- np_effect2 %>%
  filter(effect == "strong increase")

np_effect_medium <- np_effect2 %>%
  filter(effect == "medium increase")

np_effect_weak1 <- np_effect2 %>%
  filter(effect == "weak increase") %>%
  filter(initial == "medium" |initial == "high" | initial == "very high")

np_effect_weak2 <- np_effect2 %>%
  filter(effect == "weak increase") %>%
  filter(initial == "very low" |initial == "low")

np_effect_nochange <- np_effect2 %>%
  filter(effect == "no change")

np_effect_weakD1 <- np_effect2 %>%
  filter(effect == "weak decrease") %>%
  filter(initial == "high" | initial == "very high")

np_effect_weakD2 <- np_effect2 %>%
  filter(effect == "weak decrease") %>%
  filter(initial == "medium" | initial == "low" | initial == "very low")

np_effect_mediumstrongD <- np_effect2 %>%
  filter(effect == "medium decrease" | effect == "strong decrease")

filterout <- c("performancecapture","simcard","splitscreen","toner","femtocell","popup","moviebootleg","phishing","microblog","multitask","prequel","corenetwork","scriptdoctor","tripleplay")
np_effect_discarded <- np_effect2 %>%
  filter(lexical %in% filterout)

#Create function for visualizing GAM
gg_effect <- function(visreg) {
  ggplot(visreg, aes(x = time, y = visregFit, color = factor(fr_name))) +
    geom_line(size = 1.25) +
    labs(x = "Time in years", y = "Probability of use", color = "Lexical item") +
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
}

#Visualize with 'gg_effect' function
gg_ef_str <- gg_effect(np_effect_strong)
gg_ef_med <- gg_effect(np_effect_medium)
gg_ef_weak1 <- gg_effect(np_effect_weak1)
gg_ef_weak2 <- gg_effect(np_effect_weak2)
gg_ef_nochange <- gg_effect(np_effect_nochange)
gg_ef_weakD1 <- gg_effect(np_effect_weakD1)
gg_ef_weakD2 <- gg_effect(np_effect_weakD2)
gg_ef_medstrD <- gg_effect(np_effect_mediumstrongD)
gg_ef_discarded <- gg_effect(np_effect_discarded)

#Visualize GAM on all variable
gg_ef_all <- ggplot(np_effect2, aes(x = time, y = visregFit)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha=0.3, linetype=1, size=0.2) +
  facet_wrap(~fr_name, ncol = 5) +
  labs(x = "Time in years", y = "Probability of use") +
  scale_x_continuous(limits = c(-156,156),
                     breaks = c(-156,-104,-52,0,52,104,156),
                     labels = c("-3", "-2", "-1","0", "1", "2", "3")) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1),
                     labels = c("0",".25",".50",".75","1")) +
  geom_vline(xintercept = 0, color = "red") +
  theme(text = element_text(family = "Times"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        strip.text = element_text(size = 7),
        legend.position = "right",
        strip.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major = element_line(color = "grey95"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = "NA"))

#Visualize GAM in newspaper sources
source_colors <- c("La Croix" = "orange", "Le Figaro" = "steelblue1", "Le Monde" = "dodgerblue3",
                   "Le Parisien" = "purple", "Les Echos" = "red", "Journal du Net" = "black",
                   "01net" = "lightseagreen")

gg_ef_source <- ggplot(np_source$fit, aes(x = time, y = visregFit, color = factor(source))) +
  geom_line(size = 1.25) +
  scale_color_manual(values = source_colors) +
  labs(x = "Time in years", y = "Probability of use", color = "Newspaper") +
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

#Visualize distribution of variables in newspaper sources
np_sourcecount <- np %>%
  group_by(source, type) %>%
  count() %>%
  spread(type, n) %>%
  mutate(en = english / (english + prescribed),
         pr = prescribed / (english + prescribed)) %>%
  select(source, en, pr) %>%
  gather(type, percentage, -source) %>%
  mutate(type = relevel(as.factor(type), ref = "pr"),
         type = recode(type, 'en' = "English", 'pr'="prescribed"))

gg_np_sourcecount <- np_sourcecount %>%
  ggplot(aes(x = source, y = percentage, fill = type)) +
  geom_bar(stat = "identity") +
  labs(x = "Newspaper source", y = "Distribution", fill = "Borrowing type") +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1),
                     labels = c("0",".25",".50",".75","1")) +
  theme(text = element_text(family = "Times"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 10),
        legend.position = "right",
        strip.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major = element_line(color = "grey95"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = "NA"))

#Visualize effect on all variables combined
effect_colors <- c("strong decrease" = "black", "medium decrease" = "red",
                   "weak decrease" = "orange", "no change" = "purple",
                   "weak increase" = "deepskyblue", "medium increase" = "blue",
                   "strong increase" = "green3")

gg_npeffect <- np_effect %>%
  ggplot(aes(x = diffAftBef, y = before, color = effect)) + geom_point() +
  labs(x = "Effect", y = "Initial Usage", fill = "Borrowing type") +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1),
                     labels = c("0",".25",".50",".75","1")) +
  scale_x_continuous(limits = c(-1,1), breaks = c(-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1),
                     labels = c("-1","-.75","-.5","-.25","0",".25",".50",".75","1")) +
  scale_color_manual(values = effect_colors) +
  geom_text_repel(label = np_effect$fr_name, size = 3) +
  theme(text = element_text(family = "Times"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major = element_line(color = "grey95"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = "NA"))

#Save flots
ggsave(gg_ef_all, filename = "Figure-effect_all.png",
       width = 6.5, height = 8.5,  units = "in",
       family = "Times")

ggsave(gg_ef_str, filename = "Figure-effect_strong.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(gg_ef_med, filename = "Figure-effect_medium.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(gg_ef_weak1, filename = "Figure-effect_weak1.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(gg_ef_weak2, filename = "Figure-effect_weak2.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(gg_ef_nochange, filename = "Figure-effect_nochange.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(gg_ef_weakD1, filename = "Figure-effect_decrease_weak1.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(gg_ef_weakD2, filename = "Figure-effect_decrease_weak2.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(gg_ef_medstrD, filename = "Figure-effect_decrease_mediumStrong.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(gg_ef_discarded, filename = "Figure-effect_discarded.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(gg_ef_source, filename = "Figure-effect_newspaper.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(gg_np_sourcecount, filename = "Figure-source-distribution.png",
       width = 5.5, height = 3.5,  units = "in",
       family = "Times")

ggsave(gg_npeffect, filename = "Figure-effect-overall.png",
       width = 6.5, height = 5,  units = "in",
       family = "Times")
