#This file contains codes for plotting in Chapter 4

#Read in libraries
library(tidyverse)
library(lubridate)
library(visreg)
library(ggplot2)
library(ggmap)
library(ggrepel)

#Load data
cities <- load("cities.Rda")
quebec_cities <- load("quebec_cities.Rda")
twitter_df <- load("twitter_df.Rda")
hashtag_quebec <- load("hashtag_quebec.Rda")

#Create data frame for the influence score plot
twitter_df2 <- twitter_df %>%
  mutate(influence = log10(((followers + 1) / (following + 1)) * (posts + 1)),
         populationsize = log10(population)) %>%
  select(lexical, populationsize, author, influence, followers, following, posts)

#Plot influence score plot
influence_histogram <- ggplot(twitter_df2, aes(x = influence,
                                               color = lexical, fill = lexical)) +
  geom_density(breaks = seq(-1,10, by = 1),
               alpha = 0.4) +
  geom_segment(aes(x = -1.4, xend = 4, y = 0.4, yend = 0.4), color = "blue") +
  geom_segment(aes(x = 4, xend = 6, y = 0.4, yend = 0.4), color = "purple") +
  geom_segment(aes(x = 6, xend = 9.4, y = 0.4, yend = 0.4), color = "red") +
  annotate(geom = "text", x = 1, y = .38, label = "crowd", color = "blue",
           angle = 0, size = 3) +
  annotate(geom = "text", x = 5, y = .38, label = "broadcasters", color = "purple",
           angle = 0, size = 3) +
  annotate(geom = "text", x = 8, y = .38, label = "influentials", color = "red",
           angle = 0, size = 3) +
  labs(x = "Influence score", y = "Distribution", color = "Lexical concept", fill = "Lexical concept") +
  scale_x_continuous(limits = xlimits_influence, breaks = xbreaks_influence,
                     labels = xlabels_influence) +
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

#Create function for plotting visreg with 'time' variable
gg_time <- function(visreg, xlimits, xbreaks, xlabels) {
  ggplot(visreg$fit, aes(time, visregFit,
                         linetype = factor(gender), fill = factor(gender),
                         color = factor(gender), size = factor(gender))) +
    geom_line() +
    geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha=0.3, linetype=1, size=0.2) +
    labs(x = "Time in years", y = "Probability of use", fill = "Gender", linetype = "Gender", color = "Gender", size = "Gender") +
    scale_size_manual(name = "Gender",
                      breaks = c("M","F"),
                      labels = c("Male","Female"),
                      values = c(1.25,1.25,1.25)) +
    scale_linetype_discrete(name = "Gender",
                            breaks = c("M","F"),
                            labels = c("Male","Female")) +
    scale_fill_discrete(name = "Gender",
                        breaks = c("M","F"),
                        labels = c("Male","Female")) +
    scale_color_discrete(name = "Gender",
                         breaks = c("M","F"),
                         labels = c("Male","Female")) +
    scale_x_continuous(limits = xlimits, breaks = xbreaks, labels = xlabels) +
    scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1),
                       labels = c("0",".25",".50",".75","1")) +
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

#Create function for plotting visreg with 'population' variable
gg_populationsize <- function(visreg, xlimits, xbreaks, xlabels) {
  ggplot(visreg$fit, aes(populationsize, visregFit,
                         linetype = factor(gender), fill = factor(gender),
                         color = factor(gender), size = factor(gender))) +
    geom_line() +
    geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha=0.3, linetype=1, size=0.2) +
    labs(x = "Population size", y = "Probability of use", fill = "Gender", linetype = "Gender", color = "Gender", size = "Gender") +
    scale_size_manual(name = "Gender",
                      breaks = c("M","F"),
                      labels = c("Male","Female"),
                      values = c(1.25,1.25,1.25)) +
    scale_linetype_discrete(name = "Gender",
                            breaks = c("M","F"),
                            labels = c("Male","Female")) +
    scale_fill_discrete(name = "Gender",
                        breaks = c("M","F"),
                        labels = c("Male","Female")) +
    scale_color_discrete(name = "Gender",
                         breaks = c("M","F"),
                         labels = c("Male","Female")) +
    scale_x_continuous(limits = xlimits, breaks = xbreaks, labels = xlabels) +
    scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1),
                       labels = c("0",".25",".50",".75","1")) +
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

#Create function for plotting visreg with 'influence' variable
gg_influence <- function(visreg, xlimits, xbreaks, xlabels) {
  ggplot(visreg$fit, aes(influence, visregFit,
                         linetype = factor(gender), fill = factor(gender),
                         color = factor(gender), size = factor(gender))) +
    geom_segment(aes(x = -1.4, xend = 4, y = 1, yend = 1), color = "blue") +
    geom_segment(aes(x = 4, xend = 6, y = 1, yend = 1), color = "purple") +
    geom_segment(aes(x = 6, xend = 9.4, y = 1, yend = 1), color = "red") +
    annotate(geom = "text", x = 1, y = .95, label = "crowd", color = "blue",
             angle = 0, size = 3) +
    annotate(geom = "text", x = 5, y = .95, label = "broadcasters", color = "purple",
             angle = 0, size = 3) +
    annotate(geom = "text", x = 8, y = .95, label = "influentials", color = "red",
             angle = 0, size = 3) +
    geom_line() +
    geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha=0.3, linetype=1, size=0.2) +
    labs(x = "Influence score", y = "Probability of use", fill = "Gender", linetype = "Gender", color = "Gender", size = "Gender") +
    scale_size_manual(name = "Gender",
                      breaks = c("M","F"),
                      labels = c("Male","Female"),
                      values = c(1.25,1.25,1.25)) +
    scale_linetype_discrete(name = "Gender",
                            breaks = c("M","F"),
                            labels = c("Male","Female")) +
    scale_fill_discrete(name = "Gender",
                        breaks = c("M","F"),
                        labels = c("Male","Female")) +
    scale_color_discrete(name = "Gender",
                         breaks = c("M","F"),
                         labels = c("Male","Female")) +
    scale_x_continuous(limits = xlimits, breaks = xbreaks, labels = xlabels) +
    scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1),
                       labels = c("0",".25",".50",".75","1")) +
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

#Implement plotting functions
#Hashtag and Quebec data needs different variables
xlimits_time <- c(0,365)
xbreaks_time <- c(0,52,104,156,208,260,312,365)
xlabels_time <- c("2010","2011","2012","2013","2014","2015","2016","2017")

xlimits_timeHT <- c(0,234)
xbreaks_timeHT <- c(0,26,78,130,182,234)
xlabels_timeHT <- c("7/2012","2013","2014","2015","2016","2017")

xlimits_timeHTQ <- c(0,313)
xbreaks_timeHTQ <- c(0,52,104,156,208,260,313)
xlabels_timeHTQ <- c("2011","2012","2013","2014","2015","2016","2017")

xlimits_populationsize <- c(4.2,6.35)
xbreaks_populationsize <- c(4.2,4.7,5,5.3,5.68,6.35)
xlabels_populationsize <- c("20,000","50,000","100,000","200,000","500,000","2,000,000")

xlimits_populationsizeQ <- c(2.670,6.532)
xbreaks_populationsizeQ <- c(2.670,3.9,4.3,5,5.9,6.532)
xlabels_populationsizeQ <- c("500","10,000","20,000","100,000","700,000","3,400,000")

xlimits_influence <- c(-1.4,9.4)
xbreaks_influence <- c(-1.4,0,2,4,6,8,9.4)
xlabels_influence <- c("-2","0","2","4","6","8","10")

hashtag_g1 <- gg_time(hashtag_v1, xlimits_timeHT, xbreaks_timeHT, xlabels_timeHT) +
  geom_vline(xintercept = 29, color = "red") +
  annotate(geom = "text", x = 21, y = 0.5, label = "prescription time", color = "red",
           angle = 90)
hashtag_g2 <- gg_populationsize(hashtag_v2, xlimits_populationsize, xbreaks_populationsize, xlabels_populationsize)
hashtag_g3 <- gg_influence(hashtag_v3, xlimits_influence, xbreaks_influence, xlabels_influence)

email_g1 <- gg_time(email_v1, xlimits_time, xbreaks_time, xlabels_time)
email_g2 <- gg_populationsize(email_v2, xlimits_populationsize, xbreaks_populationsize, xlabels_populationsize)
email_g3 <- gg_influence(email_v3, xlimits_influence, xbreaks_influence, xlabels_influence)

cloud_g1 <- gg_time(cloud_v1, xlimits_time, xbreaks_time, xlabels_time) +
  geom_vline(xintercept = 23, color = "red") +
  annotate(geom = "text", x = 15, y = 0.7, label = "prescription time", color = "red",
           angle = 90)
cloud_g2 <- gg_populationsize(cloud_v2, xlimits_populationsize, xbreaks_populationsize, xlabels_populationsize)
cloud_g3 <- gg_influence(cloud_v3, xlimits_influence, xbreaks_influence, xlabels_influence)

trailer_g1 <- gg_time(trailer_v1, xlimits_time, xbreaks_time, xlabels_time) +
  geom_vline(xintercept = 30, color = "red") +
  annotate(geom = "text", x = 22, y = 0.8, label = "prescription time", color = "red",
           angle = 90)
trailer_g2 <- gg_populationsize(trailer_v2, xlimits_populationsize, xbreaks_populationsize, xlabels_populationsize)
trailer_g3 <- gg_influence(trailer_v3, xlimits_influence, xbreaks_influence, xlabels_influence)

hashtagQ_g1 <- gg_time(hashtagQ_v1, xlimits_timeHTQ, xbreaks_timeHTQ, xlabels_timeHTQ) +
  geom_vline(xintercept = 8, color = "red") +
  annotate(geom = "text", x = 0, y = 0.5, label = "prescription time", color = "red",
           angle = 90)
hashtagQ_g2 <- gg_populationsize(hashtagQ_v2, xlimits_populationsizeQ, xbreaks_populationsizeQ, xlabels_populationsizeQ)
hashtagQ_g3 <- gg_influence(hashtagQ_v3, xlimits_influence, xbreaks_influence, xlabels_influence)

#Save plots
ggsave(influence_histogram, filename = "Figure-influenceHistogram.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(hashtag_g1, filename = "Figure-hashtag-time.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(hashtag_g2, filename = "Figure-hashtag-populationsize.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(hashtag_g3, filename = "Figure-hashtag-influence.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(email_g1, filename = "Figure-email-time.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(email_g2, filename = "Figure-email-populationsize.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(email_g3, filename = "Figure-email-influence.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(cloud_g1, filename = "Figure-cloud-time.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(cloud_g2, filename = "Figure-cloud-populationsize.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(cloud_g3, filename = "Figure-cloud-influence.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(trailer_g1, filename = "Figure-trailer-time.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(trailer_g2, filename = "Figure-trailer-populationsize.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(trailer_g3, filename = "Figure-trailer-influence.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(hashtagQ_g1, filename = "Figure-hashtag-quebec-time.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(hashtagQ_g2, filename = "Figure-hashtag-quebec-populationsize.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")

ggsave(hashtagQ_g3, filename = "Figure-hashtag-quebec-influence.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")


