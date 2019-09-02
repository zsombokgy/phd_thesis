#This file contains codes for data analysis in Chapter 4

#Read in libraries
library(readxl)
library(tidyverse)
library(lubridate)
library(mgcv)
library(visreg)

#Load data
cities <- load("cities.Rda")
quebec_cities <- load("quebec_cities.Rda")
twitter_df <- load("twitter_df.Rda")
hashtag_quebec <- load("hashtag_quebec.Rda")

#Separate data by variables
hashtag <- twitter_df %>%
  filter(lexical == "hashtag", date > ymd_hms("2012-07-06 00:00:00"))

cloud <- twitter_df %>%
  filter(lexical == "cloud")

email <- twitter_df %>%
  filter(lexical == "email")

trailer <- twitter_df %>%
  filter(lexical == "trailer")

#Create function to prepare data frames for GAM
gam_prep <- function(data){
  data %>%
    mutate(date = floor_date(date, "week"),
           time = factor(as.numeric(difftime(date, min(date), units = "weeks")),
                         levels = c(0:365)),
           time = as.numeric(as.character(time)),
           influence = log10(((followers + 1) / (following + 1)) * (posts + 1)),
           populationsize = log10(population)) %>%
    select(time, borrow_type, populationsize, gender, influence)
}

#Implement 'gam_prep' function
email2 <- gam_prep(hashtag)
cloud2 <- gam_prep(cloud)
trailer2 <- gam_prep(trailer)
hashtag2 <- gam_prep(hashtag)
hashtagQ2 <- gam_prep(hashtag_quebec)

#Create function for GAM
tw_gam <- function(data){
  gam(borrow_type ~ s(time, k = 30) +
        s(populationsize, k = 10) +
        s(influence, by = gender, k = 20) +
        gender,
      family = "binomial", method = "REML",
      data = data)
}

#Implement 'tw_gam' function
email_gam <- tw_gam(email2)
cloud_gam <- tw_gam(cloud2)
trailer_gam <- tw_gam(trailer2)
hashtag_gam <- tw_gam(hashtag2)
hashtagQ_gam <- tw_gam(hashtagQ2)

#Diagnose GAM fit
summary(INSERT-GAM)
gam.check(INSERT-GAM, old.style = TRUE)
concurvity(INSERT-GAM)

#Create visreg function to prepare for the visualization by social variables for each term
#Hashtag and Quebec require modified implementations
visreg_twitter_time <- function(data, x, by) {
  visreg(data, x, by = by,
         scale="response", overlay=TRUE, partial=FALSE, band=TRUE, legend=FALSE,
         ylim=c(0,1), xlim=c(0,365), plot = FALSE)
}

visreg_twitter_timeHT <- function(data, x, by) {
  visreg(data, x, by = by,
         scale="response", overlay=TRUE, partial=FALSE, band=TRUE, legend=FALSE,
         ylim=c(0,1), xlim=c(0,234), plot = FALSE)
}

visreg_twitter_timeHTQ <- function(data, x, by) {
  visreg(data, x, by = by,
         scale="response", overlay=TRUE, partial=FALSE, band=TRUE, legend=FALSE,
         ylim=c(0,1), xlim=c(0,313), plot = FALSE)
}

visreg_twitter_populationsize <- function(data, x, by) {
  visreg(data, x, by = by,
         scale="response", overlay=TRUE, partial=FALSE, band=TRUE, legend=FALSE,
         ylim=c(0,1), xlim=c(4.21,6.34), plot = FALSE)
}

visreg_twitter_populationsizeQ <- function(data, x, by) {
  visreg(data, x, by = by,
         scale="response", overlay=TRUE, partial=FALSE, band=TRUE, legend=FALSE,
         ylim=c(0,1), xlim=c(2.670,6.532), plot = FALSE)
}

visreg_twitter_influence <- function(data, x, by) {
  visreg(data, x, by = by,
         scale="response", overlay=TRUE, partial=FALSE, band=TRUE, legend=FALSE,
         ylim=c(0,1), xlim=c(-1.4,9.4), plot = FALSE)
}

#Implement the visreg functions
hashtag_v1 <- visreg_twitter_timeHT(hashtag_gam, "time", "gender")
hashtag_v2 <- visreg_twitter_populationsize(hashtag_gam, "populationsize", "gender")
hashtag_v3 <- visreg_twitter_influence(hashtag_gam, "influence", "gender")

email_v1 <- visreg_twitter_time(email_gam, "time", "gender")
email_v2 <- visreg_twitter_populationsize(email_gam, "populationsize", "gender")
email_v3 <- visreg_twitter_influence(email_gam, "influence", "gender")

cloud_v1 <- visreg_twitter_time(cloud_gam, "time", "gender")
cloud_v2 <- visreg_twitter_populationsize(cloud_gam, "populationsize", "gender")
cloud_v3 <- visreg_twitter_influence(cloud_gam, "influence", "gender")

trailer_v1 <- visreg_twitter_time(trailer_gam, "time", "gender")
trailer_v2 <- visreg_twitter_populationsize(trailer_gam, "populationsize", "gender")
trailer_v3 <- visreg_twitter_influence(trailer_gam, "influence", "gender")

hashtagQ_v1 <- visreg_twitter_timeHTQ(hashtag_quebec_gam, "time", "gender")
hashtagQ_v2 <- visreg_twitter_populationsizeQ(hashtag_quebec_gam, "populationsize", "gender")
hashtagQ_v3 <- visreg_twitter_influence(hashtag_quebec_gam, "influence", "gender")


