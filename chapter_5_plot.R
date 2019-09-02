#This file contains codes for plotting in Chapter 5
#Codes in Chapter 5 use resources adapted from https://www.tidytextmining.com

#Read in libraries
library(tidyverse)
library(tidytext)
library(stringr)
library(stringi)
library(ggplot2)

#Define function for plotting frequent terms
freqplot <- function(data){
  data %>%
    select(borrow_type, year, word, n2) %>%
    group_by(borrow_type, year) %>%
    top_n(10) %>%
    arrange(desc(n2)) %>%
    ungroup() %>%
    mutate(word = reorder(word, n2)) %>%
    ggplot(aes(x = word, y = n2, fill = borrow_type)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    facet_wrap(year ~ borrow_type, scales = "free_y", ncol = 2, strip.position = "right") +
    theme(text = element_text(size = 10, family = "Times"),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(size = 10, vjust = c(0)),
          axis.text.y = element_text(size = 9),
          axis.title = element_blank(),
          panel.background = element_rect(fill = 'white', color = 'black'),
          panel.border = element_rect(fill = "NA"))
}

#Implement 'freqplot' function
email_freqplot <- freqplot(email_df)
cloud_freqplot <- freqplot(cloud_df)
trailer_freqplot <- freqplot(trailer_df)
hashtag_freqplot <- freqplot(hashtag_df)
hashtagQ_freqplot <- freqplot(hashtagQ_df)

#Define function for plotting top terms of the topics
topterms <- function(data){
  data %>%
    group_by(topic) %>%
    top_n(15, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic2))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic2, scales = "free", strip.position = "right") +
    coord_flip() +
    theme(text = element_text(size = 9, family = "Times"),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(size = 9, vjust = c(0)),
          axis.text.y = element_text(size = 7),
          axis.title = element_blank(),
          panel.background = element_rect(fill = 'white', color = 'black'),
          panel.border = element_rect(fill = "NA"))
}

#Implement the 'topterms' function
cloud_topterms <- topterms(cloud_topics)
email_topterms <- topterms(email_topics)
trailer_topterms <- topterms(trailer_topics)
hashtag_topterms <- topterms(hashtag_topics)
hashtagQ_topterms <- topterms(hashtagQ_topics)

#Define function for plot document assignments
assigndocuments <- function(data){
  data %>%
    ggplot(aes(topic2, gamma)) +
    geom_boxplot() +
    facet_wrap(~ type, strip.position = "top") +
    scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1),
                       labels = c("0",".25",".50",".75","1")) +
    theme(text = element_text(size = 9, family = "Times"),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(size = 9, vjust = c(0)),
          axis.text.y = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.title = element_blank(),
          panel.grid.major = element_line(color = "grey95"),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white', color = 'black'),
          panel.border = element_rect(fill = "NA"))
}

#Implement plots for document assignments
hashtag_assigndocuments <- assigndocuments(hashtag_documents)
hashtagQ_assigndocuments <- assigndocuments(hashtagQ_documents)
email_assigndocuments <- assigndocuments(email_documents)
cloud_assigndocuments <- assigndocuments(cloud_documents)
trailer_assigndocuments <- assigndocuments(trailer_documents)

#Plot sentiment analysis
sentiment_colors <- c("negative" = "orangered2", "neutral" = "dodgerblue2", "positive" = "green3")

hashtag_prescription_sentiment <- ggplot(motdiese_prescription, aes(x = country, fill = sentiment)) +
  geom_bar(aes(fill = as.factor(sentiment)), position = "fill") +
  scale_fill_manual(values = sentiment_colors) +
  labs(y = "Distribution", x = "Country (and variant)") +
  theme(text = element_text(size = 12, family = "Times"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12, vjust = c(0)),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        panel.grid.major = element_line(color = "grey95"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', color = 'black'),
        panel.border = element_rect(fill = "NA"))

#Save plots
ggsave(hashtag_freqplot, filename = "Figure-hashtag-freqplot.png",
       width = 6.5, height = 8.75,  units = "in",
       family = "Times")

ggsave(hashtagQ_freqplot, filename = "Figure-hashtag-quebec-freqplot.png",
       width = 6.5, height = 8.75,  units = "in",
       family = "Times")

ggsave(email_freqplot, filename = "Figure-email-freqplot.png",
       width = 6.5, height = 8.75,  units = "in",
       family = "Times")

ggsave(cloud_freqplot, filename = "Figure-cloud-freqplot.png",
       width = 6.5, height = 8.75,  units = "in",
       family = "Times")

ggsave(trailer_freqplot, filename = "Figure-trailer-freqplot.png",
       width = 6.5, height = 8.75,  units = "in",
       family = "Times")

laymatx <- rbind(c(1,1,1,1,1),
                 c(2,2,2,2,2),
                 c(3,3,3,3,3),
                 c(4,4,4,4,4),
                 c(5,5,5,5,5))

ggsave(filename = "Figure-Twitter-topterms.png",
       width = 6.5, height = 8.15,  units = "in",
       family = "Times",
       arrangeGrob(grobs = list(email_topterms, cloud_topterms,
                                trailer_topterms, hashtag_topterms,
                                hashtagQ_topterms),
                   layout_matrix = laymatx))

laymatx <- rbind(c(1,1,1,1,1),
                 c(2,2,2,2,2),
                 c(3,3,3,3,3),
                 c(4,4,4,4,4),
                 c(5,5,5,5,5))

ggsave(filename = "Figure-Twitter-assigned.png",
       width = 5, height = 8,  units = "in",
       family = "Times",
       arrangeGrob(grobs = list(email_assigndocuments, cloud_assigndocuments,
                                trailer_assigndocuments,
                                hashtag_assigndocuments, hashtagQ_assigndocuments),
                   layout_matrix = laymatx))

ggsave(hashtag_prescription_sentiment, filename = "Figure-hashtag-sentiment.png",
       width = 5.5, height = 3,  units = "in",
       family = "Times")