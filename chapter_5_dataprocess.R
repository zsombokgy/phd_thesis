#This file contains codes for data processing in Chapter 5
#Codes in Chapter 5 use resources adapted from https://www.tidytextmining.com

#Read in libraries
library(readxl)
library(tidyverse)
library(tidytext)
library(stringr)
library(stringi)
library(proustr)

#Load data
twitter_df <- load("twitter_df.Rda")
hashtag_quebec <- load("hashtag_quebec.Rda")

#Create data frames for textual analysis
twitter_df2 <- twitter_df %>%
  separate(date, c("ymd","hms"), sep = " ") %>%
  separate(ymd, c("year","month","day"), sep = "-") %>%
  select(lexical, borrow_type, year, content)

email <- twitter_df2 %>%
  filter(lexical == "email") %>%
  select(borrow_type, year, content) %>%
  mutate(year = as.factor(ordered(year, levels = c("2010","2011","2012","2013","2014","2015","2016"))))

cloud <- twitter_df2 %>%
  filter(lexical == "cloud") %>%
  select(borrow_type, year, content) %>%
  mutate(year = as.factor(ordered(year, levels = c("2010","2011","2012","2013","2014","2015","2016"))))

trailer <- twitter_df2 %>%
  filter(lexical == "trailer") %>%
  select(borrow_type, year, content) %>%
  mutate(year = as.factor(ordered(year, levels = c("2010","2011","2012","2013","2014","2015","2016"))))

hashtag <- twitter_df2 %>%
  filter(lexical == "hashtag") %>%
  select(borrow_type, year, content) %>%
  mutate(year = as.factor(ordered(year, levels = c("2013","2014","2015","2016")))) %>%
  filter(!is.na(year))

hashtagQ <- hashtag_quebec %>%
  filter(borrow_type != "motdiese") %>%
  separate(date, c("ymd","hms"), sep = " ") %>%
  separate(ymd, c("year","month","day"), sep = "-") %>%
  select(lexical, borrow_type, year, content) %>%
  mutate(lexical = as.factor(lexical),
         borrow_type = as.factor(borrow_type),
         borrow_type = recode(borrow_type, "english"="english", "prescribed"="motclic")) %>%
  select(borrow_type, year, content) %>%
  mutate(year = as.factor(ordered(year, levels = c("2011","2012","2013","2014","2015","2016")))) %>%
  filter(!is.na(year))

#Define stopwords that will be removed from the text
fr_stopwords <- proust_stopwords() %>%
  mutate(word = stri_trans_general(word, "Latin-ASCII"))
stp_tw <- as.character(c("http","https","rt","t.co","tt","cc","bit.ly","pa.net","to","you","the","for","your", "an", "and", "my", "our", "ours", "mine", "in","of","new","mov","is","liked","p", "ye"))
tw_stopwords <- as.data.frame(stp_tw) %>% rename(word = stp_tw)
en_stopwords <- tidytext::stop_words %>% select(word)

#Define function for text cleaning purposes
#Clean French elision, transform text to 'Latin-ASCII', delete letters, account tags
#Delete previously defined stop words
#Rename stemmed lexical variants
#Remove variants from their respective 'borrowing type' variable
textdf <- function(data){
  data %>%
    mutate(content = str_replace_all(content, "\u0027|\u2019|\u2018|\u02bc|\u02b9|\uff07", "e "),
           content = stri_trans_general(content, "Latin-ASCII"),
           content = str_remove_all(content, "@[:graph:]+"),
           content = str_remove_all(content, "http[:graph:]+"),
           content = str_remove_all(content, "[0-9]")) %>%
    unnest_tokens(word, content) %>%
    anti_join(fr_stopwords) %>%
    anti_join(tw_stopwords) %>%
    anti_join(en_stopwords) %>%
    pr_stem_words(word) %>%
    mutate(word = str_replace_all(word, "^mot_dies$|^motsdies$|^motdies$|^modies$|^mot$|^diese$|^dies$", "motdies"),
           word = str_replace_all(word, "^tweet$", "twitt"),
           word = str_replace_all(word, "^band$", "bande-annonce"),
           word = str_replace_all(word, "^annonc$", "bande-annonce"),
           word = str_replace_all(word, "^trail$", "trailer"),
           word = str_replace_all(word, "^lenvoi$|^envoi$", "envoy"),
           word = str_replace_all(word, "^sm$", "sms"),
           word = str_replace_all(word, "^met$", "mettr"),
           word = str_replace_all(word, "^official$", "officiel"),
           word = str_replace_all(word, "^hadop$", "hadopi")) %>%
    count(borrow_type, year, word, sort = TRUE) %>%
    mutate(n2 = log10(n)) %>%
    mutate(delete = case_when(borrow_type == "prescribed" & word == "motdies" ~ "delete",
                              borrow_type == "motclic" & word == "motclic" ~ "delete",
                              borrow_type == "prescribed" & word == "bande-annonce" ~ "delete",
                              borrow_type == "prescribed" & word == "nuag" ~ "delete",
                              borrow_type == "prescribed" & word == "courriel" ~ "delete",
                              borrow_type == "english" & word == "hashtag" ~ "delete",
                              borrow_type == "english" & word == "trailer" ~ "delete",
                              borrow_type == "english" & word == "cloud" ~ "delete",
                              borrow_type == "english" & word == "email" ~ "delete",
                              borrow_type == "english" & word == "mail" ~ "delete")) %>%
    filter(is.na(delete))
}

#Same function, specific for Quebec tweets
textdfQ <- function(data){
  data %>%
    mutate(content = str_replace_all(content, "\u0027|\u2019|\u2018|\u02bc|\u02b9|\uff07", "e "),
           content = stri_trans_general(content, "Latin-ASCII"),
           content = str_remove_all(content, "@[:graph:]+"),
           content = str_remove_all(content, "http[:graph:]+"),
           content = str_remove_all(content, "[0-9]")) %>%
    unnest_tokens(word, content) %>%
    anti_join(fr_stopwords) %>%
    anti_join(tw_stopwords) %>%
    anti_join(stop_words) %>%
    pr_stem_words(word) %>%
    mutate(word = str_replace_all(word, "^mot_dies$|^motsdies$|^motdies$|^modies$|^diese$|^dies$", "motdies"),
           word = str_replace_all(word, "^motcliqu$|^motclick$|^motsclic$|^mot$|^clic$", "motclic"),
           word = str_replace_all(word, "^tweet$", "twitt"),
           word = str_replace_all(word, "^band$", "bande-annonce"),
           word = str_replace_all(word, "^annonc$", "bande-annonce"),
           word = str_replace_all(word, "^trail$", "trailer"),
           word = str_replace_all(word, "^lenvoi$|^envoi$", "envoy"),
           word = str_replace_all(word, "^sm$", "sms"),
           word = str_replace_all(word, "^met$", "mettr"),
           word = str_replace_all(word, "^official$", "officiel"),
           word = str_replace_all(word, "^hadop$", "hadopi"),
           word = str_replace_all(word, "^nhlsubban$", "subban"),
           word = str_replace_all(word, "^canad$", "canada"),
           word = str_replace_all(word, "^lapress$", "lapresse"),
           word = str_replace_all(word, "^bellletstalk$", "bellcause"),
           word = str_replace_all(word, "^bellcaus$", "bellcause"),
           word = str_replace_all(word, "^bel$", "bellcause")) %>%
    count(borrow_type, year, word, sort = TRUE) %>%
    mutate(n2 = log10(n)) %>%
    mutate(delete = case_when(borrow_type == "motclic" & word == "motclic" ~ "delete",
                              borrow_type == "english" & word == "hashtag" ~ "delete")) %>%
    filter(is.na(delete))
}

#Create tidy text data frames using the 'textdf' function
email_df <- textdf(email) %>%
  mutate(borrow_type = str_replace_all(borrow_type, "prescribed", "courriel"),
         borrow_type = str_replace_all(borrow_type, "english", "email"),
         borrow_type = relevel(as.factor(borrow_type), ref = "courriel"))

cloud_df <- textdf(cloud) %>%
  mutate(borrow_type = str_replace_all(borrow_type, "prescribed", "nuage"),
         borrow_type = str_replace_all(borrow_type, "english", "cloud"),
         borrow_type = relevel(as.factor(borrow_type), ref = "nuage"))

trailer_df <- textdf(trailer) %>%
  mutate(borrow_type = str_replace_all(borrow_type, "prescribed", "bande-annonce"),
         borrow_type = str_replace_all(borrow_type, "english", "trailer"),
         borrow_type = relevel(as.factor(borrow_type), ref = "bande-annonce"))

hashtag_df <- textdf(hashtag) %>%
  mutate(borrow_type = str_replace_all(borrow_type, "prescribed", "mot-dièse"),
         borrow_type = str_replace_all(borrow_type, "english", "hashtag"),
         borrow_type = relevel(as.factor(borrow_type), ref = "mot-dièse"))

hashtagQ_df <- textdfQ(hashtagQ) %>%
  mutate(borrow_type = str_replace_all(borrow_type, "motclic", "mot-clic"),
         borrow_type = str_replace_all(borrow_type, "english", "hashtag"),
         borrow_type = relevel(as.factor(borrow_type), ref = "mot-clic"))

#Create prescription data frame for sentiment analysis
hashtag_prescription <- twitter_df %>%
  filter(lexical == "hashtag", borrow_type == "prescribed",
         date > ymd_hms("2013-01-22 00:00:00"), date < ymd_hms("2013-01-29 00:00:00")) %>%
  select(country, content, sentiment) %>%
  mutate(content = str_replace_all(content, "\u0027|\u2019|\u2018|\u02bc|\u02b9|\uff07", "e "),
         #content = stri_trans_general(content, "Latin-ASCII"),
         content = str_remove_all(content, "@[:graph:]+"),
         content = str_remove_all(content, "http[:graph:]+"),
         content = str_remove_all(content, "[0-9]"),
         content = str_replace_all(content, "[:blank:]+", " "),
         content = str_remove_all(content, "^[:blank:]+|[:blank:]+$"),
         retweet = ifelse(grepl("^RT", content), TRUE, FALSE))

hashtag_prescriptionQ <- hashtag_quebec %>%
  filter(borrow_type == "motdiese",
         date > ymd_hms("2013-01-22 00:00:00"), date < ymd_hms("2013-01-29 00:00:00")) %>%
  select(country, content, sentiment) %>%
  mutate(country = as.factor(country),
         country = recode(country, "Canada" = "Quebec (mot-dièse)"),
         sentiment = as.factor(ordered(sentiment,
                                       levels=c('Basic Negative','Basic Neutral','Basic Positive'))),
         sentiment = recode(sentiment, 'Basic Negative' = "negative",
                            'Basic Neutral' = "neutral",
                            'Basic Positive' = "positive")) %>%
  mutate(content = str_replace_all(content, "\u0027|\u2019|\u2018|\u02bc|\u02b9|\uff07", "e "),
         #content = stri_trans_general(content, "Latin-ASCII"),
         content = str_remove_all(content, "@[:graph:]+"),
         content = str_remove_all(content, "http[:graph:]+"),
         content = str_remove_all(content, "[0-9]"),
         content = str_replace_all(content, "[:blank:]+", " "),
         content = str_remove_all(content, "^[:blank:]+|[:blank:]+$"),
         retweet = ifelse(grepl("^RT", content), TRUE, FALSE))

hashtag_prescriptionQ_motclic <- hashtag_quebec %>%
  filter(borrow_type == "prescribed",
         date > ymd_hms("2011-01-01 00:00:00"), date < ymd_hms("2011-04-01 00:00:00")) %>%
  select(country, content, sentiment) %>%
  mutate(country = as.factor(country),
         country = recode(country, "Canada" = "Quebec (mot-clic)"),
         sentiment = as.factor(ordered(sentiment,
                                       levels=c('Basic Negative','Basic Neutral','Basic Positive'))),
         sentiment = recode(sentiment, 'Basic Negative' = "negative",
                            'Basic Neutral' = "neutral",
                            'Basic Positive' = "positive")) %>%
  mutate(content = str_replace_all(content, "\u0027|\u2019|\u2018|\u02bc|\u02b9|\uff07", "e "),
         #content = stri_trans_general(content, "Latin-ASCII"),
         content = str_remove_all(content, "@[:graph:]+"),
         content = str_remove_all(content, "http[:graph:]+"),
         content = str_remove_all(content, "[0-9]"),
         content = str_replace_all(content, "[:blank:]+", " "),
         content = str_remove_all(content, "^[:blank:]+|[:blank:]+$"),
         retweet = ifelse(grepl("^RT", content), TRUE, FALSE))

motdiese_prescription <- rbind(hashtag_prescription,hashtag_prescriptionQ,hashtag_prescriptionQ_motclic) %>%
  filter(!is.na(country))