#This file contains codes for data analysis in Chapter 5
#Codes in Chapter 5 use resources adapted from https://www.tidytextmining.com

#Read in libraries
library(tidyverse)
library(tidytext)
library(stringr)
library(stringi)
library(topicmodels)

#Create Document-Term-Matrix data frames for each variable
email_dtm <- email_df %>%
  unite(term_year, borrow_type, year, sep = "_") %>%
  cast_dtm(term_year, word, n)

cloud_dtm <- cloud_df %>%
  unite(term_year, borrow_type, year, sep = "_") %>%
  cast_dtm(term_year, word, n)

trailer_dtm <- trailer_df %>%
  unite(term_year, borrow_type, year, sep = "_") %>%
  cast_dtm(term_year, word, n)

hashtag_dtm <- hashtag_df %>%
  unite(term_year, borrow_type, year, sep = "_") %>%
  cast_dtm(term_year, word, n)

hashtagQ_dtm <- hashtagQ_df %>%
  unite(term_year, borrow_type, year, sep = "_") %>%
  cast_dtm(term_year, word, n)

#Implement Latent Dirichlet Allocation on each variable
email_lda <- LDA(email_dtm, k = 2, control = list(seed = 1234))
cloud_lda <- LDA(cloud_dtm, k = 2, control = list(seed = 1234))
trailer_lda <- LDA(trailer_dtm, k = 2, control = list(seed = 1234))
hashtag_lda <- LDA(hashtag_dtm, k = 2, control = list(seed = 1234))
hashtagQ_lda <- LDA(hashtagQ_dtm, k = 2, control = list(seed = 1234))

#Extract most frequent topics for each LDA, using the beta probabilities
hashtag_topics <- tidy(hashtag_lda, matrix = "beta") %>%
  mutate(topic2 = ifelse(topic == 2, "mot-dièse", "hashtag"),
         topic2 = relevel(as.factor(topic2), ref = "mot-dièse"))
hashtagQ_topics <- tidy(hashtagQ_lda, matrix = "beta") %>%
  mutate(topic2 = ifelse(topic == 1, "mot-clic", "hashtag"),
         topic2 = relevel(as.factor(topic2), ref = "mot-clic"))
email_topics <- tidy(email_lda, matrix = "beta") %>%
  mutate(topic2 = ifelse(topic == 1, "courriel", "email"),
         topic2 = relevel(as.factor(topic2), ref = "courriel"))
trailer_topics <- tidy(trailer_lda, matrix = "beta") %>%
  mutate(topic2 = ifelse(topic == 1, "bande-annonce", "trailer"),
         topic2 = relevel(as.factor(topic2), ref = "bande-annonce"))
cloud_topics <- tidy(cloud_lda, matrix = "beta") %>%
  mutate(topic2 = ifelse(topic == 1, "nuage", "cloud"),
         topic2 = relevel(as.factor(topic2), ref = "nuage"))

#Implement topic models to classify documents into prescribed or English
hashtag_documents <- tidy(hashtag_lda, matrix = "gamma") %>%
  separate(document, c("type", "year"), sep = "_", convert = TRUE) %>%
  mutate(type = str_replace_all(type, "prescribed", "mot-dièse"),
         type = str_replace_all(type, "english", "hashtag"),
         topic2 = ifelse(topic == 2, "prescribed", "english"),
         type = relevel(as.factor(type), ref = "mot-dièse"),
         topic2 = relevel(as.factor(topic2), ref = "prescribed"))

hashtagQ_documents <- tidy(hashtagQ_lda, matrix = "gamma") %>%
  separate(document, c("type", "year"), sep = "_", convert = TRUE) %>%
  mutate(type = str_replace_all(type, "motclic", "mot-clic"),
         type = str_replace_all(type, "english", "hashtag"),
         topic2 = ifelse(topic == 1, "prescribed", "english"),
         type = relevel(as.factor(type), ref = "mot-clic"),
         topic2 = relevel(as.factor(topic2), ref = "prescribed"))

email_documents <- tidy(email_lda, matrix = "gamma") %>%
  separate(document, c("type", "year"), sep = "_", convert = TRUE) %>%
  mutate(type = str_replace_all(type, "prescribed", "courriel"),
         type = str_replace_all(type, "english", "email"),
         topic2 = ifelse(topic == 1, "prescribed", "english"),
         type = relevel(as.factor(type), ref = "courriel"),
         topic2 = relevel(as.factor(topic2), ref = "prescribed"))

cloud_documents <- tidy(cloud_lda, matrix = "gamma") %>%
  separate(document, c("type", "year"), sep = "_", convert = TRUE) %>%
  mutate(type = str_replace_all(type, "prescribed", "nuage"),
         type = str_replace_all(type, "english", "cloud"),
         topic2 = ifelse(topic == 1, "prescribed", "english"),
         type = relevel(as.factor(type), ref = "nuage"),
         topic2 = relevel(as.factor(topic2), ref = "prescribed"))

trailer_documents <- tidy(trailer_lda, matrix = "gamma") %>%
  separate(document, c("type", "year"), sep = "_", convert = TRUE) %>%
  mutate(type = str_replace_all(type, "prescribed", "bande-annonce"),
         type = str_replace_all(type, "english", "trailer"),
         topic2 = ifelse(topic == 1, "prescribed", "english"),
         type = relevel(as.factor(type), ref = "bande-annonce"),
         topic2 = relevel(as.factor(topic2), ref = "prescribed"))