#This file contains codes for data processing in Chapter 4

#Read in libraries
library(readxl)
library(tidyr)
library(dplyr)
library(lubridate)

#Import Twitter data
twitter_import <- read_excel("INSERT-DATA")

#Create 'cities' data frame
cities <- twitter_import %>% select(city, region) %>%
  mutate(city = as.factor(city),
         region = as.factor(region)) %>%
  group_by(region, city) %>%
  count() %>%
  filter(!is.na(city), !is.na(region)) %>%
  unite("city", c("city","region"), sep = ", ") %>%
  mutate(city = paste(city, "France", sep = ", "),
         city = as.factor(city)) %>%
  filter(city != "Paris, Haute-Normandie, France", city != "Paris, Picardie, #France", city != "Rouen, Centre, France") %>%
  mutate(city = as.character(city)) %>%
  mutate_geocode(city) %>%
  separate(city, c("city","region","country"), sep = ", ") %>%
  mutate( city = relevel(as.factor(city), ref = "Paris")) %>%
  select(city, region, country, lon, lat)

#Read in population data
population <- read_excel("INSERT-DATA") %>%
  select(city, pop_2010) %>%
  mutate(city = relevel(as.factor(city), ref = "Paris"))

#Merge cities data frame with population data
cities <- cities %>%
  full_join(population, by = "city") %>%
  rename(population = pop_2010)
save(cities, file = "cities.Rda")

#Create Twitter data frame
twitter_df <- twitter_import %>%
  select(lexical, borrow_type, date, content, country, region, city, gender, posts, followers, following) %>%
  mutate(borrow_type = as.factor(borrow_type),
         lexical = as.factor(lexical),
         date = ymd_hms(date),
         country = as.factor(country),
         region = as.factor(region),
         city = relevel(as.factor(city), ref = "Paris"),
         gender = relevel(as.factor(gender), ref = "M"))

#Join cities with Twitter data frame         
cities2 <- cities %>% select(city, lon, lat, population)
twitter_df <- twitter_df %>%
  full_join(cities2, by = "city")

#Create 'citysize' variable
twitter_df <- twitter_df %>%
  mutate(borrow_type = recode(borrow_type,
                              prescribed = "prescribed",
                              alternative = "english",
                              english = "english"),
         citysize = ifelse(population > 2000000, 8,
                           ifelse(population > 200000, 7,
                                  ifelse(population > 100000, 6,
                                         ifelse(population > 50000, 5,
                                                ifelse(population > 20000, 4,
                                                       ifelse(population > 10000, 3,
                                                              ifelse(population > 5000, 2,
                                                                     ifelse(population > 2000, 1, 0)))))))),
         citysize = as.factor(ordered(citysize)))
save(twitter_df, file = "twitter_df.Rda")

#Import Quebec data
hashtag_quebec_import <- read_excel("INSERT-DATA")

#Create 'quebec_cities' data frame
quebec_cities <- hashtag_quebec_import %>% select(city, region) %>%
  filter(!city == "Pembroke") %>%
  mutate(city = as.factor(city),
         region = as.factor(region)) %>%
  group_by(region, city) %>%
  count() %>%
  filter(!is.na(city), !is.na(region)) %>%
  unite("city", c("city","region"), sep = ", ") %>%
  mutate(city = paste(city, "Canada", sep = ", "),
         city = as.factor(city),
         city = as.character(city)) %>%
  mutate_geocode(city) %>%
  separate(city, c("city","region","country"), sep = ", ") %>%
  mutate(city = relevel(as.factor(city), ref = "Montréal")) %>%
  select(city, region, country, lon, lat)

#Import Quebec population data
population_quebec <- read_excel("/Users/gyuszi/Dropbox/Research/Twitter/02 PhD Research/quebec_population.xlsx") %>%
  select(city, population, citysize) %>%
  mutate(city = relevel(as.factor(city), ref = "Montréal"),
         citysize = factor(ordered(citysize, levels = c('small','medium','large'))))

#Join Quebec cities and population data
quebec_cities <- quebec_cities %>%
  full_join(population_quebec, by = "city")
save(quebec_cities, file = "quebec_cities.Rda")

#Create 'hashtag_quebec' data frame
hashtag_quebec <- hashtag_quebec_import %>%
  filter(!city == "Pembroke") %>%
  select(lexical, borrow_type, date, content, city, sentiment, gender, posts, followers, following) %>%
  mutate(lexical = as.factor(lexical),
         borrow_type = as.factor(borrow_type),
         city = as.factor(city),
         sentiment = as.factor(ordered(sentiment,
                                       levels=c('Basic Negative',
                                                'Basic Neutral',
                                                'Basic Positive'))),
         sentiment = recode(sentiment, 'Basic Negative' = "negative",
                            'Basic Neutral' = "neutral",
                            'Basic Positive' = "positive"),
         gender = relevel(as.factor(gender), ref = "M")) %>%
  full_join(quebec_cities, by = "city") %>%
  mutate(country = as.factor(country),
         region = as.factor(region),
         city = relevel(as.factor(city), ref = "Montréal"))
save(hashtag_quebec, file = "hashtag_quebec.Rda")

#Extract journal accounts and create summary table
journal_accounts <- c("@lemonde_inter","@lemonde_pol","@lemondefr","@lemondefr_live","@Madamefigaro","@Le_Figaro","@FigaroTech","@Figaro_Etudiant","@Figaro_Culture","@LaCroix","@LeParisienTV","@LeParisienMonde","@LeParisien_95","@LeParisien_92","@LeParisien_91","@LeParisien_75","@LeParisien_60","@le_Parisien_fr","@le_Parisien","@LesEchosLive","@LesEchos","@EchosStart","@EchosMonde","@EchosFinance","@echosdoc","@ECHOS_ETUDES","@CercleLesEchos","@JDNebusiness","@journaldunet","@01netTV","@01net")

journal_twitter <- twitter_df %>%
  filter(author %in% journal_accounts) %>%
  select(lexical, borrow_type, author, content) %>%
  mutate(journal = ifelse(author %in% c("@lemonde_inter","@lemonde_pol","@lemondefr","@lemondefr_live"), "Le Monde",
                          ifelse(author %in% c("@Madamefigaro","@Le_Figaro","@FigaroTech","@Figaro_Etudiant","@Figaro_Culture"), "Le Figaro",
                                 ifelse(author %in% c("@LaCroix"), "La Croix",
                                        ifelse(author %in% c("@LeParisienTV","@LeParisienMonde","@LeParisien_95","@LeParisien_92","@LeParisien_91","@LeParisien_75","@LeParisien_60","@le_Parisien_fr","@le_Parisien"), "Le Parisien",
                                               ifelse(author %in% c("@LesEchosLive","@LesEchos","@EchosStart","@EchosMonde","@EchosFinance","@echosdoc","@ECHOS_ETUDES","@CercleLesEchos"), "Les Echos",
                                                      ifelse(author %in% c("@JDNebusiness","@journaldunet"), "Journal du Net", "01Net")))))),
         lexical = recode(lexical, "cloud" = "nuage",
                          "email" = "courriel",
                          "trailer" = "bande-annonce",
                          "hashtag" = "mot-dièse"),
         borrow_type = recode(borrow_type, "english" = "Anglicism")) %>%
  group_by(journal, borrow_type) %>%
  summarize(n = n()) %>%
  spread(borrow_type, n)



