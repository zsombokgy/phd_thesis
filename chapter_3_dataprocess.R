#This file contains codes for data processing in Chapter 3

#Read in libraries
library(readxl)
library(tidyverse)
library(stringr)
library(stringi)
library(lubridate)

#Read in every text file from the folder
data_path <- "PATH-TO-TEXT-FILES"
textfiles <- dir(path = data_path, pattern = "txt$", full.names = FALSE)

#Attach file name as variable to the file
data <- data_frame(filename = textfiles) %>%
  mutate(article = map(filename, ~read_file(file.path(data_path, .), locale = locale(encoding = "windows-1252"))))

#Separate text files into observations in a data frame
#Extract variables 'lexical' and 'type' from the file name, 'source' and 'date' from the text
text <- data %>%
  separate_rows(article, sep = "End of Document") %>%
  separate(filename, c("lexical", "type"), sep = "-") %>%
  mutate(type = str_replace(type, "(^\\D*)(\\d*)(.txt$)", "\\1"),
         article = str_replace(article, "^\\s*", ""),
         article = str_replace_all(article, "\r\n+", "\n"),
         article = str_replace_all(article, "\n+", "\n"),
         article = str_replace_all(article, "\\s*\n", "\n"),
         article = str_replace(article, "^(.*\n)(Les Echos|Le Figaro|Le Monde|01net|Le Parisien|La Croix|Journal du Net, JDN Solutions)(\n.*\n)", "\\2\\3\\1"),
         article = str_replace(article, "^(.*)\n(.*)\n", "\\1SEPARATE\\2SEPARATE")) %>%
  filter(grepl("^(Les Echos|Le Figaro|Le Monde|01net|Le Parisien|La Croix|Journal du Net, JDN Solutions)", article)) %>%
  separate(article, c("source","date","text"), sep = "SEPARATE") %>%
  mutate(source = str_replace(source, "Journal du Net, JDN Solutions", "Journal du Net"),
         source = as.factor(source),
         lexical = as.factor(lexical),
         type = as.factor(type))

#Harmonize date formats as YYYY-MM-DD
articles <- text %>%
  mutate(date = str_replace_all(date, "[Ll]undi|[Mm]ardi|[Mm]ercredi|[Jj]eudi|[Vv]endredi|[Ss]amedi|[Dd]imanche", ""),
         date = str_replace_all(date, "^\\s|\\s$|[:punct:]", ""),
         date = str_replace_all(date, "^(.*)\\s(\\d{4})", "\\2 \\1"),
         date = str_replace_all(date, "(\\d{4}\\s)(\\d{1,2}\\s)(.*)$", "\\1 \\3 \\2"),
         date = str_replace_all(date, "(\\d{4})\\s*([:alpha:]*)\\s*(\\d{1,2})\\D*$", "\\1-\\2-\\3"),
         date = str_replace_all(date, "-(\\d{1}$)", "-0\\1"),
         date = str_replace_all(date, c("[Jj]anvier"="January",
                                        "[Ff][eé]vrier"="February",
                                        "[Mm]ars"="March",
                                        "[Aa]vril"="April",
                                        "[Mm]ai"="May",
                                        "[Jj]uin"="June",
                                        "[Jj]uillet"="July",
                                        "[Aa]o[uû]t"="August",
                                        "[Ss]eptembre"="September",
                                        "[Oo]ctobre"="October",
                                        "[Nn]ovembre"="November",
                                        "[Dd][eé]cembre"="December")),
         date = ymd(date)) %>%
  filter(!is.na(date))

#Clean text from metadata created on Lexis Nexis
#Replace elision, and transform text to 'Latin-ASCII'
#Extract first 6 words in grouped data and delete duplicates
articles1 <- articles %>%
  mutate(text = str_replace_all(text, "&amp;", "&"),
         text = str_replace_all(text, regex("(;.*)?\n[Cc]opyright.*\nBody\n", dotall = TRUE), "\n"),
         text = str_replace_all(text, regex("\n[Cc]lassification.*$", dotall = TRUE), ""),
         text = str_replace_all(text, regex("\n[Ll]oad-[Dd]ate.*$", dotall = TRUE), ""),
         text = str_replace_all(text, "\u0027|\u2019|\u2018|\u02bc|\u02b9|\uff07", "e "),
         text = str_replace_all(text, "[:punct:]", ""),
         text = str_replace_all(text, "\\s+", " "),
         text = str_to_lower(text),
         text = stri_trans_general(text, "Latin-ASCII"),
         text = str_replace_all(text, "sde", "s de")) %>%
  group_by(lexical, type, source, date) %>%
  distinct() %>%
  arrange(date) %>%
  mutate(start = word(text, 1,6)) %>%
  distinct(start, .keep_all = TRUE) %>%
  select(lexical, type, source, date, text)

#Read in data on prescription
presctime <- read_excel("PRESCRIPTION DATA")
presctime <- presctime %>%
  mutate(lexical = as.factor(lexical),
         prescription = ymd(prescription),
         fr_name = as.factor(fr_name))

#Join prescription data with the articles data frame
#Create 'time' variable
newspaper <- articles1 %>%
  full_join(presctime, by = "lexical") %>%
  mutate(time = date - prescription,
         time = as.numeric(as.character(time)),
         time = round(time / 7)) %>%
  filter(time >= -156, time <= 156) %>%
  mutate(time = factor(time, levels = c(-156:156)),
         time = as.numeric(as.character(time))) %>%
  select(lexical, fr_name, type, source, date, time, prescription, text)

#Save newspaper corpus for the analysis
save(newspaper, file = "newspaper.Rda")