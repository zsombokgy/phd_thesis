#This file contains codes for data analysis in Chapter 3

#Read in libraries
library(mgcv)
library(visreg)

#Load in newspaper corpus created in 'chapter_3_dataprocess.R'
#Recode 'type' for 'English' vs. 'prescribed'
np <- load("newspaper.Rda") %>%
  ungroup() %>%
  mutate(type = recode(type,
                       "english" = "english",
                       "alternative" = "english",
                       "prescribed" = "prescribed")) %>%
  select(lexical, fr_name, type, source, time)

#Initialize Generalized Additive Model
#!!!WARNING!!! It takes 8-10 hours to complete
np_gam <- gam(type ~ s(time, k = 5, by = lexical) +
                lexical + source,
                 family = "binomial", method = "REML",
              data = np)

#Diagnose the fit of the model
summary(np_gam)
gam.check(np_gam)
concurvity(np_gam)

#Create summary tables for writing
#Parametric coefficients
summary_np_gam_parametric <- tidy(np_gam, parametric = TRUE) %>%
  mutate(estimate = round(estimate, digits = 2),
         std.error = round(std.error, digits = 2),
         statistic = round(statistic, digits = 2),
         p.value = round(p.value, digits = 3),
         significance = ifelse(p.value > 0.1, "",
                               ifelse(p.value > 0.05, ".",
                                      ifelse(p.value > 0.01, "*",
                                             ifelse(p.value > 0.001, "**", "***")))),
         p.value = round(p.value, digits = 2))

#Smooth terms
summary_np_gam_smooth <- tidy(np_gam, parametric = FALSE) %>%
  mutate(edf = round(edf, digits = 2),
         ref.df = round(ref.df, digits = 2),
         statistic = round(statistic, digits = 2),
         p.value = round(p.value, digits = 3),
         significance = ifelse(p.value > 0.1, "",
                               ifelse(p.value > 0.05, ".",
                                      ifelse(p.value > 0.01, "*",
                                             ifelse(p.value > 0.001, "**", "***")))),
         p.value = round(p.value, digits = 2))

#Visualize the model
#Lexical items over time
np_lexical <- visreg(np_gam, "time", by = "lexical", scale = "response",
                     overlay = TRUE, bands = TRUE, partial = FALSE, legend = FALSE,
                     ylim = c(0,1), xlim = c(-156,156))

#Newspaper sources over time
np_source <- visreg(np_gam, "time", by = "source", scale = "response",
                    overlay = TRUE, bands = TRUE, partial = FALSE, legend = FALSE,
                    ylim = c(0,1), xlim = c(-156,156))

#Create data frame about the effect of prescription
np_effect <- np_lexical$fit %>%
  select(time, lexical, visregFit) %>%
  group_by(lexical) %>%
  filter(time == -156| time == 156) %>%
  mutate(time = recode(time, "-156" = "before", "156" = "after")) %>%
  spread(time, visregFit) %>%
  select(lexical, before, after) %>%
  mutate(diffAftBef = after - before,
         initial = ifelse(before > 0.75, "very high",
                          ifelse(before > 0.5, "high",
                                 ifelse(before > 0.25, "medium",
                                        ifelse(before > 0.05, "low", "very low")))),
         effect = ifelse(diffAftBef > 0.5, "strong increase",
                         ifelse(diffAftBef > 0.25, "medium increase",
                                ifelse(diffAftBef > 0.01, "weak increase",
                                       ifelse(diffAftBef > -0.01, "no change",
                                              ifelse(diffAftBef > -0.25, "weak decrease",
                                                     ifelse(diffAftBef > -0.5, "medium decrease",
                                                            "strong decrease")))))),
         initial = as.factor(ordered(initial, levels = c("very low","low","medium","high","very high"))),
         effect = as.factor(ordered(effect, levels = c("strong decrease", "medium decrease", "weak decrease", "no change", "weak increase", "medium increase", "strong increase")))) %>%
  full_join(presctime, "lexical")