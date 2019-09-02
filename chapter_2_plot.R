#This file contains codes for plotting in Chapter 2

#Read in libraries
library(tidyverse)
library(ggplot2)

#Create data frame from the number of published terms
celf_terms <-  c("2007" = 317,"2008" = 268, "2009" = 276, "2010" = 247, "2011" = 392,
                 "2012" = 299, "2013" = 343, "2014" = 243, "2015" = 268, "2016" = 221,
                 "2017" = 231)
celf_terms <- as.data.frame(celf_terms) %>%
  rownames_to_column("year") %>%
  rename("count"="celf_terms") %>%
  mutate(year = as.ordered(year))

#Visualize published terms
gg_celf_terms <- ggplot(celf_terms, aes(x = year, y = count)) + geom_col(fill = "dodgerblue2", width = 0.7) +
  labs(x = "Year", y = "Count") +
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

#Save plot
ggsave(celf_terms, filename = "Figure-prescriptioncounts.png",
       width = 4.5, height = 3,  units = "in",
       family = "Times")