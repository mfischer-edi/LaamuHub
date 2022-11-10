# Laamu Coral Reef monitoring project
# Code for fish vs coral analysis

# Written by Mara Fischer
# E-mail: mf555@exeter.ac.uk
# 10/11/2022

# Packages ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(forcats)
library(lme4)
library(lmerTest)
library(DHARMa)

# Create theme for plots
theme_diss <- function(){            
  theme_bw()+                          
    theme(text = element_text(family = "sans"),
          axis.text = element_text(size = 16), 
          axis.title = element_text(size = 20),
          axis.line.x = element_line(color="black"), 
          axis.line.y = element_line(color="black"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16, face = "bold"),
          legend.position = "bottom",
          legend.key = element_blank(),
          legend.background = element_rect(color = "black", 
                                           fill = "transparent",
                                           size = 2, linetype = "blank"))
}

# Load data ----

abun19 <- read.csv("data/2019_fish_abun.csv", header = T) # raw fish abundance

unique(abun19$family)

# Data wrangling ----

## Olhutholhu inside ----

# Get abundance data for Olhutholhu inside
OI <- read.csv("data/Olhutholhu_2022.csv", header = T)

# Create dataset with fish abundance
OI_abun <- OI %>% 
  group_by(Site, Depth, Replicate.No., Family, Species) %>% 
  summarise(count = length(Species))

write.csv(OI_abun, "data/OI_abun_raw.csv")

# Create one ignoring depth and rep
OI_abun_site <- OI %>% 
  group_by(Site, Family, Species) %>% 
  summarise(count = length(Species))

write.csv(OI_abun_site, "data/OI_abun_site.csv")

## 2022 data ----

fish22 <- read.csv("data/biomass_22.csv", header = T)

# Aggregate to get abundance and biomass in same data frame
fish_all22 <- fish22 %>% 
  group_by(site, depth, replicate_no, family, species, biomass) %>% 
  summarise(abundance = length(species)) %>% 
  ungroup()

## one row per species of a certain biomass,
## so if within the same replicate, the same species was observed
## at different sizes and hence has different biomasses, there are
## multiple rows for that certain species

sum(fish_all22$abundance)
sum(fish_all22$biomass)
