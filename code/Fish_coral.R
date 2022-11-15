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

## 2022 all raw fish data ----

raw_fish22 <- read.csv("data/raw_fish_2022.csv", header = T)

# Check data
unique(raw_fish22$Site) # all 29 sites
unique(raw_fish22$Reef.type) # the 4 reef types
unique(raw_fish22$Depth)  # the 3 depths
unique(raw_fish22$Replicate.No.) # 2 reps
unique(raw_fish22$Family)  # 9 families
unique(raw_fish22$Species) # 106 species

# Aggregate to get abundance and biomass in same data frame
agg_fish22 <- raw_fish22 %>% 
  group_by(Site, Depth, Replicate.No., Family, Species, Biomass) %>% 
  summarise(abundance = length(Species),
            Site.name = unique(Site.name)) %>% 
  ungroup()
## now have the abundance of a species of a given biomass at each rep

# Create dataset with fish abundance without biomass
abun_fish22 <- raw_fish22 %>% 
  group_by(Site, Depth, Replicate.No., Family, Species) %>% 
  summarise(abundance = length(Species),
            Site.name = unique(Site.name),
            reef.type = unique(Reef.type)) %>% 
  ungroup()

write.csv(abun_fish22, "data/abun_rep_sp_2022.csv")

# Create one ignoring depth and rep
abun_site22 <- raw_fish22 %>% 
  group_by(Site, Family, Species) %>% 
  summarise(count = length(Species),
            Site.name = unique(Site.name),
            reef.type = unique(Reef.type),
            year = "2021/22") %>% 
  ungroup()

write.csv(abun_site22, "data/abun_site_sp_2022.csv")

# Dataset for overall abundance & biomass (ignoring species)
abun_site_agg22 <- raw_fish22 %>% 
  group_by(Site, Depth, Replicate.No.) %>% 
  summarise(abundance = length(Species),
            Site.name = unique(Site.name),
            reef.type = unique(Reef.type),
            year = "2021/22") %>% 
  ungroup()

write.csv(abun_site_agg22, "data/abun_site_2022.csv")

## 2019 abundance data ----

abun19 <- read.csv("data/abun_site_species_2019.csv", header = T)

# full abundance data for 2019 but aggregated by site and species,
# no distinction between depths or replicates

unique(abun19$site_id) # 20 repeat sites
unique(abun19$reef_type) # 4
unique(abun19$family) # 7, but including SL
unique(abun19$species) # 96 spp.

## Combine 2019 & 2022 abundance data ----

# Rename columns so they are the same
abun_site22 <- abun_site22 %>% 
  rename(Site.id = Site)

abun19 <- abun19 %>% 
  rename(Site.id = site_id, Site.name = site, reef.type = reef_type,
         Family = family, Species = species)

# Select only the 20 repeat sites from 2022 data
abun_site22_repeat <- abun_site22 %>% 
  filter(Site.id %in% c("OR", "OI", "FI", "LF", "FK", "HC", "MBO", "MBI", "GI", "PT",
                      "FO", "HW", "KO", "MC", "GO", "MI", "MO", "OC", "RDH", "HI")) 
  
# Change into factors
abun_site22_repeat <- abun_site22_repeat %>% 
  mutate(across(c(Site.id, Site.name, reef.type, year), as.factor))

abun19 <- abun19 %>% 
  mutate(across(c(Site.id, Site.name, reef.type, year), as.factor))

# Combine both years
abun_19_22 <- bind_rows(abun19, abun_site22_repeat)

unique(abun_19_22$Family)

# Select the six families that were surveyed in both years (and remove SL)
abun_19_22 <- abun_19_22 %>% 
  filter(Family %in% c("BUT", "EMP", "GRO", "PAR", "SNAP", "TRIG"))

# Add column for commercially vs ecologically important
abun_19_22 <- abun_19_22 %>% 
  mutate(Status = case_when(Family == "BUT" | Family == "PAR" | Family == "TRIG" ~ "ECO",
                            Family == "EMP" | Family == "GRO" | Family == "SNAP" ~ "COM"))

# Save this as final abundance dataset!

write.csv(abun_19_22, "data/FISH_ABUN_19_22.csv")

# Combine coral and fish data ----

# Import coral data aggregated by site

coral <- read.csv("data/perc_site.csv", header = T)

coral <- coral %>% 
  rename(Site.id = Site.ID, Site.name = Site, reef.type = Reef.type)

# Join to fish abun

abun_coral <- dplyr::left_join(abun_19_22, coral, by = "Site.id")
## this doesn't really work

# Aggregate total abundance at site level (all species combined)

abun_19_22_site <- abun_19_22 %>% 
  group_by(Site.id, year) %>% 
  summarise(Site.name = unique(Site.name), reef.type = unique(reef.type),
            abundance = sum(count)) %>% 
  ungroup()

# Double check this:
test <- abun_19_22 %>% 
  filter(Site.id == "FI")

sum(test$count)
## adds up yes

# Remove pink thila as no coral data for that
abun_19_22_site <- abun_19_22_site %>% 
  filter(Site.id != "PT")

# Simplify coral dataset
coral2 <- coral %>% 
  select(Site.id, year, Hard.coral)

# Add coral data to this instead

abun_coral_site <- left_join(abun_19_22_site, coral2, by = c("Site.id", "year"))

## Dataset with two rows per site, one for each year

write.csv(abun_coral_site, "data/abun_coral_site.csv")

# ANALYSIS ----

## Coral vs abun (all) ----

abun_coral_site <- read.csv("data/abun_coral_site.csv", header = T)

plot(abun_coral_site$Hard.coral ~ abun_coral_site$abundance)

# one big outlier for abundance (maavah outside)

# Remove outlier and plot again
abun_coral_site2 <- abun_coral_site %>% 
  filter(Site.id != "MO")

plot(abun_coral_site2$Hard.coral ~ abun_coral_site2$abundance) # weak pos trend

# Simple LM

lm1 <- lm(Hard.coral ~ abundance, data = abun_coral_site2)
summary(lm1)  # abundance has a significant weak positive effect on coral cover
plot(lm1)

lm2 <- lm(Hard.coral ~ abundance + year + abundance*year, data = abun_coral_site2)
summary(lm2)
