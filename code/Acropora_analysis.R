# Laamu Coral Reef monitoring project
# Code for coral growth form analysis

# Written by Mara Fischer
# E-mail: mf555@exeter.ac.uk
# 20/10/2022

# Packages ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(forcats)

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

perc_all22 <- read.csv("data/perc_all_final.csv", header = T) # full 2022 raw data (28 sites)
all22sites <- read.csv("data/all22sites.csv", header = T)  # full data, aggr. by depth

# Data exploration with raw data ----

length(unique(perc_all22$Site.name))  # 28 sites

# Mean Acropora cover: 
mean(perc_all22$Acropora)  # 4.889018

# Mean other coral cover:
mean(perc_all22$Coral)  # 22.60464

# Mean total hard coral cover:
mean(perc_all22$Acropora) + mean(perc_all22$Coral) # 27.49366

# Percentage of acropora out of total:
4.889018/27.49366 * 100  # 17.78235 %

# Percentage of non-acropora out of total:
22.60464/27.49366 * 100  # 82.21765 %

# Add column for total cover
perc_all22 <- perc_all22 %>% 
  mutate(Total.coral = Acropora + Coral)

# Calculate mean, sd and SE for both
mean_cover <- perc_all22 %>% 
  summarise(Acropora.mean = mean(Acropora), 
            Acropora.sd = sd(Acropora),
            n = n(),
            Acropora.SE = sd(Acropora)/sqrt(n()),
            Coral.mean = mean(Coral), 
            Coral.sd = sd(Coral),
            Coral.SE = sd(Coral)/sqrt(n()),
            Total.mean = mean(Total.coral),
            Total.sd = sd(Total.coral),
            Total.SE = sd(Total.coral)/sqrt(n)) %>% 
  ungroup()

# Data exploration with data aggr. by depth ----

length(unique(all22sites$Site.name))  # 28 sites

# Mean Acropora cover: 
mean(all22sites$Acropora)  # 4.889018

# Mean other coral cover:
mean(all22sites$Other.Coral)  # 22.60464

# Mean total hard coral cover:
mean(all22sites$Total.hard.coral)  # 27.49366

# Percentage of acropora out of total:
4.889018/27.49366 * 100  # 17.78235 %

# Percentage of non-acropora out of total:
22.60464/27.49366 * 100  # 82.21765 %

### All the same averages, so will use full raw dataset for now ###

perc_all22$Depth <- factor(perc_all22$Depth, levels = c("5m", "10m", "15m"),
                           labels = c("5m", "10m", "15m"))

# Visualisation ----

# Stacked barplot

# Remove unneeded columns:
perc_all22 <- perc_all22 %>% 
  select(- Tape, - Wand, - Shadow, - Sum.excl.TWS)

perc_long <- perc_all22 %>% 
  pivot_longer(cols = 7:25, names_to = "Cover", values_to = "Percent")

# With all categories
(stack <- ggplot(perc_long, aes(x = Depth, y = Percent, fill = Cover)) +
    geom_bar(position = "fill", stat = "identity", width = 0.4) +
    scale_y_continuous(labels = scales::percent,
                       expand = c(0,0)) + 
    #labs(y = "Benthic cover\n", x = "\nYear") +
    theme_diss() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          #aspect.ratio = 1.5,
          panel.border = element_rect(fill = NA),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
)

# Filter out Acropora and other coral
coral_long <- perc_long %>% 
  filter(Cover == "Acropora" | Cover == "Coral")

# Add column to use as x-axis to show Laamu-wide average
coral_long <- coral_long %>% 
  mutate(X = "Laamu")

# Stacked barplot with only coral
(coral_stack <- ggplot(coral_long, aes(x = X, y = Percent, fill = Cover)) +
    geom_bar(position = "fill", stat = "identity", width = 0.5) +
    scale_y_continuous(labels = scales::percent,
                       expand = c(0,0)) +
    scale_colour_manual(values = c("#fcd0a1", "#b1b695")) +
    scale_fill_manual(values = c("#fcd0a1", "#b1b695")) +
    labs(y = "Percent cover") +
    theme_diss() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_rect(fill = NA),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
)

save_plot(coral_stack, filename = "outputs/coral_stack.png")

ggsave(coral_stack, filename = "outputs/coral_stack.png", height = 6, width = 7)

# by depth
(coral_stack_dep <- ggplot(coral_long, aes(x = Depth, y = Percent, fill = Cover)) +
    geom_bar(position = "fill", stat = "identity", width = 0.4) +
    scale_y_continuous(labels = scales::percent,
                       expand = c(0,0)) + 
    scale_colour_manual(values = c("#fcd0a1", "#b1b695")) +
    scale_fill_manual(values = c("#fcd0a1", "#b1b695")) +
    labs(y = "Percent cover") +
    theme_diss() +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank(),
          #axis.text.x = element_blank(),
          panel.border = element_rect(fill = NA),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
)
## very similar across depths

ggsave(coral_stack_dep, filename = "outputs/coral_stack_dep.png", height = 6, width = 7)


# by reef type
(coral_stack_rt <- ggplot(coral_long, aes(x = Reef.type, y = Percent, fill = Cover)) +
    geom_bar(position = "fill", stat = "identity", width = 0.4) +
    scale_y_continuous(labels = scales::percent,
                       expand = c(0,0)) + 
    scale_colour_manual(values = c("#fcd0a1", "#b1b695")) +
    scale_fill_manual(values = c("#fcd0a1", "#b1b695")) +
    labs(y = "Percent cover") +
    coord_flip() +
    theme_diss() +
    theme(legend.title = element_blank(),
          axis.title.y = element_blank(),
          #axis.text.x = element_blank(),
          panel.border = element_rect(fill = NA),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
)
# quite marked diffs, lowest in channel, and highest in thila

# Plot with coord flipepd
(coral_stack_rt2 <- ggplot(coral_long, aes(x = Reef.type, y = Percent, fill = Cover)) +
    geom_bar(position = "fill", stat = "identity", width = 0.4) +
    scale_y_continuous(labels = scales::percent,
                       expand = c(0,0)) + 
    scale_colour_manual(values = c("#fcd0a1", "#b1b695")) +
    scale_fill_manual(values = c("#fcd0a1", "#b1b695")) +
    labs(y = "Percent cover") +
    coord_flip() +
    theme_diss() +
    theme(legend.title = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          #axis.text.x = element_blank(),
          panel.border = element_rect(fill = NA),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
)

# by site
(coral_stack_site <- ggplot(coral_long, aes(x = reorder(Site.name,(desc(Percent))), y = Percent, fill = Cover)) +
    geom_bar(position = "fill", stat = "identity", width = 0.4) +
    scale_y_continuous(labels = scales::percent,
                       expand = c(0,0)) + 
    labs(y = "Percent cover\n") +
    theme_diss() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(fill = NA),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
)

# Arrange the data to have it ordered by cover
coral_long <- coral_long %>% 
  arrange(Cover != 'Acropora', Percent) %>%
  mutate(Site = factor(Site.name, unique(Site.name)),
         Cover = factor(Cover, unique(Cover)))
  
(coral_stack_site <- ggplot(coral_long, aes(x = Site, y = Percent, fill = Cover)) +
     geom_bar(position = "fill", stat = "identity", width = 0.4) +
     scale_y_continuous(labels = scales::percent,
                        expand = c(0,0)) + 
     labs(y = "Percent cover\n") +
     theme_diss() +
     theme(legend.position = "right",
           legend.title = element_blank(),
           axis.title.x = element_blank(),
           axis.text.x = element_text(angle = 45, hjust = 1),
           panel.border = element_rect(fill = NA),
           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
)

coral_long %>%
  mutate(Site = fct_reorder(Site, Percent)) %>%
  ggplot(aes(x = Site, y = Percent, fill = Cover)) +
  geom_bar(position = "fill", stat = "identity", width = 0.4) +
  scale_y_continuous(labels = scales::percent,
                     expand = c(0,0)) +
  scale_colour_manual(values = c("#fcd0a1", "#b1b695")) +
  scale_fill_manual(values = c("#fcd0a1", "#b1b695")) +
  labs(y = "Percent cover") +
  theme_diss() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(fill = NA),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

coral_long %>%
  mutate(Site = fct_reorder(Site, Percent)) %>%
  ggplot(aes(x = Site, y = Percent, fill = Cover)) +
  geom_bar(position = "fill", stat = "identity", width = 0.4) +
  scale_y_continuous(labels = scales::percent,
                     expand = c(0,0)) +
  scale_colour_manual(values = c("#fcd0a1", "#b1b695")) +
  scale_fill_manual(values = c("#fcd0a1", "#b1b695")) +
  labs(y = "Percent cover") +
  coord_flip() +
  theme_diss() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(fill = NA),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))


# Pie chart of Laamu wide average

# Make subset with averaged proportions
coral_prop <- coral_long %>% 
  group_by(Cover) %>% 
  summarise(Prop = mean(Percent),
            Perc = Prop/27.49366) %>% 
  ungroup()

# With proportion
pie(coral_prop$Prop, labels = c("Acropora", "Other coral"))

# With percentage out of total
pie(coral_prop$Perc, labels = c("Acropora (17.8%)", "Other coral (82.2%)"), 
    col = c("#fcd0a1", "#b1b695"), border = "white")

