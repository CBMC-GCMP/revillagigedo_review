# Load libraries --------------------------------------------------------

library(tidyverse)   # Library that includes various data manipulation libraries.
library(readxl)      # Library for reading Excel files.
library(dplyr)       # Library for data manipulation.
library(lubridate)   # Library for working with dates.
library(tidyr)       # Library for data manipulation.
library(ggplot2)     # Library for plotting data.
library(tidytext)    # Library for working with text data.
library(mgcv)        # Library for generalized statistical modeling.
library(openxlsx)    # Library for reading and writing Excel files.

# Load data ------------------------------------------------------------

promares <- readRDS("data/updates/promares_pnr_update.RDS") 
conanp <- readxl::read_excel("data/conanp_pnr_historic.xlsx", sheet = "data") 

glimpse(conanp)

# Combine conanp and promares data to calculate invertebrate richness -------
invert <- bind_rows(
  conanp %>% 
    filter(Label == "INV") %>% 
    mutate(base = "conanp") %>% 
    group_by(Year, Island, Reef, Transect, base) %>% 
    summarise(richness = n_distinct(Species)),
  promares %>% 
    filter(Label == "INV", Region == "Revillagigedo") %>% 
    mutate(Transect = as.numeric(Transect), base = "promares") %>% 
    group_by(Year, Island, Reef, Transect, base) %>% 
    summarise(richness = n_distinct(Species))
) %>% 
  mutate(Island = str_replace_all(Island, "Clarión", "Clarion"))

# Create a violin plot to show invertebrate richness over time
invert %>% 
  filter(!Year < 2015 | !Year > 2010) %>% 
  group_by(Year, Island, Reef) %>% 
  summarise(richness = mean(richness)) %>% 
  ggplot(aes(x=factor(Year), y=richness)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = .1) +
  labs(x = "", y = "Richness", title = "Invertebrate Richness") +
  ylim(0, 21) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))

# Save the plot as an image
ggsave("figs/invertebrate_richness_overtime.png", dpi = 300, height = 5, width = 5)

# Create a violin plot to show invertebrate richness by island

richness_plot <- invert %>% 
  filter(!Year < 2015 | !Year > 2010) %>% # Only include years between 2010 and 2015
  group_by(Year, Island, Reef) %>% 
  summarise(richness = mean(richness)) %>% 
  mutate(Island = factor(Island, levels = c("Clarion", "Roca Partida", "Socorro", "San Benedicto"))) %>% # Convert 'Island' to a factor
  ggplot(aes(x=factor(Island), y=richness)) +
  geom_violin(trim = FALSE) + 
  geom_boxplot(width = .1) +
  labs(x = "", y = "Richness", title = "Invertebrate Richness") +
  ylim(0, 21) +
  theme_bw() +
  theme(axis.text.x = element_text(vjust = .5))

richness_plot

# Save the plot as an image
ggsave("figs/invertebrate_richness_by_island.png", dpi = 300, height = 5, width = 5)

# Fish ----------

# Combine 'conanp' and 'promares' data related to fish ('PEC')
fish <- bind_rows(
  conanp %>% 
    filter(Label == "PEC") %>% 
    mutate(base = "conanp") %>% 
    group_by(Year, Island, Reef, Transect, base) %>% 
    summarise(biomass = sum(`ton/ha`)),
  promares %>% 
    filter(Label == "PEC", Region == "Revillagigedo") %>% 
    mutate(Transect = as.numeric(Transect), base = "promares") %>% 
    group_by(Year, Island, Reef, Transect, base) %>% 
    summarise(biomass = sum(Biomass))
) %>% 
  mutate(Island = str_replace_all(Island, "Clarión", "Clarion"))

fish %>% 
  group_by(Year, Island, Reef) %>% 
  summarise(biomass = log1p(mean(biomass))) %>% 
  group_by(Island) %>% 
  mutate(mean_biom = mean(biomass)) %>% 
  mutate(partida = ifelse(Island == "Roca Partida", "Roca Partida", "Other")) %>% 
  ggplot(aes(x=factor(Year), y=(biomass))) +
  geom_violin(trim = F) +
  geom_boxplot(width = .1) +
  labs(x = "", y = "Fish Biomass (ton/ha)") +
  geom_hline(aes(yintercept = mean_biom)) +
  ylim(0, 7.5) +
  facet_wrap(Island~.,) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        strip.background = element_blank())

# Save the plot as an image
ggsave("figs/fish_biomass_overtime.png", dpi = 300, height = 5, width = 5)

fish %>% 
  group_by(Year, Island, Reef) %>% 
  summarise(biomass = log1p(mean(biomass))) %>% 
  # Convert 'Island' variable to a factor with specific levels
  mutate(Island = factor(Island, levels = c("Clarion", "Roca Partida", "Socorro", "San Benedicto"))) %>% 
  # Create the violin and boxplot
  ggplot(aes(x=factor(Island), y=biomass)) +
  geom_violin(trim = F) +  # Violin plot without width trimming
  geom_boxplot(width = .1) +  
  labs(x = "", y = "Fish Biomass (ton/ha)") +  # Axis labels
  ylim(0, 7.5) +  # Limit y-axis
  theme_bw() +  # Theme with white background and gray lines
  theme(axis.text.x = element_text(vjust = .5))  # Adjust x-axis labels

# Save the plot as an image
ggsave("figs/fish_biomass_by_island.png", dpi = 300, height = 5, width = 5)

# Average richness of invertebrates (INV) and fish (PEC) 2023 ---------------

rich <- promares |> 
  filter(Year == 2023) |> # Filter data for the year 2023
  
  # Convert Transect variable to numeric
  mutate(Transect = as.numeric(Transect)) |> 
  
  # Group data by label, island, reef, and transect, and calculate average species richness
  group_by(Label, Island, Reef, Transect) |> 
  summarise(richness = n_distinct(Species)) 

# Visualize average species richness by island and reef
rich %>% 
  group_by(Label, Island, Reef) %>% 
  summarise(richness = mean(richness)) 

# Create a violin and boxplot to visualize species richness distribution in 2023
rich %>% 
  group_by(Label, Island, Reef) %>% 
  
  # Convert 'Island' variable to a factor and set specific order for islands
  mutate(Island = factor(Island, levels = c("Clarion", "Roca Partida", "Socorro", "San Benedicto"))) %>% 
  
  ggplot(aes(x=factor(Island), y=richness)) +
  geom_violin(trim = F) +
  geom_boxplot(width = .1) +
  facet_wrap(~Label) +
  labs(x = "", y = "Richness", title = "Species Richness") +
  ylim(0, 45) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = .5))

# Save the plot as an image
ggsave("figs/promares_pnr_species_richness_islands_2023.png", width = 8.5, height = 4.5, dpi=1000)

# Calculate fish species richness by year (promares) -------------
rich_fish <- promares |> 
  filter(Label == "PEC") |> # Filter by Label: invertebrates (INV) and fish (PEC)
  group_by(Year, Region, Island, Reef, Transect, TrophicGroup) |> 
  summarise(richness = n_distinct(Species))

# Summarize fish richness for each combination of year, region, reef, and trophic group
rich_fish %>% 
  group_by(Year, Region, Reef, TrophicGroup) |> 
  summarise(richness = sum(richness)) |> 
  
  # Create a violin and boxplot to visualize fish richness distribution by region and trophic group
  ggplot(aes(x=factor(Year), y=richness)) +
  geom_violin(trim = F) +
  geom_boxplot(width = .1) +
  labs(x = "", y = "Richness", title = "Fish Richness") +
  theme_bw() +
  theme(axis.text.x = element_text(vjust = .5))

# Save the plot as an image 
ggsave("figs/promares_pnr_fish_richness_historic.png", dpi = 300, height = 5, width = 5)