# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl) 
library(ggplot2)
library(dplyr)
library(tidytext)
library(lubridate)
library(tidyr)


# Load data ---------------------------------------------------------------

rev <- readRDS("data/LTEM_extract_Revillagigedo.RDS") #CBMC's Long Term Ecological Monitoring database (LTEM) extract from Revillagigedo

inv <- rev |>
  filter(Label == "INV") |>
  mutate(Season = case_when(
    Month == 2 ~ "Winter",
    Month == 4 ~ "Spring",
    Month == 3 ~ "Spring",
    Month == 11 ~ "Autumn",
    TRUE ~ "Other"
  )) |>
  mutate(
    Season = factor(Season, levels = c("Winter", "Spring", "Autumn")),
    Year = factor(Year, levels = c(2006, 2016, 2017)),
    Island = factor(Island, levels = c("Socorro", "San Benedicto", "Clarion", "Partida Revillagigedo"),
                    labels = c("Socorro", "San Benedicto", "Clarión", "Roca Partida"))
  )


# Abundances  -----------------

(abu <- inv |> 
  filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
  group_by(Year, Season, MPA, Protection_level, Island, Reef, Transect, Depth2, Taxa2, Species) |> 
  summarise (Abundance = sum(Quantity, na.rm = T)) |> 
  group_by(Year, Taxa2, Reef) |> 
  summarise(Abundance = mean(Abundance)) |>
  ggplot(aes(x= Year, y = Abundance, col= Taxa2)) +
  geom_jitter()+
  # coord_flip()+
  facet_wrap(~Taxa2, scales="free_y") +
  scale_x_reordered() +
  scale_fill_manual(values=c("#a30808", "#b57560", "#258f4e")) +
  labs(y = "Abundance", x="Year", title= "Invertebrates Abundance") +
  theme_classic()+
  theme(axis.text.y = element_text(face= "plain", size=8),
        axis.text.x=element_text(size=10),
        plot.title = element_text(hjust=0.5, size=14, face="plain", color = "gray20"),
        plot.title.position = "plot",
        legend.position = "",
        # legend.text = element_text(face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold.italic")
  )
)

(abu <- inv |> 
    filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
    group_by(Year, Season, MPA, Protection_level, Island, Reef, Transect, Depth2, Taxa2, Species) |> 
    summarise (Abundance = sum(Quantity, na.rm = T)) |> 
    group_by(Year, Taxa2, Reef) |> 
    summarise(Abundance = mean(Abundance)) |>
    ggplot(aes(x= Year, y = Abundance, col= Taxa2)) +
    geom_point()+
    geom_boxplot()+
    # coord_flip()+
    facet_wrap(~Taxa2, scales="free_y") +
    scale_x_reordered() +
    scale_fill_manual(values=c("#a30808", "#b57560", "#258f4e")) +
    labs(y = "Abundance", x="Year", title= "Invertebrates Abundance") +
    theme_classic()+
    theme(axis.text.y = element_text(face= "plain", size=8),
          axis.text.x=element_text(size=10),
          plot.title = element_text(hjust=0.5, size=14, face="plain", color = "gray20"),
          plot.title.position = "plot",
          legend.position = "",
          # legend.text = element_text(face = "italic"),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold.italic")
    )
)
# Richness  ------------------------------------------------------

(rich <- inv |>
  filter(!is.na(Species)) |>
  filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
  # filter(Quantity > 1) |>
  group_by(Year, Taxa2) |>
  summarise(Richness = n_distinct(Species)) |>
  
  ggplot(aes(x = Year, y = Richness, col = Taxa2)) +
  geom_point() +
  facet_wrap(~Taxa2, scales = "free_y") +
  scale_fill_manual(values=c("#a30808", "#b57560", "#258f4e")) +
  labs(x = "Year", y = "Richness", title = "Invertebrates Richness") +
  theme_classic() +
   theme(axis.text.y = element_text(face= "plain", size=8),
         axis.text.x=element_text(size=10),
         plot.title = element_text(hjust=0.5, size=14, face="plain", color = "gray20"),
         plot.title.position = "plot",
         legend.position = "",
         # legend.text = element_text(face = "italic"),
         strip.background = element_blank(),
         strip.text.x = element_text(size = 10, face = "bold.italic")
   )
)



# Tendencia Abundancia ----------------------------------------------

rev |> 
  filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
  group_by(Year, Reef, Depth2, Transect, Taxa2, Genus, Species) |> 
  summarise(Abundance = sum(Quantity)) |> 
  group_by(Year, Taxa2) |> 
  summarise(Abundance =round(mean(Abundance),0)) |> 
  ggplot(aes(x = Year, y = Abundance))+
  geom_point()+
  labs(x = "Year", 
       y = "Abundance")+
  scale_x_continuous(breaks=seq(2005,2018, 2))+
  theme(axis.text.x = element_text(angle=45),
        axis.title =element_text(size=10))+
  #geom_smooth(method = "gam", formula = y ~ s(x, k = 3), method.args = list(family = "poisson"))+
  facet_wrap(~Taxa2)
  # geom_line()

# Tendencia Riqueza ----------------------------------------------

rev |>
  filter(!is.na(Species)) |>
  filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
  # filter(Quantity > 1) |>
  group_by(Year, Taxa2) |>
  summarise(Richness = n_distinct(Species)) |>
  group_by(Year, Taxa2) |> 
  summarise(Richness =round(mean(Richness),0)) |> 
  ggplot(aes(x = Year, y = Richness))+
  geom_point()+
  labs(x = "Year", 
       y = "Richness")+
  scale_x_continuous(breaks=seq(2005,2018, 2))+
  theme(axis.text.x = element_text(angle=45),
        axis.title =element_text(size=10))+
  #geom_smooth(method = "gam", formula = y ~ s(x, k = 3), method.args = list(family = "poisson"))+
  facet_wrap(~Taxa2)



