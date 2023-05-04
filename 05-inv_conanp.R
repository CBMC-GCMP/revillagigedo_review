# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(tidytext)


# Load data ---------------------------------------------------------------

inv <- read_xlsx("data/Base de datos Arrecifal PNR.xlsx", sheet = "Invertebrados")


# Invertebrates ------------------------------------------------------------

inv <- inv |> 
        rename(Date = Fecha, Species = Especie, Quantity = Total, Island =Isla,
         Site = Sitio, Depth = Profundidad, Transect = Transecto,
         TrophicGroup = GF, TrophicLevel = NT) |> 

  separate(Date, into = c("Year", "Month", "Day"), sep = "-") |>
  filter(!is.na(Species))|>
  mutate(Month = as.numeric(Month)) |> 
  mutate(Season = case_when(
    Month == 2 ~ "Winter",
    Month == 1 ~ "Winter",
    Month == 3 | Month == 4 | Month == 5 ~ "Spring",
    Month == 10 | Month == 11 | Month == 12 ~ "Autumn",
    TRUE ~ "Other"
  ),
  Season = factor(Season, levels=c("Winter", "Spring", "Autumn")),
  Year = factor(Year, levels=c(2013, 2017, 2018, 2019, 2020, 2021, 2022))
  )


# Abundance by Islands

(abu_inv <- inv |> 
    filter(!is.na(Species), Year == 2022) |>  
    group_by(Season, Island, Site, Transect, Depth, Species)) |> 
    summarise (Abundance = sum(Quantity, na.rm = T)) |> 
  group_by(Season, Island, Site, Species) |>
  summarise(Abundance = mean(Abundance)) |>
  group_by(Site, Species)  |> 
  top_n(10, Abundance) |> 
  ungroup() |>
    mutate(Species= reorder_within(Species, Abundance, Site)) |>  
    ggplot(aes(x=Species, y = Abundance, fill=Site)) +
    geom_col()+
    coord_flip()+
    facet_wrap(~Site, scales="free_y") +
    scale_x_reordered() +
    scale_fill_manual(values = c ("#66C2A5", "#FC8D62", "#8DA0CB","#E78AC3",
                                   "#A6D854","#FFD92F","#E5C494","#B3B3B3", 
                                   "#1F78B4", "#33A02C", "#E31A1C", "#FF7F00",
                                   "#6A3D9A", "#A6CEE3", "#1F969A", "#FFB347",
                                   "#B2DF8A", "#FDBF6F", "#FB9A99" ))+
    labs(y = "Abundance", x="", title= "Invertebrates Abundance CONANP 2022") +
    theme_classic()+
    theme(axis.text.y = element_text(face= "italic", size=8),
          axis.text.x=element_text(size=8),
          plot.title = element_text(hjust=0.5, size=14, face="plain", color = "gray20"),
          plot.title.position = "plot",
          legend.position = "",
          strip.background = element_blank(),
          strip.text.x = element_text(size = 6, face = "bold.italic")
    )

  ggsave("figs/2022_inv_abundance_CONANP.png", width = 8.5, height = 4.5, dpi=1000)

(inv_season <- inv|> 
    filter(Island == "Socorro") |>
    group_by(Year, Season, Site, Transect, Depth, Species) |> 
    summarise (Abundance = sum(Quantity, na.rm = T)) |>
    group_by(Year, Season, Site, Species) |>
    summarise(Abundance = mean(Abundance)) |>
    group_by(Season, Species) |>
    top_n(10, Abundance) |>
    ungroup() |>
    mutate(Species= reorder_within(Species, Abundance, Season)) |>  
    ggplot(aes(x= Species, y = Abundance, fill= Season)) +
    geom_col()+
    coord_flip()+
    facet_wrap(~ Season, scales="free_y") +
    scale_x_reordered() +
    scale_fill_manual(values = c ("#66C2A5", "#FC8D62", "#8DA0CB","#E78AC3",
                                  "#A6D854","#FFD92F","#E5C494","#B3B3B3", 
                                  "#1F78B4", "#33A02C", "#E31A1C", "#FF7F00" ))+
    labs(y = "Abundance", x="Species", title= "Invertebrates Abundance Socorro Island") +
    theme_classic()+
    theme(axis.text.y = element_text(face= "italic", size=8),
          axis.text.x=element_text(size=9),
          plot.title = element_text(hjust=0.5, size=12, face="plain", color = "gray20"),
          plot.title.position = "plot",
          legend.position = "",
          strip.background = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold.italic")
    )
)
ggsave("figs/conanp_inv_Season_Socorro.png", width = 8.5, height = 4.5, dpi=1000)

 