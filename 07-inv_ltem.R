
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidytext)
library(lubridate)
library(RColorBrewer)
library(gridExtra)

# Load data ---------------------------------------------------------------

# CBMC's Long Term Ecological Monitoring database (LTEM) extract from Revillagigedo

ltem <- readRDS("data/LTEM_extract_Revillagigedo.RDS")

  

# Invertebrates ------------------------------------------------------------
  
  # Add Season
  
  inv <- ltem |> 
    filter (Label == "INV") |> 
    mutate(Season= case_when(Month==2~"Winter",
                           Month==4~"Spring",
                           Month==3~"Spring",
                           Month==11~"Autumn",
                           TRUE~"Other"),
         Season= factor(Season, levels=c("Winter", "Spring", "Autumn")),
         Year= factor(Year, levels=c(2006, 2016, 2017)))
  
  # Plot by Island 
  
  # 2006
  (inv_06 <- inv|> 
      filter(Year ==2006) |> 
      group_by(Year, Season, MPA, Island, Reef, Transect, Depth, Taxa1, Species) |> 
      summarise (Abundance = sum(Quantity, na.rm = T)) |>
      group_by(Year, Season, Island, Reef, Taxa1, Species) |>
      summarise(Abundance = mean(Abundance)) |>
      filter(Abundance> 1) |>
      group_by(Reef, Species)  |> 
      top_n(10, Abundance) |> 
      ungroup() |>
      mutate(Species= reorder_within(Species, Abundance, Reef)) |>  
      ggplot(aes(x= Species, y = Abundance, fill= Reef)) +
      geom_col()+
      coord_flip()+
      facet_wrap(~ Reef, scales="free_y") +
      scale_x_reordered() +
      scale_fill_manual(values = c ("#66C2A5", "#FC8D62", "#8DA0CB","#E78AC3",
                                    "#A6D854","#FFD92F","#E5C494","#B3B3B3", 
                                    "#1F78B4", "#33A02C", "#E31A1C", "#FF7F00" ))+
      labs(y = "Abundance", x="Species", title= "Invertebrates Abundance by Reef 2006") +
      theme_classic()+
      theme(axis.text.y = element_text(face= "italic", size=8),
            axis.text.x=element_text(size=10),
            plot.title = element_text(hjust=0.5, size=14, face="plain", color = "gray20"),
            plot.title.position = "plot",
            legend.position = "",
            strip.background = element_blank(),
            strip.text.x = element_text(size = 4, face = "bold.italic")
      )
  )
  ggsave("figs/2006_inv_ltem_reef.png", width = 8.5, height = 4.5, dpi=1000)
  
  # 2016
  (inv_16 <- inv|> 
      filter(Year ==2016) |> 
      group_by(Year, Season, MPA, Island, Reef, Transect, Depth, Taxa1, Species) |> 
      summarise (Abundance = sum(Quantity, na.rm = T)) |>
      group_by(Year, Season, Island, Reef, Taxa1, Species) |>
      summarise(Abundance = mean(Abundance)) |>
      filter(Abundance> 1) |>
      group_by(Reef, Species) |> 
      top_n(10, Abundance) |> 
      ungroup() |>
      mutate(Species= reorder_within(Species, Abundance, Reef)) |>  
      ggplot(aes(x= Species, y = Abundance, fill= Reef)) +
      geom_col()+
      coord_flip()+
      facet_wrap(~ Reef, scales="free_y") +
      scale_x_reordered() +
      scale_fill_manual(values = c ("#66C2A5", "#FC8D62", "#8DA0CB","#E78AC3",
                                    "#A6D854","#FFD92F","#E5C494","#B3B3B3", 
                                    "#1F78B4", "#33A02C", "#E31A1C", "#FF7F00",
                                    "#6A3D9A", "#A6CEE3", "#1F969A", "#FFB347",
                                    "#B2DF8A", "#FDBF6F", "#FB9A99"))+
      labs(y = "Abundance", x="Species", title= "Invertebrates Abundance 2016") +
      theme_classic()+
      theme(axis.text.y = element_text(face= "italic", size=4),
            axis.text.x=element_text(size=5),
            plot.title = element_text(hjust=0.5, size=12, face="plain", color = "gray20"),
            plot.title.position = "plot",
            legend.position = "",
            strip.background = element_blank(),
            strip.text.x = element_text(size = 3, face = "bold.italic")
      )
  )
 
   ggsave("figs/2016_inv_ltem_reef.png", width = 8.5, height = 4.5, dpi=1000)
  
   # 2017
  (inv_17 <- inv|> 
      filter(Year ==2017) |> 
      group_by(Year, Season, MPA, Island, Reef, Transect, Depth, Taxa1, Species) |> 
      summarise (Abundance = sum(Quantity, na.rm = T)) |>
      group_by(Year, Season, Island, Reef, Taxa1, Species) |>
      summarise(Abundance = mean(Abundance)) |>
      filter(Abundance> 1) |>
      group_by(Reef, Species) |> 
      top_n(10, Abundance) |> 
      ungroup() |>
      mutate(Species= reorder_within(Species, Abundance, Reef)) |>  
      ggplot(aes(x= Species, y = Abundance, fill= Reef)) +
      geom_col()+
      coord_flip()+
      facet_wrap(~ Reef, scales="free_y") +
      scale_x_reordered() +
      scale_fill_manual(values = c ("#66C2A5", "#FC8D62", "#8DA0CB","#E78AC3",
      "#A6D854","#FFD92F","#E5C494","#B3B3B3", "#1F78B4", "#33A02C", "#E31A1C", "#FF7F00" ))+
      labs(y = "Abundance", x="Species", title= "Invertebrates Abundance 2017") +
      theme_classic()+
      theme(axis.text.y = element_text(face= "italic", size=6),
            axis.text.x=element_text(size=8),
            plot.title = element_text(hjust=0.5, size=12, face="plain", color = "gray20"),
            plot.title.position = "plot",
            legend.position = "",
            strip.background = element_blank(),
            strip.text.x = element_text(size = 4, face = "bold.italic")
      )
  )
 
  ggsave("figs/2017_inv_ltem_reef.png", width = 8.5, height = 4.5, dpi=1000)
  
  # Plot by season
  
  (inv_season <- inv|> 
      filter(Island == "Socorro") |>
      group_by(Year, Season, MPA, Island, Reef, Transect, Depth, Taxa1, Species) |> 
      summarise (Abundance = sum(Quantity, na.rm = T)) |>
      group_by(Year, Season, Island, Reef, Taxa1, Species) |>
      summarise(Abundance = mean(Abundance)) |>
      filter(Abundance> 1) |>
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
  ggsave("figs/LTEM_inv_Season_spp.png", width = 8.5, height = 4.5, dpi=1000)
 