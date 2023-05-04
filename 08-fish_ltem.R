# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidytext)
library(lubridate)
library(RColorBrewer)
library(gridExtra)

library(scales)

# Load data ---------------------------------------------------------------

# CBMC's Long Term Ecological Monitoring database (LTEM) extract from Revillagigedo

ltem <- readRDS("data/LTEM_extract_Revillagigedo.RDS")


#  Fish --------------------------------------------------------------------

fish <- ltem |> 
  filter (Label == "PEC") |>  
  mutate(A_ord = as.numeric(A_ord), 
         B_pen= as.numeric(B_pen),
         Quantity = as.numeric(Quantity ),
         Size=as.numeric(Size),
         Area= as.numeric(Area),
         Biomass = (Quantity * A_ord* (Size^B_pen))/(Area * 100)) |>  
  mutate(Season= case_when(Month==2~"Winter",
                                    Month==4~"Spring",
                                    Month==3~"Spring",
                                    Month==11~"Autumn",
                                    TRUE~"Other"),
                  Season= factor(Season, levels=c("Winter", "Spring", "Autumn")),
                  Year= factor(Year, levels=c(2006, 2016, 2017)),
                  Biomass=as.numeric(Biomass),
                  TrophicGroup = factor(TrophicGroup, 
                               levels = c("Piscivoro", 
                                          "Carnivoro", 
                                          "Herbivoro", 
                                          "Zooplanctivoro")), 
         Region = factor(Region),
         TrophicLevelF = cut(as.numeric(TrophicLevel), 
                             breaks = c(2, 2.5, 3, 3.5, 4, 4.6), 
                             labels = c("2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-4.5"), 
                             right = FALSE)) |> 
  mutate(Island= factor(Island, levels=c("Socorro", "San Benedicto", "Clarion", "Partida Revillagigedo"),
                        labels=c("Socorro", "San Benedicto", "Clarión", "La Partida"))
  )  


(fish_06 <- fish|> 
    filter(Year ==2006) |> 
    group_by(Season, MPA, Island, Reef, Transect, Depth, Family, Species) |> 
    summarise (Abundance = sum(Quantity, na.rm = T)) |>
    group_by(Season, Island, Reef, Family, Species) |>
    summarise(Abundance = mean(Abundance)) |>
    # filter(Abundance> 1) |>
    group_by(Reef, Family)  |>
    top_n(10, Abundance) |>
    ungroup() |>
    mutate(Family= reorder_within(Family, Abundance, Reef)) |>  
    ggplot(aes(x= Family, y = Abundance, fill= Reef)) +
    geom_col()+
    coord_flip()+
    facet_wrap(~ Reef, scales="free_y") +
    scale_x_reordered() +
    scale_fill_manual(values = c ("#66C2A5", "#FC8D62", "#8DA0CB","#E78AC3",
                                  "#A6D854","#FFD92F","#E5C494","#B3B3B3", 
                                  "#1F78B4", "#33A02C", "#E31A1C", "#FF7F00" ))+
    labs(y = "Abundance", x="Family", title= "Fishes Abundance by Reef 2006") +
    theme_classic()+
    theme(axis.text.y = element_text(face= "italic", size=4.5),
          axis.text.x=element_text(size=8),
          plot.title = element_text(hjust=0.5, size=12, face="plain", color = "gray20"),
          plot.title.position = "plot",
          legend.position = "",
          strip.background = element_blank(),
          strip.text.x = element_text(size = 5, face = "bold.italic")
    )
)
    ggsave("figs/2006_PEC_ltem_reef.png", width = 8.5, height = 4.5, dpi=1000)

# 2017

    (fish_16 <- fish |> 
    filter(Year ==2017) |> 
    group_by(Season, MPA, Island, Reef, Transect, Depth, Family, Species) |> 
    summarise (Abundance = sum(Quantity, na.rm = T)) |>
    group_by(Season, Island, Reef, Family, Species) |>
    summarise(Abundance = mean(Abundance)) |>
    # filter(Abundance> 1) |>
    group_by(Reef, Family) |> 
    top_n(10, Abundance) |> 
    ungroup() |>
    mutate(Family= reorder_within(Family, Abundance, Reef)) |>  
    ggplot(aes(x= Family, y = Abundance, fill= Reef)) +
    geom_col()+
    coord_flip()+
    facet_wrap(~Reef, scales="free_y") +
    scale_x_reordered() +
    scale_fill_manual(values = c ("#66C2A5", "#FC8D62", "#8DA0CB","#E78AC3",
                                  "#A6D854","#FFD92F","#E5C494","#B3B3B3", 
                                  "#1F78B4", "#33A02C", "#E31A1C", "#FF7F00",
                                  "#6A3D9A", "#A6CEE3", "#1F969A", "#FFB347",
                                  "#B2DF8A", "#FDBF6F", "#FB9A99"))+
    labs(y = "Abundance", x="Family", title= "Fish Abundance 2017") +
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
   

ggsave("figs/2017_PEC_ltem_reef.png", width = 8.5, height = 4.5, dpi=1000)


# Plot by season

(fish_season <- fish|> 
    filter(Island == "La Partida") |>
    group_by(Season, MPA, Island, Reef, Transect, Depth, Family, Species) |> 
    summarise (Abundance = sum(Quantity, na.rm = T)) |>
    group_by(Season, Island, Reef, Family, Species) |>
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
    labs(y = "Abundance", x="Species", title= "Fish Abundance La Partida Island") +
    theme_classic()+
    theme(axis.text.y = element_text(face= "italic", size=6),
          axis.text.x=element_text(size=9),
          plot.title = element_text(hjust=0.5, size=12, face="plain", color = "gray20"),
          plot.title.position = "plot",
          legend.position = "",
          strip.background = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold.italic")
    )
)
ggsave("figs/LTEM_pec_Season_Partida.png", width = 8.5, height = 4.5, dpi=1000)


(p1 <- fish |> 
  group_by(Season, Reef, Transect, TrophicGroup, Species) |> 
  summarise(Biomass = sum(Biomass)) |>  
  group_by(Season, TrophicGroup) |>  
  summarise(Biomass = mean(Biomass)) |> 
  ggplot(aes(x=Season , y=Biomass, fill=TrophicGroup )) +
  geom_bar(position = "fill", stat="identity", col="black")+
  scale_fill_manual(values=c("#66C2A5", "#FC8D62", "#8DA0CB","#E78AC3")) +
  # ylim(0, 0.7) +
  labs(x = "", y = "Biomass (Ton/ha)", fill = "Trophic Group") +
  theme_classic())
  # theme(legend.position = "bottom") +
  # guides(colour = "none"))

(p2 <- fish |> 
  filter(!is.na(TrophicGroup)) |>                 
  group_by(Island, Depth2, TrophicGroup) |> 
  summarise(Biomass= mean(Biomass)) |> 
  ggplot(aes(fill=TrophicGroup, y=Biomass, x=Island)) +
  
  geom_bar(position="fill", stat="identity", col="black") +
  scale_fill_manual(values=c("#66C2A5", "#FC8D62", "#8DA0CB","#E78AC3"))+
  facet_wrap(~Depth2)+
  theme_classic()+
  labs(x="", fill="Trophic Group", y="Biomass (ton/ha)")
)

ggsave("figs/LTEM_pec_biomass_island.png", width = 8.5, height = 4.5, dpi=1000)
