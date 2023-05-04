# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(tidytext)


# Load data ---------------------------------------------------------------

fish <- read_xlsx("data/Base de datos Arrecifal PNR.xlsx", sheet = "Fish")

fish <- fish |> 
  rename(A_ord = a, B_pen = b, Transect = Transecto, TrophicGroup = GF, TrophicLevel = NT)|> 
  separate(Date, into = c("Year", "Month", "Day"), sep = "-") |>
  filter(!is.na(Species))|>
  mutate(Month = as.numeric(Month)) |> 
  mutate(Season = case_when(
    Month == 1 ~ "Winter",
    Month == 3 ~ "Spring",
    Month == 10 ~ "Autumn",
    TRUE ~ "Other"
  ),
  Season = factor(Season, levels=c("Winter", "Spring", "Autumn")),
  Year = factor(Year, levels=c(2019, 2020))
  )

# Abundance by Islands

(abu_fish <- fish |> 
    filter(!is.na(Species)) |>  
    group_by(Year, Season, Island, Site, Transect, Depth, Species)) |> 
  summarise (Abundance = sum(Quantity, na.rm = T)) |> 
  group_by(Year, Season, Island, Site, Species) |>
  # summarise(Abundance = mean(Abundance)) |>
  group_by(Site, Species)  |> 
  top_n(10, Abundance) |> 
  ungroup() |>
  mutate(Species= reorder_within(Species, Abundance, Site)) |>  
  ggplot(aes(x=Species, y = Abundance, fill=Site)) +
  geom_col()+
  coord_flip()+
  facet_wrap(~Year, scales="free_y") +
  scale_x_reordered() +
  scale_fill_manual(values = c ("#66C2A5", "#FC8D62", "#8DA0CB","#E78AC3",
                                "#A6D854","#FFD92F","#E5C494","#B3B3B3", 
                                "#1F78B4", "#33A02C", "#E31A1C", "#FF7F00",
                                "#6A3D9A", "#A6CEE3", "#1F969A", "#FFB347",
                                "#B2DF8A", "#FDBF6F", "#FB9A99" ))+
  labs(y = "Abundance", x="", title= "Fish Abundance CONANP") +
  theme_classic()+
  theme(axis.text.y = element_text(face= "italic", size=10),
        axis.text.x=element_text(size=8),
        plot.title = element_text(hjust=0.5, size=14, face="plain", color = "gray20"),
        plot.title.position = "plot",
        # legend.position = "",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold.italic")
  )

ggsave("figs/2019-20_pec_abundance_CONANP.png", width = 8.5, height = 4.5, dpi=1000)

(fish_season <- fish|> 
    # filter(Island == "Socorro") |>
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
    labs(y = "Abundance", x="Species", title= "Fish Abundance by Island") +
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
ggsave("figs/conanp_pec_Season_Island.png", width = 8.5, height = 4.5, dpi=1000)



fish <- fish |> 
  mutate(A_ord = as.numeric(A_ord), 
         B_pen= as.numeric(B_pen),
         Biomasa = as.numeric(Biomasa),
         Quantity = as.numeric(Quantity ),
         Size=as.numeric(Size),
         Area= as.numeric(Area),
         Biomass = (Quantity * A_ord* (Size^B_pen))/(Area * 100)) |>  
  mutate(
    # TrophicGroup = factor(TrophicGroup,
    # 
    #                            levels = c("Piscivoro",
    #                                       "Carnivoro",
    #                                       "Herbivoro",
    #                                       "Zooplanctivoro")),
    # Region = factor(Region),
    TrophicLevelF = cut(as.numeric(TrophicLevel), 
                        breaks = c(2, 2.5, 3, 3.5, 4, 4.6), 
                        labels = c("2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-4.5"), 
                        right = FALSE)) |> 
  mutate(Island= factor(Island, levels=c("Socorro", "San_Benedicto"), 
                        # "Clarion", "Partida Revillagigedo"),
                        labels=c("Socorro", "San_Benedicto"))
         # , "Clarión", "La Partida"))
  )

#  Biomass Composition ---------------------------------------

fish |> 
  group_by(Year, Month, Island, Site, Species) |> 
  summarise(Biomass= mean(Biomass)) |> 
  ggplot(aes(fill=Species, y=Biomass, x=Site)) +
  
  geom_bar(position="fill", stat="identity", col="black") +
  scale_fill_brewer(palette = "Set2") +
  # facet_wrap(~Month)+
  theme_classic()+
  labs(x="", fill="Species", y="Biomass (ton/ha)") +
  theme(legend.text = element_text(face = "italic"))

(p2 <- fish |> 
    filter(!is.na(Species)) |>                 
    group_by(Island, Depth, Species) |> 
    summarise(Biomass= mean(Biomass)) |> 
    ggplot(aes(fill=Species, y=Biomass, x=Island)) +
    
    geom_bar(position="fill", stat="identity", col="black") +
    scale_fill_manual(values=c("#66C2A5", "#FC8D62", "#8DA0CB","#E78AC3",
                               "#A6D854","#FFD92F","#E5C494","#B3B3B3", 
                               "#1F78B4", "#33A02C", "#E31A1C", "#FF7F00" ))+
    facet_wrap(~Depth)+
    theme_classic()+
    labs(x="", fill="Trophic Group", y="Biomass (ton/ha)")
)

ggsave("figs/conanp_pec_biomass_island.png", width = 8.5, height = 4.5, dpi=1000)

