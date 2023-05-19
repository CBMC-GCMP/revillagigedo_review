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

rev <- readRDS("data/LTEM_extract_Revillagigedo.RDS")
ltem <- readRDS("data/LTEM_historic_updated_27122022.RDS")
  
#  Fish --------------------------------------------------------------------

fish <- ltem |> 
  filter(Label == "PEC") |>  
  mutate(A_ord = as.numeric(A_ord), 
         B_pen= as.numeric(B_pen),
         Quantity = as.numeric(Quantity ),
         Size=as.numeric(Size),
         Area= as.numeric(Area),
         Biomass = (Quantity * A_ord* (Size^B_pen))/(Area * 100)) |>  
  mutate(Season= case_when(Month==2| Month==12 ~"Winter",
                           Month==3| Month== 4 ~"Spring",
                           Month==6| Month==7| Month== 8 ~"Summer", 
                           Month==11| Month== 9| Month== 10 ~"Autumn",
                           TRUE~"Other"),
         Season= factor(Season, levels=c("Winter", "Spring", "Autumn")),
         # Year= factor(Year, levels=c(2006, 2016, 2017)),
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
                             right = FALSE)) 

   
# Trophic Group Biomass Composition ---------------------------------------

(p1 <- fish |> 
    filter(!is.na(TrophicGroup), Region == "Revillagigedo") |>
    mutate(Island = factor(Island,
                            levels = c("Socorro", "San Benedicto", "Clarion", "Partida Revillagigedo"),
                            labels = c("Socorro", "San Benedicto", "Clarión", "Roca Partida"))) |> 
    group_by(Year, Island, Reef, Transect, Depth2, TrophicGroup, Species, Season) |> 
    summarise(Biomass = sum(Biomass)) |>  
    group_by(Island, TrophicGroup) |>  
    summarise(Biomass = mean(Biomass)) |> 
    ggplot(aes(x=Island , y=Biomass, fill=TrophicGroup)) +
    geom_bar(position = "fill", stat="identity", col="black")+
    scale_fill_manual(values=c("#a30808", "#b57560", "#114a06", "#258f4e")) +
    # facet_wrap(~)+
    labs(x = "", y = "Biomass (Ton/ha)", fill = "Trophic Group") +
    theme_classic())


# Biomass Region Comparison -----------------------------------------------

(p2 <- fish |>
    mutate(Region = case_when(
      Island == "Partida Revillagigedo" ~ "Roca Partida",
      TRUE ~ Region )) |>
    mutate(Region = factor(Region, levels = c("Revillagigedo", "Roca Partida", "Loreto", "Cabo Pulmo", "Los Cabos", "La Paz", "Islas Marias"))) |>
    filter(!is.na(TrophicGroup), Region %in% c("Revillagigedo", "Roca Partida", "Loreto", "Cabo Pulmo", "Los Cabos", "La Paz", "Islas Marias")) |>
    group_by(Year, Region, Reef, Transect, Depth2, TrophicGroup, Species, Season) |>
    summarise(Biomass = sum(Biomass)) |>
    group_by(Region, TrophicGroup) |>
    summarise(Biomass = mean(Biomass)) |>
    ggplot(aes(x = Region, y = Biomass, fill = TrophicGroup)) +
    geom_bar(position = "fill", stat = "identity", col = "black") +
    scale_fill_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e")) +
    labs(x = "", y = "Biomass (Ton/ha)", fill = "Trophic Group") +
    theme_classic())

ggsave("figs/LTEM_fish_biomass_comparassion.png", width = 8.5, height = 4.5, dpi=1000)

# Biomass Comparison -----------------------------------------------
unique(ltem$Protection_level)
(p3 <- fish |> 
  filter(!is.na(TrophicGroup), Region %in% c("Loreto", "Cabo Pulmo", "Los Cabos", "Revillagigedo", "La Paz", "Islas Marias")) |> 
  mutate(MPA = factor(Protection_level, levels = c("Prohibited", "Open Area"), labels = c("Fully Protected", "Open Area"))) |> 
  group_by(Region, MPA, Reef, Depth2, Transect) |>
  summarise(Biomass = mean(Biomass)) |>
  ggplot(aes(x = Region, y = Biomass, fill=Region)) +
  geom_violin(trim = F,  alpha = .3) +
  geom_boxplot(width  = 0.1) +
  geom_jitter(width = 0.09, pch = 21,  alpha = .3) +
  scale_color_manual(values = c("#0f2359", "#7AC5CD")) +
  labs(x = "", y = "Biomass (Ton/ha)") +
  theme_classic()+
  theme(legend.position = "") +
  guides(colour = "none")
)



# Tendencia Biomasa ----------------------------------------------

(p4 <- fish |>
  filter(!is.na(TrophicGroup), Region %in% c("Revillagigedo")) |>
  group_by(Year, Region, Reef, Transect, Depth2, TrophicGroup, Species, Season) |>
  summarise(Biomass = sum(Biomass)) |>
  group_by(Year, Region, TrophicGroup) |>
  summarise(Biomass = mean(Biomass)) |>
  ggplot(aes(x= Year, y = Biomass, col= TrophicGroup)) +
  geom_point()+
  # coord_flip()+
  facet_wrap(~TrophicGroup, scales="free_y")
)

(p5 <- fish |>
  filter(!is.na(TrophicGroup), Region %in% c("Revillagigedo")) |>
  group_by(Year, Region, Reef, Depth2, Transect, TrophicGroup, Genus, Species) |> 
  summarise(Biomass = sum(Biomass)) |> 
  group_by(Year, TrophicGroup) |> 
  summarise(Biomass =round(mean(Biomass),0)) |> 
  ggplot(aes(x = Year, y = Biomass, col= TrophicGroup))+
  geom_point()+
  labs(x = "Year", 
       y = "Biomass")+
  scale_x_continuous(breaks=seq(2005,2018, 2))+
  theme(axis.text.x = element_text(angle=45),
        axis.title =element_text(size=10))+
  #geom_smooth(method = "gam", formula = y ~ s(x, k = 3), method.args = list(family = "poisson"))+
  facet_wrap(~TrophicGroup) 
)




# NMDS ---------------
