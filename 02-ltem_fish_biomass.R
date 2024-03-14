# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(tidytext)
library(mgcv)
library(openxlsx)


# Load data ---------------------------------------------------------------

revilla <- readRDS("data/correct/promares_pnr_historic_20231109.RDS") 

# LTEM-------------------------

# Biomasa de peces------------


fish <- readRDS("data/ltem_historic_updated_2024-02-27.RDS") |> 
  filter(Label == "PEC") |> 
  mutate(
    A_ord = as.numeric(A_ord),
    B_pen= as.numeric(B_pen),
    Quantity = as.numeric(Quantity),
    Size=as.numeric(Size),
    Area= as.numeric(Area),
    Month= as.numeric(Month),
    Biomass = (Quantity * A_ord* (Size^B_pen))/(Area * 100)) |> 
  mutate(
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
  mutate(Island = str_replace_all(Island, c("Clarión" = "Clarion", "Partida Revillagigedo" = "Roca Partida")))


# Biomass 2023-----------


fish |>
  filter(!is.na(TrophicGroup), Region == "Revillagigedo", Year == 2023) |>
  mutate(Island = factor(Island,
                         levels = c("Socorro", "San Benedicto", "Roca Partida", "Clarion"),
                         labels = c("Socorro", "San Benedicto", "Roca Partida", "Clarion"))) |> 
  group_by(Year, Island, Reef, Transect, Depth2, TrophicGroup) |>
  summarise(Biomass = sum(Biomass)) |>
  group_by(Island, TrophicGroup) |>
  summarise(Biomass = mean(Biomass)) |>
  ggplot(aes(x = Island, y = Biomass, fill = TrophicGroup, col=TrophicGroup)) +
  geom_bar(position = "fill", stat = "identity", alpha=0.85) +
  scale_y_continuous(labels = scales::percent_format())+
  scale_fill_manual(name="Grupo trófico", values = c("#a30808", "#b57560",  "#258f4e", "#114a06")) +
  scale_color_manual(name="Grupo trófico", values = c("#a30808", "#b57560",  "#258f4e", "#114a06")) +
  labs(x = "", y = "Biomasa relativa (%)", fill = "Grupos tróficos") +
  theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.y = element_text(size=17, face="bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"))

# ggsave("figs/ltem_pnr_fish_biomass_gt_2023_.png", width = 8.5, height = 4.5, dpi=1000)
# ggsave("figs/ltem_pnr_fish_biomass_gt_2023_.pdf", width = 10, height = 5)

# PN Revilla -------

(p <- fish |> 
    filter(!is.na(TrophicGroup), Region == "Revillagigedo") |>
    mutate(Region = "Revillagigedo") |> 
    mutate(Region = ifelse(Island == "Partida Revillagigedo", "Roca Partida", Region)) |> 
    mutate(Region = factor(Region, levels = c("Revillagigedo", "Roca Partida"))) |>
    mutate(Year = factor(Year, levels = c(2006, 2016, 2017, 2023))) |> 
    group_by(Year, Region, Island, Reef, Transect, Depth2, TrophicGroup) |> 
    summarise(Biomass = sum(Biomass)) |>  
    group_by(Year, Region, TrophicGroup) |>  
    summarise(Biomass = mean(Biomass)) |> 
    ggplot(aes(x=Year , y=Biomass, fill=TrophicGroup)) +
    geom_bar(position = "fill", stat="identity", col="black")+
    scale_fill_manual(values=c("#a30808", "#b57560", "#114a06", "#258f4e"),  guide = "none") +
    facet_wrap(~Region)+
    labs(x = "Year", y = "Biomass (Ton/ha)", fill = "Trophic Group") +
    theme_classic())

# ggsave("figs/fish_ltem_Year.png", width = 8.5, height = 4.5, dpi=1000)

print(p2, n=24)

# Biomass Region Comparison -----------------------------------------------

fish |>
  filter(Label == "PEC" & Year!=2023) |>
  mutate(Region = factor(Region, levels = c("Revillagigedo", "Roca Partida", "Islas Marias", "Los Cabos", "Cabo Pulmo","La Paz", "Loreto"))) |>
  filter(!is.na(TrophicGroup), Region %in% c("Revillagigedo", "Roca Partida", "Islas Marias", "Los Cabos", "Cabo Pulmo","La Paz", "Loreto")) |>
  group_by(Year, Region, Reef, Transect, Depth2, TrophicGroup) |>
  summarise(Biomass = sum(Biomass)) |>
  group_by(Region, TrophicGroup) |>
  summarise(Biomass = mean(Biomass)) |>
  ggplot(aes(x = Region, y = Biomass, fill = TrophicGroup, col=TrophicGroup)) +
  geom_bar(position = "fill", stat = "identity", alpha=0.85) +
  scale_y_continuous(labels = scales::percent_format())+
  scale_fill_manual(name="Grupo trófico", values = c("#a30808", "#b57560",  "#258f4e", "#114a06")) +
  scale_color_manual(name="Grupo trófico", values = c("#a30808", "#b57560",  "#258f4e", "#114a06")) +
  labs(x = "", y = "Biomasa relativa (%)", fill = "Grupos tróficos") +
  theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.y = element_text(size=17, face="bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"))

# ggsave("figs/ltem_region_biomass_comparassion_gt_historic_proporcion_estetica.pdf", width = 10, height = 5)

# ggsave("figs/ltem_region_biomass_comparassion_gt_historic_proporcion.png", width = 8.5, height = 4.5, dpi=1000)

# Biomass Comparison -----------------------------------------------

unique(ltem$Protection_level)
(p3 <- fish |> 
    filter(!is.na(TrophicGroup), Region %in% c("Revillagigedo", "Islas Marias", "Los Cabos", "Cabo Pulmo","La Paz", "Loreto")) |> 
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
# ggsave("figs/ltem_region_biomass_comparassion2.png", width = 8.5, height = 4.5, dpi=1000)


#Shannon index ------------

library(vegan)

Shannon <- fish %>%
  filter(Region == "Revillagigedo", Year ==2023) %>%
  filter(!is.na(Species)) %>%
  group_by(Region, Island, Reef, Depth2, Species) %>%
  summarise(Quantity = sum(Quantity)) %>%
  group_by(Region, Island, Reef, Depth2) %>%
  summarise(Diversity = vegan::diversity(Quantity, "shannon"))

ggplot(data = Shannon, aes(x = Island, y = Diversity)) +
  geom_violin(aes(fill = Island)) +
  geom_jitter(height = 0, width = 0.0001) +
  geom_boxplot(width = 0.1) +
  theme_minimal() +
  theme(legend.position = "",
        plot.title = element_text(size = 10, colour = "gray")) +
  guides(colour = "none") +
  ggtitle("Fish diversity") +
  labs(x = "", y = "Shannon (H)")

# ggsave("figs/ltem_region_index_comparassion.png", width = 8.5, height = 4.5, dpi=1000)

