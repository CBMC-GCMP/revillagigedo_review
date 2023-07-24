# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(tidytext)
library(writexl)
library(purrr)

# Load data ---------------------------------------------------------------

# Base de peces CONANP
# 
# fish <- read_xlsx("data/raw/Base de datos Arrecifal PNR.xlsx", sheet = "Peces") |>
#   select(-c(18:44)) |>
#   slice(-1) |>
#   # rename(`Constancias de Crecimiento` = a,  `...46`  = b,`...47`  = Biomasa, `...48` = Densidad, `...49` = NT,
#   #        `...50` = GF, `...51` = g/m2, `...52` = kg/ha, `...53` = ton/ha, `...54` = Corriente, `...55` = Direccion, `...56` =Comentarios )
# rename(`Constancias de Crecimiento` = a,  `46`  = b,`47`  = Biomasa, `48` = Densidad, `49` = NT,
#        `50` = GF, `51` = g/m2, `52` = kg/ha, `53` = ton/ha, `54` = Corriente, `55` = Direccion, `56` =Comentarios )
  # head(fish)

fish <- read_xlsx("data/fish_conanp.xlsx") |> 
  rename(Reef = Sitio, Island = Isla, Year = Año, Date = Fecha, Depth = Profundidad,
         Species = Especie, Transect = Transecto, Biomass = Biomasa, Quantity = Total) |>
  filter(!Area == "250") |> 
  separate(Date, into = c("Year", "Month", "Day"), sep = "-") |>
  mutate(Genus = sub("^(\\w+).*", "\\1", Species)) |> 
  mutate(TrophicGroup = case_when(
    Genus %in% c("Acanthurus", "Calotomus") ~ "Herbivoro",
    Genus %in% c("Alphestes", "Aluterus", "Anisotremus", "Arothron", "Balistes") ~ "Carnivoro",
    Genus %in% c("Bodianus", "Cantherhines", "Gymnomuraena", "Heteropriacanthus", "Hippocampus") ~ "Carnivoro",
    Genus %in% c("Aulostomus", "Bothus", "Carangoides") ~ "Piscivoro",
    Genus %in% c("Prionurus", "Scarus", "Stegastes", "Ophioblennius") ~ "Herbivoro",
    Genus %in% c("Nicholsina", "Microspathodon", "Kyphosus", "Holacanthus") ~ "Herbivoro",
    Genus %in% c("Chilomycterus", "Canthigaster", "Cantherhines", "Chaetodon", "Mulloidichthys") ~ "Carnivoro",
    Genus %in% c("Forcipiger", "Euthynnus", "Diodon", "Decapterus", "Cirrhitichthys", "Muraena") ~ "Carnivoro",
    Genus %in% c("Melichthys", "Johnrandallia", "Halichoeres", "Gymnothorax", "Pomacanthus") ~ "Carnivoro",
    Genus %in% c("Ostracion", "Novaculichthys", "Plagiotremus", "Prognathodes", "Serranus") ~ "Carnivoro",
    Genus %in% c("Zanclus", "Xanthichthys", "Narcine", "Myripristis", "Sargocentron") ~ "Carnivoro",
    Genus %in% c("Rypticus", "Sufflamen", "Thalassoma", "Trachinotus", "Scorpaena") ~ "Carnivoro",
    Genus %in% c("Hoplopagrus", "Sarda", "Seriola", "Sphyrna", "Triaenodon") ~ "Piscivoro",
    Genus %in% c("Gnathanodon", "Galeocerdo", "Fistularia", "Elagatis", "Cephalopholis") ~ "Piscivoro",
    Genus %in% c("Dermatolepis", "Cirrhitus", "Carcharhinus", "Caranx") ~ "Piscivoro",
    Genus %in% c("Apogon", "Mobula", "Azurina", "Chromis") ~ "Zooplanctivoro",
    Species %in% c("Lutjanus viridis", "Epinephelus labriformis") ~ "Carnivoro",
    Species %in% c("Lutjanus novemfasciatus", "Lutjanus argentiventris", "Lutjanus aratus") ~ "Piscivoro",
    Species %in% c("Abudefduf troschelii") ~ "Zooplanctivoro",
    TRUE ~ "Other"
  )) |> 
  mutate(Reef = recode(Reef, "Roca_Partida" = "ROCA_PARTIDA",
                       "El_Boiler" = "SAN_BENEDICTO_BOILER",
                       "El_Canon" = "SAN_BENEDICTO_CANION",
                       "Las_Cuevitas" = "SAN_BENEDICTO_CUEVITAS",
                       "El_Fondeadero" = "SAN_BENEDICTO_FONDEADERO",
                       "Cabo_Pearce" = "SOCORRO_CABO_PEARCE_NORTE",
                       "Punta_Tosca" = "SOCORRO_PUNTA_TOSCA",
                       "Roca_O_Neal" = "SOCORRO_ROCA_ONEIL")) |> 
  mutate(Island = recode(Island, "San_Benedicto" = "San Benedicto",
                         "Roca_Partida" = "Roca Partida")) |> 
  mutate(Season = case_when(
    Month == "01" | Month == "02" | Month == "12" ~ "Winter",
    Month == "03" | Month == "04" | Month == "05" ~ "Spring",
    Month == "06" ~ "Summer", 
    Month == "10" ~ "Autumn",
    TRUE ~ "Other"),
    Season= factor(Season, levels=c("Winter", "Spring", "Summer", "Autumn")),
    TrophicGroup = factor(TrophicGroup, 
                          levels = c("Piscivoro", 
                                     "Carnivoro", 
                                     "Herbivoro", 
                                     "Zooplanctivoro"))) |> 
  mutate(Depth2 = case_when(
    Depth < 15 ~ "Shallow",
    Depth >= 15 ~ "Deep",
    TRUE ~ "Other")) |> 
  mutate(Area = recode(Area, "200" = "100",
                       "100" = "100")) |> 
  mutate(Biomass = as.numeric(Biomass),
         Area= as.numeric(Area))



fish23 <- read_xlsx("data/conanp feb-mar 2023.xlsx", sheet = "Peces") |> 
  rename(Reef = Sitio, Island = Isla, Year = Año, Date = Fecha, Depth = Profundidad,
         Species = Especie, Transect = Transecto, Biomass = Biomasa, Quantity = Total) |> 
  separate(Date, into = c("Year", "Month", "Day"), sep = "-") |>
  mutate(Genus = sub("^(\\w+).*", "\\1", Species)) |> 
  mutate(TrophicGroup = case_when(
    Genus %in% c("Acanthurus", "Calotomus") ~ "Herbivoro",
    Genus %in% c("Alphestes", "Aluterus", "Anisotremus", "Arothron", "Balistes") ~ "Carnivoro",
    Genus %in% c("Bodianus", "Cantherhines", "Gymnomuraena", "Heteropriacanthus", "Hippocampus") ~ "Carnivoro",
    Genus %in% c("Aulostomus", "Bothus", "Carangoides") ~ "Piscivoro",
    Genus %in% c("Prionurus", "Scarus", "Stegastes", "Ophioblennius") ~ "Herbivoro",
    Genus %in% c("Nicholsina", "Microspathodon", "Kyphosus", "Holacanthus") ~ "Herbivoro",
    Genus %in% c("Chilomycterus", "Canthigaster", "Cantherhines", "Chaetodon", "Mulloidichthys") ~ "Carnivoro",
    Genus %in% c("Forcipiger", "Euthynnus", "Diodon", "Decapterus", "Cirrhitichthys", "Muraena") ~ "Carnivoro",
    Genus %in% c("Melichthys", "Johnrandallia", "Halichoeres", "Gymnothorax", "Pomacanthus") ~ "Carnivoro",
    Genus %in% c("Ostracion", "Novaculichthys", "Plagiotremus", "Prognathodes", "Serranus") ~ "Carnivoro",
    Genus %in% c("Zanclus", "Xanthichthys", "Narcine", "Myripristis", "Sargocentron") ~ "Carnivoro",
    Genus %in% c("Rypticus", "Sufflamen", "Thalassoma", "Trachinotus", "Scorpaena") ~ "Carnivoro",
    Genus %in% c("Hoplopagrus", "Sarda", "Seriola", "Sphyrna", "Triaenodon") ~ "Piscivoro",
    Genus %in% c("Gnathanodon", "Galeocerdo", "Fistularia", "Elagatis", "Cephalopholis") ~ "Piscivoro",
    Genus %in% c("Dermatolepis", "Cirrhitus", "Carcharhinus", "Caranx") ~ "Piscivoro",
    Genus %in% c("Apogon", "Mobula", "Azurina", "Chromis") ~ "Zooplanctivoro",
    Species %in% c("Lutjanus viridis", "Epinephelus labriformis") ~ "Carnivoro",
    Species %in% c("Lutjanus novemfasciatus", "Lutjanus argentiventris", "Lutjanus aratus") ~ "Piscivoro",
    Species %in% c("Abudefduf troschelii") ~ "Zooplanctivoro",
    TRUE ~ "Other"
  )) |> 
  mutate(Season = case_when(
    Month == "01" | Month == "02" | Month == "12" ~ "Winter",
    Month == "03" | Month == "04" | Month == "05" ~ "Spring",
    Month == "06" ~ "Summer", 
    Month == "10" ~ "Autumn",
    TRUE ~ "Other"),
    Season= factor(Season, levels=c("Winter", "Spring", "Summer", "Autumn")),
    TrophicGroup = factor(TrophicGroup, 
                          levels = c("Piscivoro", 
                                     "Carnivoro", 
                                     "Herbivoro", 
                                     "Zooplanctivoro"))) |> 
  mutate(Depth2 = case_when(
    Depth < 15 ~ "Shallow",
    Depth >= 15 ~ "Deep",
    TRUE ~ "Other")) 



# CBMC's Long Term Ecological Monitoring database (LTEM) 

ltem <- readRDS("data/LTEM_historic_updated_27122022.RDS") |> 
  filter(Label == "PEC") |>   
  mutate(A_ord = as.numeric(A_ord), 
         B_pen= as.numeric(B_pen),
         Quantity = as.numeric(Quantity),
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


# Filtrar especies

ltemspp <- read_xlsx("data/lists/ltem_monitoring_species_2022-12-09.xlsx") |> 
  filter(Label == "PEC")

sppltem <- unique(ltemspp$Species)
sppconanp <- unique(fish$Species)

fish <- fish |> 
  filter(Species %in% sppltem)

speciesQuedan <- unique(fish$Species)
speciesEliminadas <- setdiff(sppconanp, speciesQuedan)

ssp23 <- unique(fish23$Species)

fish23 <- fish23 |> 
  filter(Species %in% sppltem)

sppQuedan <- unique(fish23$Species)
sppEliminadas <- setdiff(ssp23, sppQuedan)


# Merge bases conanp

merge_fish <- bind_rows(fish, fish23)

fish <- merge_fish |>
  mutate(
    Area= as.numeric(Area),
    Year= as.numeric(Year),
    Season = factor(Season, levels = c("Winter", "Spring","Summer", "Autumn")),
    # Year = factor(Year, levels = c(2006, 2013, 2014, 2017, 2018, 2019, 2020, 2021, 2022, 2023)),
    Island = factor(Island, levels = c("Socorro", "San Benedicto", "Roca Partida", "Clarion" ),
                    labels = c("Socorro", "San Benedicto", "Roca Partida" , "Clarión"))
  )

# saveRDS(fish, "data/correct/CONANP_Revillagigedo_fish_historic.RDS")

# Estandarizar valores de biomasa

# biomasa x transecto/ area
# Toneladas/# de transecto
# sumar la biomasa de las spp x transectos 
#  Biomass * Transect/Area
# Area/minimo comun denominador
# x/100

# Biomass = (Quantity * A_ord* (Size^B_pen))/(Area * 100))

fish |> 
  group_by(Year, Island, Reef, Transect, Depth2, TrophicGroup, Species) |>
  # group_by(Year, Island) |>
  # mutate(Biomass = Biomass /((length(unique(Transect)))*(Area*100))) |> 
  mutate(Biomass = Biomass /(Area*100)) |>
  summarise(Biomass=sum(Biomass)) |> 
  group_by(Year, Island) |>
  summarise(Biomass = mean(Biomass, na.rm = TRUE))


# Agrupar y resumir los datos por año

fish |> 
  filter(Year >= 2017) |>
  group_by(Year, Island, Reef, Transect, Depth2, TrophicGroup, Species) |>
  # group_by(Year, Island) |>
  mutate(Biomass = Biomass /(Area*100)) |>
  summarise(Biomass = sum(Biomass, na.rm = TRUE)) |>
  group_by(Year, Island) |>
  summarise(Biomass = mean(Biomass, na.rm = TRUE)) |> 
  ggplot(aes(x=Year, y= Biomass))+
  geom_smooth()

ggsave("figs/pec_conanp.png", width = 8.5, height = 4.5, dpi=1000)
  
# Graficar la biomasa por Isla ------------------

fish |> 
    filter(!is.na(TrophicGroup)) |>
    group_by(Year, Island, Reef, Depth2, TrophicGroup) |>
    # group_by(Island, Reef, TrophicGroup) |>
    mutate(Biomass = Biomass /(Area*100)) |>
    summarise(Biomass=sum(Biomass)) |>  
    group_by(Island, TrophicGroup) |>  
    summarise(Biomass = mean(Biomass)) |> 
    ggplot(aes(x=Island , y=Biomass, fill=TrophicGroup)) +
    geom_bar(position = "fill", stat="identity", col="black")+
    scale_fill_manual(values=c("#a30808", "#b57560", "#114a06", "#258f4e")) +
    # facet_wrap(~)+
    labs(x = "", y = "Biomass (Ton/ha)", fill = "Trophic Group") +
    theme_classic() 

ggsave("figs/pec_gt_conanp.png", width = 8.5, height = 4.5, dpi=1000)

# Graficar la biomasa por año ------------------

fish |> 
  filter(!is.na(Biomass)) |>
  filter(!is.na(TrophicGroup)) |>
  # mutate(Region = "Revillagigedo") |> 
  # mutate(Region = ifelse(Island == "Roca Partida", "Roca Partida", Region)) |> 
  # mutate(Region = factor(Region, levels = c("Revillagigedo", "Roca Partida"))) |>
  group_by(Year, Island, Reef, Depth2, TrophicGroup) |>
  # group_by(Island, Reef, TrophicGroup) |>
  mutate(Biomass = Biomass /(Area*100)) |>
  mutate(Year = factor(Year, levels = c(2012, 2013, 2014, 2017, 2018, 2019, 2020, 2021, 2022, 2023))) |> 
  summarise(Biomass=sum(Biomass)) |>  
  group_by(Year,TrophicGroup) |>  
  summarise(Biomass = mean(Biomass)) |> 
  ggplot(aes(x=Year , y=Biomass, fill=TrophicGroup)) +
  geom_bar(position = "stack", stat="identity", col="black")+
  scale_fill_manual(values=c("#a30808", "#b57560", "#114a06", "#258f4e")) +
  # facet_wrap(~Region)+
  labs(x = "", y = "Biomass (Ton/ha)", fill = "Trophic Group") +
  theme_classic() 

ggsave("figs/pec_gt_year_conanp.png", width = 8.5, height = 4.5, dpi=1000)

# Biomass Region Comparison -----------------------------------------------

(p1 <- fish |>
  mutate(Region = "Revillagigedo") |> 
  mutate(Region = ifelse(Island == "Roca Partida", "Roca Partida", Region)) |> 
  mutate(Region = factor(Region, levels = c("Revillagigedo", "Roca Partida"))) |>
  filter(!is.na(TrophicGroup), Region %in% c("Revillagigedo", "Roca Partida")) |>
  group_by(Year, Region, Reef, Depth2, TrophicGroup) |>
  summarise(Biomass = sum(Biomass)) |>
  group_by(Region, TrophicGroup) |>
  summarise(Biomass = mean(Biomass)) |>
  ggplot(aes(x = Region, y = Biomass, fill = TrophicGroup)) +
  geom_bar(position = "fill", stat = "identity", col = "black") +
  scale_fill_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e"), guide = "none") +
  labs(x = "", y = "Biomass (Ton/ha)", fill = " ") +
  theme_classic())

  # Biomass Region Comparison -----------------------------------------------

(p2 <- ltem |>
    mutate(Region = case_when(
      Island == "Partida Revillagigedo" ~ "Roca Partida",
      TRUE ~ Region )) |>
   mutate(Region = factor(Region, levels = c("Los Cabos", "Islas Marias"))) |>
   filter(!is.na(TrophicGroup), Region %in% c("Los Cabos", "Islas Marias")) |>
   # mutate(Region = factor(Region, levels = c("Revillagigedo", "Roca Partida", "Los Cabos", "Islas Marias"))) |>
   # filter(!is.na(TrophicGroup), Region %in% c("Revillagigedo", "Roca Partida", "Los Cabos", "Islas Marias")) |>
   group_by(Year, Region, Reef, Transect, Depth2, TrophicGroup, Species) |>
   summarise(Biomass = sum(Biomass)) |>
   group_by(Region, TrophicGroup) |>
   summarise(Biomass = mean(Biomass)) |>
   ggplot(aes(x = Region, y = Biomass, fill = TrophicGroup)) +
   geom_bar(position = "fill", stat = "identity", col = "black") +
   scale_fill_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e")) +
   labs(x = "", y = "Biomass (Ton/ha)", fill = "Trophic Group") +
   theme_classic())


biomass_plot <- patchwork::wrap_plots(p1, p2)

biomass_plot

ggsave("figs/pec_gt_biomass.png", width = 8.5, height = 4.5, dpi=1000)


# Garficas por Season
(p3 <- fish |>
    mutate(Region = "Revillagigedo") |> 
    mutate(Region = ifelse(Island == "Roca Partida", "Roca Partida", Region)) |> 
    mutate(Region = factor(Region, levels = c("Revillagigedo", "Roca Partida"))) |>
    filter(!is.na(TrophicGroup), Region %in% c("Revillagigedo", "Roca Partida")) |>
    group_by(Year, Region, Reef,Depth2, TrophicGroup, Season) |>
    summarise(Biomass = sum(Biomass)) |>
    group_by(Region, TrophicGroup, Season) |>
    summarise(Biomass = mean(Biomass)) |>
    ggplot(aes(x = Season, y = Biomass, fill = TrophicGroup)) +
    geom_bar(position = "fill", stat = "identity", col = "black") +
    scale_fill_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e")) +
    facet_wrap(~Region)+
    labs(x = "", y = "Biomass (Ton/ha)", fill = "Trophic Group") +
    theme_classic())

ggsave("figs/pec_season_biomass.png", width = 8.5, height = 4.5, dpi=1000)

(p4 <- ltem |>
    mutate(Region = case_when(
      Island == "Partida Revillagigedo" ~ "Roca Partida",
      TRUE ~ Region )) |>
    mutate(Region = factor(Region, levels = c("Revillagigedo", "Roca Partida"))) |>
    filter(!is.na(TrophicGroup), Region %in% c("Revillagigedo", "Roca Partida", "Los Cabos")) |>
    group_by(Year, Region, Reef, Transect, Depth2, TrophicGroup, Species, Season) |>
    summarise(Biomass = sum(Biomass)) |>
    group_by(Region, TrophicGroup, Season) |>
    summarise(Biomass = mean(Biomass)) |>
    ggplot(aes(x = Season, y = Biomass, fill = TrophicGroup)) +
    geom_bar(position = "fill", stat = "identity", col = "black") +
    scale_fill_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e")) +
    labs(x = "", y = "Biomass (Ton/ha)", fill = "Trophic Group") +
    facet_wrap(~Region)+
    theme_classic())


combined_plot <- p3 + p4
combined_plot

#  Graficas por año 2017 y Season

(p5 <- fish |>
    mutate(Region = "Revillagigedo") |> 
    filter(!is.na(TrophicGroup), Year == 2017) |>
    mutate(Island = factor(Island,
                           levels = c("Socorro", "San Benedicto", "Clarion", "Roca Partida"),
                           labels = c("Socorro", "San Benedicto", "Clarión", "Roca Partida"))) |> 
    group_by(Year, Region, Island, Reef, Transect, Depth2, TrophicGroup, Species, Season) |>
    summarise(Biomass = sum(Biomass)) |>
    group_by(Island, TrophicGroup, Season) |>
    summarise(Biomass = mean(Biomass)) |>
    ggplot(aes(x = Island, y = Biomass, fill = TrophicGroup)) +
    geom_bar(position = "fill", stat = "identity", col = "black") +
    scale_fill_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e"), guide = "none") +
    facet_wrap(~Season)+
    labs(x = "", y = "Biomass (Ton/ha)", fill = " ") +
    theme_classic())

(p6 <- ltem |>
   filter(Year==2017, Region =="Revillagigedo") |> 
    mutate(Island = factor(Island,
                           levels = c("Socorro", "San Benedicto", "Clarion", "Partida Revillagigedo"),
                           labels = c("Socorro", "San Benedicto", "Clarión", "Roca Partida"))) |> 
   group_by(Year, Region, Island, Reef, Transect, Depth2, TrophicGroup, Species, Season) |>
   summarise(Biomass = sum(Biomass)) |>
   group_by(Island, TrophicGroup, Season) |>
   summarise(Biomass = mean(Biomass)) |>
   ggplot(aes(x = Island, y = Biomass, fill = TrophicGroup)) +
   geom_bar(position = "fill", stat = "identity", col = "black") +
   scale_fill_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e")) +
   facet_wrap(~Season)+
   labs(x = "", y = "Biomass (Ton/ha)", fill = "Trophic Group") +
   theme_classic())

combined_plot <- p5 + p6
combined_plot

fish |> 
  # filter(Year == 2017) |> 
  distinct(Month, Season)
