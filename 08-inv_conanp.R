# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl) 
library(ggplot2)
library(dplyr)
library(tidytext)
library(lubridate)
library(tidyr)


# Load data ---------------------------------------------------------------

inv23 <- read_xlsx("data/raw/Monitoreo Feb-Mar SS.xlsx", sheet = "Invertebrados") |> 
  rename(Date = Fecha, Species = Especie, Quantity = Total, Island =Isla,
         Reef = Sitio, Depth = Profundidad, Transect = Transecto,
         TrophicGroup = GF, TrophicLevel = NT) |> 
  separate(Date, into = c("Year", "Month", "Day"), sep = "-") |>
  filter(!is.na(Species))
  

inv <- read_xlsx("data/raw/Base de datos Arrecifal PNR.xlsx", sheet = "Invertebrados") |>  #  Agregar fecha, corregir nombres, establecer estaciones
  rename(Date = Fecha, Species = Especie, Quantity = Total, Island =Isla,
         Reef = Sitio, Depth = Profundidad, Transect = Transecto,
         TrophicGroup = GF, TrophicLevel = NT) |> 
  
  separate(Date, into = c("Year", "Month", "Day"), sep = "-") |>
  filter(!is.na(Species))|>
  mutate (Reef = recode(Reef, "Roca_Partida" = "ROCA_PARTIDA", 
                        "El_Boiler" = "SAN_BENEDICTO_BOILER",
                        "El_Canon" =  "SAN_BENEDICTO_CANION",
                        "Las_Cuevitas" = "SAN_BENEDICTO_CUEVITAS",
                        "El_Fondeadero" = "SAN_BENEDICTO_FONDEADERO",
                        "Cabo_Pearce" = "SOCORRO_CABO_PEARCE_NORTE",
                        "Punta_Tosca" = "SOCORRO_PUNTA_TOSCA",
                        "Roca_O_Neal" = "SOCORRO_ROCA_ONEIL")) |> 
  mutate (Island = recode(Island, "San_Benedicto" = "San Benedicto",
                          "Roca_Partida"  = "Roca Partida" )) 

inv_merge <- bind_rows(inv, inv23)

inv_conanp <- inv_merge |>
  mutate(Month = as.numeric(Month)) |> 
  mutate(Year = as.numeric(Year)) |>
  mutate(Day = as.numeric(Day)) |> 
  mutate(Season = case_when(
    Month == 2 ~ "Winter",
    Month == 1 ~ "Winter",
    Month == 3 | Month == 4 | Month == 5 ~ "Spring",
    Month == 10 | Month == 11 | Month == 12 ~ "Autumn",
    TRUE ~ "Other")) |> 
  mutate(Genus = sub("^(\\w+).*", "\\1", Species)) |> # Agregar Género y Taxa2
  mutate(Taxa2 = case_when(
    Genus == "Acanthaster" | Genus == "Asteropsis"|Genus == "Mithrodia" ~ "Asteroidea",
    Genus == "Pentaceraster" | Genus ==  "Linckia" |Genus == "Leiaster"~ "Asteroidea",
    Genus == "Conus" |Genus == "Hexaplex" ~ "Caenogastropoda",
    Genus == "Diadema" | Genus ==  "Echinometra"| Genus == "Eucidaris" | Genus == "Tripneustes" ~ "Echinoidea",
    Genus == "Toxopneustes"  | Genus == "Arbacia" | Genus == "Centrostephaunus"~ "Echinoidea",
    Genus == "Doriprismatica" ~ "Heterobranchia",
    Genus == "Euapta" |  Genus == "Holothuria" |Genus == "Isostichopus" ~ "Holothuroidea",
    Genus == "Hyotissa" | Genus == "Ostrea" | Genus == "Spondylus" ~ "Pteriomorphia",
    Genus == "Octopus" ~ "Coleoidea",
    Genus == "Panulirus" | Genus == "Scyllarides" ~ "Pleocyemata",
    Genus == "Strombus" | Genus ==  "Turbo"  ~ "Gastropoda " ,
    TRUE ~ "Other")) |> 
  mutate(Phylum = case_when(
    Taxa2 ==  "Asteroidea"| Taxa2 =="Echinoidea" | Taxa2== "Holothuroidea" ~ "Echinodermata" ,
    Taxa2 == "Caenogastropoda" |Taxa2 == "Gastropoda "| Taxa2 == "Coleoidea" | Taxa2 == "Pteriomorphia" | Taxa2 == "Heterobranchia" ~ "Mollusca" ,
    Taxa2 == "Pleocyemata" ~ "Crustacea",  
    TRUE ~ "Other")) |> 
  mutate(Depth2 = case_when( # Determinar profundidad superficial y profunda
    Depth <  15 ~ "Shallow" ,
    Depth >=  15 ~ "Deep" ,
    TRUE ~ "Other"
  )) |> 
  mutate(
    Season = factor(Season, levels = c("Winter", "Spring","Summer", "Autumn")),
    # Year = factor(Year, levels = c(2006, 2013, 2014, 2017, 2018, 2019, 2020, 2021, 2022, 2023)),
    Island = factor(Island, levels = c("Socorro", "San Benedicto", "Roca Partida", "Clarion" ),
                    labels = c("Socorro", "San Benedicto", "Roca Partida" , "Clarión"))
  )

# saveRDS(inv_conanp, "data/correct/CONANP_PNR_Invertebrates_historic.RDS")

# Abundance --------------

(abun <- inv_conanp |> 
    filter(Taxa2 %in% c("Asteroidea", "Echinoidea")) |>
    group_by(Year, Season, Island, Reef, Depth2, Taxa2, Species) |> 
    summarise (Abundance = sum(Quantity, na.rm = T)) |>
    group_by(Island, Taxa2) |> 
    summarise(Abundance = mean(Abundance)) |>
    ggplot(aes(x= Island, y = Abundance, col= Taxa2)) +
    geom_point() +
    # geom_boxplot()+
    # geom_jitter()+
    # coord_flip()+
    facet_wrap(~Taxa2, scales="free_y") +
    scale_x_reordered() +
    scale_fill_manual(values=c("#a30808", "#b57560",  "#258f4e")) +
    labs(y = "Abundance", x="Island") +
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

(abun4 <- inv_conanp |> 
    filter(Taxa2 %in% c("Asteroidea", "Echinoidea")) |>
    group_by(Year, Season, Island, Depth2, Taxa2, Species) |> 
    mutate(Year = factor(Year, levels = c(2006, 2013, 2014, 2017, 2018, 2019, 2020, 2021, 2022, 2023))) |> 
    summarise (Abundance = sum(Quantity, na.rm = T)) |>
    group_by(Year, Taxa2) |> 
    summarise(Abundance = mean(Abundance)) |>
    ggplot(aes(x= Year, y = Abundance, col= Taxa2)) +
    geom_point() +
    # geom_boxplot()+
    facet_wrap(~Taxa2, scales="free_y") +
    scale_x_reordered() +
    scale_fill_manual(values=c("#a30808", "#b57560",  "#258f4e")) +
    labs(y = "Abundance", x="Year") +
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

ggsave("figs/inv_conanp_island.png", width = 8.5, height = 4.5, dpi=1000)

inv_conanp |> 
  filter(Year >= 2017) |>
  group_by(Year, Island) |>  
  mutate(Density =(Quantity / (Area*100))) |>
  summarise (Density = sum(Density, na.rm = T)) |>
  group_by(Year) |>
  summarise(Density = mean(Density, na.rm = TRUE)) |> 
  ggplot(aes(x=as.numeric(Year), y= Density))+
  geom_smooth()

unique(inv_conanp$Year)

# Density-------------
# mutate(Density =(Quantity / (Area*100))) |>
  # mutate(Density =(Quantity / (Area))) |>

(density <- inv_conanp |> 
   filter(Taxa2 %in% c("Asteroidea", "Echinoidea")) |>
   group_by(Year, Season, Island, Depth2, Taxa2, Species) |> 
   mutate(Year = factor(Year, levels = c(2006, 2013, 2014, 2017, 2018, 2019, 2020, 2021, 2022, 2023))) |> 
   mutate(Density =(Quantity / (Area))) |>
   summarise (Density = sum(Density, na.rm = T)) |>
   group_by(Year, Island, Taxa2) |> 
   summarise(Density = mean(Density)) |>
   ggplot(aes(x= Year, y = Density, col= Taxa2)) +
   geom_point() +
   geom_boxplot()+
   facet_wrap(~Taxa2, scales="free_y") +
   scale_x_reordered() +
   scale_fill_manual(values=c("#a30808", "#b57560",  "#258f4e")) +
   labs(y = "Density", x="Year") +
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

ggsave("figs/inv_conanp_density.png", width = 8.5, height = 4.5, dpi=1000)

# Richness  ------------------------------------------------------

(richess <- inv_conanp |>
   filter(!is.na(Species))|>
   filter(Taxa2 %in% c("Asteroidea", "Echinoidea")) |>
   mutate(Year = factor(Year, levels = c(2006, 2013, 2014, 2017, 2018, 2019, 2020, 2021, 2022, 2023))) |> 
   group_by(Year, Taxa2) |>
   summarise(Richness = n_distinct(Species)) |>
   
   ggplot(aes(x = Year, y = Richness, col = Taxa2)) +
   geom_point() +
   facet_wrap(~Taxa2, scales = "free_y") +
   scale_fill_manual(values=c("#a30808", "#b57560",  "#258f4e")) +
   labs(x = "Year", y = "Richness") +
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


ggsave("figs/inv_conanp_richness.png", width = 8.5, height = 4.5, dpi=1000)
