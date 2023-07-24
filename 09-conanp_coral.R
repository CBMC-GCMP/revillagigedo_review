# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(tidytext)


# Load data ---------------------------------------------------------------

coral <- read_xlsx("data/raw/Base de datos Arrecifal PNR.xlsx", sheet = "Estado coralino") |> 
  rename(Species = Especie, Island = Isla, Year = Año, Reef = Sitio, Depth = Profundidad, Transect = Transecto, Date = Fecha) |>
  mutate(Quantity = rep(1, nrow(coral))) |> 
  mutate(Species = case_when(Codigo_E == "parn" ~ "Porites arnaudi",
                             Codigo_E == "pca" ~ "Pocillopora capitata",
                             Codigo_E == "pcl" ~ "Pavona clavus",
                             Codigo_E == "pda" ~ "Pocillopora damicornis",
                             Codigo_E == "pdu" ~ "Pavona duerdeni",
                             Codigo_E == "pey" ~ "Pocillopora eydouxi",
                             Codigo_E == "pgi" ~ "Pavona gigantea",
                             Codigo_E == "plo" ~ "Porites lobata",
                             Codigo_E == "plu" ~ "Porites lutea",
                             Codigo_E == "pme" ~ "Pocillopora meandrina",
                             Codigo_E == "pmi" ~ "Pavona minuta",
                             Codigo_E == "poc" ~ "Pocillopora sp.",
                             Codigo_E == "por" ~ "Porcellanidae",
                             Codigo_E == "ppan" ~ "Porites panamensis",
                             Codigo_E == "psa" ~ "Psammocora sp.",
                             Codigo_E == "pva" ~ "Pavona varians",
                             Codigo_E == "pve" ~ "Pocillopora verrucosa",
                             Codigo_E == "pwo" ~ "Pocillopora woodjonesi",
                             TRUE ~ Species)) |> 
  mutate(Genus = sub("^(\\w+).*", "\\1", Species)) |> 
  mutate(Taxa2 = case_when(
    Genus == "Pocillopora" | Genus == "Pavona" |Genus == "Porites" | Genus == "Psammocora" ~ "Hexacorallia",)) |>
  mutate(Reef = case_when(`Código Sitio` == "cv" ~ "SAN_BENEDICTO_CUEVITAS",
                          `Código Sitio` == "bc" ~ "Bahia_Cornwallis",
                          `Código Sitio` == "bvl" ~ "Bahia_Vargas_Lozano",
                          `Código Sitio` == "bvle" ~ "Bahia_Vargas_Lozano_este",
                          `Código Sitio` == "bvlo" ~ "Bahia_Vargas_Lozano_oeste",
                          `Código Sitio` == "cb" ~ "Caleta_el_Barquito",
                          `Código Sitio` == "cp" ~ "SOCORRO_CABO_PEARCE_NORTE",
                          `Código Sitio` == "eb" ~ "SAN_BENEDICTO_BOILER",
                          `Código Sitio` == "lb" ~ "La_Braulia",
                          `Código Sitio` == "pc" ~ "Punta_Cornwallis",
                          `Código Sitio` == "pt" ~ "SOCORRO_PUNTA_TOSCA",
                          `Código Sitio` == "ro" ~ "SOCORRO_ROCA_ONEIL",
                          `Código Sitio` == "rp" ~ "ROCA_PARTIDA",
                          TRUE ~ Reef)) |> 
  mutate(Island = case_when(
    Reef == "ROCA_PARTIDA" ~ "Roca Partida",
    Reef %in% c("SAN_BENEDICTO_BOILER", "SAN_BENEDICTO_CANION", "SAN_BENEDICTO_FONDEADERO", "SAN_BENEDICTO_CUEVITAS") ~ "San Benedicto",
    Reef %in% c("SOCORRO_CABO_PEARCE_NORTE", "SOCORRO_PUNTA_TOSCA", "SOCORRO_ROCA_ONEIL") ~ "Socorro",
    Reef %in% c("Bahia_Cornwallis", "Bahia_Vargas_Lozano", "Bahia_Vargas_Lozano_este", "Bahia_Vargas_Lozano_oeste") ~ "Socorro",
    Reef %in% c("Caleta_el_Barquito", "La_Braulia", "Punta_Cornwallis") ~ "Socorro",
    TRUE ~ "Other" ))  |> 
  mutate(Depth2 = case_when(
    Depth <  15 ~ "Shallow" ,
    Depth >=  15 ~ "Deep" ,
    TRUE ~ "Other" )) |> 
  separate(Date, into = c("Year", "Month", "Day"), sep = "-") |>
  mutate(Season = case_when(
    Month == "12" | Month == "02" ~ "Winter",
    Month == "05" ~ "Spring",
    Month == "09" ~ "Autumn",
    TRUE ~ "Other"
  ))|> 
  mutate(Area= case_when(Year== 2019 ~"75",
                         Year== 2018 ~"60",
                         Year== 2017 ~"60",
                           TRUE~"Other"))

# saveRDS(coral, "data/correct/CONANP_PNR_Estado_Coralino_historic.RDS")

# Abundances  -----------------
coral |> 
  group_by(Year, Island) |>  
  mutate(Area= as.numeric(Area), Year= as.numeric(Year)) |> 
  summarise (Abundance = sum(Quantity, na.rm = T)) |>
  group_by(Year) |>
  summarise(Abundance = mean(Abundance, na.rm = TRUE)) |> 
  ggplot(aes(x=Year, y= Abundance))+
  geom_smooth()


(abun <- coral |> 
   filter(!is.na(Genus), !Genus %in% c("Porcellanidae", "Psammocora")) |> 
   group_by(Year, Season, Island, Depth2, Taxa2, Genus, Species) |>
   # group_by(Island, Year, Genus) |>
   mutate(Area= as.numeric(Area)) |> 
   summarise(Abundance = sum(Quantity)) |>
   group_by(Year, Genus) |> 
   summarise(Abundance = mean(Abundance)) |>
   ggplot(aes(x= Year, y = Abundance, col= Genus)) +
   geom_point() +
   # geom_jitter()+
   facet_wrap(~Genus, scales="free_y") +
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

ggsave("figs/coral_conanp_island.png", width = 8.5, height = 4.5, dpi=1000)


(abun <- coral |> 
    filter(!is.na(Genus), !Genus %in% c("Porcellanidae", "Psammocora")) |> 
    group_by(Year, Season, Island, Depth2, Taxa2, Genus, Species) |>
    # group_by(Island, Year, Genus) |>
    mutate(Area= as.numeric(Area)) |> 
    summarise(Abundance = sum(Quantity)) |>
    group_by(Year, Taxa2) |> 
    summarise(Abundance = mean(Abundance)) |>
    ggplot(aes(x= Year, y = Abundance, col= Taxa2)) +
    geom_point() +
    # geom_jitter()+
    # facet_wrap(~Taxa2, scales="free_y") +
    scale_x_reordered() +
    scale_fill_manual(values=c("#a30808", "#b57560",  "#258f4e")) +
    labs(y = "Abundance", x="") +
    theme_classic()+
    theme(axis.text.y = element_text(face= "plain", size=8),
          axis.text.x=element_text(face = "italic", size=10),
          plot.title = element_text(hjust=0.5, size=14, face="plain", color = "gray20"),
          plot.title.position = "plot",
          legend.position = "",
          # legend.text = element_text(face = "italic"),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold.italic")
    )
)

ggsave("figs/coral_conanp_island.png", width = 8.5, height = 4.5, dpi=1000)
# Density -----------------

coral |> 
  group_by(Year, Island) |>  
  mutate(Area= as.numeric(Area), Year= as.numeric(Year)) |> 
  mutate(Density =(Quantity /(Area))) |>
  summarise (Density = sum(Density, na.rm = T)) |>
  group_by(Year) |>
  summarise(Density = mean(Density, na.rm = TRUE)) |> 
  ggplot(aes(x=Year, y= Density))+
  geom_smooth()

# Richness  ------------------------------------------------------

(richess <- coral |>
   filter(!is.na(Species))|>
   filter(!is.na(Genus), !Genus %in% c("Porcellanidae", "Psammocora")) |> 
   filter(Taxa2== "Hexacorallia") |>
   group_by(Year, Taxa2, Genus) |>
   summarise(Richness = n_distinct(Species)) |>
   
   ggplot(aes(x = Year, y = Richness, col = Genus)) +
   geom_point() +
   facet_wrap(~Genus, scales = "free_y") +
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


ggsave("figs/coral_conanp_richness.png", width = 8.5, height = 4.5, dpi=1000)


#  Cobertura --------------

cobertura <- read_xlsx("data/Base de datos Arrecifal PNR.xlsx", sheet = "Cobertura")|> 
  rename(Island = Isla, Year = Año, Reef = Sitio, Depth = Profundidad, Transect = Transecto, Date = Fecha,
         Porites = PO, Psamocora = PS, Pocillopora = POC, Pavona = PA, Leptogorgia = LE, Pacifigorgia = PAC,	Muricea = MU,
         Eugorgia = EU,Tubastrea = TUB) |> 
  mutate (Reef = recode(Reef, "Roca_Partida" = "ROCA_PARTIDA", 
                        "El_Boiler" = "SAN_BENEDICTO_BOILER",
                        "El_Canon" =  "SAN_BENEDICTO_CANION",
                        "Las_Cuevitas" = "SAN_BENEDICTO_CUEVITAS",
                        "El_Fondeadero" = "SAN_BENEDICTO_FONDEADERO",
                        "Cabo_Pearce" = "SOCORRO_CABO_PEARCE_NORTE",
                        "Punta_Tosca" = "SOCORRO_PUNTA_TOSCA",
                        "Roca_O_Neal" = "SOCORRO_ROCA_ONEIL")) |> 
  mutate (Island = recode(Island, "San_Benedicto" = "San Benedicto",
                          "Roca_Partida"  = "Roca Partida" )) |> 
  mutate(Depth2 = case_when(
    Depth <  15 ~ "Shallow" ,
    Depth >=  15 ~ "Deep" ,
    TRUE ~ "Other" )) 

coberturas <- cobertura|>
  pivot_longer(cols = c(Porites, Pocillopora, Psamocora, Pavona, Leptogorgia, Pacifigorgia, Muricea, Eugorgia, Tubastrea),
               names_to = "Genus",
               values_to = "Coverage") |> 
  mutate(Taxa2 = case_when(
    Genus == "Pocillopora" | Genus == "Pavona" |Genus == "Porites" | Genus == "Psammocora" | Genus == "Tubastraea" ~ "Hexacorallia",
    Genus == "Eugorgia" | Genus == "Leptogorgia" |Genus == "Muricea" | Genus == "Pacifigorgia" ~ "Octocorallia")) 
  

(abun <- coberturas |> 
    filter(!is.na(Year), !is.na(Taxa2))|>
    group_by(Year, Island, Taxa2, Genus)) |> 
    mutate(Area= as.numeric(Area)) |> 
    summarise (Abundance = sum(Coverage, na.rm = T)) |>
    group_by(Year, Taxa2) |> 
    summarise(Abundance = mean(Abundance)) |>
    ggplot(aes(x= Year, y = Abundance, col= Taxa2)) +
    geom_point() +
    # geom_jitter()+
    # coord_flip()+
    facet_wrap(~Taxa2, scales="free_y") +
    scale_x_reordered() +
    scale_fill_manual(values=c("#a30808", "#b57560",  "#258f4e")) +
    labs(y = "Abundance", x="Year", title= "Coral Abundance") +
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

coberturas |> 
  filter(!is.na(Year), !is.na(Taxa2))|>
  group_by(Year, Island) |>  
  mutate(Area= as.numeric(Area)) |> 
  mutate(Density =(Coverage / (Area*100))) |>
  summarise (Density = sum(Density, na.rm = T)) |>
  group_by(Year) |>
  summarise(Density = mean(Density, na.rm = TRUE)) |> 
  ggplot(aes(x=as.numeric(Year), y= Density))+
  geom_smooth()

