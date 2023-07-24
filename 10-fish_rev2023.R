# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(tidytext)
library(writexl)

# Load data ---------------------------------------------------------------

# # Corregir spp

# ltem<- readRDS("data/raw/ltem_revilla_2023_integrada.RDS") |>
#   mutate(
#     Species= ifelse(Species=="Acanthaster solaris","Acanthaster planci" , Species),
#     Species=case_when(Species=="Ostracion meleagris meleagris"~"Ostracion meleagris",
#                       Species=="Kyphosus analogus"~"Kyphosus vaigiensis",
#                       Species=="Chilomicterus reticulatus"~"Chilomycterus reticulatus",
#                       Species=="Cirrhithus rivulatus"~"Cirrhitus rivulatus",
#                       Species=="Aulostomo chinensis"~"Aulostomus chinensis",
#                       Species=="Sargocentron suborbitalis"~"Sargocentron suborbitale",
#                       Species=="Prognathodes falcifert"~"Prognathodes falcifer",
#                       TRUE~Species))
# 
# # Estandarizar sitios
# 
# sitios <- read_xlsx("data/lists/Coordenadas sitios linea base.xlsx") |>
#   mutate(Sinonimias = case_when(
#     Sinonimias == "CLARION_ROCA_CUERVO" ~ "CLARION_PUNTA_ESTE",
#     TRUE ~ Sinonimias))


rev2023 <- readRDS("data/ltem_revillagigedo_2023_12072023.RDS")  |> 
  mutate(Area = ifelse(IDMonitor == "PBS" & Reef %in% c("SOCORRO_BARQUITO", "SOCORRO_ACUARIO") & Depth == 10, 200, #Añadir area
                       ifelse(IDMonitor == "PBS", 250, Area))) |>  
  mutate(Area = ifelse(IDMonitor == "OHG" & Reef == c("CLARION_BARBAS_DE_BIN_LADEN") & Transect == 3, 5, Transect))|>  
  mutate(IDSize = case_when(
                         Size %in% 1:5 ~ "A",
                         Size %in% 6:10 ~ "B",
                         Size %in% 11:15 ~ "C",
                         Size %in% 16:20 ~ "D",
                         Size %in% 21:25 ~ "E",
                         Size %in% 26:30 ~ "F",
                         Size %in% 31:35 ~ "G",
                         Size %in% 36:40 ~ "H",
                         Size %in% 41:45 ~ "I",
                         Size %in% 46:50 ~ "J",
                         Size %in% 51:55 ~ "K",
                         Size %in% 56:60 ~ "L",
                         Size %in% 61:65 ~ "M",
                         Size %in% 66:70 ~ "N",
                         Size %in% 71:75 ~ "O",
                         Size %in% 76:80 ~ "P",
                         Size %in% 81:85 ~ "Q",
                         Size %in% 86:90 ~ "R",
                         Size %in% 91:95 ~ "S",
                         Size %in% 96:100 ~ "T",
                         Size %in% 101:105 ~ "U",
                         Size %in% 106:110 ~ "V",
                         Size %in% 111:115 ~ "W",
                         Size %in% 116:120 ~ "X",
                         Size %in% 121:125 ~ "Y",
                         Size %in% 126:130 ~ "Z",
                         Size %in% 131:135 ~ "A'",
                         Size %in% 136:140 ~ "B'",
                         Size %in% 141:145 ~ "C'",
                         Size %in% 146:150 ~ "D'",
                         Size %in% 151:155 ~ "E'",
                         Size %in% 156:160 ~ "F'",
                         Size %in% 161:165 ~ "G'",
                         Size %in% 166:170 ~ "H'",
                         Size %in% 171:175 ~ "I'",
                         Size %in% 176:180 ~ "J'",
                         Size %in% 181:185 ~ "K'",
                         Size %in% 186:190 ~ "L'",
                         Size %in% 191:195 ~ "M'",
                         Size %in% 196:200 ~ "N'",
                         Size %in% 201:205 ~ "O'",
                         Size %in% 206:210 ~ "P'",
                         Size %in% 211:215 ~ "Q'",
                         Size %in% 216:220 ~ "R'",
                         Size %in% 221:225 ~ "S'",
                         Size %in% 226:230 ~ "T'",
                         Size %in% 231:235 ~ "U'",
                         Size %in% 236:240 ~ "V'",
                         Size %in% 241:245 ~ "W'",
                         Size %in% 246:250 ~ "X'",
                         TRUE ~ ""
                       )) |> #Añadir IDSize
  mutate(Day = case_when(
    IDMonitor == "OHG" & Reef == "CLARION_CALETA_NORTE"  ~ 23, 
    TRUE ~ Day )) |>  # Corregir fecha
  mutate(Island = case_when( #Añadir columna Island
      Reef %in% c("CLARION_BARBAS_DE_BIN_LADEN", "CLARION_CALETA_NORTE", "CLARION_HERRADURA_PUNTA_FARO", "CLARION_ISLOTE_SURESTE", "CLARION_PUNTA_ESTE", "CLARION_PUNTA_SUROESTE", "CLARION_ROCA_CUERVO") ~ "Clarion",
      Reef %in% c("ROCA_PARTIDA") ~ "Roca Partida",
      Reef %in% c("SAN_BENEDICTO_BOILER", "SAN_BENEDICTO_CANION", "SAN_BENEDICTO_CUEVITAS", "SAN_BENEDICTO_PARTE_NORTE", "SAN_BENEDICTO_ZOOLOGICO") ~ "San Benedicto",
      Reef %in% c("SOCORRO_ACUARIO", "SOCORRO_BARQUITO", "SOCORRO_BRAULIA", "SOCORRO_CABO_PEARCE_SUR", "SOCORRO_PUNTA_NORTE", "SOCORRO_PUNTA_TOSCA", "SOCORRO_VARGAS_LOZANO") ~ "Socorro",
      TRUE ~ ""
    )) |> 
  mutate (Region = recode(Region, "REVILLAGIGEDO" = "Revillagigedo")) |> 
  mutate(Season = case_when(
    Month == 3 | Month == 4 | Month == 5 ~ "Spring",
    TRUE ~ "Other"),
    TrophicGroup = factor(TrophicGroup, 
                          levels = c("Piscivoro", 
                                     "Carnivoro", 
                                     "Herbivoro", 
                                     "Zooplanctivoro"))) |> 
  mutate(Depth2 = case_when(
    Depth < 15 ~ "Shallow",
    Depth >= 15 ~ "Deep",
    TRUE ~ "Other")) 

# saveRDS(rev2023, "data/correct/PN_Revillagigedo_2023.RDS")

fish <- rev2023 |>
  filter(Label=="PEC")|> 
  mutate(A_ord = as.numeric(A_ord), 
         B_pen= as.numeric(B_pen),
         Quantity = as.numeric(Quantity),
         Size=as.numeric(Size),
         Area= as.numeric(Area),
         Month= as.numeric(Month),
         Biomass = (Quantity * A_ord* (Size^B_pen))/(Area * 100)) |> 
  filter(!is.na(Biomass)) 


ltem <- readRDS("data/LTEM_fish_14022023.RDS")  |> 
  filter(Label=="PEC") 
  
  
# Trophic Group Biomass Composition ---------------------------------------

fish |> 
   filter(!is.na(TrophicGroup)) |>
   mutate(Island = factor(Island,
                          levels = c("Socorro", "San Benedicto", "Roca Partida", "Clarion"),
                          labels = c("Socorro", "San Benedicto", "Roca Partida", "Clarion"))) |> 
   group_by(Island, Transect, Depth2, TrophicGroup, Species) |> 
   summarise(Biomass = sum(Biomass)) |>  
   group_by(Island, TrophicGroup) |>  
   summarise(Biomass = mean(Biomass)) |> 
   ggplot(aes(x=Island , y=Biomass, fill=TrophicGroup)) +
   geom_bar(position = "fill", stat="identity", col="black")+
   scale_fill_manual(values=c("#a30808", "#b57560", "#114a06", "#258f4e")) +
   # facet_wrap(~)+
   labs(x = "", y = "Biomass (Ton/ha)", fill = "Trophic Group") +
   theme_classic()

ggsave("figs/fish_rev_2023.png", width = 8.5, height = 4.5, dpi=1000)

# Biomass Region Comparison -----------------------------------------------

(p0 <- fish |>
   filter(!is.na(Biomass)) |>
   mutate(Region = "Revillagigedo") |> 
   mutate(Region = ifelse(Island == "Roca Partida", "Roca Partida", Region)) |> 
   mutate(Region = factor(Region, levels = c("Revillagigedo", "Roca Partida"))) |>
   filter(!is.na(TrophicGroup), Region %in% c("Revillagigedo", "Roca Partida")) |>
   mutate(Year = factor(Year, levels = c(2023))) |> 
   group_by(Region, Reef, Depth2, TrophicGroup, Species) |>
   summarise(Biomass = sum(Biomass)) |>
   group_by(Region, TrophicGroup) |>
   summarise(Biomass = mean(Biomass)) |>
   ggplot(aes(x = Region , y = Biomass, fill = TrophicGroup)) +
   geom_bar(position = "fill", stat = "identity", col = "black") +
   scale_fill_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e"), guide = "none") +
   # facet_wrap(~Region)+
   labs(x = "", y = "Biomass (Ton/ha)", fill = " ") +
   theme_classic())

ggsave("figs/fish_2023_rev.png", width = 8.5, height = 4.5, dpi=1000)

(p1 <- fish |>
   filter(!is.na(Biomass)) |>
   mutate(Region = "Revillagigedo") |> 
   mutate(Region = ifelse(Island == "Roca Partida", "Roca Partida", Region)) |> 
   mutate(Region = factor(Region, levels = c("Revillagigedo", "Roca Partida"))) |>
   filter(!is.na(TrophicGroup), Region %in% c("Revillagigedo", "Roca Partida")) |>
   mutate(Year = factor(Year, levels = c(2023))) |> 
   group_by(Year, Region, Reef, Depth2, TrophicGroup, Species) |>
   summarise(Biomass = sum(Biomass)) |>
   group_by(Year, Region, TrophicGroup) |>
   summarise(Biomass = mean(Biomass)) |>
   ggplot(aes(x = Year , y = Biomass, fill = TrophicGroup)) +
   geom_bar(position = "stack", stat = "identity", col = "black") +
   scale_fill_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e")) +
   facet_wrap(~Region)+
   labs(x = "Year", y = "Biomass (Ton/ha)", fill = " ") +
   theme_classic())

ggsave("figs/fish_2023_rev.png", width = 8.5, height = 4.5, dpi=1000)

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


biomass_plot <- patchwork::wrap_plots(p0, p2)

biomass_plot

ggsave("figs/pec_2023_biomass.png", width = 8.5, height = 4.5, dpi=1000)


bio <-  patchwork::wrap_plots(p, p1)
bio

ggsave("figs/pec_LTEM_biomass.png", width = 8.5, height = 4.5, dpi=1000)
       
       