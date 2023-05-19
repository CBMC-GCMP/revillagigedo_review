# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl) 
library(ggplot2)
library(dplyr)
library(tidytext)
library(lubridate)
library(tidyr)


# Load data ---------------------------------------------------------------

inv_ps <- read_xlsx("data/Inver_Pristine.xlsx")  # PCU (Pristine Seas 2016)
pcu <- read_xlsx("data/pcu_converted2.xlsx") # PCU (Pristine Seas 2016).
rev <- readRDS("data/LTEM_extract_Revillagigedo.RDS") # CBMC's ltem Revillagigedo
inv_conanp <- read_xlsx("data/Base de datos Arrecifal PNR.xlsx", sheet = "Invertebrados") # CONANP


inv_ps <- inv_ps |> 
  separate(Date, into=c("Day", "Month"), sep=" ") |> 
  mutate(Month=case_when(Month=="march"~03,
                         Month=="april"~04),
         Year=2016,
         Date= as.Date(paste0(Year, "-",Month,"-", Day), "%Y-%m-%d" ))|> 
  rename(Size = `Test Diameter`, Quantity = Number, Transect = Quadrat) |> 
  filter( !Species %in% c("none","None"), !is.na(Species))|> 
  mutate (Species = recode(Species, "Diadema mexicanum?" = "Diadema mexicanum")) |> 
  mutate (Island = recode(Island, "The Boiler" = "San Benedicto" )) |>
  mutate(Month = as.numeric(Month)) |> 
  mutate(Day = as.numeric(Day)) |> 
  mutate(Season = case_when(
    Month == 3 ~ "Spring",
    Month == 4 ~ "Spring",
    TRUE ~ "Other")) 

pcu <- pcu|> 
  filter (Label == "INV") |>
  separate(Date, into=c("Day", "Month"), sep=" ") |> 
  mutate(Month=case_when(Month=="march"~03,
                         Month=="april"~04),
         Year=2016,
         Date= as.Date(paste0(Year, "-",Month,"-", Day), "%Y-%m-%d" ))|> 
  mutate (Species = recode(Species, "Spirobranchus triquetrus?" = "Spirobranchus triquetrus")) |> 
  mutate (Island = recode(Island, "The Boiler" = "San Benedicto" )) |>
  filter(Label != "NA", Label != "CYA", !is.na(Species)) |> 
  mutate(Season = case_when(
    Month == 3 ~ "Spring",
    Month == 4 ~ "Spring",
    TRUE ~ "Other"))


ltem_inv <- rev |> 
  filter (Label == "INV") |>
  mutate (Island = recode(Island,  "Partida Revillagigedo"  = "Roca Partida" )) |>
  mutate(Season= case_when(Month==2~"Winter",
                           Month==4~"Spring",
                           Month==3~"Spring",
                           Month==11~"Autumn",
                           TRUE~"Other"))


inv_conanp <- inv_conanp |> 
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
                          "Roca_Partida"  = "Roca Partida" )) |>
  mutate(Month = as.numeric(Month)) |> 
  mutate(Year = as.numeric(Year)) |>
  mutate(Day = as.numeric(Day)) |> 
  mutate(Season = case_when(
    Month == 2 ~ "Winter",
    Month == 1 ~ "Winter",
    Month == 3 | Month == 4 | Month == 5 ~ "Spring",
    Month == 10 | Month == 11 | Month == 12 ~ "Autumn",
    TRUE ~ "Other")
  )

inv_ps <- inv_ps |> 
  mutate(Genus = sub("^(\\w+).*", "\\1", Species)) |> 
  mutate(Taxa2 = case_when(
    Genus == "Acanthaster" | Genus == "Mithrodia" ~ "Asteroidea",
    Genus == "Hexaplex" ~ "Caenogastropoda",
    Genus == "Diadema" | Genus ==  "Echinometra"| Genus == "Eucidaris" | Genus == "Tripneustes" ~ "Echinoidea",
    Genus == "Holothuria" |Genus == "Isostichopus" ~ "Holothuroidea",
    Genus ==  "Pinctada" ~ "Bivalvia", 
    Genus ==  "Thais"  ~ "Gastropoda" ,
    TRUE ~ "Other"
  ))

inv_ps <- inv_ps |> 
  mutate(Phylum = case_when(
    Taxa2 ==  "Asteroidea"| Taxa2 =="Echinoidea" | Taxa2== "Holothuroidea" ~ "Echinodermata" ,
    Taxa2 == "Bivalvia"| Taxa2 == "Gastropoda " ~ "Mollusca" ,
    TRUE ~ "Other"
  ))

pcu <- pcu |> 
  mutate(Genus = sub("^(\\w+).*", "\\1", Species)) |> 
  mutate(Taxa2 = case_when(
    Genus == "Aplysina" | Genus == "Hexadella" ~ "Verongimorpha",
    Genus == "Pocillopora" | Genus == "Psammocora" | Genus == "Pavona" |Genus == "Porites"| Genus== "Tubastraea" ~ "Hexacorallia",
    Genus == "Leptogorgia"  ~ "Octocorallia",
    Genus == "Ircinia"  ~ "Keratosa",
    Genus ==  "Clathrina" ~ "Calcinea", 
    Genus ==  "Spirobranchus"  ~ "Sedentaria" ,
    TRUE ~ "Other"
  ))
pcu <- pcu |> 
  mutate(Phylum = case_when(
    Taxa2 == "Verongimorpha" | Taxa2 == "Calcinea"| Taxa2== "Ketarosa" ~ "Porifera",
    Taxa2 == "Hexacorallia" |Taxa2 == "Octocorallia" ~ "Cnidaria",
    Taxa2 ==  "Sedentaria"  ~ "Annelida" ,
    TRUE ~ "Other"
  ))


inv_conanp <- inv_conanp |> 
  mutate(Genus = sub("^(\\w+).*", "\\1", Species)) |> 
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
    TRUE ~ "Other"
  ))

inv_conanp <- inv_conanp |> 
  mutate(Phylum = case_when(
    Taxa2 ==  "Asteroidea"| Taxa2 =="Echinoidea" | Taxa2== "Holothuroidea" ~ "Echinodermata" ,
    Taxa2 == "Caenogastropoda" |Taxa2 == "Gastropoda "| Taxa2 == "Coleoidea" | Taxa2 == "Pteriomorphia" | Taxa2 == "Heterobranchia" ~ "Mollusca" ,
    Taxa2 == "Pleocyemata" ~ "Crustacea",  
    TRUE ~ "Other"
  ))

inv_conanp <- inv_conanp |> 
  mutate(Depth2 = case_when(
    Depth <  15 ~ "Shallow" ,
    Depth >=  15 ~ "Deep" ,
    TRUE ~ "Other"
  ))

inv_ps <- inv_ps |> 
  mutate(Depth2 = case_when(
    Depth <  15 ~ "Shallow" ,
    Depth >=  15 ~ "Deep" ,
    TRUE ~ "Other"
  ))

pcu <- pcu |> 
  mutate(Depth2 = case_when(
    Depth <  15 ~ "Shallow" ,
    Depth >=  15 ~ "Deep" ,
    TRUE ~ "Other"
  ))


inv_ps <- inv_ps |> 
  group_by(Year, Month, Day, Island, Reef, Depth2, Phylum, Taxa2, Genus, Species, Quantity, Season) |> 
  summarise (Abundance = mean(Quantity, na.rm = T))|> 
  group_by(Year, Island, Reef, Depth2, Taxa2, Genus, Species, Season) |> 
  summarise(Abundance = mean(Abundance))
  

pcu <- pcu |> 
  group_by(Year, Month, Day, Island, Reef, Depth2, Phylum, Taxa2, Genus, Species, Count, Season) |> 
  summarise (Abundance = sum(Count, na.rm = T)) |> 
  group_by(Year, Island, Reef, Depth2, Taxa2, Genus, Species, Season) |> 
  summarise(Abundance = mean(Abundance))


ltem_inv <- ltem_inv |> 
  group_by(Year, Month, Day, Island, Reef, Depth2, Phylum, Taxa2, Genus, Species, Quantity, Season) |> 
  summarise (Abundance = sum(Quantity, na.rm = T))|> 
  group_by(Year, Island, Reef, Depth2, Taxa2, Genus, Species, Season) |> 
  summarise(Abundance = mean(Abundance))

inv_conanp <- inv_conanp |> 
  group_by(Year, Month, Day, Island, Reef, Depth2, Phylum, Taxa2, Genus, Species, Quantity, Season) |> 
  summarise (Abundance = sum(Quantity, na.rm = T)) |> 
  group_by(Year, Island, Reef, Depth2, Taxa2, Genus, Species, Season) |> 
  summarise(Abundance = mean(Abundance))

merged_inv <- bind_rows(inv_ps, ltem_inv, inv_conanp, pcu)

inv_mer <- merged_inv |>
  mutate(
    Season = factor(Season, levels = c("Winter", "Spring", "Autumn")),
    Year = factor(Year, levels = c(2006, 2013, 2016, 2017, 2018, 2019, 2020, 2021, 2022)),
    Island = factor(Island, levels = c("Socorro", "San Benedicto", "Clarion", "Roca Partida"),
                    labels = c("Socorro", "San Benedicto", "Clarión", "Roca Partida"))
  )



# Abundances  -----------------

(abun <- inv_mer |> 
   filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
   # filter(Taxa2== "Echinoidea") |>
   group_by(Year, Season, Island, Reef, Depth2, Taxa2, Species) |> 
   summarise (Abundance = sum(Abundance, na.rm = T)) |> 
   group_by(Year, Taxa2) |> 
   summarise(Abundance = mean(Abundance)) |>
   ggplot(aes(x= Year, y = Abundance, col= Taxa2)) +
   geom_jitter()+
   # coord_flip()+
   facet_wrap(~Taxa2, scales="free_y") +
   scale_x_reordered() +
   scale_fill_manual(values=c("#a30808", "#b57560",  "#258f4e")) +
   labs(y = "Abundance", x="Year", title= "Invertebrates Abundance") +
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

# Richness  ------------------------------------------------------

(richess <- inv_mer |>
   filter(!is.na(Species))|>
   filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
   # filter(Taxa2== "Hexacorallia") |>
   group_by(Year, Taxa2) |>
   summarise(Richness = n_distinct(Species)) |>
   
   ggplot(aes(x = Year, y = Richness, col = Taxa2)) +
   geom_point() +
   facet_wrap(~Taxa2, scales = "free_y") +
   scale_fill_manual(values=c("#a30808", "#b57560",  "#258f4e")) +
   labs(x = "Year", y = "Richness", title = "Invertebrates Richness") +
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



# Tendencia Abundancia ----------------------------------------------

merged_inv |> 
  filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea"))|>
  # filter(Taxa2== "Echinoidea") |> 
  group_by(Year, Season, Island, Reef, Depth2, Taxa2, Species) |> 
  summarise(Abundance = sum(Abundance)) |> 
  group_by(Year, Taxa2) |> 
  summarise(Abundance =round(mean(Abundance),0)) |> 
  ggplot(aes(x = Year, y = Abundance, col= Taxa2))+
  geom_point()+
  # geom_line()+
  labs(x = "Year", 
       y = "Abundance")+
  scale_x_continuous(breaks=seq(2005,2023, 2))+
  theme(axis.text.x = element_text(angle=45),
        axis.title =element_text(size=10))+
  #geom_smooth(method = "gam", formula = y ~ s(x, k = 3), method.args = list(family = "poisson"))+
  facet_wrap(~Taxa2)


# Tendencia Riqueza ----------------------------------------------

merged_inv |>
  filter(!is.na(Species)) |>
  # filter(Taxa2== "Echinoidea") |> 
  filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
  group_by(Year, Taxa2) |>
  summarise(Richness = n_distinct(Species)) |>
  group_by(Year, Taxa2) |> 
  summarise(Richness =round(mean(Richness),0)) |> 
  ggplot(aes(x = Year, y = Richness, col = Taxa2))+
  geom_point()+
  # geom_line()+
  labs(x = "Year", 
       y = "Richness")+
  scale_x_continuous(breaks=seq(2005,2023, 2))+
  theme(axis.text.x = element_text(angle=45),
        axis.title =element_text(size=10))+
  #geom_smooth(method = "gam", formula = y ~ s(x, k = 3), method.args = list(family = "poisson"))+
  facet_wrap(~Taxa2)





# Echinoidea by Reef -----------------

(abun <- inv_mer |> 
   filter(Taxa2== "Echinoidea") |>
   group_by(Year, Season, Island, Reef, Taxa2, Species) |> 
   summarise (Abundance = sum(Abundance, na.rm = T)) |> 
   group_by(Year, Island, Taxa2) |> 
   summarise(Abundance = mean(Abundance)) |>
   ggplot(aes(x= Year, y = Abundance, col= Island)) +
   geom_point()+
   # coord_flip()+
   facet_wrap(~Island, scales="free_y") +
   scale_x_reordered() +
   scale_fill_manual(values=c("#a30808", "#b57560", "#114a06", "#258f4e")) +
   labs(y = "Abundance", x="Year", title= "Echinoidea Abundance") +
   # theme_classic()+
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

# Echinoidea Richness  ------------------------------------------------------

(rich_ech <- inv_mer |>
   filter(!is.na(Species), Taxa2== "Echinoidea")|>
   group_by(Year, Island, Taxa2) |>
   summarise(Richness = n_distinct(Species)) |>
   
   ggplot(aes(x = Year, y = Richness, col = Island)) +
   geom_point() +
   facet_wrap(~Island, scales = "free_y") +
   scale_fill_manual(values=c("#a30808", "#b57560", "#114a06", "#258f4e")) +
   labs(x = "Year", y = "Richness", title = "Invertebrates Richness") +
   # theme_classic() +
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



# Tendencia de Abundancia Echinoidea por Isla  ----------------------------------------------

merged_inv |> 
  filter(Taxa2== "Echinoidea") |> 
  group_by(Year, Island, Reef, Taxa2, Genus, Species) |> 
  summarise(Abundance = sum(Abundance)) |> 
  group_by(Year, Island, Genus) |> 
  summarise(Abundance =round(mean(Abundance),0)) |> 
  ggplot(aes(x = Year, y = Abundance, col= Genus))+
  geom_point()+
  # geom_line()+
  labs(x = "Year", 
       y = "Abundance")+
  scale_x_continuous(breaks=seq(2005,2023, 2))+
  theme(axis.text.x = element_text(angle=45),
        axis.title =element_text(size=10))+
  #geom_smooth(method = "gam", formula = y ~ s(x, k = 3), method.args = list(family = "poisson"))+
  facet_wrap(~Island)


# Echinoidea Riqueza por Isla----------------------------------------------

merged_inv |>
  filter(!is.na(Species)) |>
  filter(Taxa2== "Echinoidea") |>
  group_by(Year, Island) |>
  summarise(Richness = n_distinct(Species)) |>
  group_by(Year, Island) |> 
  summarise(Richness =round(mean(Richness),0)) |> 
  ggplot(aes(x = Year, y = Richness, col= Island))+
  geom_point()+
  geom_line()+
  labs(x = "Year", 
       y = "Richness")+
  scale_x_continuous(breaks=seq(2005,2023, 2))+
  theme(axis.text.x = element_text(angle=45),
        axis.title =element_text(size=10))+
  #geom_smooth(method = "gam", formula = y ~ s(x, k = 3), method.args = list(family = "poisson"))+
  facet_wrap(~Island)





 