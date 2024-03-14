library(tidyverse)

unique(revilla$Island)
revilla <- readRDS("data/correct/ltem_historic_20231109.RDS") |> 
  # filter(Region=="Revillagigedo") |>
  mutate(Island = case_when(
    Island == "Roca Partida" ~ "Partida Revillagigedo",
    TRUE ~ Island )) |> 
  # select(-c(CONCAT:Fishery)) |>
  rename(Habitat=Habitat.x)

reefs <- readxl::read_excel("data/correct/ltem_monitoring_reefs_2023-11-08.xlsx") |> 
  mutate(Island = case_when(
    Island == "Roca Partida" ~ "Partida Revillagigedo",
    TRUE ~ Island )) |> 
  rename(Habitat=Habitat.x)

ref<- revilla |> 
  mutate( IDReef = case_when(
    Reef == "CLARION_CALETA_NORTE" ~ "255",
    Reef == "CLARION_ISLOTE_SURESTE" ~ "438",
    Reef == "SAN_BENEDICTO_BOILER" ~ "255",
    Reef == "SAN_BENEDICTO_PARTE_NORTE" ~ "430",
    Reef == "SOCORRO_PUNTA_TOSCA" ~ "267",
    TRUE ~ IDReef))

khe2 <- merge(revilla, reefs, by=c("IDReef", "Reef", "Region", "Habitat", "CONCAT",
                                  "MPA", "Location", "Island", "Protection_status",
                                  "Protection_level", "Type", "Fishery", "Longitude", "Latitude"),
             all.x = TRUE)

khe <- revilla %>%
  left_join(reefs, by=c("IDReef", "Reef", "Region", "Habitat", "CONCAT",
                        "MPA", "Location", "Island", "Protection_status",
                        "Protection_level", "Type", "Fishery", "Longitude", "Latitude")) 

testo <- merge(ref, reefs, by=c("Region", "IDReef","Reef", "Habitat","Longitude","Latitude"), all.x = T) 
  # rename(IDReef=IDReef.x) |> 
  # mutate(IDReef=case_when(is.na(IDReef)~IDReef.y,
                          # TRUE~IDReef))
yaporfis <- yaamm |> 
  filter(Region== "Revillagigedo") |> 
  distinct(IDReef,  Reef)

head(test)
khe <- test |> 
  mutate(Island = case_when(
    Island == "Roca Partida" ~ "Partida Revillagigedo",
    TRUE ~ Island )) 
  
  merge(ref, reefs, by=c("IDReef","Reef", "Region","Habitat","CONCAT",
                        "MPA","Location","Island",
                        "Protection_status","Protection_level",
                        "Type","Fishery","Longitude","Latitude"), all.x = T)

# Suponiendo que ya has realizado el merge y almacenado el resultado en 'khe'
khe <- merge(ref, reefs, by=c("IDReef","Reef", "Region","Habitat","CONCAT",
                              "MPA","Location","Island",
                              "Protection_status","Protection_level",
                              "Type","Fishery","Longitude","Latitude"), all.x = TRUE)


)


missing.reefs <- test |> 
  select(IDReef, Reef) |> 
  unique() |> 
  filter(is.na(IDReef))

str(khe)

unique(khe$Island)

ref<- ltem |> 
  mutate( IDReef = case_when(
    Reef == "CLARION_CALETA_NORTE" ~ "255",
    Reef == "CLARION_ISLOTE_SURESTE" ~ "438",
    Reef == "SAN_BENEDICTO_BOILER" ~ "255",
    Reef == "SAN_BENEDICTO_PARTE_NORTE" ~ "430",
    Reef == "SOCORRO_PUNTA_TOSCA" ~ "267",
    TRUE ~ IDReef))



ltemA <- readRDS("data/correct/ltem_historic_20231109.RDS") |> 
  # filter(Region=="Revillagigedo") |> 
  select(-c(CONCAT:Fishery)) |>
  rename(Habitat=Habitat.x)


reefs <- readxl::read_excel("data/correct/ltem_monitoring_reefs_2023-11-08.xlsx") |> 
  rename(Habitat=Habitat.x)


plox <- merge(ref, reefs, by=c("IDReef","Region", "Island", "Reef", "Habitat","Longitude","Latitude"), all.x = T) |> 
  rename(IDReef=IDReef.x) |> 
  mutate(IDReef=case_when(is.na(IDReef)~IDReef.y,
                          TRUE~IDReef))

  merge(plox, reefs, by=c("IDReef","Reef", "Region","Habitat","CONCAT",
                                        "MPA","Location","Island",
                                        "Protection_status","Protection_level",
                                        "Type","Fishery","Longitude","Latitude"), all.x = T)
avers <- reefs |> 
  # select(-IDReef.y, -Habitat.y)
  # filter(Region == "Revillagigedo") |> 
  distinct(Reef, IDReef) 

unique(avers$Reef)
str(yaporfis)


lem <- ref |> 
  mutate(Island = case_when(
    Island == "Roca Partida" ~ "Partida Revillagigedo",
    TRUE ~ Island ))


library(tidyverse)


revilla <- readRDS("data/correct/ltem_historic_20231109.RDS") |> 
  # filter(Region=="Revillagigedo") |> 
  select(-c(CONCAT:Fishery)) |> 
  rename(Habitat=Habitat.x) |> 
  select(-Habitat.y)  

revilla<- revilla |> 
  mutate(Habitat = case_when(
    Habitat == "Pared" ~ "PARED",
    TRUE ~ Habitat 
  )) %>%
  # mutate( IDReef = case_when(
  # Reef == "CLARION_CALETA_NORTE" & Habitat == "PARED" ~ "442",
  # Reef == "CLARION_CALETA_NORTE" & Habitat == "BLOQUES" ~ "454",
  # Reef == "CLARION_ISLOTE_SURESTE" & Habitat == "PARED" ~ "438",
  # Reef == "CLARION_ISLOTE_SURESTE" & Habitat == "BLOQUES" ~ "455",
  # Reef == "SAN_BENEDICTO_BOILER" ~ "255",
  # Reef == "SAN_BENEDICTO_PARTE_NORTE" & Habitat == "BLOQUES" ~ "430",
  # Reef == "SAN_BENEDICTO_PARTE_NORTE" & Habitat == "PARED" ~ "456",
  # Reef == "SOCORRO_PUNTA_TOSCA" & Habitat == "PARED" ~ "267",
  # Reef == "SOCORRO_PUNTA_TOSCA" & Habitat == "BLOQUES" ~ "457",
  # TRUE ~ IDReef))
mutate( IDReef = case_when(
  Reef == "CLARION_CALETA_NORTE" ~ "442",
  Reef == "CLARION_ISLOTE_SURESTE" ~ "438",
  Reef == "SAN_BENEDICTO_BOILER" ~ "255",
  Reef == "SAN_BENEDICTO_PARTE_NORTE" ~ "430",
  Reef == "SOCORRO_PUNTA_TOSCA" ~ "267",
  TRUE ~ IDReef))


reefs <- readxl::read_excel("data/correct/ltem_monitoring_reefs_2023-11-08.xlsx") |> 
  rename(Habitat=Habitat.x)  
mutate( IDReef = case_when(
  Reef == "CLARION_CALETA_NORTE" ~ "442",
  Reef == "CLARION_ISLOTE_SURESTE" ~ "438",
  Reef == "SAN_BENEDICTO_BOILER" ~ "255",
  Reef == "SAN_BENEDICTO_PARTE_NORTE" ~ "430",
  Reef == "SOCORRO_PUNTA_TOSCA" ~ "267",
  TRUE ~ IDReef))


test <- merge(revilla, reefs, by=c("Region","Reef","Longitude","Latitude"), all.x = T) |> 
  rename(IDReef=IDReef.x) |> 
  mutate(IDReef=case_when(is.na(IDReef)~IDReef.y,
                          TRUE~IDReef))


missing.reefs <- test |> 
  select(IDReef, Reef) |> 
  unique() |> 
  filter(is.na(IDReef))


yaamm <- test |> 
  # filter(Label=="PEC") |>
  # filter(Region == "Revillagigedo") %>%
  # mutate(Island = case_when(
  #   Island == "Roca Partida" ~ "Partida Revillagigedo",
  #   TRUE ~ Island 
  # )) %>%
  # mutate(Habitat = case_when(
  #   Habitat == "Pared" ~ "PARED",
  #   TRUE ~ Habitat 
  # )) %>%
  select(-Habitat.y, -IDReef.y)
distinct(Region, IDReef,  Reef, Island,           
         CONCAT, Type, Habitat.x, Habitat.y,
         Protection_status, Protection_level, MPA, Location,
         Fishery)
distinct(Region, IDReef,  Reef, Habitat, Longitude, Latitude, Label, Year, Month, Day, 
         Degree, Depth,  Depth2, Transect, Area, IDSpecies,
         Species,   Phylum,    Taxa1,        Taxa2,          Taxa3,        Family,        
         Genus, TrophicLevelF, TrophicLevel,
         TrophicGroup, Functional_groups, A_ord, B_pen, Quantity, Size, Biomass,  Island,           
         CONCAT, Type,
         Protection_status, Protection_level, MPA, Location,
         Fishery)

