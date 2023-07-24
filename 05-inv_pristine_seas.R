# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl) 
library(ggplot2)
library(dplyr)
library(tidytext)
library(lubridate)
library(tidyr)
library(vegan)



# Load data ---------------------------------------------------------------

# Inver_Pristine tiene datos de invertebrados realizados por método PCU (Pristine Seas 2016).

inv <- read_xlsx("data/raw/Inver_Pristine.xlsx") |> 
  separate(Date, into=c("Day", "Month"), sep=" ") |> #  Agregar fecha, corregir nombres, establecer estaciones
  mutate(Month=case_when(Month=="march"~03,
                         Month=="april"~04),
         Year=2016,
         Date= as.Date(paste0(Year, "-",Month,"-", Day), "%Y-%m-%d" ))|> 
  rename(Size = `Test Diameter`, Quantity = Number, Transect = Quadrat) |> 
  filter( !Species %in% c("none","None"), !is.na(Species))|> 
  mutate (Species = recode(Species, "Diadema mexicanum?" = "Diadema mexicanum", "Pinctada matzalanica" = "Pinctada mazatlanica", 
                           "Mithrodia bradelyi" ="Mithrodia bradleyi")) |> 
  mutate (Island = recode(Island, "The Boiler" = "San Benedicto" )) |>
  mutate(Month = as.numeric(Month)) |> 
  mutate(Day = as.numeric(Day)) |> 
  mutate(Season = case_when(
    Month == 3 ~ "Spring",
    Month == 4 ~ "Spring",
    TRUE ~ "Other")) |> 
  mutate(Genus = sub("^(\\w+).*", "\\1", Species)) |> # Agregar Género y Taxa2
  mutate(Taxa2 = case_when(
    Genus == "Acanthaster" | Genus == "Mithrodia" ~ "Asteroidea",
    Genus == "Hexaplex" ~ "Caenogastropoda",
    Genus == "Diadema" | Genus ==  "Echinometra"| Genus == "Eucidaris" | Genus == "Tripneustes" ~ "Echinoidea",
    Genus == "Holothuria" |Genus == "Isostichopus" ~ "Holothuroidea",
    Genus ==  "Pinctada" ~ "Bivalvia", 
    Genus ==  "Thais"  ~ "Gastropoda" ,
    TRUE ~ "Other")) |> 
  mutate(Phylum = case_when(
    Taxa2 ==  "Asteroidea"| Taxa2 =="Echinoidea" | Taxa2== "Holothuroidea" ~ "Echinodermata" ,
    Taxa2 == "Bivalvia"| Taxa2 == "Gastropoda" ~ "Mollusca" ,
    TRUE ~ "Other")) |> 
  mutate(Depth2 = case_when( # Determinar profundidad 
    Depth <  15 ~ "Shallow" ,
    Depth >=  15 ~ "Deep" ,
    TRUE ~ "Other"
  ))

# saveRDS(inv, "data/correct/Pristine_Seas_PNR_Invertebrates_2016.RDS")

# Abundance -------------

(abun <- inv |> 
    filter(Taxa2 %in% c("Asteroidea", "Echinoidea")) |>
    group_by(Year, Season, Island, Reef, Depth2, Taxa2, Species) |> 
    summarise (Abundance = sum(Quantity, na.rm = T)) |>
    group_by(Island, Taxa2) |> 
    summarise(Abundance = mean(Abundance)) |>
    ggplot(aes(x= Island, y = Abundance, col= Taxa2)) +
    geom_point() +
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

ggsave("figs/2016_inv_abundance_island.png", width = 8.5, height = 4.5, dpi=1000)


# Abundance by groups-------------

(abun2 <- inv |> 
   filter(Taxa2 %in% c("Asteroidea", "Echinoidea")) |>
   group_by(Year, Season, Island, Reef, Depth2, Taxa2, Species) |> 
   summarise (Abundance = sum(Quantity, na.rm = T)) |>
   group_by(Year, Taxa2) |> 
   summarise(Abundance = mean(Abundance)) |>
   ggplot(aes(x= Taxa2, y = Abundance, col= Taxa2)) +
   geom_point() +
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

ggsave("figs/2016_inv_abundance_island_gps.png", width = 8.5, height = 4.5, dpi=1000)

# Density by Island ---------------

(den_inv <- inv |> 
  filter(Taxa2 %in% c("Asteroidea", "Echinoidea")) |>
  group_by(Year, Season, Island, Reef, Depth2, Taxa2, Species) |> 
  summarise(Density = sum(Quantity, na.rm = T)/10) |>  #10m2 o 6.25
  group_by(Island, Taxa2) |>
  summarise(Density = mean(Density, na.rm = T)) |> 
  ggplot(aes(x= Island, y = Density, col= Taxa2)) +
  geom_point() +
  facet_wrap(~Taxa2, scales="free_y") +
  scale_x_reordered() +
  scale_fill_manual(values=c("#a30808", "#b57560",  "#258f4e")) +
  labs(y = "Density (organisms/m2)", x="") +
    theme_classic()+
    theme(axis.text.y = element_text(face= "plain", size=8),
          axis.text.x=element_text(size=10),
          plot.title = element_text(hjust=0.5, size=14, face="plain", color = "gray20"),
          plot.title.position = "plot",
          legend.position = "",
          # legend.text = element_text(face = "italic"),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold.italic"))
)

ggsave("figs/2016_inv_density_islands.png", width = 8.5, height = 4.5, dpi=1000)

# Richness by Island ---------------

(Richness <- inv  |> 
  filter(Taxa2 %in% c("Asteroidea", "Echinoidea")) |>
  filter(!is.na(Species)) |> 
  # filter(!Island %in% c("Socorro", "Roca Partida") 
  # filter(Quantity> 1) |>
   group_by(Island, Taxa2) |>
   summarise(Richness = n_distinct(Species)) |>
   
   ggplot(aes(x = Island, y = Richness, col = Taxa2)) +
   geom_point() +
   facet_wrap(~Taxa2, scales = "free_y") +
   scale_fill_manual(values=c("#a30808", "#b57560", "#258f4e")) +
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
ggsave("figs/2016_inv_richness_islands.png", width = 8.5, height = 4.5, dpi=1000)


# Shannon-Wiener, H index 

Shannon <- inv |>
  filter(!is.na(Species)) |> 
  group_by(Island, Depth, Species) |> 
  summarise(Quantity=sum(Quantity, na.rm=T)) |> 
  filter(!Quantity==1) |> 
  group_by(Island, Depth) |> 
  mutate(Diversity= vegan::diversity(Quantity, "shannon"))

Shannon <- Shannon |> 
  mutate(Depth2= case_when(Depth==10~"Shallow",
                           Depth==20~ "Deep"))
    
ggplot(Shannon, aes(x = Island, y = Diversity, fill = Island)) +
geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Island", y = "Shannon (H)", title = "Invertebrates Diversity") +
  # coord_flip() +
  facet_wrap(~Depth2,  scales= "free_y") +
  scale_fill_brewer(palette = "Set2") +
  theme_classic()+
  theme(axis.text.y = element_text(face= "plain", size=10),
        axis.text.x=element_text(size=8),
        plot.title = element_text(hjust=0.5, size=16, face="plain", color = "gray20"),
        plot.title.position = "plot",
        legend.position = "",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold.italic")
  )


ggsave("Figs/2016_inv_Shannon_Diversity.png", width = 8.5, height = 4.5, dpi=1000)


# Simpson index 1-D

Simpson <- inv |> 
  filter(!is.na(Species)) |> 
  # filter(!Quantity==1) |>
  group_by(Island, Depth, Species) |> 
  summarise(Quantity=sum(Quantity, na.rm=T)) |> 
  group_by(Island, Depth) |> 
  mutate(Diversity= vegan::diversity(Quantity, "simpson"))

Simpson <- Simpson |> 
  mutate(Depth2= case_when(Depth==10~"Shallow",
                           Depth==20~ "Deep"))

ggplot(Simpson, aes(x = Island, y = Diversity, fill = Island)) +
  geom_bar(stat = "identity", position = "dodge" ) +
  labs(x = "Island", y = "Simpson", title = "Invertebrates Diversity") +
  # coord_flip()+
  facet_wrap(~Depth2,  scales= "free_y") +
  scale_fill_brewer(palette = "Set2") +
  theme_classic()+
  theme(axis.text.y = element_text(face= "plain", size=10),
        axis.text.x=element_text(size=8),
        plot.title = element_text(hjust=0.5, size=16, face="plain", color = "gray20"),
        plot.title.position = "plot",
        legend.position = "",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold.italic")
  )

ggsave("Figs/2016_inv_Simpson_Diversity.png", width = 8.5, height = 4.5, dpi=1000)

# Combine Simpson & shannon indexes

diversity <- rbind(Simpson %>% mutate(Index = "Simpson"),
                    Shannon %>% mutate(Index = "Shannon"))

ggplot(diversity, aes(x = Island, y = Diversity, fill = Index)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of diversity indexes",
       x = "Island",
       y = "Diversity",
       fill = "Index") +
  # coord_flip()+
  facet_wrap(~Depth2,  scales= "free_y") +
  scale_fill_brewer(palette = "Set2") +
  theme_classic()+
  theme(axis.text.y = element_text(face= "plain", size=10),
        axis.text.x=element_text(size= 8),
        plot.title = element_text(hjust=0.5, size=16, face="plain", color = "gray20"),
        plot.title.position = "plot",
        # legend.position = "",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold.italic")
  )

ggplot(diversity, aes(x = Island, y = Diversity, fill = Depth2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of diversity indexes",
       x = "Island",
       y = "Diversity",
       fill = "Depth") +
  # coord_flip()+
  facet_wrap(~Index,  scales= "free_y") +
  scale_fill_brewer(palette = "Set2") +
  theme_classic()+
  theme(axis.text.y = element_text(face= "plain", size=10),
        axis.text.x=element_text(size= 8),
        plot.title = element_text(hjust=0.5, size=16, face="plain", color = "gray20"),
        plot.title.position = "plot",
        # legend.position = "",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold.italic")
  )

ggsave("Figs/2016_inv_indixes_Diversity.png", width = 8.5, height = 4.5, dpi=1000)

