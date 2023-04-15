# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl) 
library(ggplot2)
library(dplyr)
library(tidytext)
library(RColorBrewer)
library(ggiraphExtra)
library(vegan)



# Load data ---------------------------------------------------------------

# Inver_Pristine tiene datos de invertebrados realizados por método PCU (Pristine Seas 2016).

inv <- read_xlsx("data/Inver_Pristine.xlsx") 

# format date, rename, filter

inv <- inv |> 
  separate(Date, into=c("day", "month"), sep=" ") |> 
  mutate(month=case_when(month=="march"~03,
                         month=="april"~04),
         year=2016,
         Date= as.Date(paste0(year, "-",month,"-", day), "%Y-%m-%d" ))|> 
  rename(Size = `Test Diameter`, Quantity = Number) |> 
  filter( !Species %in% c("none","None"), !is.na(Species))|> 
  mutate (Species = recode(Species, "Diadema mexicanum?" = "Diadema mexicanum"))

# Abundance by Islands

(abu_inv <- inv |> 
  group_by(Island, Species) |> 
  summarise (Abundance = sum(Quantity, na.rm = T)) |> 
  mutate(Species= reorder_within(Species, Abundance, Island)) |>  
  ggplot(aes(x=Species, y = Abundance, fill=Island)) +
  geom_col()+
  coord_flip()+
  facet_wrap(~Island, scales="free_y") +
  scale_x_reordered() +
  scale_fill_manual(values = brewer.pal(n = 4, name = "Set2"))+
  labs(y = "Abundance", x="", title= "Invertebrates Abundance") +
  theme_classic()+
  theme(axis.text.y = element_text(face= "italic", size=12),
        axis.text.x=element_text(size=10),
        plot.title = element_text(hjust=0.5, size=16, face="plain", color = "gray20"),
        plot.title.position = "plot",
        legend.position = "",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold.italic")
  )
)


ggsave("figs/2016_inv_abundance_island.png", width = 8.5, height = 4.5, dpi=1000)

# Density by Island

(den_inv <- inv |> 
  group_by(Island, Species) |> 
  summarise(Density = sum(Quantity, na.rm = T)/10) |>  #10m2 o 6.25
  mutate(Species= reorder_within(Species, Density, Island)) |>
  ggplot(aes(x=Species, y = Density, fill=Island)) +
  geom_col()+
  facet_wrap(~Island, scales="free_y")+
  coord_flip()+
  scale_x_reordered() +
  scale_fill_manual(values = brewer.pal(n = 4, name = "Set2"))+
  labs(y = "Density (organisms/m2)", x="", title= "Invertebrate Density") +
    theme_classic()+
    theme(axis.text.y = element_text(face= "italic", size=12),
          axis.text.x=element_text(size=10),
          plot.title = element_text(hjust=0.5, size=16, face="plain", color = "gray20"),
          plot.title.position = "plot",
          legend.position = "",
          strip.background = element_blank(),
          strip.text.x = element_text(size = 12, face = "bold.italic")
    )
)

ggsave("figs/2016_inv_density_islands.png", width = 8.5, height = 4.5, dpi=1000)

# Richness by Island
(Richness <- inv  |> 
  filter(!is.na(Species)) |> 
  # filter(!Island %in% c("Socorro", "Roca Partida") 
  filter(Quantity> 1) |>
  group_by(Island) |> 
  summarise(Richness= length(unique(Species))) |> 
 
  ggplot(aes(x = Island, y = Richness, fill=Island)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values= brewer.pal(n = 4, name = "Set2")) +
  labs(x = "Island", y = "Richness", title = "Invertebrates Richness") +
  theme_classic() +
  theme(axis.text.y = element_text(size=12),
        axis.text.x=element_text(size=12),
        plot.title = element_text(hjust=0.5, size=16, face="plain", color = "gray20"),
        plot.title.position = "plot",
        legend.position = "",
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

