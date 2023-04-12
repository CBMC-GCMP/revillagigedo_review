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

# PCU_Pristine tiene datos de algas realizadas por el método PCU (Pristine Seas 2016).

pcu <- read_xlsx("data/pcu_converted2.xlsx")


# Rename 

pcu <- pcu|>
  mutate (Species = recode(Species, "Spirobranchus triquetrus?" = "Spirobranchus triquetrus"))


pcu <- filter(pcu, Label != "NA", Label != "CYA")

head(pcu)
glimpse(pcu)

# Abundance PCU 2016 

abun <- pcu |> 
  filter(!is.na(Species)) |> 
  filter(Count> 1) |> 
  group_by(Label, Island, Reef, Taxa1, Species) |> 
  summarise(Abundance = sum(Count))

# per groups & island

abu_pcu <- abun |> 
  group_by(Island, Species, Taxa1) |> 
  # summarise(Abundance = mean(Abundance)) |>
  group_by(Taxa1) |> 
  # top_n(10, Abundance) |>
  ungroup() |> 
  mutate(Taxa1= reorder_within(Taxa1, Abundance, Island)) |>  
  ggplot(aes(x=Taxa1, y = Abundance, fill=Island)) +
  geom_col()+
  coord_flip()+
  facet_wrap(~Island, scales="free_y")+
  scale_x_reordered() +
  scale_fill_manual(values = brewer.pal(n = 3, name = "Set1"))+
  labs(y = "Abundance", x="", title= "PCU Pristine Seas 2016") +
  theme(legend.position = "") +
  guides(colour = "none")+
  theme(axis.text.y = element_text(face= "italic"),
        plot.title = element_text(hjust=0.5)
  )

abu_pcu

ggsave("figs/2016_PCU_abundance.png", width = 8.5, height = 4.5, dpi=1000)

# stacked bar chart
  
  ggplot(abun, aes(x = Island, y = Abundance, fill = Taxa1)) +
    geom_bar(stat = "identity", position = position_stack())+
    theme(legend.text = element_text(face = "italic"))+
    labs(fill = "Groups")

  ggsave("figs/2016_PCU_abundance_groups.png", width = 8.5, height = 4.5, dpi=1000)

# Abundance of macroalgae spp per groups

abu_mac <- abun |> 
  filter(Label=="Macroalgae") |> 
  group_by(Taxa1, Species) |> 
  summarise(Abundance = mean(Abundance)) |>
  group_by(Taxa1) |> 
  # top_n(10, Abundance) |>
  ungroup() |> 
  mutate(Species= reorder_within(Species, Abundance, Taxa1)) |>  
  ggplot(aes(x=Species, y = Abundance, fill=Taxa1)) +
  geom_col()+
  coord_flip()+
  facet_wrap(~Taxa1, scales="free_y")+
  scale_x_reordered() +
  scale_fill_manual(values = c("chartreuse4", "darkgoldenrod", "darkred", "darkblue" ))+
  labs(y = "Abundance", x="", title= "Macroalgae per groups") +
  theme(legend.position = "") +
  guides(colour = "none")+
  theme(axis.text.y = element_text(face= "italic"),
        plot.title = element_text(hjust=0.5)
  )

abu_mac

ggsave("figs/2016_MC_abundance_ssp.png", width = 8.5, height = 4.5, dpi=1000)


# Abundance of macroalgae groups

abu_gf <- abun |> 
  filter(Label=="Macroalgae") |>
  group_by(Island, Species, Taxa1) |>  
  # summarise(Abundance = mean(Abundance)) |>
  group_by(Taxa1) |> 
  ggplot(aes(x=Taxa1, y = Abundance, fill = Taxa1)) +
  geom_col()+
  scale_x_reordered() +
  scale_fill_manual(values = c("darkgreen", "goldenrod", "darkred", "darkblue" ))+
  labs(y = "Abundance", x="", title= "Macroalgae groups") +
  theme(legend.position = "") +
  guides(colour = "none")+
  theme(axis.text.y = element_text(face= "italic"),
        plot.title = element_text(hjust=0.5)
  )

abu_gf

ggsave("figs/2016_GMC_abundance_gt.png", width = 8.5, height = 4.5, dpi=1000)

mac_grp <- abun |> 
  filter(Label=="Macroalgae") |>
  ggplot(aes(x = Island, y = Abundance, fill = Taxa1)) +
  geom_bar(stat = "identity", position = position_stack())+
  scale_fill_manual(values = c("darkgreen", "goldenrod", "darkred", "darkblue" ))+
  theme(legend.text = element_text(face = "italic"))+
  labs(fill = "Groups")

mac_grp

ggsave("figs/2016_GMC_abundance_ISL.png", width = 8.5, height = 4.5, dpi=1000)

# Abundance of sessile invertebrates spp

abu_inv <- abun|>
  filter(Label=="INV") |> 
  group_by(Island, Taxa1, Species) |>
  # summarise(Abundance = mean(Abundance)) |>
  group_by(Island) |>
  # top_n(10, Abundance) |>
  ungroup() |>
  mutate(Species= reorder_within(Species, Abundance, Island)) |>
  ggplot(aes(x=Species, y = Abundance, fill=Island)) +
  geom_col()+
  coord_flip()+
  facet_wrap(~Island, scales="free_y")+
  scale_x_reordered() +
  scale_fill_manual(values = brewer.pal(n = 3, name = "Set2"))+
  labs(y = "Abundance", x="", title= "Sessile invertebrates") +
  theme(legend.position = "") +
  guides(colour = "none")+
  theme(axis.text.y = element_text(face= "italic"),
        plot.title = element_text(hjust=0.5)
  )

abu_inv

ggsave("figs/2016_SS_abundance_ssp.png", width = 8.5, height = 4.5, dpi=1000)


# Abundance of sessile invertebrates groups

abu_ssgf <- abun|> 
  filter(Label=="INV") |> 
  group_by(Island, Taxa1, Species) |>  
  # summarise(Abundance = mean(Abundance)) |> 
  group_by(Taxa1) |> 
  ggplot(aes(x=Taxa1, y = Abundance, fill = Taxa1)) +
  geom_col()+
  scale_x_reordered() +
  scale_fill_manual(values = brewer.pal(n = 7, name = "Set2"))+
  labs(y = "Abundance", x="Groups", title= "Sessile invertebrates") +
  theme(legend.position = "") +
  guides(colour = "none")+
  theme(axis.text.y = element_text(face= "italic"),
        plot.title = element_text(hjust=0.5)
  )

abu_ssgf

abu_ss <- abun |> 
  filter(Label=="INV") |> 
  ggplot(aes(x = Island, y = Abundance, fill = Taxa1)) +
  geom_bar(stat = "identity", position = position_stack())+
  theme(legend.text = element_text(face = "italic"))+
  labs(fill = "Groups")
abu_ss

ggsave("figs/2016_SS_abundance_gf.png", width = 8.5, height = 4.5, dpi=1000)


# Cover by groups

cover <- pcu |> 
  group_by(Label, Island, Reef, Taxa1, Species) |> 
  filter(Count> 1) |>
  summarise(Cover = sum(Count/Points)) 

cov_pcu <- cover|>
  group_by(Island, Reef, Taxa1, Species) |>  
  # summarise(Cover = mean(Cover)) |>
  group_by(Taxa1) |> 
  ggplot(aes(x=Taxa1, y = Cover, fill = Taxa1)) +
  geom_col()+
  scale_x_reordered() +
  scale_fill_manual(values = c("darkturquoise", "darkgreen", "deeppink", "red", "coral1", "goldenrod",
                               "coral4", "yellow", "darkred", "cornflowerblue", "cadetblue"))+
  labs(y = "Cover", x="", title= "Pristine Seas 2016") +
  theme(legend.position = "") +
  guides(colour = "none")+
  theme(axis.text.y = element_text(face= "italic"),
        plot.title = element_text(hjust=0.5)
  )
cov_pcu

ggsave("figs/2016_PCU_cover.png", width = 8.5, height = 4.5, dpi=1000)

# Cover by Island

ggplot(cover, aes(x = Island, y = Cover, fill = Taxa1)) +
  geom_bar(stat = "identity", position = position_stack())+
  theme(legend.text = element_text(face = "italic"))+
  labs(fill = "Groups")

ggsave("figs/2016_PCU_cover_island.png", width = 8.5, height = 4.5, dpi=1000)


# cover by macroalgae groups

cov_gf <- cover |>
  filter(Label=="Macroalgae") |> 
  group_by(Taxa1, Species) |>  
  # summarise(Cover = mean(Cover)) |> 
  group_by(Taxa1) |> 
  ggplot(aes(x=Taxa1, y = Cover, fill = Taxa1)) +
  geom_col()+
  scale_x_reordered() +
  scale_fill_manual(values = c("darkgreen", "goldenrod", "darkred", "darkblue" ))+
  labs(y = "Cover", x="", title= "Pristine Seas 2016") +
  theme(legend.position = "") +
  guides(colour = "none")+
  theme(axis.text.y = element_text(face= "italic"),
        plot.title = element_text(hjust=0.5)
  )

cov_gf

ggsave("figs/2016_GMC_cobertura_gf.png", width = 8.5, height = 4.5, dpi=1000)

# Cover by macroalgas spp

cov_mac <- cover|> 
  filter(Label=="Macroalgae") |> 
  group_by(Taxa1, Species) |>  
  # summarise(Count = mean(Count)) |>
  group_by(Taxa1) |> 
  # top_n(10, Count) |>
  ungroup() |> 
  mutate(Species= reorder_within(Species, Cover, Taxa1)) |>  
  ggplot(aes(x=Species, y = Cover, fill=Taxa1)) +
  geom_col()+
  coord_flip()+
  facet_wrap(~Taxa1, scales="free_y")+
  scale_x_reordered() +
  scale_fill_manual(values = c("chartreuse4", "darkgoldenrod", "darkred", "darkgrey" ))+
  labs(y = "Count", x="", title= "PCU Pristine Seas 2016") +
  theme(legend.position = "") +
  guides(colour = "none")+
  theme(axis.text.y = element_text(face= "italic"),
        plot.title = element_text(hjust=0.5)
  )

cov_mac

ggsave("figs/2016_MC_cobertura_ssp.png", width = 8.5, height = 4.5, dpi=1000)


# Richness by Region

Richness <- pcu  |> 
  filter(!is.na(Species)) |>
  filter(Count> 1) |> 
  group_by(Island, Taxa1, Species) |> 
  summarise(Richness= length(unique(Species)))

ggplot(Richness, aes(x = Island, y = Richness)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Island", y = "Richness", title = "PCU Richness")+
  theme(legend.text = element_text(face = "italic"))


ggsave("figs/2016_PCU_richness_islands.png", width = 8.5, height = 4.5, dpi=1000)


ggplot(Richness, aes(x = Island, y = Richness, fill = Taxa1)) +
  geom_bar(stat = "identity", position = position_stack())+
  labs(x = "Island", y = "Richness", title = "PCU Richness")+
  theme(legend.text = element_text(face = "italic"))+
  labs(fill="Groups")

ggsave("figs/2016_PCU_richness_s.png", width = 8.5, height = 4.5, dpi=1000)

# Shannon-Wiener index, H   

Shannon <- pcu |> 
  filter(!is.na(Species)) |> 
  group_by(Island, Reef, Sample, Depth, Taxa1, Species) |> 
  summarise(Count=sum(Count)) |> 
  group_by(Island, Reef, Depth) |> 
  mutate(Diversity= vegan::diversity(Count, "shannon")) 

ggplot(Shannon, aes(x = Island, y = Diversity, fill=Taxa1)) +
  geom_bar(stat = "identity") +
  labs(x = "Island", y = "Shannon (H)", fill= "Groups", title = "PCU diversity")+
  theme(legend.text = element_text(face = "italic"))

ggsave("Figs/Shannon_PCU_Diversity.png", width = 8.5, height = 4.5, dpi=1000)


#Simpson index 

Simpson <- pcu |> 
  filter(!is.na(Species)) |> 
  group_by(Island, Reef, Depth, Taxa1,Species) |> 
  summarise(Count=sum(Count)) |> 
  group_by(Island, Reef, Depth) |> 
  mutate(Diversity= vegan::diversity(Count, "simpson")) 
  
  ggplot(Simpson, aes(x=Island, y = Diversity, fill=Taxa1)) +
                         geom_bar(stat = "identity") +
                         labs(x = "Island", y = "Simpson", fill= "Groups", title = "PCU diversity")+
                         theme(legend.text = element_text(face = "italic"))
                       
ggsave("Figs/Simpson_PCU_Diversity.png", width = 8.5, height = 4.5, dpi=1000)
                       

# Inverse Simpson, 1/D

invD2 <- pcu |>
  filter(!is.na(Species)) |>
  group_by(Island, Reef, Depth, Taxa1, Species) |>
  summarise(Count=sum(Count)) |>
  group_by(Island, Reef, Depth) |>
  mutate(Diversity= vegan::diversity(Count, "invsimpson"))

ggplot(invD2, aes(x = Island, y = Diversity, fill=Taxa1)) +
  geom_bar(stat = "identity") +
  labs(x = "Island", y = "Inverse Simpson", fill= "Groups", title = "PCU diversity")+
  theme(legend.text = element_text(face = "italic"))

# Combine Simpson & shannon indexes

diversity <- rbind(Simpson %>% mutate(Index = "Simpson"),
                   Shannon %>% mutate(Index = "Shannon"))

ggplot(diversity, aes(x = Island, y = Diversity, fill = Index)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of diversity indexes",
       x = "Island",
       y = "Diversity",
       fill = "Index") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) 

ggsave("Figs/indexes_PCU_Diversity.png", width = 8.5, height = 4.5, dpi=1000)



