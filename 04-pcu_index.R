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
  group_by(Label, Island, Reef, Taxa1, Species) |> 
  summarise(Abundance = sum(Count))

# per groups & island

abu_pcu <- abun |> 
  group_by(Island, Species, Taxa1) |> 
  summarise(Abundance = mean(Abundance)) |>
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
  labs(y = "Abundance", x="", title= "Sessile invertebrates and macroalgae") +
  theme(legend.position = "") +
  guides(colour = "none")+
  theme(axis.text.y = element_text(face= "italic"),
        plot.title = element_text(hjust=0.5)
  )

abu_pcu

ggsave("figs/2016_PCU_abundance.png", width = 8.5, height = 4.5, dpi=1000)

# Abundance of macroalgae spp per groups

abu_mac <- abun |> 
  filter(Label=="Macroalgae") |> 
  group_by(Taxa1, Species) |> 
  # summarise(Abundance = mean(Abundance)) |>
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

ggsave("figs/2016_PCU_abundance_MCssp.png", width = 8.5, height = 4.5, dpi=1000)


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

ggsave("figs/2016_PCU_abundance_MCgt.png", width = 8.5, height = 4.5, dpi=1000)

# Abundance of sessile invertebrates spp

abu_inv <- abun|>
  filter(Label=="INV") |> 
  group_by(Island, Taxa1, Species) |>
  # summarise(Abundance = mean(Abundance)) |>
  group_by(Island) |>
  top_n(10, Abundance) |>
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

ggsave("figs/2016_PCU_abundance_SS_ssp.png", width = 8.5, height = 4.5, dpi=1000)


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

ggsave("figs/2016_PCU_abundance_SS_gf.png", width = 8.5, height = 4.5, dpi=1000)


# Cover by groups

cover <- pcu |> 
  group_by(Label, Island, Reef, Taxa1, Species) |> 
  summarise(Cover = sum(Count)/(Points*100))

cov_pcu <- cover |> 
  group_by(Island, Species, Taxa1) |> 
  # summarise(Cover = mean(Cover)) |>
  group_by(Taxa1) |> 
  # top_n(10, Cover) |>
  ungroup() |> 
  mutate(Taxa1= reorder_within(Taxa1, Cover, Island)) |>  
  ggplot(aes(x=Taxa1, y = Cover, fill=Island)) +
  geom_col()+
  coord_flip()+
  facet_wrap(~Island, scales="free_y")+
  scale_x_reordered() +
  scale_fill_manual(values = brewer.pal(n = 3, name = "Set1"))+
  labs(y = "Cover (%)", x="", title= "Sessile invertebrates and macroalgae") +
  theme(legend.position = "") +
  guides(colour = "none")+
  theme(axis.text.y = element_text(face= "italic"),
        plot.title = element_text(hjust=0.5)
  )

cov_pcu

ggsave("figs/2016_PCU_cover.png", width = 8.5, height = 4.5, dpi=1000)


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
  labs(y = "Count", x="", title= "Macroalgae groups") +
  theme(legend.position = "") +
  guides(colour = "none")+
  theme(axis.text.y = element_text(face= "italic"),
        plot.title = element_text(hjust=0.5)
  )

cov_mac

ggsave("figs/2016_PCU_cobertura_MC_ssp.png", width = 8.5, height = 4.5, dpi=1000)

mac_grp <- cover |> 
  filter(Label=="Macroalgae") |>
  ggplot(aes(x = Island, y = Cover, fill = Taxa1)) +
  geom_bar(stat = "identity", position = position_stack())+
  scale_fill_manual(values = c("darkgreen", "goldenrod", "darkred", "darkblue" ))+
  theme(legend.text = element_text(face = "italic"))+
  labs(y= "Cover (%)", fill = "Groups")

mac_grp

ggsave("figs/2016_PCU_cobertura_MC_grp.png", width = 8.5, height = 4.5, dpi=1000)

# Richness by Island

Richness <- pcu  |> 
  filter(!is.na(Species)) |>
  filter(Count> 1) |> 
  group_by(Island) |> 
  summarise(Richness= length(unique(Species)))

ggplot(Richness, aes(x = Island, y = Richness)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Island", y = "Richness", title = "PCU Richness")+
  theme(legend.text = element_text(face = "italic"))

ggsave("figs/2016_PCU_richness_islands.png", width = 8.5, height = 4.5, dpi=1000)

# Shannon-Wiener index, H   

Shannon <- pcu |> 
  filter(!is.na(Species)) |> 
  group_by(Island, Reef, Sample, Depth, Taxa1, Species) |> 
  summarise(Count=sum(Count)) |> 
  group_by(Island, Reef, Depth) |> 
  mutate(Diversity= vegan::diversity(Count, "shannon"))

ggplot(Shannon, aes(x = Island, y = Diversity)) +
  geom_bar(stat = "summary", fun = "mean", fill = "blue") +
  labs(x = "Island", y = "Shannon") +
  theme_classic()

ggplot(Shannon, aes(x = Reef, y = Diversity)) +
  geom_bar(stat = "summary", fun = "mean", fill = "blue") +
  coord_flip()+
  labs(x = "Reef", y = "Shannon") +
  theme_classic()


ggsave("Figs/2016_PCU_Shannon_Diversity.png", width = 8.5, height = 4.5, dpi=1000)


#Simpson index 

Simpson <- pcu |> 
  filter(!is.na(Species)) |> 
  group_by(Island, Reef, Depth, Taxa1,Species) |> 
  summarise(Count=sum(Count)) |> 
  group_by(Island, Reef, Depth) |> 
  mutate(Diversity= vegan::diversity(Count, "simpson")) 

ggplot(Simpson, aes(x = Island, y = Diversity)) +
  geom_bar(stat = "summary", fun = "mean", fill = "blue") +
  labs(x = "Island", y = "Simpson") +
  theme_classic()

ggplot(Simpson, aes(x = Reef, y = Diversity)) +
  geom_bar(stat = "summary", fun = "mean", fill = "blue") +
  coord_flip()+
  labs(x = "Reef", y = "Simpson") +
  theme_classic()

ggplot(Simpson, aes(x = Island, y = Diversity, fill=Island)) +
  geom_violin(trim = F,  alpha = .3) +
  geom_boxplot(width  = 0.1) +
  geom_jitter(width = 0.09, pch = 21,  alpha = .3) +
  scale_color_manual(values = c("#0f2359", "#7AC5CD", "red", "darkred")) +
  scale_fill_manual(values = c("#0f2359", "#7AC5CD", "red", "darkred")) +
  # ylim(0, 1.5) +
  labs(x = "", y = "Simpson") +
  theme_classic()+
  theme(legend.position = "") +
  guides(colour = "none")

ggsave("Figs/2016_PCUSimpson_Diversity.png", width = 8.5, height = 4.5, dpi=1000)
  

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
