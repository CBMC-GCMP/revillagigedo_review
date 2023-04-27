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

# Rename & filter

pcu <- pcu|>
  mutate (Species = recode(Species, "Spirobranchus triquetrus?" = "Spirobranchus triquetrus")) |> 
  filter(Label != "NA", Label != "CYA", !is.na(Species))

head(pcu)
glimpse(pcu)

# Abundance PCU 2016 

abun <- pcu |> 
  filter(!is.na(Species)) |> 
  group_by(Label, Island, Reef, Taxa1, Species) |> 
  summarise(Abundance = sum(Count, na.rm = T))

# per groups & island

(abu_pcu <- abun |> 
  group_by(Island, Species, Taxa1) |> 
  ungroup() |> 
  mutate(Taxa1= reorder_within(Taxa1, Abundance, Island)) |>  
  ggplot(aes(x=Taxa1, y = Abundance, fill=Island)) +
  geom_col()+
  coord_flip()+
  facet_wrap(~Island, scales="free_y") +
  scale_x_reordered() +
  scale_fill_manual(values = brewer.pal(n = 4, name = "Set2"))+
  labs(y = "Abundance", x="", title= "PCU Groups Abundance") +
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


ggsave("figs/2016_PCU_abundance.png", width = 8.5, height = 4.5, dpi=1000)

# Abundance of macroalgae spp per groups

(abu_mac <- abun |> 
  filter(Label=="Macroalgae") |> 
  group_by(Taxa1, Species) |> 
  # summarise(Abundance = mean(Abundance)) |>
  group_by(Taxa1) |> 
  # top_n(10, Abundance) |>
  ungroup() |> 
  mutate(Species= reorder_within(Species, Abundance, Taxa1)) |>  
  ggplot(aes(x=Species, y = Abundance, fill=Taxa1)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~Taxa1, scales="free_y")+
  scale_x_reordered() +
  scale_fill_manual(values = brewer.pal(n = 4, name = "Set2"))+
  labs(y = "Abundance", x="", title= "Macroalgae Abundance") +
  theme_classic()+
  theme(axis.text.y = element_text(face= "italic", size=10),
        axis.text.x=element_text(size=10),
        plot.title = element_text(hjust=0.5, size=16, face="plain", color = "gray20"),
        plot.title.position = "plot",
        legend.position = "",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold.italic")
  )
)

ggsave("figs/2016_PCU_abundance_MC_ssp.png", width = 8.5, height = 4.5, dpi=1000)


# Abundance of macroalgae groups

(abu_gf <- abun |> 
  filter(Label=="Macroalgae") |>
  group_by(Island, Species, Taxa1) |>  
  # summarise(Abundance = mean(Abundance)) |>
  group_by(Taxa1) |> 
  ggplot(aes(x=Taxa1, y = Abundance, fill = Taxa1)) +
  geom_col()+
  scale_x_reordered() +
  scale_fill_manual(values = brewer.pal(n = 4, name = "Set2"))+
  labs(y = "Abundance", x="", title= "Macroalgae Groups Abundance") +
  theme_classic()+
  theme(axis.text.y = element_text(face= "plain", size=10),
        axis.text.x=element_text(face= "italic", size=10),
        plot.title = element_text(hjust=0.5, size=16, face="plain", color = "gray20"),
        plot.title.position = "plot",
        legend.position = "",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold.italic")
  )
)


ggsave("figs/2016_PCU_abundance_MC_gt.png", width = 8.5, height = 4.5, dpi=1000)

# Abundance of sessile invertebrates spp

(abu_inv <- abun|>
  filter(Label=="INV") |> 
  group_by(Island, Taxa1, Species) |>
  filter(Abundance > 1) |>
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
  scale_fill_manual(values = brewer.pal(n = 4, name = "Set2"))+
  labs(y = "Abundance", x="", title= "Sessile Invertebrates Abundance") +
  theme_classic()+
  theme(axis.text.y = element_text(face= "italic", size=8),
        axis.text.x=element_text(face= "plain", size=10),
        plot.title = element_text(hjust=0.5, size=16, face="plain", color = "gray20"),
        plot.title.position = "plot",
        legend.position = "",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold.italic")
  )
)

ggsave("figs/2016_PCU_abundance_SS_ssp.png", width = 8.5, height = 4.5, dpi=1000)


# Abundance of sessile invertebrates groups

(abu_ssgf <- abun|> 
  filter(Label=="INV") |> 
  group_by(Island, Taxa1, Species) |>  
  # summarise(Abundance = mean(Abundance)) |> 
  group_by(Taxa1) |> 
  ggplot(aes(x=Taxa1, y = Abundance, fill = Taxa1)) +
  geom_col()+
  scale_x_reordered() +
  scale_fill_manual(values = brewer.pal(n = 7, name = "Set2"))+
  labs(y = "Abundance", x="Groups", title= "Sessile Invertebrates Abundance") +
  theme_classic()+
  theme(axis.text.y = element_text(face= "italic", size=8),
        axis.text.x=element_text(face= "plain", size=10),
        plot.title = element_text(hjust=0.5, size=16, face="plain", color = "gray20"),
        plot.title.position = "plot",
        legend.position = "",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold.italic")
  )
)

ggsave("figs/2016_PCU_abundance_SS_gf.png", width = 8.5, height = 4.5, dpi=1000)


# Cover by groups

cover <- pcu |> 
  group_by(Label, Island, Reef, Taxa1, Species) |> 
  summarise(Cover = sum(Count, na.rm = T)/(Points*100))

(cov_pcu <- cover |> 
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
  scale_fill_manual(values = brewer.pal(n = 4, name = "Set2"))+
  labs(y = "Cover (%)", x="", title= "PCU Groups Cover") +
  theme_classic()+
  theme(axis.text.y = element_text(face= "italic", size=8),
        axis.text.x=element_text(face= "plain", size=10),
        plot.title = element_text(hjust=0.5, size=16, face="plain", color = "gray20"),
        plot.title.position = "plot",
        legend.position = "",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold.italic")
  )
)

ggsave("figs/2016_PCU_cover.png", width = 8.5, height = 4.5, dpi=1000)


# Cover by macroalgas spp

(cov_mac <- cover|> 
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
  scale_fill_manual(values = brewer.pal(n = 4, name = "Set2")) +
  labs(y = "Count", x="", title= "Macroalgae groups") +
  theme_classic()+
  theme(axis.text.y = element_text(face= "italic", size=8),
        axis.text.x=element_text(face= "plain", size=10),
        plot.title = element_text(hjust=0.5, size=16, face="plain", color = "gray20"),
        plot.title.position = "plot",
        legend.position = "",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold.italic")
  )
)

ggsave("figs/2016_PCU_cobertura_MC_ssp.png", width = 8.5, height = 4.5, dpi=1000)

(mac_grp <- cover |> 
  filter(Label=="Macroalgae") |>
  ggplot(aes(x = Island, y = Cover, fill = Taxa1)) +
  geom_bar(stat = "identity", position = position_stack())+
    scale_fill_manual(values = brewer.pal(n = 4, name = "Set2")) +
    theme_classic()+
    theme(axis.text.y = element_text(face= "plain", size=12),
          axis.text.x=element_text(face= "plain", size=12),
          plot.title = element_text(hjust=0.5, size=16, face="plain", color = "gray20"),
          plot.title.position = "plot",
          strip.background = element_blank(),
          strip.text.x = element_text(size = 12, face = "bold.italic")
    )
)
ggsave("figs/2016_PCU_cobertura_MC_grp.png", width = 8.5, height = 4.5, dpi=1000)

# Richness by Island

Richness <- pcu  |> 
  filter(!is.na(Species)) |>
  filter(Count> 1) |> 
  group_by(Island) |> 
  summarise(Richness= length(unique(Species)))

ggplot(Richness, aes(x = Island, y = Richness, fill= Island)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values= brewer.pal(n = 4, name = "Set2")) +
  labs(x = "Island", y = "Richness", title = "PCU Richness")+
  theme_classic() +
  theme(axis.text.y = element_text(size=12),
        axis.text.x=element_text(size=12),
        plot.title = element_text(hjust=0.5, size=16, face="plain", color = "gray20"),
        plot.title.position = "plot",
        legend.position = ""
  )

ggsave("figs/2016_PCU_richness_islands.png", width = 8.5, height = 4.5, dpi=1000)

# Shannon-Wiener index, H   

Shannon <- pcu |> 
  filter(!is.na(Species)) |> 
  group_by(Island, Reef, Sample, Depth, Taxa1, Species) |> 
  summarise(Count=sum(Count, na.rm=T)) |> 
  group_by(Island, Reef, Depth) |> 
  mutate(Diversity= vegan::diversity(Count, "shannon"))

Shannon <- Shannon |> 
  mutate(Depth2= case_when(Depth==10~"Shallow",
                           Depth==20~ "Deep"))

ggplot(Shannon, aes(x = Island, y = Diversity, fill = Island)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Island", y = "Shannon (H)", title = "PCU Diversity") +
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

ggsave("Figs/2016_PCU_Shannon_Diversity.png", width = 8.5, height = 4.5, dpi=1000)


#Simpson index 

Simpson <- pcu |> 
  filter(!is.na(Species)) |> 
  group_by(Island, Reef, Depth, Taxa1,Species) |> 
  summarise(Count=sum(Count, na.rm=T)) |> 
  group_by(Island, Reef, Depth) |> 
  mutate(Diversity= vegan::diversity(Count, "simpson")) 

Simpson <- Simpson |> 
  mutate(Depth2= case_when(Depth==10~"Shallow",
                           Depth==20~ "Deep"))

ggplot(Simpson, aes(x = Island, y = Diversity, fill = Island)) +
  geom_bar(stat = "identity", position = "dodge" ) +
  labs(x = "Island", y = "Simpson", title = "PCU Diversity") +
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

ggsave("Figs/2016_PCU_Simpson_Diversity.png", width = 8.5, height = 4.5, dpi=1000)
  

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

ggsave("Figs/indexes_PCU_Diversity.png", width = 8.5, height = 4.5, dpi=1000)
