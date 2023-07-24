# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(tidytext)

# Load data ---------------------------------------------------------------

# inv2023 <- readRDS("data/ltem_revillagigedo_2023_12072023.RDS") |> 
#   filter(Label=="INV")

inv23 <- read_xlsx("data/revillagigedo_2023.xlsx")|> 
  filter(Label=="INV") |> 
  mutate(Genus = sub("^(\\w+).*", "\\1", Species)) # Agregar Género


# Abundance by Island -------------

(abu <- inv23 |> 
   filter(Taxa2 %in% c("Echinoidea", "Octocorallia", "Asteroidea", "Hexacorallia")) |> 
   # filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
   group_by(Island, Reef, Transect, Depth2, Taxa2, Species) |>
   summarise (Abundance = sum(Quantity, na.rm = T)) |> 
   group_by(Taxa2, Island) |> 
   summarise(Abundance = mean(Abundance)) |>
   ggplot(aes(x= Island, y = Abundance, col= Taxa2)) +
   geom_point()+
   # geom_jitter()+
   # coord_flip()+
   facet_wrap(~Taxa2, scales="free_y") +
   scale_x_reordered() +
   scale_fill_manual(values=c("#a30808", "#b57560", "#258f4e")) +
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
ggsave("figs/inv_2023_island.png", width = 8.5, height = 4.5, dpi=1000)

# Abundance by groups -------------

(abu <- inv23 |> 
   filter(Taxa2 %in% c("Echinoidea", "Octocorallia", "Asteroidea", "Hexacorallia")) |> 
   # filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
   group_by(Island, Reef, Transect, Depth2, Taxa2, Species) |>
   summarise (Abundance = sum(Quantity, na.rm = T)) |> 
   group_by(Taxa2) |> 
   summarise(Abundance = mean(Abundance)) |>
   ggplot(aes(x= Taxa2, y = Abundance, col= Taxa2)) +
   geom_point()+
   # geom_jitter()+
   # coord_flip()+
   # facet_wrap(~Taxa2, scales="free_y") +
   scale_x_reordered() +
   scale_fill_manual(values=c("#a30808", "#b57560", "#258f4e")) +
   labs(y = "Abundance", x="Groups") +
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

ggsave("figs/inv_2023_groups.png", width = 8.5, height = 4.5, dpi=1000)

# abunndance Hexacorallia ------

(abu <- inv23 |> 
    filter(Genus%in% c("Pavona", "Pocillopora", "Porites")) |> 
    group_by(Island, Reef, Transect, Depth2, Genus, Species) |> 
    summarise (Abundance = sum(Quantity, na.rm = T)) |> 
    group_by(Genus, Island) |> 
    summarise(Abundance = mean(Abundance)) |>
    ggplot(aes(x= Island, y = Abundance, col= Genus)) +
    geom_point()+
    # geom_boxplot()+
    # coord_flip()+
    # facet_wrap(~Genus, scales="free_y") +
    scale_x_reordered() +
    scale_fill_manual(values=c("#a30808", "#b57560", "#258f4e")) +
    labs(y = "Abundance", x="Island", tittle =" ") +
    theme_classic()+
    theme(axis.text.y = element_text(face= "plain", size=8),
          axis.text.x=element_text(size=10),
          plot.title = element_text(hjust=0.5, size=14, face="plain", color = "gray20"),
          plot.title.position = "plot",
          # legend.position = "",
          legend.text = element_text(face = "italic"),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold.italic")
    )
)

ggsave("figs/inv_2023.png", width = 8.5, height = 4.5, dpi=1000)

(abu <- inv23 |> 
    filter(Genus%in% c("Pavona", "Pocillopora", "Porites")) |> 
    group_by(Island, Reef, Transect, Depth2, Genus, Species) |> 
    summarise (Abundance = sum(Quantity, na.rm = T)) |> 
    group_by(Genus) |> 
    summarise(Abundance = mean(Abundance)) |>
    ggplot(aes(x= Genus, y = Abundance, col= Genus)) +
    geom_point()+
    # geom_boxplot()+
    # coord_flip()+
    # facet_wrap(~Genus, scales="free_y") +
    scale_x_reordered() +
    scale_fill_manual(values=c("#a30808", "#b57560", "#258f4e")) +
    labs(y = "Abundance", x="Genus", tittle =" ") +
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

ggsave("figs/inv_genus_2023.png", width = 8.5, height = 4.5, dpi=1000)

# Density----------------
# Densidad poblacional = Número de individuos / Área o volumen
# mutate(Density =(Quantity / (Area))) 

(den <- inv23 |> 
   filter(Taxa2 %in% c("Echinoidea", "Octocorallia", "Asteroidea", "Hexacorallia")) |> 
   # filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
   group_by(Island, Reef, Transect, Depth2, Taxa2, Species) |>
   mutate(Density =(Quantity / (Area))) |> 
   summarise (Density = sum(Density, na.rm = T)) |> 
   group_by(Taxa2, Island) |> 
   summarise(Density = mean(Density)) |>
   ggplot(aes(x= Island, y = Density, col= Taxa2)) +
   geom_point()+
   # geom_jitter()+
   # coord_flip()+
   facet_wrap(~Taxa2, scales="free_y") +
   scale_x_reordered() +
   scale_fill_manual(values=c("#a30808", "#b57560", "#258f4e")) +
   labs(y = "Density", x="Island") +
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

ggsave("figs/inv_dens_island_2023.png", width = 8.5, height = 4.5, dpi=1000)


(den <- inv23 |> 
    filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
    # filter(Genus%in% c("Pavona", "Pocillopora", "Porites")) |> 
    group_by(Island, Reef, Transect, Depth2, Taxa2, Genus) |> 
    mutate(Density =(Quantity / (Area))) |> 
    summarise (Density = sum(Density, na.rm = T)) |> 
    group_by(Genus, Island) |> 
    summarise(Density = mean(Density)) |>
    ggplot(aes(x= Island, y = Density, col= Genus)) +
    geom_point()+
    # geom_boxplot()+
    # coord_flip()+
    # facet_wrap(~Genus, scales="free_y") +
    scale_x_reordered() +
    # scale_fill_manual(values=c("#a30808", "#b57560", "#258f4e")) +
    labs(y = "Density", x="Island") +
    theme_classic()+
    theme(axis.text.y = element_text(face= "plain", size=8),
          axis.text.x=element_text(size=10),
          plot.title = element_text(hjust=0.5, size=14, face="plain", color = "gray20"),
          plot.title.position = "plot",
          # legend.position = "",
          legend.text = element_text(face = "italic"),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold.italic")
    )
)

ggsave("figs/inv_2023_dens.png", width = 8.5, height = 4.5, dpi=1000)

# Richness  ------------------------------------------------------

(rich <- inv23 |>
   filter(!is.na(Species)) |>
   # filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
   filter(Taxa2 %in% c("Echinoidea", "Octocorallia", "Asteroidea", "Hexacorallia")) |> 
   
   # filter(Quantity > 1) |>
   group_by(Taxa2) |>
   summarise(Richness = n_distinct(Species)) |>
   
   ggplot(aes(x = Taxa2, y = Richness, col = Taxa2)) +
   geom_point() +
   # facet_wrap(~Taxa2, scales = "free_y") +
   scale_fill_manual(values=c("#a30808", "#b57560", "#258f4e")) +
   labs(x = "Groups", y = "Richness") +
   theme_classic() +
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


ggsave("figs/rich_groups_2023.png", width = 8.5, height = 4.5, dpi=1000)


