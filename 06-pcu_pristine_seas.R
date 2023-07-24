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

# PCU_Pristine tiene datos de algas realizadas por el método PCU (Pristine Seas 2016).

pcu <- read_xlsx("data/pcu_converted2.xlsx")|>
    filter (Label == "INV") |>
    separate(Date, into=c("Day", "Month"), sep=" ") |> #  Agregar fecha, corregir nombres, establecer estaciones
    mutate(Month=case_when(Month=="march"~03,
                           Month=="april"~04),
           Year=2016,
           Date= as.Date(paste0(Year, "-",Month,"-", Day), "%Y-%m-%d" ))|> 
    mutate (Species = recode(Species, "Spirobranchus triquetrus?" = "Spirobranchus triquetrus", 
                             "Tubastrea coccinea " = "Tubastraea coccinea")) |> 
    mutate (Island = recode(Island, "The Boiler" = "San Benedicto", "La Partida" = "Roca Partida" )) |>
    filter(Label != "NA", Label != "CYA", !is.na(Species)) |> 
    mutate(Season = case_when(
      Month == 3 ~ "Spring",
      Month == 4 ~ "Spring",
      TRUE ~ "Other")) |> 
    mutate(Genus = sub("^(\\w+).*", "\\1", Species)) |> # Agregar Género y Taxa2
    mutate(Taxa2 = case_when(
      Genus == "Aplysina" | Genus == "Hexadella" ~ "Verongimorpha",
      Genus == "Pocillopora" | Genus == "Psammocora" | Genus == "Pavona" |Genus == "Porites"| Genus== "Tubastraea" ~ "Hexacorallia",
      Genus == "Leptogorgia"| Genus == "Eugorgia"  ~ "Octocorallia",
      Genus == "Ircinia"  ~ "Keratosa",
      Genus ==  "Clathrina" ~ "Calcinea", 
      Genus ==  "Spirobranchus"  ~ "Sedentaria" ,
      TRUE ~ "Other")) |> 
    mutate(Phylum = case_when(
      Taxa2 == "Verongimorpha" | Taxa2 == "Calcinea"| Taxa2== "Ketarosa" ~ "Porifera",
      Taxa2 == "Hexacorallia" | Taxa2 == "Octocorallia" ~ "Cnidaria",
      Taxa2 ==  "Sedentaria"  ~ "Annelida" ,
      TRUE ~ "Other")) |>  
    mutate(Depth2 = case_when( # Determinar profundidad 
      Depth <  15 ~ "Shallow" ,
      Depth >=  15 ~ "Deep" ,
      TRUE ~ "Other"
    ))

# saveRDS(pcu, "data/correct/Pristine_Seas_PNR_PCU_2016.RDS")

# Abundance of invertebrates -----------

  (abun <- pcu |> 
      filter(Taxa2 %in% c("Hexacorallia", "Octocorallia")) |>
      group_by(Year, Season, Island, Reef, Depth2, Taxa2, Species) |> 
      summarise (Abundance = sum(Count, na.rm = T)) |>
      group_by(Island, Taxa2) |> 
      summarise(Abundance = mean(Abundance)) |>
      ggplot(aes(x= Island, y = Abundance, col= Taxa2)) +
      geom_point() +
      # geom_jitter()+
      # coord_flip()+
      facet_wrap(~Taxa2, scales="free_y") +
      scale_x_reordered() +
      # scale_color_manual(values=c("#a30808", "#b57560",  "#258f4e")) +
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
  
ggsave("figs/2016_PCU_abundance.png", width = 8.5, height = 4.5, dpi=1000)

# Abundance of invertebrates by groups -----------

(abun3 <- pcu |> 
   filter(Taxa2 %in% c("Hexacorallia", "Octocorallia")) |>
   group_by(Year, Season, Island, Reef, Depth2, Taxa2, Species) |> 
   summarise (Abundance = sum(Count, na.rm = T)) |>
   group_by(Year, Taxa2) |> 
   summarise(Abundance = mean(Abundance)) |>
   ggplot(aes(x= Taxa2, y = Abundance, col= Taxa2)) +
   geom_point() +
   # geom_jitter()+
   # coord_flip()+
   # facet_wrap(~Taxa2, scales="free_y") +
   scale_x_reordered() +
   # scale_color_manual(values=c("#a30808", "#b57560",  "#258f4e")) +
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

abu <- patchwork::wrap_plots(abun2, abun3)

abu

ggsave("figs/2016_PCU_abundance_groups.png", width = 8.5, height = 4.5, dpi=1000)


(abu <- pcu |> 
    filter(Genus%in% c("Pavona", "Pocillopora", "Porites")) |> 
    group_by(Island, Reef, Depth2, Genus) |> 
    summarise (Abundance = sum(Count, na.rm = T)) |>
    group_by(Island, Genus) |> 
    summarise(Abundance = mean(Abundance)) |>
    ggplot(aes(x= Island, y = Abundance, col= Genus)) +
    geom_point() +
    # geom_jitter()+
    # coord_flip()+
    # facet_wrap(~Genus, scales="free_y") +
    scale_x_reordered() +
    # scale_color_manual(values=c("#a30808", "#b57560",  "#258f4e")) +
    labs(y = "Abundance", x="Island") +
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
ggsave("figs/2016_PCU_abundance-hex.png", width = 8.5, height = 4.5, dpi=1000)

# cover-------------

(cov_pcu <- pcu |> 
   group_by(Island, Reef, Depth2, Genus) |> 
   filter(Genus%in% c("Pavona", "Pocillopora", "Porites")) |> 
   summarise(Cover = sum(Count, na.rm = T)/(Points*100)) |> 
   group_by(Genus, Island) |>
   summarise(Cover = mean(Cover)) |>   
   ggplot(aes(x=Island, y = Cover, col=Genus)) +
   geom_point()+
   # coord_flip()+
   facet_wrap(~Genus, scales="free_y")+
   scale_x_reordered() +
   # scale_color_manual(values=c("#258f4e", "#b57560", "#a30808", "blue"))  +
   labs(y = "Cover (%)", x="Island") +
   theme_classic()+
   theme(axis.text.y = element_text(face= "italic", size=8),
         axis.text.x=element_text(face= "plain", size=10),
         plot.title = element_text(hjust=0.5, size=16, face="plain", color = "gray20"),
         plot.title.position = "plot",
         legend.position = "",
         # legend.text = element_text(face = "italic"),
         strip.background = element_blank(),
         strip.text.x = element_text(size = 12, face = "bold.italic")
   )
)

ggsave("figs/2016_PCU_cover.png", width = 8.5, height = 4.5, dpi=1000)

# Richness ---------

Richness <- pcu |> 
  filter(Taxa2 %in% c("Hexacorallia", "Octocorallia")) |>
  filter(!is.na(Species)) |> 
  group_by(Island, Taxa2) |> 
  summarise(Richness= length(unique(Species)))

ggplot(Richness, aes(x = Taxa2, y = Richness, col= Taxa2)) +
  geom_point() +
  # facet_wrap(~Taxa2, scales = "free_y") +
  # scale_color_manual(values=c("#258f4e", "#b57560", "#a30808", "blue"))  +
  labs(x = "", y = "Richness")+
  theme_classic() +
  theme(axis.text.y = element_text(face= "plain", size=8),
        axis.text.x=element_text(face = "italic", size=10),
        plot.title = element_text(hjust=0.5, size=14, face="plain", color = "gray20"),
        plot.title.position = "plot",
        legend.position = "",
        # legend.text = element_text(face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold.italic"))

ggsave("figs/2016_PCU_richness_taxa2.png", width = 8.5, height = 4.5, dpi=1000)

pcu |>
  filter(!is.na(Species))|>
  filter(Genus%in% c("Pavona", "Pocillopora", "Porites")) |>
  group_by(Year, Taxa2, Genus) |>
  summarise(Richness = n_distinct(Species)) |>
  
  ggplot(aes(x = Genus, y = Richness, col = Genus)) +
  geom_point() +
  # facet_wrap(~Genus, scales = "free_y") +
  scale_fill_manual(values=c("#a30808", "#b57560",  "#258f4e")) +
  labs(x = "Genus", y = "Richness") +
  theme_classic() +
  theme(axis.text.y = element_text(face= "plain", size=8),
        axis.text.x=element_text(face= "italic", size=10),
        plot.title = element_text(hjust=0.5, size=14, face="plain", color = "gray20"),
        plot.title.position = "plot",
        legend.position = "",
        # legend.text = element_text(face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold.italic")
  )

ggsave("figs/2016_PCU_richness_genuss.png", width = 8.5, height = 4.5, dpi=1000)

# Macroalgas---------------

mac <- read_xlsx("data/pcu_converted2.xlsx")|>
  filter (Label == "Macroalgae") |>
  separate(Date, into=c("Day", "Month"), sep=" ") |> #  Agregar fecha, corregir nombres, establecer estaciones
  mutate(Month=case_when(Month=="march"~03,
                         Month=="april"~04),
         Year=2016,
         Date= as.Date(paste0(Year, "-",Month,"-", Day), "%Y-%m-%d" ))|> 
  mutate (Island = recode(Island, "The Boiler" = "San Benedicto", "La Partida" = "Roca Partida")) |>
  filter(Label != "NA", Label != "CYA", !is.na(Species)) |> 
  mutate(Season = case_when(
    Month == 3 ~ "Spring",
    Month == 4 ~ "Spring",
    TRUE ~ "Other")) |> 
  mutate(Genus = sub("^(\\w+).*", "\\1", Species)) |> # Agregar Género 
  mutate(Depth2 = case_when( # Determinar profundidad 
    Depth <  15 ~ "Shallow" ,
    Depth >=  15 ~ "Deep" ,
    TRUE ~ "Other"
  ))

# Abundance mc ------------

(abu <- mac |> 
   # filter(Taxa2 %in% c("Hexacorallia", "Octocorallia")) |>
   group_by(Year, Season, Island, Reef, Depth2, Groups, Genus) |> 
   summarise (Abundance = sum(Count, na.rm = T)) |>
   group_by(Island, Groups) |> 
   summarise(Abundance = mean(Abundance)) |>
   ggplot(aes(x= Island, y = Abundance, col= Groups)) +
   geom_point() +
   # geom_jitter()+
   # coord_flip()+
   # facet_wrap(~Groups, scales="free_y") +
   scale_x_reordered() +
   scale_color_manual(values=c("#258f4e", "#b57560", "#a30808", "blue")) +
   labs(y = "Abundance", x="Island") +
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

ggsave("figs/2016_PCU_abundance_MC.png", width = 8.5, height = 4.5, dpi=1000)



# Cover by islands ------------------------

(cov_mac <- mac |> 
    group_by(Island, Reef, Depth2, Groups) |> 
    summarise(Cover = sum(Count, na.rm = T)/(Points*100)) |> 
    group_by(Groups, Island) |>
    summarise(Cover = mean(Cover)) |>   
    ggplot(aes(x=Island, y = Cover, col=Groups)) +
    geom_point()+
    # coord_flip()+
    # facet_wrap(~Island, scales="free_y")+
    scale_x_reordered() +
    scale_color_manual(values=c("#258f4e", "#b57560", "#a30808", "blue"))  +
    labs(y = "Cover (%)", x="Island") +
    theme_classic()+
    theme(axis.text.y = element_text(face= "italic", size=8),
        axis.text.x=element_text(face= "plain", size=10),
        plot.title = element_text(hjust=0.5, size=16, face="plain", color = "gray20"),
        plot.title.position = "plot",
        # legend.position = "",
        legend.text = element_text(face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold.italic")
  )
)

ggsave("figs/2016_PCU_cover.png", width = 8.5, height = 4.5, dpi=1000)

# Cover by groups ------------------------

(cov_mac <- mac |> 
   group_by(Year, Island, Reef, Depth2, Groups) |> 
   reframe(Cover = sum(Count, na.rm = T)/(Points*100)) |> 
   # summarise(Cover = sum(Count, na.rm = T)/(Points*100)) |> 
   group_by(Year, Groups) |>
   summarise(Cover = mean(Cover)) |>   
   ggplot(aes(x=Groups, y = Cover, col=Groups)) +
   geom_point()+
   # coord_flip()+
   # facet_wrap(~Island, scales="free_y")+
   scale_x_reordered() +
   scale_color_manual(values=c("#258f4e", "#b57560", "#a30808", "blue"))  +
   labs(y = "Cover (%)", x="") +
   theme_classic()+
   theme(axis.text.y = element_text(face= "italic", size=8),
         axis.text.x=element_text(face= "italic", size=10),
         plot.title = element_text(hjust=0.5, size=16, face="plain", color = "gray20"),
         plot.title.position = "plot",
         legend.position = "",
         # legend.text = element_text(face = "italic"),
         strip.background = element_blank(),
         strip.text.x = element_text(size = 12, face = "bold.italic")
   )
)

ggsave("figs/2016_PCU_cover_gps.png", width = 8.5, height = 4.5, dpi=1000)

# Richness by Island

Richness <- mac |> 
  filter(!is.na(Species), !Groups=="Turf") |> 
  group_by(Island, Groups) |> 
  summarise(Richness= length(unique(Species)))

ggplot(Richness, aes(x = Groups, y = Richness, col= Groups)) +
  geom_point() +
  # facet_wrap(~Groups, scales = "free_y") +
  scale_color_manual(values=c("#258f4e", "#b57560", "#a30808", "blue"))  +
  labs(x = "Macroalgae Groups", y = "Richness")+
  theme_classic() +
  theme(axis.text.y = element_text(face= "plain", size=8),
        axis.text.x=element_text(size=10),
        plot.title = element_text(hjust=0.5, size=14, face="plain", color = "gray20"),
        plot.title.position = "plot",
        legend.position = "",
        # legend.text = element_text(face = "italic"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold.italic"))

ggsave("figs/2016_PCU_richness_MCGroups.png", width = 8.5, height = 4.5, dpi=1000)



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
