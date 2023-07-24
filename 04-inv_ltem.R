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

rev <- readRDS("data/LTEM_extract_Revillagigedo.RDS") #CBMC's Long Term Ecological Monitoring database (LTEM) extract from Revillagigedo

inv <- rev |>
  filter(Label == "INV") |>
  mutate(Season = case_when(
    Month == 2 ~ "Winter",
    Month == 4 ~ "Spring",
    Month == 3 ~ "Spring",
    Month == 11 ~ "Autumn",
    TRUE ~ "Other"
  )) |>
  mutate(
    Season = factor(Season, levels = c("Winter", "Spring", "Autumn")),
    Year = factor(Year, levels = c(2006, 2016, 2017)),
    Island = factor(Island, levels = c("Socorro", "San Benedicto", "Clarion", "Partida Revillagigedo"),
                    labels = c("Socorro", "San Benedicto", "Clarión", "Roca Partida"))
  )


# Abundances  -----------------

(abu <- inv |> 
   filter(Taxa2 %in% c("Echinoidea", "Octocorallia", "Asteroidea", "Hexacorallia")) |> 
  # filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
  group_by(Year, Season, MPA, Protection_level, Island, Reef, Transect, Depth2, Taxa2, Species) |>
  summarise (Abundance = sum(Quantity, na.rm = T)) |> 
  group_by(Year, Taxa2, Island) |> 
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
ggsave("figs/inv_ltem_island.png", width = 8.5, height = 4.5, dpi=1000)

(abu <- inv |> 
    filter(Taxa2 %in% c("Echinoidea", "Octocorallia", "Asteroidea", "Hexacorallia")) |> 
    # filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
    group_by(Year, Season, MPA, Protection_level, Island, Reef, Transect, Depth2, Taxa2, Species) |> 
    summarise (Abundance = sum(Quantity, na.rm = T)) |> 
    group_by(Year, Taxa2) |> 
    summarise(Abundance = mean(Abundance)) |>
    ggplot(aes(x= Year, y = Abundance, col= Taxa2)) +
    geom_point()+
    # geom_boxplot()+
    # coord_flip()+
    facet_wrap(~Taxa2, scales="free_y") +
    scale_x_reordered() +
    # scale_fill_manual(values=c("#a30808", "#b57560", "#258f4e")) +
    labs(y = "Abundance", x="Year") +
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

ggsave("figs/inv_ltem_groups_years.png", width = 8.5, height = 4.5, dpi=1000)

(abu <- inv |> 
    # filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
    filter(Genus%in% c("Pavona", "Pocillopora", "Porites")) |> 
    group_by(Year, Season, MPA, Protection_level, Island, Reef, Transect, Depth2, Taxa2, Genus, Species) |> 
    summarise (Abundance = sum(Quantity, na.rm = T)) |> 
    group_by(Year, Genus, Island) |> 
    summarise(Abundance = mean(Abundance)) |>
    ggplot(aes(x= Year, y = Abundance, col= Genus)) +
    geom_point()+
    # geom_boxplot()+
    # coord_flip()+
    facet_wrap(~Genus, scales="free_y") +
    scale_x_reordered() +
    scale_fill_manual(values=c("#a30808", "#b57560", "#258f4e")) +
    labs(y = "Abundance", x="Year") +
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

ggsave("figs/inv_ltem.png", width = 8.5, height = 4.5, dpi=1000)

# Density----------------
# Densidad poblacional = Número de individuos / Área o volumen
# mutate(Density =(Quantity / (Area*100))) 

(den <- inv |> 
   filter(Taxa2 %in% c("Echinoidea", "Octocorallia", "Asteroidea", "Hexacorallia")) |> 
   # filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
   group_by(Year, Season, MPA, Protection_level, Island, Reef, Transect, Depth2, Taxa2, Species) |>
   mutate(Density =(Quantity / (Area))) |> 
   summarise (Density = sum(Density, na.rm = T)) |> 
   group_by(Year, Taxa2, Island) |> 
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

ggsave("figs/inv_dens_island.png", width = 8.5, height = 4.5, dpi=1000)

(den <- inv |> 
    filter(Taxa2 %in% c("Echinoidea", "Octocorallia", "Asteroidea", "Hexacorallia")) |> 
    # filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
    group_by(Year, Season, MPA, Protection_level, Island, Reef, Transect, Depth2, Taxa2, Species) |> 
    mutate(Density =(Quantity / (Area))) |> 
    summarise (Density = sum(Density, na.rm = T)) |> 
    group_by(Year, Taxa2) |> 
    summarise(Density = mean(Density)) |>
    ggplot(aes(x= Year, y = Density, col= Taxa2)) +
    geom_point()+
    # geom_boxplot()+
    # coord_flip()+
    facet_wrap(~Taxa2, scales="free_y") +
    scale_x_reordered() +
    # scale_fill_manual(values=c("#a30808", "#b57560", "#258f4e")) +
    labs(y = "Density", x="Year") +
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

ggsave("figs/inv_ltem_density_years.png", width = 8.5, height = 4.5, dpi=1000)

(den <- inv |> 
    # filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
    filter(Genus%in% c("Pavona", "Pocillopora", "Porites")) |> 
    group_by(Year, Season, MPA, Protection_level, Island, Reef, Transect, Depth2, Taxa2, Genus) |> 
    mutate(Density =(Quantity / (Area))) |> 
    summarise (Density = sum(Density, na.rm = T)) |> 
    group_by(Year, Genus, Island) |> 
    summarise(Density = mean(Density)) |>
    ggplot(aes(x= Year, y = Density, col= Genus)) +
    geom_point()+
    # geom_boxplot()+
    # coord_flip()+
    facet_wrap(~Genus, scales="free_y") +
    scale_x_reordered() +
    # scale_fill_manual(values=c("#a30808", "#b57560", "#258f4e")) +
    labs(y = "Density", x="Year") +
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

ggsave("figs/inv_ltem_dens.png", width = 8.5, height = 4.5, dpi=1000)

# Richness  ------------------------------------------------------

(rich <- inv |>
  filter(!is.na(Species)) |>
  # filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
  filter(Taxa2 %in% c("Echinoidea", "Octocorallia", "Asteroidea", "Hexacorallia")) |> 
   
  # filter(Quantity > 1) |>
  group_by(Year, Taxa2) |>
  summarise(Richness = n_distinct(Species)) |>
  
  ggplot(aes(x = Year, y = Richness, col = Taxa2)) +
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


ggsave("figs/rich_inv_ltem.png", width = 8.5, height = 4.5, dpi=1000)



# Tendencia Abundancia ----------------------------------------------

rev |> 
  filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
  group_by(Year, Reef, Depth2, Transect, Taxa2, Genus, Species) |> 
  summarise(Abundance = sum(Quantity)) |> 
  group_by(Year, Taxa2) |> 
  summarise(Abundance =round(mean(Abundance),0)) |> 
  ggplot(aes(x = Year, y = Abundance))+
  geom_point()+
  labs(x = "Year", 
       y = "Abundance")+
  scale_x_continuous(breaks=seq(2005,2018, 2))+
  theme(axis.text.x = element_text(angle=45),
        axis.title =element_text(size=10))+
  #geom_smooth(method = "gam", formula = y ~ s(x, k = 3), method.args = list(family = "poisson"))+
  facet_wrap(~Taxa2)
  # geom_line()

# Tendencia Riqueza ----------------------------------------------

rev |>
  filter(!is.na(Species)) |>
  filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
  # filter(Quantity > 1) |>
  group_by(Year, Taxa2) |>
  summarise(Richness = n_distinct(Species)) |>
  group_by(Year, Taxa2) |> 
  summarise(Richness =round(mean(Richness),0)) |> 
  ggplot(aes(x = Year, y = Richness))+
  geom_point()+
  labs(x = "Year", 
       y = "Richness")+
  scale_x_continuous(breaks=seq(2005,2018, 2))+
  theme(axis.text.x = element_text(angle=45),
        axis.title =element_text(size=10))+
  #geom_smooth(method = "gam", formula = y ~ s(x, k = 3), method.args = list(family = "poisson"))+
  facet_wrap(~Taxa2)


# NMDS-----------------------

set.seed(666) 

community_matrix <- inv |>  
  filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
  group_by(Year, Island,  Reef, Transect, Species) |>    
  summarise(Quantity = sum(Quantity)) |>    
  group_by(Island, Reef, Species) |>    
  summarise(Quantity = round(mean(Quantity),1)) |>    
  pivot_wider(names_from = "Species", values_from = "Quantity") |>    remove_rownames()|>    
  column_to_rownames(var="Reef") #Nombramos a las comunidades como los arrecifes

#Ausencias verdaderas  
community_matrix[is.na(community_matrix)] <- 0 


#Retiramos la columna de Islas
rev_NMDS <- metaMDS(community_matrix[,2:32], #Matriz de arrecife por especie
                    k=2, #A estas dimensiones escalamos el NMDS (Reduced dimensions)
                    trymax =1000) #Número de iteraciones que se van a realizar                                        (cambiar si es necesario)   

stressplot(rev_NMDS)


plot(rev_NMDS)

island <- community_matrix$Island



ordiplot(rev_NMDS,type="n")
orditorp(rev_NMDS,display="species",col="red",air=2, pch = "+")
orditorp(rev_NMDS,display="sites",col=c(rep("firebrick",2),rep("darkgreen",1),
                                        rep("blue",13), rep("orange",14 )),
         air=3,cex=0.3)
ordihull(rev_NMDS,groups=island,draw="polygon",label=T, fill="white", col =c("firebrick", "darkgreen", "lightblue", "gold"), alpha = 0.25 )
# ordilabel(rev_NMDS, labels=island, fill="white", border=NA)


print(unique(island))


# NMDS----------------------------

set.seed(666) 

community_matrix <- inv |>  
  filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
  group_by(Year, Island,  Reef, Transect, Taxa2) |>    
  summarise(Quantity = sum(Quantity)) |>    
  group_by(Island, Reef, Taxa2) |>    
  summarise(Quantity = round(mean(Quantity),1)) |>    
  pivot_wider(names_from = "Taxa2", values_from = "Quantity") |>    remove_rownames()|>    
  column_to_rownames(var="Island") #Nombramos a las comunidades como los arrecifes

#Ausencias verdaderas  
community_matrix[is.na(community_matrix)] <- 0 


#Retiramos la columna de Islas
rev_NMDS <- metaMDS(community_matrix[,2:4], #Matriz de arrecife por especie
                    k=2, #A estas dimensiones escalamos el NMDS (Reduced dimensions)
                    trymax =1000) #Número de iteraciones que se van a realizar                                        (cambiar si es necesario)   

stressplot(rev_NMDS)


plot(rev_NMDS)

island <- community_matrix$Island



ordiplot(rev_NMDS,type="n")
orditorp(rev_NMDS,display="species",col="red",air=2, pch = "+")
orditorp(rev_NMDS,display="sites",col=c(rep("firebrick",2),rep("darkgreen",1),
                                        rep("blue",13), rep("orange",14 )),
         air=3,cex=0.3)
ordihull(rev_NMDS,groups=island,draw="polygon",label=T, fill="white", col =c("firebrick", "darkgreen", "lightblue", "gold"), alpha = 0.25 )
# ordilabel(rev_NMDS, labels=island, fill="white", border=NA)


print(unique(island))
