# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(tidytext)
library(mgcv)
library(openxlsx)
library(vegan)
library(BiodiversityR)

# Load data ---------------------------------------------------------------

# Metodología PROMARES

ltem <- readRDS("data/correct/ltem_historic_20231109.RDS")

# Abundancia DE INV ---------------

inv <- ltem |> 
  filter(Label=="INV", Region =="Revillagigedo")

# inv |> 
#   filter(!is.na(Biomass)) |>
#   filter(!is.na(TrophicGroup)) |>
#   # filter(Year >= 2017) |>
#   group_by(Year, Island, Reef, Transect, Depth2, Taxa2) |>
#   summarise (Abundance = sum(Quantity, na.rm = T)) |> 
#   group_by(Year, Island, Reef, Transect, Depth2, Taxa2) |> 
#   summarise(Abundance = mean(Abundance)) |>
#   mutate(Year = as.Date(paste0(Year, "-01-01"), "%Y-%m-%d")) |>  
#   ggplot(aes(x=sqrt(sqrt((Abundance))), fill=Island))+
#   geom_density(alpha=.4)

(abu <- inv |> 
   # filter(Taxa2 %in% c("Echinoidea", "Octocorallia", "Asteroidea", "Hexacorallia")) |> 
     # filter(Year >= 2017) |>
   group_by(Year, MPA, Protection_level, Island, Reef, Transect, Depth2, Taxa2, Species) |>
   summarise (Abundance = sum(Quantity, na.rm = T)) |> 
    group_by(Year, Island, Reef, Transect, Depth2) |>
    summarise(Abundance = mean(Abundance)) |>
    mutate(Year = as.Date(paste0(Year, "-01-01"), "%Y-%m-%d")) |> ggplot( aes(x = Year, y = Abundance)) +
    geom_smooth(aes(x = Year, y = Abundance, color = Island, group = Island), method = "loess", se = F) +
    geom_point(aes(x = Year, y = Abundance, color = Island, group = Island), alpha = .3) +
    geom_smooth(col = "black")+
    
    labs(title = "Invertebrates abundance",
         x = "Year",
         y = "Abundance") +
    theme_classic())

(abu <- inv |> 
    # filter(Taxa2 %in% c("Echinoidea", "Octocorallia", "Asteroidea", "Hexacorallia")) |> 
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

# ggsave("figs/inv_ltem_groups_years.png", width = 8.5, height = 4.5, dpi=1000)

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

# ggsave("figs/inv_ltem.png", width = 8.5, height = 4.5, dpi=1000)

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




set.seed(666)

community_matrix <- inv |>  
  # filter(Taxa2 %in% c("Hexacorallia", "Octocorallia", "Echinoidea")) |>
  group_by(Year, Island,  Reef, Transect, Species) |>    
  summarise(Quantity = sum(Quantity)) |>    
  group_by(Reef, Species) |>    
  summarise(Quantity = round(mean(Quantity),1)) |>    
  pivot_wider(names_from = "Species", values_from = "Quantity") |>    remove_rownames()|>    
  column_to_rownames(var="Reef") #Nombramos a las comunidades como los arrecifes

#Ausencias verdaderas  
community_matrix[is.na(community_matrix)] <- 0 


#Retiramos la columna de Islas
rev_NMDS <- metaMDS(community_matrix[,2:79], #Matriz de arrecife por especie
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


community_matrix

Riqueza=specnumber(community_matrix)#para calcular el numero de especies

Riqueza
barplot(Riqueza)


# abundancia ----
# Familias con mayor presencia

boxplot(community_matrix, ylab="Abundance", xlab = "",
        legend = rownames(community_matrix), las = 2,
        cex.lab =1,
        cex.axis = 0.5)

# Indice de Shannon

H=diversity(community_matrix, index = "shannon")
H

# fisher alpha

fisher.alpha(community_matrix, se=T)


# Indice de Simpson
# cuantifica la equidas, especies similares
simp=diversity(community_matrix, index = "simpson")
simp


# Inverso simpson
invsimp=diversity(community_matrix, index = "inv")
invsimp

# relacion entre H e invsimp

plot(invsimp, H, pch=21, bg=3, cex=1.9)
grid()
identify(invsimp, H)

# curva de acumulación de species
# Esfuerzo de muestreo. se aplanan?

curva = specaccum(community_matrix)
curva

plot(curva, ci.type = "poly", col = "blue", lwd=2, ci.lty = 0,
     ci.col = "grey90")

