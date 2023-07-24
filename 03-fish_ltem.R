# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidytext)
library(lubridate)
library(RColorBrewer)
library(gridExtra)
library(scales)
library(vegan)

# Load data ---------------------------------------------------------------

# CBMC's Long Term Ecological Monitoring database (LTEM) extract from Revillagigedo

rev <- readRDS("data/LTEM_extract_Revillagigedo.RDS") |> 
  filter(Label == "PEC") 


ltem16 <- readRDS("data/ltem_revillagigedo_2016_12072023.RDS") |> 
  subset(select = -STATION) |> 
  mutate(Habitat.x = case_when(
    Habitat.x == "Wall" ~ "BLOQUES",
    Habitat.x == "Boulders" & Reef == "SOCORRO_2" ~ "PARED",
    Habitat.x == "Boulders" & Reef == "SOCORRO_3" ~ "BLOQUES",
    TRUE ~ Habitat.x)) |> 
  mutate(
         # Year= factor(Year, levels=c(2006, 2016, 2017)),
         Biomass=as.numeric(Biomass),
         TrophicGroup = factor(TrophicGroup, 
                               levels = c("Piscivoro", 
                                          "Carnivoro", 
                                          "Herbivoro", 
                                          "Zooplanctivoro")), 
         Region = factor(Region),
         TrophicLevelF = cut(as.numeric(TrophicLevel), 
                             breaks = c(2, 2.5, 3, 3.5, 4, 4.6), 
                             labels = c("2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-4.5"), 
                             right = FALSE)) 


#  Fish --------------------------------------------------------------------

fish_ltem <- readRDS("data/LTEM_historic_updated_27122022.RDS") |> 
  filter(Label == "PEC") |>  
  mutate(A_ord = as.numeric(A_ord), 
         B_pen= as.numeric(B_pen),
         Quantity = as.numeric(Quantity),
         Size=as.numeric(Size),
         Area= as.numeric(Area),
         Month= as.numeric(Month),
         Biomass = (Quantity * A_ord* (Size^B_pen))/(Area * 100)) |>  
  mutate(
         # Year= factor(Year, levels=c(2006, 2016, 2017)),
         Biomass=as.numeric(Biomass),
         TrophicGroup = factor(TrophicGroup, 
                               levels = c("Piscivoro", 
                                          "Carnivoro", 
                                          "Herbivoro", 
                                          "Zooplanctivoro")), 
         Region = factor(Region),
         TrophicLevelF = cut(as.numeric(TrophicLevel), 
                             breaks = c(2, 2.5, 3, 3.5, 4, 4.6), 
                             labels = c("2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-4.5"), 
                             right = FALSE)) 


# fish <- rbind(fish_ltem, ltem16) |> 
#   filter(!is.na(Biomass)) |>
#   mutate(Season= case_when(Month==12 |Month==2|Month==1 ~"Winter",
#                            Month==3| Month== 4 | Month==5 ~"Spring",
#                            Month==6| Month== 7| Month== 8 ~"Summer", 
#                            Month==11| Month== 9| Month== 10 ~"Autumn",
#                            TRUE~"Other"),
#          Season= factor(Season, levels=c("Winter", "Spring", "Summer", "Autumn")),
#     Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Autumn")))
# 
# fish_Revilla <- rbind(fish_ltem, ltem16) |>
#   # filter(Region == "Revillagigedo") |>
#   mutate(Season= case_when(Month==12 |Month==2|Month==1 ~"Winter",
#                            Month==3| Month== 4 | Month==5 ~"Spring",
#                            Month==6| Month== 7| Month== 8 ~"Summer",
#                            Month==11| Month== 9| Month== 10 ~"Autumn",
#                            TRUE~"Other"),
#          Season= factor(Season, levels=c("Winter", "Spring", "Summer", "Autumn")),
#          Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Autumn"))) |>
#          mutate(Island = factor(Island,
#                             levels = c("Socorro", "San Benedicto", "Clarion", "Partida Revillagigedo"),
#                             labels = c("Socorro", "San Benedicto", "Clarión", "Roca Partida")))
# 
# saveRDS(fish_Revilla, "data/correct/LTEM_fish_14022023.RDS")

# Trophic Group Biomass Composition ---------------------------------------

(p1 <- fish |> 
    filter(!is.na(TrophicGroup), Region == "Revillagigedo") |>
    mutate(Island = factor(Island,
                            levels = c("Socorro", "San Benedicto", "Partida Revillagigedo", "Clarion"),
                            labels = c("Socorro", "San Benedicto", "Roca Partida", "Clarion"))) |> 
    group_by(Year, Island, Reef, Transect, Depth2, TrophicGroup, Species) |> 
    summarise(Biomass = sum(Biomass)) |>  
    group_by(Island, TrophicGroup) |>  
    summarise(Biomass = mean(Biomass)) |> 
    ggplot(aes(x=Island , y=Biomass, fill=TrophicGroup)) +
    geom_bar(position = "fill", stat="identity", col="black")+
    scale_fill_manual(values=c("#a30808", "#b57560", "#114a06", "#258f4e")) +
    # facet_wrap(~)+
    labs(x = "", y = "Biomass (Ton/ha)", fill = "Trophic Group") +
    theme_classic())

ggsave("figs/fish_ltem.png", width = 8.5, height = 4.5, dpi=1000)


(p <- fish |> 
    filter(!is.na(TrophicGroup), Region == "Revillagigedo") |>
    mutate(Region = "Revillagigedo") |> 
    mutate(Region = ifelse(Island == "Partida Revillagigedo", "Roca Partida", Region)) |> 
    mutate(Region = factor(Region, levels = c("Revillagigedo", "Roca Partida"))) |>
    mutate(Year = factor(Year, levels = c(2006, 2016, 2017))) |> 
    group_by(Year, Region, Island, Reef, Transect, Depth2, TrophicGroup, Species) |> 
    summarise(Biomass = sum(Biomass)) |>  
    group_by(Year, Region, TrophicGroup) |>  
    summarise(Biomass = mean(Biomass)) |> 
    ggplot(aes(x=Year , y=Biomass, fill=TrophicGroup)) +
    geom_bar(position = "fill", stat="identity", col="black")+
    scale_fill_manual(values=c("#a30808", "#b57560", "#114a06", "#258f4e"),  guide = "none") +
    facet_wrap(~Region)+
    labs(x = "Year", y = "Biomass (Ton/ha)", fill = "Trophic Group") +
    theme_classic())

ggsave("figs/fish_ltem_Year.png", width = 8.5, height = 4.5, dpi=1000)


# Biomass Region Comparison -----------------------------------------------

(p2 <- fish |>
    mutate(Region = case_when(
      Island == "Partida Revillagigedo" ~ "Roca Partida",
      TRUE ~ Region )) |>
    mutate(Region = factor(Region, levels = c("Revillagigedo", "Roca Partida", "Islas Marias", "Los Cabos", "Cabo Pulmo","La Paz", "Loreto"))) |>
    filter(!is.na(TrophicGroup), Region %in% c("Revillagigedo", "Roca Partida", "Islas Marias", "Los Cabos", "Cabo Pulmo","La Paz", "Loreto")) |>
    group_by(Year, Region, Reef, Transect, Depth2, TrophicGroup, Species, Season) |>
    summarise(Biomass = sum(Biomass)) |>
    group_by(Region, TrophicGroup) |>
    summarise(Biomass = mean(Biomass)) |>
    ggplot(aes(x = Region, y = Biomass, fill = TrophicGroup)) +
    geom_bar(position = "fill", stat = "identity", col = "black") +
    scale_fill_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e")) +
    labs(x = "", y = "Biomass (Ton/ha)", fill = "Trophic Group") +
    theme_classic())

ggsave("figs/LTEM_fish_biomass_comparassion.png", width = 8.5, height = 4.5, dpi=1000)

# Biomass Comparison -----------------------------------------------
unique(ltem$Protection_level)
(p3 <- fish |> 
  filter(!is.na(TrophicGroup), Region %in% c("Loreto", "Cabo Pulmo", "Los Cabos", "Revillagigedo", "La Paz", "Islas Marias")) |> 
  mutate(MPA = factor(Protection_level, levels = c("Prohibited", "Open Area"), labels = c("Fully Protected", "Open Area"))) |> 
  group_by(Region, MPA, Reef, Depth2, Transect) |>
  summarise(Biomass = mean(Biomass)) |>
  ggplot(aes(x = Region, y = Biomass, fill=Region)) +
  geom_violin(trim = F,  alpha = .3) +
  geom_boxplot(width  = 0.1) +
  geom_jitter(width = 0.09, pch = 21,  alpha = .3) +
  scale_color_manual(values = c("#0f2359", "#7AC5CD")) +
  labs(x = "", y = "Biomass (Ton/ha)") +
  theme_classic()+
  theme(legend.position = "") +
  guides(colour = "none")
)



# Tendencia Biomasa ----------------------------------------------

(p4 <- fish |>
  filter(!is.na(TrophicGroup), Region %in% c("Revillagigedo")) |>
  group_by(Year, Region, Reef, Transect, Depth2, TrophicGroup, Species, Season) |>
  summarise(Biomass = sum(Biomass)) |>
  group_by(Year, Region, TrophicGroup) |>
  summarise(Biomass = mean(Biomass)) |>
  ggplot(aes(x= Year, y = Biomass, col= TrophicGroup)) +
  geom_point()+
  # coord_flip()+
  facet_wrap(~TrophicGroup, scales="free_y")
)

(p5 <- fish |>
  filter(!is.na(TrophicGroup), Region %in% c("Revillagigedo")) |>
  group_by(Year, Region, Reef, Depth2, Transect, TrophicGroup, Genus, Species) |> 
  summarise(Biomass = sum(Biomass)) |> 
  group_by(Year, TrophicGroup) |> 
  summarise(Biomass =round(mean(Biomass),0)) |> 
  ggplot(aes(x = Year, y = Biomass, col= TrophicGroup))+
  geom_point()+
  labs(x = "Year", 
       y = "Biomass")+
  scale_x_continuous(breaks=seq(2005,2018, 2))+
  theme(axis.text.x = element_text(angle=45),
        axis.title =element_text(size=10))+
  #geom_smooth(method = "gam", formula = y ~ s(x, k = 3), method.args = list(family = "poisson"))+
  facet_wrap(~TrophicGroup) 
)




# NMDS ---------------


set.seed(666) 

fish <- rev |> 
  filter(Label == "PEC") |>  
  mutate(A_ord = as.numeric(A_ord), 
         B_pen= as.numeric(B_pen),
         Quantity = as.numeric(Quantity ),
         Size=as.numeric(Size),
         Area= as.numeric(Area),
         Biomass = (Quantity * A_ord* (Size^B_pen))/(Area * 100)) |>  
  mutate(Season= case_when(Month==2| Month==12 ~"Winter",
                           Month==3| Month== 4 ~"Spring",
                           Month==6| Month==7| Month== 8 ~"Summer", 
                           Month==11| Month== 9| Month== 10 ~"Autumn",
                           TRUE~"Other"),
         Season= factor(Season, levels=c("Winter", "Spring", "Autumn")),
         # Year= factor(Year, levels=c(2006, 2016, 2017)),
         Biomass=as.numeric(Biomass),
         TrophicGroup = factor(TrophicGroup, 
                               levels = c("Piscivoro", 
                                          "Carnivoro", 
                                          "Herbivoro", 
                                          "Zooplanctivoro")), 
         Region = factor(Region),
         TrophicLevelF = cut(as.numeric(TrophicLevel), 
                             breaks = c(2, 2.5, 3, 3.5, 4, 4.6), 
                             labels = c("2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-4.5"), 
                             right = FALSE)) |> 
  mutate(Island = factor(Island,
                         levels = c("Socorro", "San Benedicto", "Clarion", "Partida Revillagigedo"),
                         labels = c("Socorro", "San Benedicto", "Clarión", "Roca Partida"))) 


community_matrix <- fish |>    
  group_by(Year, Island,  Reef, Transect, TrophicGroup) |>    
  summarise(Quantity = sum(Quantity)) |>    
  group_by(Island, Reef, TrophicGroup) |>    
  summarise(Quantity = round(mean(Quantity),1)) |>    
  pivot_wider(names_from = "TrophicGroup", values_from = "Quantity") |>    remove_rownames()|>    
  column_to_rownames(var="Reef") #Nombramos a las comunidades como los arrecifes

#Ausencias verdaderas  
community_matrix[is.na(community_matrix)] <- 0 


#Retiramos la columna de Islas
rev_NMDS <- metaMDS(community_matrix[,2:5], #Matriz de arrecife por especie
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


# NMDS POR SPP-----------------------------------

fish <- f |> 
  filter(Label == "PEC") |>  
  mutate(A_ord = as.numeric(A_ord), 
         B_pen= as.numeric(B_pen),
         Quantity = as.numeric(Quantity ),
         Size=as.numeric(Size),
         Area= as.numeric(Area),
         Biomass = (Quantity * A_ord* (Size^B_pen))/(Area * 100)) |>  
  mutate(Season= case_when(Month==2| Month==12 ~"Winter",
                           Month==3| Month== 4 ~"Spring",
                           Month==6| Month==7| Month== 8 ~"Summer", 
                           Month==11| Month== 9| Month== 10 ~"Autumn",
                           TRUE~"Other"),
         Season= factor(Season, levels=c("Winter", "Spring", "Autumn")),
         # Year= factor(Year, levels=c(2006, 2016, 2017)),
         Biomass=as.numeric(Biomass),
         TrophicGroup = factor(TrophicGroup, 
                               levels = c("Piscivoro", 
                                          "Carnivoro", 
                                           "Herbivoro", 
                                          "Zooplanctivoro")), 
         Region = factor(Region),
         TrophicLevelF = cut(as.numeric(TrophicLevel), 
                             breaks = c(2, 2.5, 3, 3.5, 4, 4.6), 
                             labels = c("2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-4.5"), 
                             right = FALSE)) |> 
  mutate(Island = factor(Island,
                         levels = c("Socorro", "San Benedicto", "Clarion", "Partida Revillagigedo"),
                         labels = c("Socorro", "San Benedicto", "Clarión", "Roca Partida"))) 


community_matrix <- fish |>    
  group_by(Year, Island,  Reef, Transect, Species) |>    
  summarise(Quantity = sum(Quantity)) |>    
  group_by(Island, Reef, Species) |>    
  summarise(Quantity = round(mean(Quantity),1)) |>    
  pivot_wider(names_from = "Species", values_from = "Quantity") |>    remove_rownames()|>    
  column_to_rownames(var="Reef") #Nombramos a las comunidades como los arrecifes

#Ausencias verdaderas  
community_matrix[is.na(community_matrix)] <- 0 


#Retiramos la columna de Islas
rev_NMDS <- metaMDS(community_matrix[,2:72], #Matriz de arrecife por especie
                    k=2, #A estas dimensiones escalamos el NMDS (Reduced dimensions)
                    trymax =1000) #Número de iteraciones que se van a realizar                                        (cambiar si es necesario)   

stressplot(rev_NMDS) #Se genera un gráfico (stressplot) para evaluar la bondad del ajuste del NMDS


plot(rev_NMDS)

island <- community_matrix$Island



ordiplot(rev_NMDS,type="n") #Se genera un gráfico vacío (ordiplot) para los resultados del NMDS.
orditorp(rev_NMDS,display="species",col="red",air=2, pch = "+") #Se añaden etiquetas
orditorp(rev_NMDS,display="sites",col=c(rep("firebrick",2),rep("darkgreen",1),
                                        rep("blue",13), rep("orange",14 )),
         air=3,cex=0.3)
ordihull(rev_NMDS,groups=island,draw="polygon",label=T, fill="white", col =c("firebrick", "darkgreen", "lightblue", "gold"), alpha = 0.25 ) #Se dibuja el poolígono
# ordilabel(rev_NMDS, labels=island, fill="white", border=NA)


print(unique(island))


# NMDS POR Functional_groups-------------------------------------------

fish <- rev |> 
  filter(Label == "PEC") |>  
  mutate(A_ord = as.numeric(A_ord), 
         B_pen= as.numeric(B_pen),
         Quantity = as.numeric(Quantity ),
         Size=as.numeric(Size),
         Area= as.numeric(Area),
         Biomass = (Quantity * A_ord* (Size^B_pen))/(Area * 100)) |>  
  mutate(Season= case_when(Month==2| Month==12 ~"Winter",
                           Month==3| Month== 4 ~"Spring",
                           Month==6| Month==7| Month== 8 ~"Summer", 
                           Month==11| Month== 9| Month== 10 ~"Autumn",
                           TRUE~"Other"),
         Season= factor(Season, levels=c("Winter", "Spring", "Autumn")),
         # Year= factor(Year, levels=c(2006, 2016, 2017)),
         Biomass=as.numeric(Biomass),
         TrophicGroup = factor(TrophicGroup, 
                               levels = c("Piscivoro", 
                                          "Carnivoro", 
                                          "Herbivoro", 
                                          "Zooplanctivoro")), 
         Region = factor(Region),
         TrophicLevelF = cut(as.numeric(TrophicLevel), 
                             breaks = c(2, 2.5, 3, 3.5, 4, 4.6), 
                             labels = c("2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-4.5"), 
                             right = FALSE)) |> 
  mutate(Island = factor(Island,
                         levels = c("Socorro", "San Benedicto", "Clarion", "Partida Revillagigedo"),
                         labels = c("Socorro", "San Benedicto", "Clarión", "Roca Partida"))) |> 
  # mutate(Reef = case_when(Reef== "CLARION_ROCA_CUERVO"| Reef== "CLARION_ROCA_MONUMENTO"  ~ "Clarión",
  #                         Reef== "SAN_BENEDICTO_BOILER" | Reef== "SAN_BENEDICTO_OESTE_NORTE"|Reef== "SAN_BENEDICTO_CANION"  ~ "SAN_BENEDICTO",
  #                         Reef=="SOCORRO_PUNTA_NORTE"| Reef== "SOCORRO_ROCA_ONEIL"| Reef== "SOCORRO_CABO_PEARCE_SUR" ~ "SOCORRO",
  #                         Reef== "SAN_BENEDICTO_OESTE"| Reef== "SAN_BENEDICTO_FONDEADERO" | Reef== "SAN_BENEDICTO_CUEVITAS" | Reef=="SAN_BENEDICTO_OESTE_SUR" ~ "SAN_BENEDICTO",
  #                         Reef=="SOCORRO_BAJO_PEDRO"| Reef== "SOCORRO_CABO_PEARCE_NORTE" ~ "SOCORRO",
  #                         Reef== "ROCA_PARTIDA" ~ "ROCA_PARTIDA")) |> 
  mutate(Functional_groups = case_when(Functional_groups=="grazers" |Functional_groups== "browsers"~ "Herbivoro",
                                       Functional_groups==  "corallivore" |Functional_groups== "micro-invertivores" ~ "Invertivores",
                                       Functional_groups== "pisci-invertivores"|Functional_groups== "piscivores" ~ "Piscivoro",
                                       Functional_groups== "planktivores" |Functional_groups== "macro-invertivores" ~ "Zooplanctivoro",
                                       Functional_groups== "detritivores" ~ "Detritivores",
                                       Functional_groups== "excavator/scraper" ~ "excavator"))



community_matrix <- fish |>    
  group_by(Year, Island,  Reef, Transect, Functional_groups) |>    
  summarise(Quantity = sum(Quantity)) |>    
  group_by(Island, Reef, Functional_groups) |>    
  summarise(Quantity = round(mean(Quantity),1)) |>    
  pivot_wider(names_from = "Functional_groups", values_from = "Quantity") |>    remove_rownames()|>    
  column_to_rownames(var="Reef") #Nombramos a las comunidades como los arrecifes

#Ausencias verdaderas  
community_matrix[is.na(community_matrix)] <- 0 


#Retiramos la columna de Islas
rev_NMDS <- metaMDS(community_matrix[,2:7], #Matriz de arrecife por especie
                    k=2, #A estas dimensiones escalamos el NMDS (Reduced dimensions)
                    trymax =1000) #Número de iteraciones que se van a realizar                                        (cambiar si es necesario)   

stressplot(rev_NMDS) #Se genera un gráfico (stressplot) para evaluar la bondad del ajuste del NMDS


plot(rev_NMDS)

island <- community_matrix$Island



ordiplot(rev_NMDS,type="n") #Se genera un gráfico vacío (ordiplot) para los resultados del NMDS.
orditorp(rev_NMDS,display="species",col="red",air=2, pch = "+") #Se añaden etiquetas
orditorp(rev_NMDS,display="sites",col=c(rep("firebrick",2),rep("darkgreen",1),
                                        rep("blue",13), rep("orange",14 )),
         air=3,cex=0.3)
ordihull(rev_NMDS,groups=island,draw="polygon",label=T, fill="white", col =c("firebrick", "darkgreen", "lightblue", "gold"), alpha = 0.25 ) #Se dibuja el poolígono
# ordilabel(rev_NMDS, labels=island, fill="white", border=NA)


print(unique(island))


unique(rev$TrophicGroup)
unique(rev$Functional_groups)

# Gererar un NMDS COMPARANDO LOS AÑOS 2006 Y 2017

