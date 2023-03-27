
# Load libraries ----------------------------------------------------------



library(tidyverse)



# Load data ---------------------------------------------------------------

#CBMC's Long Term Ecological Monitoring database (LTEM) extract from Revillagigedo

ltem <- readRDS("data/LTEM_extract_Revillagigedo.RDS") |>  
  mutate(A_ord = as.numeric(A_ord), 
         B_pen= as.numeric(B_pen),
         Quantity = as.numeric(Quantity ),
         Size=as.numeric(Size),
         Area= as.numeric(Area),
         Biomass = (Quantity * A_ord* (Size^B_pen))/(Area * 100)) |>  
  mutate(TrophicGroup = factor(TrophicGroup, 
                               levels = c("Piscivoro", 
                                          "Carnivoro", 
                                          "Herbivoro", 
                                          "Zooplanctivoro")), 
         Region = factor(Region),
         TrophicLevelF = cut(as.numeric(TrophicLevel), 
                             breaks = c(2, 2.5, 3, 3.5, 4, 4.6), 
                             labels = c("2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-4.5"), 
                             right = FALSE)) |> 
  mutate(Island= factor(Island, levels=c("Socorro", "San Benedicto", "Clarion", "Partida Revillagigedo"),
                        labels=c("Socorro", "San Benedicto", "Clarión", "La Partida"))
  )








# Trophic Group Biomass Composition ---------------------------------------

ltem |> 
  filter(Label=="PEC") |> 
  filter(!is.na(TrophicGroup)) |>                 
  group_by(Island, Depth2, TrophicGroup) |> 
  summarise(Biomass= mean(Biomass)) |> 
  ggplot(aes(fill=TrophicGroup, y=Biomass, x=Island)) +
  
  geom_bar(position="fill", stat="identity", col="black") +
  scale_fill_manual(values=c("#a30808", "#b57560", "#114a06", "#258f4e"))+
  facet_wrap(~Depth2)+
  theme_classic()+
  labs(x="", fill="Trophic Group", y="Biomass (ton/ha)")



# Biomass Island Comparison -----------------------------------------------
unique(ltem$Protection_level)
 ltem |> 
mutate(MPA = factor(Protection_level, levels = c("Prohibited", "Open Area"), labels = c("Fully Protected", "Open Area"))) |> 
  filter(Label == "PEC") |> 
  group_by(MPA, Reef, Depth2, Transect) |>
  # summarise(Biomass = sum(Biomass)) |>
  # group_by(Season, ID_map, MPA, Reef, Transect) |>
  summarise(Biomass = mean(Biomass)) |>
  ggplot(aes(x = Depth2, y = Biomass, fill=Depth2)) +
  geom_violin(trim = F,  alpha = .3) +
  geom_boxplot(width  = 0.1) +
  geom_jitter(width = 0.09, pch = 21,  alpha = .3) +
  scale_color_manual(values = c("#0f2359", "#7AC5CD")) +
  scale_fill_manual(values = c("#0f2359", "#7AC5CD")) +
  # ylim(0, 1.5) +
  labs(x = "", y = "Biomass (Ton/ha)") +
  theme_classic()+
  theme(legend.position = "") +
  guides(colour = "none")

