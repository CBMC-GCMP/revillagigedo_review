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


# format date

inv$date <- paste(inv$Date, "2016")

# inv$date <- as.Date(inv$date, format = "%d %B %Y")
head(inv)


# Rename columns 

inv <- inv|> 
  rename(Size = `Test Diameter`)
inv <- inv|>
  rename(Quantity = Number)
head (inv)

# Clean database

inv <- filter(inv, Species != "none", Species != "None")

inv <- inv |> 
  mutate (Species = recode(Species, "Diadema mexicanum?" = "Diadema mexicanum"))


# Abundance by Islands

abu_inv <- inv |> 
  # filter(Quantity> 1) |>
  group_by(Island, Site, Quadrat, Species) |> 
  summarise (Abundance = sum(Quantity)) |> 
  group_by(Island, Species) |>  
  # summarise(Abundance = mean(Abundance)) |> 
  group_by(Island) |> 
  ungroup() |> 
  mutate(Species= reorder_within(Species, Abundance, Island)) |>  
  ggplot(aes(x=Species, y = Abundance, fill=Island)) +
  geom_col()+
  coord_flip()+
  facet_wrap(~Island, scales="free_y")+
  scale_x_reordered() +
  scale_fill_manual(values = brewer.pal(n = 4, name = "Set1"))+
  labs(y = "Abundance", x="", title= "Invertebrates per Island") +
  theme(legend.position = "") +
  guides(colour = "none")+
  theme(axis.text.y = element_text(face= "italic"),
        plot.title = element_text(hjust=0.5)
  )

abu_inv

ggsave("figs/2016_inv_abundance_islands.png", width = 8.5, height = 4.5, dpi=1000)

# stacked bar chart

Abun <- inv |> 
  filter(!is.na(Species)) |> 
  # filter(Quantity> 1) |>
  group_by(Island, Species) |> 
  summarise(Abundance = sum(Quantity)) |> 
  ggplot(aes(x = Island, y = Abundance, fill = Species)) +
  geom_bar(stat = "identity", position = position_stack())+
  theme(legend.text = element_text(face = "italic"))

Abun 

ggsave("figs/2016_inv_abundance_islands_PS.png", width = 8.5, height = 4.5, dpi=1000)


# Density by Island

den_inv <- inv |> 
  filter(Quantity> 1) |>
  group_by(Island, Site, Quadrat, Species) |> 
  # summarise(Density = sum(Quantity)/ (10*100)) |> # (Area*100))  
  summarise(Density = sum(Quantity/10)*100) |> 
  group_by(Island, Species) |> 
  summarise(Density = mean(Density)) |>
  group_by(Island) |> 
  # top_n(10, Density) |> 
  ungroup() %>% 
  mutate(Species= reorder_within(Species, Density, Island)) |> 
  ggplot(aes(x=Species, y = Density, fill=Island)) +
  geom_col()+
  coord_flip()+
  facet_wrap(~Island, scales="free_y")+
  scale_x_reordered() +
  # scale_color_material_d() +
  scale_fill_manual(values = brewer.pal(n = 4, name = "Set1"))+
  # labs(y = "Density (organisms/ha)", x="", title= "2016") +
  labs(y = "Density", x="", title= "Invertebrates per Island") +
  theme(legend.position = "") +
  guides(colour = "none")+
  theme(axis.text.y = element_text(face= "italic"),
        plot.title = element_text(hjust=0.5)
  )
den_inv

ggsave("figs/2016_inv_density_islands.png", width = 8.5, height = 4.5, dpi=1000)

# Richness by Island
Richness <- inv  |> 
  filter(!is.na(Species)) |> 
  # filter(Quantity> 1) |>
  group_by(Island, Species) |> 
  summarise(Richness= length(unique(Species))) 
 
  ggplot(Richness, aes(x = Island, y = Richness)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Island", y = "Richness", title = "Invertebrates per island")+
  theme(legend.text = element_text(face = "italic"))


ggsave("figs/2016_inv_richness_islands.png", width = 8.5, height = 4.5, dpi=1000)


  ggplot(Richness, aes(x = Island, y = Richness, fill = Species)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "Island", y = "Richness", title = "Invertebrates per island")+
  theme(legend.text = element_text(face = "italic"))


ggsave("figs/2016_inv_richness_spp.png", width = 8.5, height = 4.5, dpi=1000)

# Shannon-Wiener, H index 

Shannon <- inv |> 
  filter(!is.na(Species)) |> 
  group_by(Island, Site, Depth, Species) |> 
  summarise(Quantity=sum(Quantity)) |> 
  group_by(Island, Site, Depth) |> 
  mutate(Diversity= vegan::diversity(Quantity, "shannon")) 

  ggplot(Shannon, aes(x = Island, y = Diversity, fill=Species)) +
  geom_bar(stat = "identity") +
  labs(x = "Island", y = "Shannon (H)", title = "Invertebrates diversity")+
  theme(legend.text = element_text(face = "italic"))

ggsave("Figs/Shannon_INV_Diversity.png", width = 8.5, height = 4.5, dpi=1000)


# Simpson index 1-D

Simpson <- inv |> 
  filter(!is.na(Species)) |> 
  group_by(Island, Site, Depth, Species) |> 
  summarise(Quantity=sum(Quantity)) |> 
  group_by(Island, Site, Depth) |> 
  mutate(Diversity= vegan::diversity(Quantity, "simpson"))

ggplot(Simpson, aes(x = Island, y = Diversity, fill=Species)) +
  geom_bar(stat = "identity") +
  labs(x = "Island", y = "Simpson", title = "Invertebrates diversity")+
  theme(legend.text = element_text(face = "italic"))

ggsave("Figs/Simpson_INV_Diversity.png", width = 8.5, height = 4.5, dpi=1000)


# Inverse Simpson, 1/D

invD2 <- inv |>
  filter(!is.na(Species)) |>
  group_by(Island, Site, Depth, Species) |>
  summarise(Quantity=sum(Quantity)) |>
  group_by(Island, Site, Depth) |>
  mutate(Diversity= vegan::diversity(Quantity, "invsimpson"))

ggplot(invD2, aes(x = Island, y = Diversity, fill=Species)) +
  geom_bar(stat = "identity") +
  labs(x = "Island", y = "Inverse Simpson", title = "Invertebrates diversity")+
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

ggsave("Figs/indexes_INV_Diversity.png", width = 8.5, height = 4.5, dpi=1000)

