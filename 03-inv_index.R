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
# 
# inv$date <- paste(inv$Date, "2016")
# inv$date <- as.Date(inv$date, format = "%d %B %Y")

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
  group_by(Island, Species) |> 
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

ggsave("figs/2016_inv_abundance_island.png", width = 8.5, height = 4.5, dpi=1000)

# Density by Island

den_inv <- inv |> 
  group_by(Island, Site, Quadrat, Species) |> 
  summarise(Density = sum(Quantity/10)) |>  #10m2 o 6.25
  group_by(Island, Species) |> 
  # summarise(Density = mean(Density)) |>
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
  labs(y = "Density (organisms/m2)", x="", title= "Invertebrates per Island") +
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
  filter(Quantity> 1) |>
  group_by(Island) |> 
  summarise(Richness= length(unique(Species))) 
 
  ggplot(Richness, aes(x = Island, y = Richness)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Island", y = "Richness", title = "Invertebrates per island")+
  theme(legend.text = element_text(face = "italic"))


ggsave("figs/2016_inv_richness_islands.png", width = 8.5, height = 4.5, dpi=1000)


# Shannon-Wiener, H index 

Shannon <- inv |>
  filter(!is.na(Species)) |> 
  group_by(Island, Site, Depth, Species) |> 
  summarise(Quantity=sum(Quantity)) |> 
  group_by(Island, Site, Depth) |> 
  mutate(Diversity= vegan::diversity(Quantity, "shannon")) 

ggplot(Shannon, aes(x = Island, y = Diversity, fill=Island)) +
  geom_violin(trim = F,  alpha = .3) +
  geom_boxplot(width  = 0.1) +
  geom_jitter(width = 0.09, pch = 21,  alpha = .3) +
  scale_color_manual(values = c("#0f2359", "#7AC5CD", "red", "darkred")) +
  scale_fill_manual(values = c("#0f2359", "#7AC5CD", "red", "darkred")) +
  # ylim(0, 1.5) +
  labs(x = "", y = "Shannon (H)") +
  theme_classic()+
  theme(legend.position = "") +
  guides(colour = "none")

ggplot(data =Shannon,
       mapping = aes(x=Island,
                     y= Diversity))+
  geom_violin(trim=FALSE, aes(fill=Island))+
  geom_jitter(height = 0, width = 0.0001) + 
  geom_boxplot(width=0.1) + 
  theme_classic()+
  theme(legend.position = "",
        plot.title = element_text(size=10, colour = "gray"))+
  guides(colour = "none")+
  ggtitle("Invertebrates diversity")+
  labs(x = "", y = "Shannon (H)")

ggplot(Shannon, aes(x = Island, y = Diversity, fill = Island)) +
  geom_violin(trim = FALSE, scale = "width") +
  geom_jitter(width = 0.2, size = 2, alpha = 0.5) +
  labs(x = "Island", y = "Shannon (H)") +
  theme_minimal()

ggplot(Shannon, aes(x = Island, y = Diversity)) +
  geom_bar(stat = "summary", fun = "mean", fill = "red") +
  labs(x = "Island", y = "Shannon") +
  theme_classic()

ggsave("Figs/2016_inv_Shannon_Diversity.png", width = 8.5, height = 4.5, dpi=1000)


# Simpson index 1-D

Simpson <- inv |> 
  filter(!is.na(Species)) |> 
  group_by(Island, Site, Depth, Species) |> 
  summarise(Quantity=sum(Quantity)) |> 
  group_by(Island, Site, Depth) |> 
  mutate(Diversity= vegan::diversity(Quantity, "simpson"))

ggplot(Simpson, aes(x = Island, y = Diversity)) +
  geom_bar(stat = "summary", fun = "mean", fill = "red") +
  labs(x = "Island", y = "Simpson") +
  theme_classic()

ggplot(Simpson, aes(x = Island, y = Diversity, fill = Island)) +
  geom_boxplot() +
  xlab("Island") + ylab("Simpson") +
  ggtitle("Invertebrates diversity") +
  theme_classic()

ggplot(data =Simpson,
       mapping = aes(x=Island,
                     y= Diversity))+
  geom_violin(trim=FALSE, aes(fill=Island))+
  geom_jitter(height = 0, width = 0.0001) + 
  geom_boxplot(width=0.1) + 
  theme_classic()+
  theme(legend.position = "",
        plot.title = element_text(size=10, colour = "gray"))+
  guides(colour = "none")+
  ggtitle("Invertebrates diversity")+
  labs(x = "", y = "Simpson")

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

ggsave("Figs/Simpson_INV_Diversity.png", width = 8.5, height = 4.5, dpi=1000)

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

ggsave("Figs/2016_inv_indixes_Diversity.png", width = 8.5, height = 4.5, dpi=1000)

