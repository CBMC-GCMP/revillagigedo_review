# Load libraries --------------------------------------------------------

library(tidyverse)     # Main library for data manipulation and visualization
library(readxl)        # For reading Excel files
library(dplyr)         # For data manipulation
library(lubridate)     # For working with dates
library(tidyr)         # For data manipulation
library(ggplot2)       # For data visualization
library(tidytext)      # For text manipulation
library(openxlsx)      # For reading and writing Excel files
library(gridExtra)     # For arranging multiple plots in one visualization

# Load data ------------------------------------------------------------

data <- read_rds("data/updates/promares_pnr_update.RDS")   

# Show the structure of the dataset
str(data)

# Create a new variable "Ulate" to classify species into categories 
inverts <- data %>%
  mutate(
    Ulate = case_when(
      Taxa2 == "Hexacorallia" ~ "Hexacorallia",
      Taxa2 == "Octocorallia" ~ "Octocorallia",
      Taxa2 == "Asteroidea" ~ "Asteroidea",
      Taxa1 == "Bivalvia" ~ "Bivalvia",
      Taxa1 == "Gastropoda" ~ "Gastropoda",
      Taxa1 == "Decapoda" ~ "Decapoda",
      Taxa2 == "Echinoidea" ~ "Echinoidea",
      Taxa1 == "Hydrozoa" ~ "Hydrozoa",
      Taxa1 == "Polychaeta" ~ "Polychaeta",
      Taxa2 == "Holothuroidea" ~ "Holothuroidea",
      Taxa2 == "Ophiuroidea" ~ "Ophiuroidea",
      Taxa1 == "Cephalopoda" ~ "Cephalopoda",
      Taxa1 == "Demospongiae" ~ "Demospongiae",
      Taxa1 == "Calcarea"  ~ "Calcarea",
      TRUE ~ Taxa2
    )
  )

# Invertebrate richness ------

richness <- inverts %>%
  # filter(Year == 2023) %>% # Filter a specific year
  filter(Label == "INV", Region == "Revillagigedo") %>%
  group_by(Ulate) %>%
  summarise(richness = n_distinct(Species))

# Plot richness

plot_richness <- ggplot(richness, aes(x = reorder(Ulate, richness), y = richness, fill = Ulate)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(x = " ", y = "Number of species", title = "A)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y.right = element_blank()) + # Remove y-axis labels
  scale_y_continuous(trans = "reverse", breaks = seq(max(richness$richness), 0, by = -1)) +  # Invert y-axis numbers
  scale_x_discrete(position = "top") +  # Place x-axis at the top
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),  # Adjust y-axis title margin
        axis.text.y = element_blank(),  # Remove original y-axis labels
        axis.ticks.y = element_blank()) +  # Remove original y-axis ticks
  theme(axis.text = element_text(size=13),
        axis.title.y = element_text(size=17, face="bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"))+
  coord_flip()

plot_richness

# Average density in 2023 --------

# Filter data for the year 2023
density <- inverts %>%
  # filter(Year == 2023) %>% # Filter a specific year
  filter(Label == "INV", Region == "Revillagigedo") %>%
  group_by(Island, Reef, Transect, Depth2, Ulate, Species) %>%
  # Calculate density by dividing quantity by area
  summarise(Density = Quantity / Area) %>%
  group_by(Ulate) %>%
  summarise(Density = mean(Density, na.rm = TRUE),
            richness = n_distinct(Species))

# Create the bar plot
plot_density <- ggplot(density, aes(x = reorder(Ulate, richness), y = Density, fill = Ulate)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(x = " ", y = "Average density (org/m²)", title = "B)") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +  # Adjust x-axis labels
  coord_flip() +  # Flip the plot to show x-axis labels horizontally
  theme(axis.text = element_text(size=13),  # Adjust x-axis text size
        axis.title.y = element_text(size=17, face="bold"),  # Adjust y-axis title size and style
        legend.text = element_text(size=12),  # Adjust legend text size
        legend.title = element_text(size=12, face="bold"))  # Adjust legend title size and style

plot_density

# Combine both plots
combined_plot <- grid.arrange(plot_richness, plot_density, ncol = 2)

# Save the combined plot as a PNG file
ggsave("figs/promares_pnr_inv_richness-and-density.png", combined_plot, width = 8.5, height = 4.5, dpi = 1000)
