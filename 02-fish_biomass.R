# Load libraries --------------------------------------------------------

library(tidyverse)   # Library that includes various data manipulation libraries.
library(readxl)      # Library for reading Excel files.
library(dplyr)       # Library for data manipulation.
library(lubridate)   # Library for working with dates.
library(tidyr)       # Library for data manipulation.
library(ggplot2)     # Library for plotting data.
library(tidytext)    # Library for working with text data.
library(mgcv)        # Library for generalized statistical modeling.
library(openxlsx)    # Library for reading and writing Excel files.

# Load data ------------------------------------------------------------

data <- readRDS("data/updates/promares_pnr_update.RDS") 


# Fish biomass --------------------------------------------------------

# Calculate fish biomass
fish <- data |> 
  filter(Label == "PEC") |> 
  mutate(
    A_ord = as.numeric(A_ord),
    B_pen= as.numeric(B_pen),
    Quantity = as.numeric(Quantity),
    Size=as.numeric(Size),
    Area= as.numeric(Area),
    Month= as.numeric(Month),
    Biomass = (Quantity * A_ord * (Size^B_pen))/(Area * 100)) |> # Formula to calculate biomass (ton/ha)
  mutate(Biomass = as.numeric(Biomass))

# Plot biomass in 2023 by island -------------------------------------

fish |>
  filter(!is.na(TrophicGroup), Region == "Revillagigedo", Year == 2023) |>
  mutate(Island = factor(Island,
                         levels = c("Socorro", "San Benedicto", "Roca Partida", "Clarion"),
                         labels = c("Socorro", "San Benedicto", "Roca Partida", "Clarion"))) |> 
  group_by(Year, Island, Reef, Transect, Depth2, TrophicGroup) |>
  summarise(Biomass = sum(Biomass)) |>
  group_by(Island, TrophicGroup) |>
  summarise(Biomass = mean(Biomass)) |> # Average biomass by island and trophic group
  ggplot(aes(x = Island, y = Biomass, fill = TrophicGroup, col=TrophicGroup)) +
  geom_bar(position = "fill", stat = "identity", alpha=0.85) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(name="Trophic group", values = c("#a30808", "#b57560",  "#258f4e", "#114a06")) +
  scale_color_manual(name="Trophic group", values = c("#a30808", "#b57560",  "#258f4e", "#114a06")) +
  labs(x = "", y = "Relative biomass (%)", fill = "Trophic groups") +
  theme_classic() +
  theme(axis.text = element_text(size=13),
        axis.title.y = element_text(size=17, face="bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"))

# Save figure ----------------------------------------------------------

# Save the figure in PNG format
ggsave("figs/promares_pnr_fish_biomass_2023.png", width = 8.5, height = 4.5, dpi=1000)

# Save the figure in PDF format
ggsave("figs/promares_pnr_fish_biomass_2023.pdf", width = 10, height = 5)


# Revillagigedo National Park -------

# Calculate biomass for the Revillagigedo region by year

(p <- fish |> 
   filter(!is.na(TrophicGroup), Region == "Revillagigedo") |>
   mutate(Region = "Revillagigedo") |>  # Set region as "Revillagigedo"
   mutate(Year = factor(Year, levels = c(2006, 2016, 2017, 2023))) |>  # Factorize the year
   group_by(Year, Region, Island, Reef, Transect, Depth2, TrophicGroup) |>  # Group data
   summarise(Biomass = sum(Biomass)) |>  # Sum biomass
   group_by(Year, Region, TrophicGroup) |>  # Group by year, region, and trophic group
   summarise(Biomass = mean(Biomass)) |>  # Calculate average biomass
   ggplot(aes(x=Year , y=Biomass, fill=TrophicGroup)) +  # Create the plot
   geom_bar(position = "fill", stat="identity", col="black") +  # Add bars
   scale_fill_manual(values=c("#a30808", "#b57560", "#114a06", "#258f4e"),  guide = "none") +  # Customize colors
   facet_wrap(~Region) +  # Split by region
   labs(x = "Year", y = "Biomass (ton/ha)", fill = "Trophic Group") +  # Labels
   theme_classic())

# Save figure 

ggsave("figs/promares_pnr_fish_biomass_historic.png", width = 8.5, height = 4.5, dpi=1000)
