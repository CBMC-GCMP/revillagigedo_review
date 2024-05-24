# Cargar librerías --------------------------------------------------------

library(tidyverse)   # Librería que incluye varias librerías de manipulación de datos.
library(readxl)      # Librería para leer archivos de Excel.
library(dplyr)       # Librería para manipulación de datos.
library(lubridate)   # Librería para trabajar con fechas.
library(tidyr)       # Librería para manipulación de datos.
library(ggplot2)     # Librería para graficar datos.
library(tidytext)    # Librería para trabajar con datos de texto.
library(mgcv)        # Librería para modelado estadístico generalizado.
library(openxlsx)    # Librería para leer y escribir archivos de Excel.

# Cargar datos ------------------------------------------------------------

datos <- readRDS("data/promares_pnr_historic.RDS") 


# Biomasa de peces --------------------------------------------------------

# Calculamos la biomasa de peces
fish <- datos |> 
  filter(Label == "PEC") |> 
  mutate(
    A_ord = as.numeric(A_ord),
    B_pen= as.numeric(B_pen),
    Quantity = as.numeric(Quantity),
    Size=as.numeric(Size),
    Area= as.numeric(Area),
    Month= as.numeric(Month),
    Biomass = (Quantity * A_ord * (Size^B_pen))/(Area * 100)) |> # Fórmula para calcular la biomasa (ton/ha)
  mutate(Biomass = as.numeric(Biomass))

# Graficar la biomasa en 2023 por isla -------------------------------------

fish |>
  filter(!is.na(TrophicGroup), Region == "Revillagigedo", Year == 2023) |>
  mutate(Island = factor(Island,
                         levels = c("Socorro", "San Benedicto", "Roca Partida", "Clarion"),
                         labels = c("Socorro", "San Benedicto", "Roca Partida", "Clarion"))) |> 
  group_by(Year, Island, Reef, Transect, Depth2, TrophicGroup) |>
  summarise(Biomass = sum(Biomass)) |>
  group_by(Island, TrophicGroup) |>
  summarise(Biomass = mean(Biomass)) |> # Promediar la biomasa por isla y grupo trófico
  ggplot(aes(x = Island, y = Biomass, fill = TrophicGroup, col=TrophicGroup)) +
  geom_bar(position = "fill", stat = "identity", alpha=0.85) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(name="Grupo trófico", values = c("#a30808", "#b57560",  "#258f4e", "#114a06")) +
  scale_color_manual(name="Grupo trófico", values = c("#a30808", "#b57560",  "#258f4e", "#114a06")) +
  labs(x = "", y = "Biomasa relativa (%)", fill = "Grupos tróficos") +
  theme_classic() +
  theme(axis.text = element_text(size=13),
        axis.title.y = element_text(size=17, face="bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"))

# Guardar figura ----------------------------------------------------------

# Guardamos la figura en formato PNG
ggsave("figs/promares_pnr_pec_biomasa_2023.png", width = 8.5, height = 4.5, dpi=1000)

# Guardamos la figura en formato PDF
ggsave("figs/promares_pnr_pec_biomasa_2023.pdf", width = 10, height = 5)


# PN Revilla -------

# Calculamos la biomasa para la región de Revillagigedo por año

(p <- fish |> 
   filter(!is.na(TrophicGroup), Region == "Revillagigedo") |>
   mutate(Region = "Revillagigedo") |>  # Establecemos la región como "Revillagigedo"
   mutate(Year = factor(Year, levels = c(2006, 2016, 2017, 2023))) |>  # Factorizamos el año
   group_by(Year, Region, Island, Reef, Transect, Depth2, TrophicGroup) |>  # Agrupamos los datos
   summarise(Biomass = sum(Biomass)) |>  # Sumamos la biomasa
   group_by(Year, Region, TrophicGroup) |>  # Agrupamos por año, región y grupo trófico
   summarise(Biomass = mean(Biomass)) |>  # Calculamos el promedio de la biomasa
   ggplot(aes(x=Year , y=Biomass, fill=TrophicGroup)) +  # Creamos el gráfico
   geom_bar(position = "fill", stat="identity", col="black") +  # Agregamos las barras
   scale_fill_manual(values=c("#a30808", "#b57560", "#114a06", "#258f4e"),  guide = "none") +  # Personalizamos los colores
   facet_wrap(~Region) +  # Dividimos por región
   labs(x = "Año", y = "Biomasa (ton/ha)", fill = "Grupo Trófico") +  # Etiquetas
   theme_classic())

# Guardar figura 

ggsave("figs/promares_pnr_pec_biomasa_historica.png", width = 8.5, height = 4.5, dpi=1000)
