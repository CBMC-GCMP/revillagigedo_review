# Cargar librerías --------------------------------------------------------

library(tidyverse)     # Librería principal para manipulación y visualización de datos
library(readxl)        # Para leer archivos de Excel
library(dplyr)         # Para manipulación de datos
library(lubridate)     # Para trabajar con fechas
library(tidyr)         # Para manipulación de datos
library(ggplot2)       # Para visualización de datos
library(tidytext)      # Para manipulación de texto
library(openxlsx)      # Para leer y escribir archivos de Excel
library(gridExtra)     # Para organizar múltiples gráficos en una sola visualización

# Cargar datos ------------------------------------------------------------

datos <- read_rds("data/promares_pnr_historic.RDS")   

# Mostrar la estructura del conjunto de datos
str(datos)

# Crear una nueva variable "Ulate" para clasificar las especies en categorías 
invert <- datos %>%
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


# Riqueza de INV  ------

riqueza <- invert %>%
  # filter(Year == 2023) %>% # Filtrar un año en especifico
  filter(Label == "INV", Region == "Revillagigedo") %>%
  group_by(Ulate) %>%
  summarise(richness = n_distinct(Species))

# Graficar riqueza

plot_riqueza <- ggplot(riqueza, aes(x = reorder(Ulate, richness), y = richness, fill = Ulate)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(x = " ", y = "Número de especies", title = "A)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y.right = element_blank()) + # Eliminar las etiquetas del eje y
  scale_y_continuous(trans = "reverse", breaks = seq(max(riqueza$richness), 0, by = -1)) +  # Invertir los números en el eje y
  scale_x_discrete(position = "top") +  # Colocar el eje x en la parte superior
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),  # Ajustar el margen del título del eje y
        axis.text.y = element_blank(),  # Eliminar las etiquetas del eje y original
        axis.ticks.y = element_blank()) +  # Eliminar las marcas del eje y original
  theme(axis.text = element_text(size=13),
        axis.title.y = element_text(size=17, face="bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"))+
  coord_flip()

plot_riqueza

# Densidad promedio 2023 --------

# Filtrar los datos para el año 2023
densidad <- invert %>%
  filter(Year == 2023, Label == "INV", Region == "Revillagigedo") %>%
  group_by(Island, Reef, Transect, Depth2, Ulate, Species) %>%
  # Calcular la densidad dividiendo la cantidad por el área
  summarise(Density = Quantity / Area) %>%
  group_by(Ulate) %>%
  summarise(Density = mean(Density, na.rm = TRUE),
            richness = n_distinct(Species))

# Crear el gráfico de barras
plot_densidad <- ggplot(densidad, aes(x = reorder(Ulate, richness), y = Density, fill = Ulate)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(x = " ", y = "Densidad promedio (org/m²)", title = "B)") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +  # Ajustar etiquetas del eje x
  coord_flip() +  # Girar el gráfico para mostrar las etiquetas del eje x de manera horizontal
  theme(axis.text = element_text(size=13),  # Ajustar el tamaño del texto del eje x
        axis.title.y = element_text(size=17, face="bold"),  # Ajustar el tamaño y el estilo del título del eje y
        legend.text = element_text(size=12),  # Ajustar el tamaño del texto de la leyenda
        legend.title = element_text(size=12, face="bold"))  # Ajustar el tamaño y el estilo del título de la leyenda

plot_densidad

# Unir ambas gráficas
combined_plot <- grid.arrange(plot_riqueza, plot_densidad, ncol = 2)

# Guardar la gráfica combinada como un archivo PNG
ggsave("figs/promares_pnr_inv_riqueza-y-densidad.png", combined_plot, width = 8.5, height = 4.5, dpi = 1000)

