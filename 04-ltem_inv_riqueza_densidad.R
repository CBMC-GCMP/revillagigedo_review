library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(tidytext)
library(openxlsx)



# data -----
 
ltem <- read_rds("data/ltem_historic_updated_2024-02-27.RDS")#CORRECT & ACTUAL
conanp <- readxl::read_excel("data/correct/conanp_pnr_historic_20231027.xlsx") 

spp <- ltem3 |> 
  filter( is.na(Phylum)) |> 
  distinct (Phylum, Taxa1, Taxa2, Taxa3, IDSpecies,Species, TrophicGroup)
print(spp, n=22)


# Similar a Ulate 2016 --------

invert <- ltem3 %>%
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

taxas <- invert |> 
  filter(Label == "INV", Region == "Revillagigedo") |> 
  distinct(Ulate, Species)
#Resultados-------------


# Riqueza de INV  2023------

inve <- invert %>%
  filter(Year == 2023, 
         Label == "INV",
         Region == "Revillagigedo") %>%
  group_by(Ulate) %>%
  summarise(richness = n_distinct(Species))
  # mutate(Island = str_replace_all(Island, c("Clarión" = "Clarion", "Partida Revillagigedo" = "Roca Partida")))

# Crear el gráfico de barras con Ulate ordenado al revés y el eje x mostrando números enteros

plot_riqueza <- ggplot(inve, aes(x = reorder(Ulate, richness), y = richness, fill = Ulate)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(x = " ", y = "Número de especies", title = "A)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y.right = element_blank()) + # Eliminar las etiquetas del eje y
  # axis.text.y.right = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(trans = "reverse", breaks = seq(max(invh$richness), 0, by = -1)) +  # Invertir los números en el eje y
  scale_x_discrete(position = "top") +  # Colocar el eje x en la parte superior
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),  # Ajustar el margen del título del eje y
        axis.text.y = element_blank(),  # Eliminar las etiquetas del eje y original
        axis.ticks.y = element_blank()) +  # Eliminar las marcas del eje y original
  theme(axis.text = element_text(size=13),
        axis.title.y = element_text(size=17, face="bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"))+
  coord_flip()


# Densidad y riqueza promedio por grupo 2023

densidad23 <- invert %>%
  filter(Year == 2023, Label == "INV", Region == "Revillagigedo") %>%
  group_by(Island, Reef, Transect, Depth2, Ulate, Species) %>%
  summarise(Density = Quantity / Area) %>%
  group_by(Ulate) %>%
  summarise(Density = mean(Density, na.rm = TRUE),
            richness = n_distinct(Species))


plot_densidad <- ggplot(densidad23, aes(x = reorder(Ulate, richness), y = Density, fill = Ulate)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(x = " ", y = "Densidad promedio (org/m²)", title = "B)") +
  theme_classic() +
  theme(
    # axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 12),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  coord_flip()+
  theme(axis.text = element_text(size=13),
        axis.title.y = element_text(size=17, face="bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"))

# Unir ambas gráficas
# grid.arrange(plot_riqueza, plot_densidad, ncol = 2)
# 
# combined_plot23 <- grid.arrange(plot_riqueza, plot_densidad, ncol = 2)
# 
# # Guardar la gráfica combinada como un archivo PNG
# ggsave("figs/inv_pnr-ltem-riqueza-y-densidad-2023_ULATE.png", combined_plot23, width = 8.5, height = 4.5, dpi = 1000)
# # 
# # 
# ggsave("figs/pnr-ltem-riqueza-y-densidad-2023-ulate.pdf", combined_plot23, width = 10, height = 5)

# Densidad y riqueza promedio por grupo Historica ------


# Riqueza de INV ------

invh <- invert %>%
  filter(!Year == 2023) |> 
  filter(Label == "INV", Region == "Revillagigedo") %>%
  group_by(Ulate) %>%
  summarise(richness = n_distinct(Species))
# mutate(Island = str_replace_all(Island, c("Clarión" = "Clarion", "Partida Revillagigedo" = "Roca Partida")))

# Crear el gráfico de barras con Taxa2 ordenado al revés y el eje x mostrando números enteros


ggplot(invh, aes(x = reorder(Ulate, richness), y = richness, fill = Ulate)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(x = " ", y = "Número de especies", title = "a)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, max(invh$richness), by = 1))+  # Establecer los números enteros en el eje y
  coord_flip()+
  theme(axis.text = element_text(size=10),
        axis.title.y = element_text(size=17, face="bold"),
        legend.text = element_text(size=9),
        legend.title = element_text(size=9, face="bold"))
# theme(
#   axis.text = element_text(size = 9),  # Tamaño de los números en los ejes
#   axis.title.y = element_text(size = 9, face = "bold"),  # Tamaño del título del eje y
#   legend.text = element_text(size = 10),  # Tamaño del texto en la leyenda
#   legend.title = element_text(size = 10, face = "bold")  # Tamaño del título en la leyenda
# )

# ggsave("figs/inv_numero-de-especies2006-2016-2017.png", width = 8.5, height = 4.5, dpi=1000)



plot_riqueza <- ggplot(invh, aes(x = reorder(Ulate, richness), y = richness, fill = Ulate)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(x = " ", y = "Número de especies", title = "A)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1),
        axis.text.y.right = element_blank()) + # Eliminar las etiquetas del eje y
        # axis.text.y.right = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(trans = "reverse", breaks = seq(max(invh$richness), 0, by = -1)) +  # Invertir los números en el eje y
  scale_x_discrete(position = "top") +  # Colocar el eje x en la parte superior
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),  # Ajustar el margen del título del eje y
        axis.text.y = element_blank(),  # Eliminar las etiquetas del eje y original
        axis.ticks.y = element_blank()) +  # Eliminar las marcas del eje y original
  coord_flip() +
  theme(axis.text = element_text(size=11),
        axis.title.y = element_text(size=17, face="bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"))




# Densidad y riqueza promedio por grupo 

densidad <- invert %>%
  filter(!Year == 2023, Label == "INV", Region == "Revillagigedo") %>%
  group_by(Island, Reef, Transect, Depth2, Ulate, Species) %>%
  summarise(Density = Quantity / Area) %>%
  group_by(Ulate) %>%
  summarise(Density = mean(Density, na.rm = TRUE),
            richness = n_distinct(Species))

print(invh, n=21)
plot_densidad <- ggplot(densidad, aes(x = reorder(Ulate, richness), y = Density, fill = Ulate)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(x = " ", y = "Densidad promedio (org/m²)", title = "B)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  coord_flip()+
  theme(axis.text = element_text(size=13),
        axis.title.y = element_text(size=17, face="bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"))
  theme(
    axis.text = element_text(size = 9),  # Tamaño de los números en los ejes
    axis.title.y = element_text(size = 9, face = "bold"),  # Tamaño del título del eje y
    legend.text = element_text(size = 10),  # Tamaño del texto en la leyenda
    legend.title = element_text(size = 10, face = "bold")  # Tamaño del título en la leyenda
  )


# Unir ambas gráficas
# grid.arrange(plot_riqueza, plot_densidad, ncol = 2)
# 
# library(gridExtra)
# 
# # Unir ambas gráficas
# combined_plot <- grid.arrange(plot_riqueza, plot_densidad, ncol = 2)
# 
# # Guardar la gráfica combinada como un archivo PNG
# ggsave("figs/pnr-ltem-riqueza-y-densidad-historica_Ulate.png", combined_plot, width = 8.5, height = 4.5, dpi = 1000)
# # Guardar la gráfica combinada como un archivo PDF
# ggsave("figs/pnr-ltem-riqueza-y-densidad-historica_Ulate.pdf", combined_plot, width = 10, height = 5)

# ggsave("figs/inv_densidad_promedio_3-años.png", width = 8.5, height = 4.5, dpi=1000)
