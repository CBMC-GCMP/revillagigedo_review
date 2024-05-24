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

promares <- readRDS("data/promares_pnr_historic.RDS") 
conanp <- readxl::read_excel("data/conanp_pnr_historic.xlsx", sheet = "data") 

glimpse(conanp)


# Combinar datos de conanp y promares para calcular la riqueza de invertebrados -------
invert <- bind_rows(
  conanp %>% 
    filter(Label == "INV") %>% 
    mutate(base = "conanp") %>% 
    group_by(Year, Island, Reef, Transect, base) %>% 
    summarise(richness = n_distinct(Species)),
  promares %>% 
    filter(Label == "INV", Region == "Revillagigedo") %>% 
    mutate(Transect = as.numeric(Transect), base = "promares") %>% 
    group_by(Year, Island, Reef, Transect, base) %>% 
    summarise(richness = n_distinct(Species))
) %>% 
  mutate(Island = str_replace_all(Island, "Clarión", "Clarion"))

# Crear un gráfico de violín para mostrar la riqueza de invertebrados a lo largo del tiempo
invert %>% 
  filter(!Year < 2015 | !Year > 2010) %>% 
  group_by(Year, Island, Reef) %>% 
  summarise(richness = mean(richness)) %>% 
  ggplot(aes(x=factor(Year), y=richness)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = .1) +
  labs(x = "", y = "Riqueza", title = "Riqueza de invertebrados") +
  ylim(0, 21) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))

# Guardar el gráfico como una imagen
ggsave("figs/riqueza_invertebrados_overtime.png", dpi = 300, height = 5, width = 5)


# Se crea un gráfico de violín para mostrar la riqueza de invertebrados por isla

grafico_riqueza <- invert %>% 
  filter(!Year < 2015 | !Year > 2010) %>% # Solo se incluyen los años entre 2010 y 2015
  group_by(Year, Island, Reef) %>% 
  summarise(richness = mean(richness)) %>% 
  mutate(Island = factor(Island, levels = c("Clarion", "Roca Partida", "Socorro", "San Benedicto"))) %>% # Se convierte la variable 'Island' en un factor
  ggplot(aes(x=factor(Island), y=richness)) +
  geom_violin(trim = FALSE) + 
  geom_boxplot(width = .1) +
  labs(x = "", y = "Riqueza", title = "Riqueza de invertebrados") +
  ylim(0, 21) +
  theme_bw() +
  theme(axis.text.x = element_text(vjust = .5))

grafico_riqueza

# Guardar el gráfico como una imagen
ggsave("figs/riqueza_invertebrados_islas.png", dpi = 300, height = 5, width = 5)



# Peces ----------

# Combinar los datos de 'conanp' y 'promares' relacionados con peces ('PEC')
peces <- bind_rows(
  conanp %>% 
    filter(Label == "PEC") %>% 
    mutate(base = "conanp") %>% 
    group_by(Year, Island, Reef, Transect, base) %>% 
    summarise(biomass = sum(`ton/ha`)),
  promares %>% 
    filter(Label == "PEC", Region == "Revillagigedo") %>% 
    mutate(Transect = as.numeric(Transect), base = "promares") %>% 
    group_by(Year, Island, Reef, Transect, base) %>% 
    summarise(biomass = sum(Biomass))
) %>% 
  mutate(Island = str_replace_all(Island, "Clarión", "Clarion"))


peces %>% 
  group_by(Year, Island, Reef) %>% 
  summarise(biomass = log1p(mean(biomass))) %>% 
  group_by(Island) %>% 
  mutate(mean_biom = mean(biomass)) %>% 
  mutate(partida = ifelse(Island == "Roca Partida", "Roca Partida", "Other")) %>% 
  ggplot(aes(x=factor(Year), y=(biomass))) +
  geom_violin(trim = F) +
  geom_boxplot(width = .1) +
  labs(x = "", y = "Biomasa de peces (ton/ha)") +
  geom_hline(aes(yintercept = mean_biom)) +
  ylim(0, 7.5) +
  facet_wrap(Island~.,) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        strip.background = element_blank())

# Guardar el gráfico como una imagen
ggsave("figs/biomasa_peces_tiempo.png", dpi = 300, height = 5, width = 5)


peces %>% 
  group_by(Year, Island, Reef) %>% 
  summarise(biomass = log1p(mean(biomass))) %>% 
  # Convertir la variable 'Isla' a un factor con niveles específicos
  mutate(Island = factor(Island, levels = c("Clarion", "Roca Partida", "Socorro", "San Benedicto"))) %>% 
  # Crear el gráfico de violín y boxplot
  ggplot(aes(x=factor(Island), y=biomass)) +
  geom_violin(trim = F) +  # Gráfico de violín sin ajuste de ancho
  geom_boxplot(width = .1) +  
  labs(x = "", y = "Biomasa de peces (ton/ha)") +  # Etiquetas de los ejes
  ylim(0, 7.5) +  # Limitar el eje y
  theme_bw() +  # Tema de fondo blanco y líneas grises
  theme(axis.text.x = element_text(vjust = .5))  # Ajustar etiquetas del eje x

# Guardar el gráfico como una imagen
ggsave("figs/biomasa_peces_por_isla.png", dpi = 300, height = 5, width = 5)




# Riqueza promedio de invertebrados (INV) y peces (PEC) 2023 ---------------

rich <- promares |> 
  filter(Year == 2023) |> # Filtrar los datos para obtener solo el año 2023
  
  # Convertir la variable Transect a numérica
  mutate(Transect = as.numeric(Transect)) |> 
  
  # Agrupar los datos por etiqueta, isla, arrecife y transecto, y calcular la riqueza promedio de especies
  group_by(Label, Island, Reef, Transect) |> 
  summarise(richness = n_distinct(Species)) 

# Visualizar la riqueza promedio de especies por isla y arrecife
rich %>% 
  group_by(Label, Island, Reef) %>% 
  summarise(richness = mean(richness)) 

# Crear un gráfico de violín y boxplot para visualizar la distribución de la riqueza de especies en 2023
rich %>% 
  group_by(Label, Island, Reef) %>% 
  
  # Convertir la variable 'Isla' a factor y establecer un orden específico para las islas
  mutate(Island = factor(Island, levels = c("Clarion", "Roca Partida", "Socorro", "San Benedicto"))) %>% 
  
  ggplot(aes(x=factor(Island), y=richness)) +
  geom_violin(trim = F) +
  geom_boxplot(width = .1) +
  facet_wrap(~Label)+
  labs(x = "", y = "Riqueza", title = "Riqueza de especies") +
  ylim(0, 45) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = .5))

# Guardar el gráfico como una imagen
ggsave("figs/promares_pnr_riqueza_islas_2023.png", width = 8.5, height = 4.5, dpi=1000)


# Calcular la riqueza de especies de peces por año (promares) -------------
rich_fish <- promares |> 
  filter(Label == "PEC") |> # Filtrar por Label: invertebrados (INV) y peces (PEC)
  group_by(Year, Region, Island, Reef, Transect, TrophicGroup) |> 
  summarise(richness = n_distinct(Species))

# Sumarizar la riqueza de peces para cada combinación de año, región, arrecife y grupo trófico
rich_fish %>% 
  group_by(Year, Region, Reef, TrophicGroup) |> 
  summarise(richness = sum(richness)) |> 
  
  # Crear un gráfico de violín y boxplot para visualizar la distribución de la riqueza de peces por región y grupo trófico
  ggplot(aes(x=factor(Year), y=richness)) +
  geom_violin(trim = F) +
  geom_boxplot(width = .1) +
  labs(x = "", y = "Riqueza", title = "Riqueza de peces") +
  theme_bw() +
  theme(axis.text.x = element_text( vjust = .5))

# Guardar el gráfico como una imagen 
ggsave("figs/promares_pnr_pec_riqueza_historica.png", dpi = 300, height = 5, width = 5)
