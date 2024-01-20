library(tidyverse)
library(sf)
library(patchwork)
library(ggthemes)
library(rnaturalearth)
library(ggspatial)
library(ggrepel)



# Crear el objeto espacial de México

mx <- read_sf("data/mexico/gadm41_MEX_0.shp")

# Leer los datos de sitios de buceo
dive_sites <- read.csv("data/pnr_dive_sites.csv") %>%
        filter(Island %in% c("Socorro", "San Benedicto", "Clarion", "Roca Partida"))

# Resumir y transformar en objeto espacial
sitios <- dive_sites %>%
        group_by(Island, Site, Longitude, Latitude) %>%
        summarise(Latitude = mean(Latitude),
                  Longitude = mean(Longitude))
 
sitios_sf <- st_as_sf(sitios, coords = c("Longitude", "Latitude"), crs = 4326)


# Crear un data.frame con las coordenadas y etiquetas de las islas

islas_data <- data.frame(
        Island = c("Socorro", "San Benedicto", "Clarion"),
        Lon = c(-110.975278, -110.816111, -114.723333),
        Lat = c(18.788056, 19.303333, 18.356389),
        Label = c("Socorro", "San Benedicto",  "Clarion")
)

# Mapa del Parque Nacional Revillagigedo -----------

mapa_pnr <- ggplot() +
        geom_sf(data = mx, col = NA, fill = "bisque3") +
        geom_sf(data = sitios_sf, size = 1, shape = 21, col = "#242424", fill = "blue") +
        geom_text_repel(data = sitios_sf,
                        aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], label = Site),
                        size = 2, box.padding = 1, segment.size = 0.2) +
        geom_text_repel(data = islas_data,
                        aes(x = Lon, y = Lat, label = Label),
                        size = 3, box.padding = 0.4, segment.size = 0, fontface = "bold") +
        xlab("Longitud") + ylab("Latitud") +
        coord_sf(xlim = c(-114.8, -110.3), ylim = c(18.2, 19.6)) +
        theme_bw() +
        ggtitle("Parque Nacional Revillagigedo") +
        theme(legend.position = "",
              panel.grid = element_blank(), 
              panel.border = element_rect(fill = NA, color = "gray90"),
              strip.text = element_text(face = "italic"), 
              strip.background = element_blank(), 
              axis.text.x = element_text(angle = 90),
              plot.title = element_text(size = 9)) +
        annotation_north_arrow(location = "tl", which_north = "true",
                               pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                               style = north_arrow_minimal)  +
        annotation_scale(location ="br") 

print(mapa_pnr)

#Save file -------------
# ggsave("mapas/mapa_islas.pdf", plot = mapa_pnr, width = 10, height = 7)



sitios_socorro <- sitios_sf %>% filter(Island == "Socorro")
sitios_clarion <- sitios_sf %>% filter(Island == "Clarion")
sitios_sanbenedicto <- sitios_sf %>% filter(Island == "San Benedicto")
sitios_partida <- sitios_sf %>% filter(Island == "Roca Partida")


# Mapa de la isla Socorro----------------

mapa_socorro <- ggplot() +
        geom_sf(data = mx, col = NA, fill = "#EEDFCC") +
        geom_sf(data = sitios_socorro, size = 1.9, shape = 21, col = "#242424", fill = "blue") +
        geom_text_repel(data = sitios_socorro,
                        aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = Site),
                        size = 3, box.padding = 0.15, segment.size = 0) +
        xlab("Longitud") + ylab("Latitud") +
        coord_sf(xlim = c(-111.1, -110.85), ylim = c(18.7, 18.9)) +
        theme_bw() +
        ggtitle("Isla Socorro") +
        theme(legend.position = "",
              panel.grid = element_blank(), 
              panel.border = element_rect(fill = NA, color = "gray90"),
              strip.text = element_text(face = "italic"), 
              strip.background = element_blank(), 
              axis.text.x = element_text(angle = 90),
              plot.title = element_text(size = 9)) +
        annotation_north_arrow(location = "bl", which_north = "true",
                               pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                               style = north_arrow_minimal)  +
        annotation_scale(location ="tr") 

mapa_socorro


# Mapa de la Isla Clarion ----------------

isla_clarion <- ggplot() +
        geom_sf(data = mx, col = NA, fill = "#EEDFCC") +
        geom_sf(data = sitios_clarion, size = 1.9, shape = 21, col = "#242424", fill = "blue") +
        geom_text_repel(data = sitios_clarion,
                        aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = Site),
                        size = 3, box.padding = 0.15, segment.size = 0) +
        xlab("Longitud") + ylab("Latitud") +
        coord_sf(xlim = c(-114.64, -114.82), ylim = c(18.28, 18.45)) +
        theme_bw() +
        ggtitle("Isla Clarion") +
        theme(legend.position = "",
              panel.grid = element_blank(), 
              panel.border = element_rect(fill = NA, color = "gray90"),
              strip.text = element_text(face = "italic"), 
              strip.background = element_blank(), 
              axis.text.x = element_text(angle = 90),
              plot.title = element_text(size = 9)) +
        annotation_north_arrow(location = "bl", which_north = "true",
                               pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                               style = north_arrow_minimal)  +
        annotation_scale(location ="tr") 

isla_clarion


# Mapa de la isla San Benedicto ----------------

isla_sanbenedicto <- ggplot() +
        geom_sf(data = mx, col = NA, fill = "#EEDFCC") +
        geom_sf(data = sitios_sanbenedicto, size = 1.9, shape = 21, col = "#242424", fill = "blue") +
        geom_text_repel(data = sitios_sanbenedicto,
                        aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = Site),
                        size = 3, box.padding = 0.15, segment.size = 0) +
        xlab("Longitud") + ylab("Latitud") +
        coord_sf(xlim = c(-110.75, -110.88), ylim = c(19.25, 19.37)) +
        theme_bw() +
        ggtitle("Isla San Benedicto") +
        theme(legend.position = "",
              panel.grid = element_blank(), 
              panel.border = element_rect(fill = NA, color = "gray90"),
              strip.text = element_text(face = "italic"), 
              strip.background = element_blank(), 
              axis.text.x = element_text(angle = 90),
              plot.title = element_text(size = 9)) +
        annotation_north_arrow(location = "bl", which_north = "true",
                               pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                               style = north_arrow_minimal)  +
        annotation_scale(location ="tr") 

isla_sanbenedicto


# Mapa de la isla Roca Partida ----------------
# isla_partida <- ggplot() +
#         geom_sf(data = mx, col = NA, fill = "#EEDFCC") +
#         geom_sf(data = sitios_partida, size = 1.9, shape = 21, col = "#242424", fill = "blue") +
#         xlab("Longitud") + ylab("Latitud") +
#         coord_sf(xlim = c(-112.08, -112.84), ylim = c(18.9, 19)) +
#         # coord_sf(xlim = c(-112.1, -112.05), ylim = c(18.98, 19))+
#         theme_bw() +
#         ggtitle("Isla Roca Partida") +
#         theme(legend.position = "",
#               panel.grid = element_blank(), 
#               panel.border = element_rect(fill = NA, color = "gray90"),
#               strip.text = element_text(face = "italic"), 
#               strip.background = element_blank(), 
#               axis.text.x = element_text(angle = 90),
#               plot.title = element_text(size = 9)) +
#         annotation_north_arrow(location = "bl", which_north = "true",
#                                pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
#                                style = north_arrow_minimal)  +
#         annotation_scale(location ="tr") 
# 
# isla_partida



library(gridExtra)

arranged_map <- grid.arrange(mapa_socorro, isla_sanbenedicto, isla_clarion, ncol = 3)

ggsave( "figs/pnr_islas_socorro_sb_clarion.png", plot = arranged_map, width = 15, height = 10, dpi=1000)


# Guardar los mapas como archivos PDF ------------------

ggsave("mapas/mapa_islas.pdf", plot = mapa_pnr, width = 10, height = 7)
ggsave("mapas/mapa_socorro.pdf", plot = mapa_socorro, width = 10, height = 7)
ggsave("mapas/mapa_sanbenedicto.pdf", plot = isla_sanbenedicto, width = 10, height = 7)
ggsave("mapas/mapa_clarion.pdf", plot = isla_clarion, width = 10, height = 7)

ggsave("figs/mapa_islas.png", plot = mapa_pnr, width = 15, height = 10, dpi=1000)
