library(openxlsx)
library(dplyr)
library(readxl)


# INV Carlos ---------

carlos <- read_xlsx("data/20240120 Revillagigedo Base Invertebrados PFA - CONANP_FIN.xlsx")

str(carlos)

spp <- carlos |> 
  select(Label, Isla, Arrecife, Habitat, Phylum, Taxa1, Taxa2, Taxa3, Family, Especie, Origen, Año) |> 
  distinct(Label, Phylum, Taxa1, Taxa2, Taxa3, Family, Especie, Año)


# ruta_archivo <- "data/listado_de_spp_carlos_sanchez.xlsx"

# Exportar el dataframe a un archivo Excel
# write.xlsx(spp, file = ruta_archivo)

# Algas Elisa Serviere -----------

algas <- read_xlsx("data/Base Identificación Revillagigedo 2024_02_07.xlsx")
str(algas)
head(algas)

mac <- algas |> 
  mutate(fuente = "Elisa_Serviere") |> 
  mutate(Label = "Algas") |> 
  distinct(Label, Phylum, Clase,`Orden (actual)`, `Familia (actual)`, `Nombre aceptado`, `Año de colecta`, fuente)

ruta_archivo <- "data/listado_de_spp_elisa_serviere.xlsx"

# Exportar el dataframe a un archivo Excel
write.xlsx(mac, file = ruta_archivo)

# Algas Elisa Serviere Historica -----------

elisa <- read_xlsx("data/correct/ESZ_macroalgas_2024-02-13.xlsx")
str(elisa)
head(elisa)

mac2 <- elisa |> 
  mutate(fuente = "Serviere_et_al") |> 
  mutate(Label = "ALG") |> 
  distinct(phyllum_valid,class_valid, order_valid, family_valid,  genus_reported, specie_reported, sp_f_var_aff_valid, subspecie_valid_2)



mac22 <- elisa %>%
  mutate(fuente = "Serviere_et_al") %>%
  mutate(Label = "ALG") %>%
  distinct(phyllum_valid, class_valid, order_valid, family_valid, genus_reported, specie_reported, sp_f_var_aff_valid, subspecie_valid_2, citation, year) %>%
  mutate(across(c(genus_reported, specie_reported, sp_f_var_aff_valid, subspecie_valid_2), ~ coalesce(., ""))) %>%
  unite(Especie, genus_reported, specie_reported, sp_f_var_aff_valid, subspecie_valid_2, sep = " ")

mac22 <- mac22 %>%
  mutate(Especie = gsub('"', '', trimws(Especie)))


colnames(mac22)

library(dplyr)
library(stringr)

mac22 <- mac22 %>%
  mutate(
    Phylum = str_to_title(gsub("_valid", "", phyllum_valid)),
    Class = str_to_title(gsub("_valid", "", class_valid)),
    Order = str_to_title(gsub("_valid", "", order_valid)),
    Family = str_to_title(gsub("_valid", "", family_valid))
  ) %>%
  select(-phyllum_valid, -class_valid, -order_valid, -family_valid) # Eliminamos las columnas antiguas

alghisto <- mac22 |> 
  mutate(base = "historico") |> 
  mutate(fuente = "Serviere_et_al") |> 
  mutate(Label = "ALG") |> 
  rename(Año_reportada = year, Fuente_historica = citation)


# CORALES Jenny -----------

jenny <- read_csv("data/correct/JCRV_salud_coralina_salud_coralsp_final.csv")
str(jenny)
head(jenny)

coral <- jenny |> 
  mutate(fuente = "jenny_rodriguez") |> 
  mutate(Label = "estado_coralino") |> 
  distinct(Label, Species, Year, fuente)

ruta_archivo <- "data/listado_de_spp_jenny_rodriguez.xlsx"

# Exportar el dataframe a un archivo Excel
write.xlsx(coral, file = ruta_archivo)


# Diatomeas Omar Lopez -----------------

listadi_diatomeas <- read_excel("data/listadi_diatomeas.xlsx", sheet = "Listado floristico") |> 
  utate(fuente = "omar_lopez") |> 
  mutate(Label = "diatomeas") |> 
  mutate(Year = "2023")
View(listadi_diatomeas)


listadi_diatomeas <- read_excel("data/listadi_diatomeas.xlsx", sheet = "Listado floristico") |> 
  mutate(fuente = "omar_lopez") |> 
  mutate(Label = "diatomeas") |> 
  mutate(Year = "2023")

nuevo_registro <- read_excel("data/listadi_diatomeas.xlsx", sheet = "Nuevos registros") |> 
  mutate(fuente = "omar_lopez") |> 
  mutate(nuevo_registro = "nuevo_registro") |> 
  mutate(Year = "2023")

# diatomeas historico  -------

historico <- read.xlsx("data/listados/listadi_diatomeas.xlsx", sheet = "historico") |> 
  mutate(fuente = "Lopez_et_al") |> 
  mutate(Label = "diatomeas") |> 
  mutate(historico = "historico")

head(historico)  

historico$Especies <- tolower(historico$Especies)  # Convertir todo a minúsculas
historico$Especies <- paste0(toupper(substr(historico$Especies, 1, 1)), substr(historico$Especies, 2, nchar(historico$Especies)))

# Verificar el resultado
head(historico)

  



# Fusionar los nuevos registros con el listado existente
listado_diatomeas <- left_join(listadi_diatomeas, nuevo_registro, by = c("Species" = "Species"))

# Agregar una columna para indicar si es un nuevo registro
listado_diatomeas <- listado_diatomeas %>%
  mutate(Nuevo_Registro = ifelse(is.na(nuevo_registro$Species), "No", "Sí"))

# Ver los primeros registros del listado con la nueva columna
head(listado_diatomeas)

listado <- listado_diatomeas |> 
  select(Label, Species, Year.x, fuente.x, nuevo_registro)


ruta_archivo <- "data/listado_de_diatomeas_omar_lopez.xlsx"

# Exportar el dataframe a un archivo Excel
write.xlsx(listado, file = ruta_archivo)

# ADN- Adrian -----------

adrian3 <- read_xlsx("data/listados/Anexo3.xlsx")
adrian4 <- read_xlsx("data/listados/Anexo4.xlsx")

str(adrian4)
head(adrian4)

eucariota <- adrian3 %>%
  mutate_all(~ str_replace_all(str_extract(., "\"(.+?)\""), "\"", "")) |>  
  mutate(fuente = "adrian_munguia") |> 
  mutate(Label = "eucariotas_adn") |> 
  mutate(Year = "2023") |> 
  select(Label, Phyla, Clase, Orden, Familia, Genero, Especie, fuente, Year)

peces <- adrian4 %>%
  mutate_all(~ str_replace_all(str_extract(., "\"(.+?)\""), "\"", "")) |>  
  mutate(Label = "peces_adn") |> 
  mutate(fuente = "adrian_munguia") |> 
  mutate(Year = "2023") |> 
  select(Label, Clase, Orden, Familia, Genero, Especie, fuente, Year)

print(peces, n = 67)

ruta_archivo <- "data/listados/listado_peces_adrian_munguia.xlsx"

# Exportar el dataframe a un archivo Excel
write.xlsx(peces, file = ruta_archivo)


# megafauna ---------

carone <- read.xlsx("data/listados/Listado_de_especies-PNR_19-02-2024.xlsx", sheet = "Megafauna") 


unique(carone$Fuente)

# Reemplazar los caracteres no deseados en los nombres
carone$Fuente <- gsub("Carone et al ", "Carone_et_al", carone$Fuente)
carone$Fuente <- gsub("Soldatini et al", "Soldatini_et_al", carone$Fuente)

str(carone)
unique(carone$Fuente)

megafauna <- carone %>%
  mutate(estatus = ifelse(Especie %in% plan$Especie, "confirmado", "no_confirmado"),
         fuente_confirmacion = ifelse(Especie %in% plan$Especie, 
                                      plan$FUENTES[match(Especie, plan$Especie)], 
                                      NA))


# Realizar un merge entre 'megafauna' y 'plan' solo en las filas donde Phylum y Familia estén vacías en 'megafauna'
megafauna <- merge(megafauna, plan[, c("Especie", "Phylum", "Familia")], 
                   by = "Especie", all.x = TRUE)

# Ahora, actualiza 'Phylum' y 'Familia' en 'megafauna' solo donde estén vacías
megafauna$Phylum[is.na(megafauna$Phylum)] <- megafauna$Phylum.y[is.na(megafauna$Phylum)]
megafauna$Familia[is.na(megafauna$Familia)] <- megafauna$Familia.y[is.na(megafauna$Familia)]

# Elimina las columnas temporales 'Phylum.y' y 'Familia.y'
megafauna <- subset(megafauna, select = -c(Phylum.y, Familia.y))

# Definir el nuevo orden de las columnas
new_column_order <- c("Label", "Method", "Phylum", "Taxa1", "Taxa2", "Taxa3", 
                      "Familia.x", "Especie", "Fuente", "año_reportada", "estatus", 
                      "fuente_confirmacion", "nombre_anterior", "plan_de_manejo")

# Reordenar las columnas del dataframe
megafaunafin <- subset(megafauna, select = new_column_order)

str(megafaunafin)


ruta_archivo <- "data/listados/listado_megafauna_confirmado.xlsx"

# Exportar el dataframe a un archivo Excel
write.xlsx(megafaunafin, file = ruta_archivo)

# listado final---------------

listado23 <- readxl::read_excel("data/listados/Listado_de_especies-PNR_19-02-2024.xlsx", sheet = "exp23_confimada") 
plan <- readxl::read_excel("data/listados/Listado_de_especies-PNR.xlsx", sheet = "listado_plan_de_manejo_marino") 


plan <- plan %>%
  filter(!FUENTES == "FUENTE")

unique(plan$FUENTES)

# Convertir FUENTES a cadena de texto
plan$FUENTES <- as.character(plan$FUENTES)

# Verificar el tipo de dato después de la conversión
str(plan$FUENTES)


glimpse(listado23)
glimpse(plan)
str(plan)

unique(listado23$Fuente)

# Reemplazar los nombres uno por uno
listado23$Fuente <- gsub("omar_dominguez", "Dominguez_et_al", listado23$Fuente)
listado23$Fuente <- gsub("adrian_munguia", "Munguia_et_al", listado23$Fuente)
listado23$Fuente <- gsub("carlos_sanchez", "Sanchez_et_al", listado23$Fuente)
listado23$Fuente <- gsub("James_Ketchum", "Ketchum_et_al", listado23$Fuente)
listado23$Fuente <- gsub("omar_lopez", "Lopez_et_al", listado23$Fuente)
listado23$Fuente <- gsub("Elisa_Serviere", "Serviere_et_al", listado23$Fuente)
listado23$Fuente <- gsub("jenny_rodriguez", "Rodriguez_et_al", listado23$Fuente)


unique(listado23$Grupo)


unique(listado23$Metodo)

listado23$Metodo <- gsub("\\b([a-z])", "\\U\\1", listado23$Metodo, perl = TRUE)
unique(listado23$Metodo)


listado23 <- listado23 |> 
  mutate(base = "expedicion2023")

plan <- plan |> 
  mutate(base = "plan_de_manejo")



result2023 <- listado23 %>%
  mutate(status = ifelse(Especie %in% plan$Especie, "confirmado", "no_confirmado"),
         fuente_confirmacion = ifelse(Especie %in% plan$Especie, 
                                      plan$FUENTES[match(Especie, plan$Especie)], 
                                      NA))

result2023 <- merge(result2023, plan[, c("Especie", "Phylum", "Familia")], 
                   by = "Especie", all.x = TRUE)


result2023$Phylum[is.na(result2023$Phylum)] <- result2023$Phylum.y[is.na(result2023$Phylum)]
result2023$Family[is.na(result2023$Family)] <- result2023$Family[is.na(result2023$Family)]

# Elimina las columnas temporales 'Phylum.y' y 'Familia.y'
result2023 <- subset(result2023, select = -c(Phylum.y, Familia.y))

# Visualizar los primeros registros del resultado
head(result2023)

new_column_order <- c("Label", "Metodo", "Grupo","Phylum.x", "Phylum.y", "Taxa1", "Taxa2", "Taxa3", 
                      "Family", "Familia", "Especie", "Fuente", "año_reportada", "estatus", "status",
                      "fuente_confirmacion", "nombre_anterior", "plan_de_manejo")

# Reordenar las columnas del dataframe
result2023fin <- subset(result2023, select = new_column_order)


ruta_archivo <- "data/listados/listado_2023_confirmado.xlsx"

# Exportar el dataframe a un archivo Excel
write.xlsx(result2023fin, file = ruta_archivo)



exp_confirmado <-readxl::read_excel("data/listados/listado_2023_confirmado.xlsx") 
plan_confirmada <-readxl::read_excel("data/listados/Listado_de_especies-PNR_19-02-2024.xlsx", sheet = "plan_de_manejo_confirmada") 

plan_confirmada$FUENTES <- as.character(plan_confirmada$FUENTES)

# Fusionar los dos conjuntos de datos--------
result <- plan_confirmada %>%
  mutate(status = ifelse(Especie %in% exp_confirmado$Especie, "confirmado", "no_confirmado"),
         # mutate(estatus = ifelse(Especie %in% listado23$Especie, 
         #                         paste("confirmado por", listado23$Fuente[match(Especie, listado23$Especie)]), 
         #                         "no_found"),
         fuente_confirmacion = ifelse(Especie %in% exp_confirmado$Especie, 
                                      exp_confirmado$Fuente[match(Especie, exp_confirmado$Especie)], 
                                      NA))


# Agrupar los datos por especie y crear una lista de fuentes de confirmación para cada especie
data_grouped <- exp_confirmado %>%
  group_by(Especie) %>%
  summarise(fuentes_confirmacion = list(unique(Fuente)))  # Utiliza unique para asegurarte de que no haya duplicados

# Unir la lista de fuentes de confirmación a los datos originales como una nueva columna
result <- left_join(plan_confirmada, data_grouped, by = "Especie")


# Visualizar los primeros registros del resultado
head(result)


ruta_archivo <- "data/listados/plan_confirmado.xlsx"

# Exportar el dataframe a un archivo Excel
write.xlsx(result, file = ruta_archivo)




listado23 <- readxl::read_excel("data/listados/Listado_de_especies-PNR_2024-02-20.xlsx", sheet = "exp23_confimada")
head(listado23)
