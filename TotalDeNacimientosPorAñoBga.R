library(readxl)

tab_nacimientos = data.frame(NACIMIENTOS_EN_EL_INSTITUTOS_DE_SALUD_DE_BUCARAMANGA_E_S_E_ISABU_20240627)

dim(tab_nacimientos)

# Filtrar los datos para el año 2023
nacimientos_2023 <- tab_nacimientos[tab_nacimientos$Año.REPORTE == 2023, ]

## se calcula el numero de filas que coinciden con el año 2023
total_casos_2023 <- nrow(nacimientos_2023)

total_casos_2023


# Filtramos los datos para el año 2024
nacimientos_2024 <- tab_nacimientos[tab_nacimientos$Año.REPORTE == 2024, ]

total_casos_2024 <- nrow(nacimientos_2024)

total_casos_2024
