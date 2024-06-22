## casos de dengue respecto a semanas y año 

datosDengue_TABLA = data.frame(datosDengue_TABLA)

# Filtrar datos del año 2022

datos_2022 = datosDengue_TABLA[which(datosDengue_TABLA$año == 2022), ]
#datos_2022 = datosDengue_TABLA[datosDengue_TABLA$año == 2022, ]
# Generar secuencia de semanas

semanas = seq(as.Date("2015-01-04"), as.Date("2022-06-18"), by = "week")


# Crear una matriz para conteo de casos por semana y año
conteo = matrix(0, nrow = 53, ncol = length(2015:2022))
# Rellenar la matriz con los conteos semanales

for (i in 1:8) {
  #year= 2014+i
  conteo[1:length(table(datosDengue_TABLA$semana[which(datosDengue_TABLA$año == (2014+i))])), i] = 
    table(datosDengue_TABLA$semana[which(datosDengue_TABLA$año == (2014+i))])
}

# Asignar NA (los datos que no se conocen)

conteo[53, c(1:5, 7)] = NA
conteo1 = ts(c(conteo))


#install.packages("imputeTS")
library(imputeTS)
conteo2 = na_interpolation(conteo1, option = "spline")

plot(semanas, conteo2[1:389], lwd = 2, type = "l",xlab = "Semana-Año",
     ylab= "N° de casos",
     main= "Número de casos de DENGUE, Bucaramanga 2015-2022")

### pasar a gráfico ggplot

x <- semanas
y <- conteo2[1:389]

# Convertir los datos en un data frame
df <- data.frame(x = x, y = y)
library(ggplot2)
# Gráfico con ggplot2
ggplot(df, aes(x = x, y = y)) +
  geom_point(color = "blue", size = 1) +  # Puntos personalizados
  #geom_line(color = "blue") +
  xlab("Mes-Año") +
  ylab("No. de casos")  + 
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%y")

