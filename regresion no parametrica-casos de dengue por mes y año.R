## Regresión no paramétrica para Imputación de datos

datos_IRA = data.frame(datosDengue_TABLA)

head(datosDengue_TABLA)


dim(datosDengue_TABLA)


# Filtrar datos del año 2022

datos_2022 = datosDengue_TABLA[which(datosDengue_TABLA$año == 2022), ]


# Generar secuencia de semanas

semanas = seq(as.Date("2015-01-04"), as.Date("2022-06-18"), by = "week")
length(semanas)

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

library(imputeTS)
conteo2 = na_interpolation(conteo1, option = "spline")


casos = c(conteo2)


plot(casos, pch = 20)

semanas


which(semanas == "2014-12-28")

#install.packages("npregfast")

library(npregfast)


#?frfast

tabla = data.frame(semanas,casos[1:389])

tabla$conteo = 1:389  ## añade una nueva columna que cuenta los datos


plot(tabla$semanas, tabla$casos, pch = 20)

## Aqui se hace una estimación no paramétrica 
##de la relación entre casos y conteo


np_casos = frfast(tabla$casos ~ tabla$conteo, 
                  
                  smooth = "kernel", 
                  
                  kernel = "gaussian", 
                  
                  p = 3, kbin = 389)

## se crea un nuevo data frame con la estimación realizada
np_df = data.frame(np_casos$p)

np_df


plot(tabla$semanas, tabla$casos, pch = 20)
lines(tabla$semanas, np_df$X1, col = "red", lwd = 2)

plot(np_df$X2, type = "l", lwd = 2)
abline(h = 0, col = "blue", lwd = 2)
abline(v = which(abs(np_df$X2) < 0.03), col = "red", lwd = 2)
np_df$X2[which(abs(np_df$X2) < 0.03)]

puntos_infle = c(13, 40, 118, 137, 160, 318, 361, 378)

plot(tabla$semanas, tabla$casos, pch = 20)
lines(tabla$semanas, np_df$X1, col = "red", lwd = 2)
abline(v = tabla$semanas[puntos_infle], col = "purple", lwd = 2)

####-------------------------------------

######-------------------------------------------------
library(ggplot2)
x <- tabla$semanas
y <- np_df$X1
# Convertir los datos en un data frame
df <- data.frame(x = x, y = y)

## segundo conjunto de datos a graficar
z <- tabla$semanas
w <- tabla$casos.1.389.
# Convertir los datos en un data frame
df2 <-  data.frame(z=z,w=w)

# Gráfico con ggplot2
ggplot(df, aes(x = x, y = y)) +
  #geom_point(color = "red", size = 1) +  # Puntos personalizados
  geom_line(color = "red", linetype = "solid") +
  geom_point(data = df2,aes(x=z,y=w), color = "blue", size=1) +
  xlab("Mes-Año") +
  ylab("No. de casos")+
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%y")
