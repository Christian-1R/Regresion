## Regresión no paramétrica para Imputación de datos

library(readxl)
datos_DEN = data.frame(datosDengue_TABLA)

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
datosDengue_TABLA$semana[which(datosDengue_TABLA$año == (2015))


conteo[53, c(1:5, 7)] = NA
conteo1 = ts(c(conteo))


library(imputeTS)
conteo2 = na_interpolation(conteo1, option = "spline")


casos = c(conteo2)


plot(casos, pch = 20)

semanas


which(semanas == "2014-12-28")


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



plot(tabla$semanas[1:13], tabla$casos[1:13], pch = 20)
lines(tabla$semanas, np_df$X1, col = "red", lwd = 2)
tabla$casos[1]
plot(np_df$X2, type = "l", lwd = 2)
abline(h = 0, col = "blue", lwd = 2)
abline(v = which(abs(np_df$X2) < 0.03), col = "red", lwd = 2)
np_df$X2[which(abs(np_df$X2) < 0.095)]  #0.03, 0.025, 0.04, 0.035, 0.05

#puntos_infle = c(13, 40, 118, 137, 160, 318, 361, 378)
#puntos_infle = c(13,40, 65, 118,137,160,318,361,378)
puntos_infle = c(13,40, 65, 118,137,160,242,318,361,378)
plot(tabla$semanas, tabla$casos, pch = 20)
lines(tabla$semanas, np_df$X1, col = "red", lwd = 2)
abline(v = tabla$semanas[puntos_infle], col = "purple", lwd = 2)
+
scale_x_date(date_breaks = "6 months", date_labels = "%m-%y")


### pasar a gráfico ggplot

x <- tabla$semanas
y <- tabla$casos.1.389.

# Convertir los datos en un data frame
df <- data.frame(x=x,y=y)

##agrego la información para gráficar la estimación de I_h(t).

library(ggplot2)
# Gráfico con ggplot2
ggplot(df, aes(x = x, y = y)) +
  geom_point(color = "blue", size = 1) +  
  geom_vline(xintercept = tabla$semanas[puntos_infle], color = "purple", linetype = "solid", size = 1.2)+
  geom_line(aes(x=x,y = np_df$X1), color= "red", linetype="solid",size=1.5)+
  xlab("Mes-Año") +
  ylab("No. de casos")+
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%y") 

plot(x,np_df$X2, col="blue", pch=20)
abline(h = 0, col = "red", lwd = 2)
abline(v= x[c(13,40,65,118,137,160,242,318,361,378)], col="purple",lwd=2 )


###Gráfica solo de la función y los puntos #####

library(ggplot2)
# Gráfico con ggplot2
ggplot(df, aes(x = x, y = y)) +
  geom_point(color = "blue", size = 1) +  
  geom_line(aes(x=x,y = np_df$X1), color= "red", linetype="solid",size=1.5)+
  xlab("Mes-Año") +
  ylab("No. de personas infectadas")+
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%y") 

## convertir gráfica de la derivada a ggplot

install.packages("ggplot2")
library(ggplot2)

x <- tabla$semanas
y <- np_df$X2

df2 <- data.frame(x = x, y = y)

ggplot(df2, aes(x = x, y = y)) +  # Asegúrate de usar ggplot, no ggpot
  geom_line(color = "blue", size = 1.5) +
  geom_vline(xintercept = x[c(13, 40, 65, 118, 137, 160, 242, 318, 361, 378)], 
             color = "purple", linetype = "solid", size = 1.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "solid", size = 1)+
  xlab("Año") +
  ylab(expression(phi * "'(x)"))
