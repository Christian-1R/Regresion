library(readxl)

tab_pob = data.frame(anex_DCD_Proypoblacion_PerteneniaEtnicoRacialmun)
head(tab_pob)

dim(tab_pob)  ## 60594 filas y 13 columnas 

tab_pob[7:60594,5:13]

base_bga = data.frame(tab_pob[7:60594,])

proyeccion_bga = data.frame(base_bga$...4 ,
                            base_bga$...5,
                            base_bga$...6,
                            base_bga$...7)

head(proyeccion_bga)
proyeccion_bga[which(proyeccion_bga[,1] =="Bucaramanga"), ]

proyeccion_bga2 = NULL

for (i in 1:18){
  proyeccion_bga2[i]= as.numeric(proyeccion_bga[which(
    (proyeccion_bga[,1]=="Bucaramanga") & (proyeccion_bga[,2]==2017+i)
                                                          & proyeccion_bga[,3]==  "Total"),4])
}
plot(2018:2035,proyeccion_bga2,type="p",col="blue",pch=20, xlab="Año", ylab="Total de habitantes")

### pasar a gráfico ggplot
library(ggplot2)  
x <- 2018:2035
y <- proyeccion_bga2


# Convertir los datos en un data frame
df <- data.frame(x = x, y = y)
ggplot(df, aes(x = x, y = y)) +
  geom_point(color = "blue", size = 1)+
  xlab("Año") +
  ylab("Total de habitantes")+
  scale_x_continuous(breaks = seq(2018, 2035, 1))

## Regresión no paramétrica 
#install.packages("npregfast")
library(npregfast)

poblacion_NA = c(rep(NA, 51), proyeccion_bga2[1], # 2018 
                 
                 rep(NA, 51), proyeccion_bga2[2], # 2019 
                 
                 rep(NA, 52), proyeccion_bga2[3], # 2020
                 
                 rep(NA, 51), proyeccion_bga2[4], # 2021
                 
                 rep(NA, 51), proyeccion_bga2[5], # 2022
                 
                 rep(NA, 51), proyeccion_bga2[6], # 2023
                 
                 rep(NA, 51), proyeccion_bga2[7]) # , # 2024
                 
length(poblacion_NA)
plot(poblacion_NA, pch = 20, ylim = c(0, max(na.omit(poblacion_NA))))


datos_finales = data.frame(año = (2018:2024), poblacion = proyeccion_bga2[1:7])

datos_finales

regresion_np = frfast(datos_finales[, 2] ~ datos_finales[, 1], model = "np", 
                      
                      p = 3, smooth = "kernel", kbin = (365-51))

imputacion_pob = data.frame(regresion_np$p)

plot(regresion_np$x, poblacion_NA[52:365], pch = 20, 
     
     col = "blue", xlab= "Año", ylab= "Total de habitantes")

lines(regresion_np$x, imputacion_pob$X1, lwd = 2, col = "orchid")

### pasar a gráfico ggplot

z <- regresion_np$x
w <- poblacion_NA[52:365]

# Convertir los datos en un data frame
df <- data.frame(z = z, w = w)

# Data frame para hacer la linea 
g <- regresion_np$x
h <- imputacion_pob$X1
df1 <- data.frame(g=g,h=h)



library(ggplot2)
# Gráfico con ggplot2
ggplot(df, aes(x = z, y = w)) +
  geom_point(color = "blue", size = 1) +  # Puntos personalizados
  geom_line(aes(x=g, y= h), color = "orchid") +
  xlab("Año") +
  ylab("Total de habitantes")+
  scale_x_continuous(breaks = seq(2018, 2035, 1)) 

## ESTIMACIÓN DEL PROMEDIO DE NUEVOS INDIVIDUOS
datos_finales$año
eta = numeric(6) ## el vector eta se crea del tamaño de la cantidad de años, en este caso, 7
#eta[1]= datos_finales$poblacion[2]- datos_finales$poblacion[1]
## calculamos las diferencias de población
## las incluimos en el vector eta
for (i in 1: 6){
  eta[i] = datos_finales$poblacion[i+1]- datos_finales$poblacion[i]
  }
eta
promedio = mean(eta)
promedio
