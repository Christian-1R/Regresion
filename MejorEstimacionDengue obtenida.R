## Estimaciones para el dengue en Bucaramanga 

##----------------------------------------------
## Regresión no paramétrica para Imputación de datos

library(readxl)
datos_DEN = data.frame(datosDengue_TABLA)

head(datosDengue_TABLA)


dim(datosDengue_TABLA)


# Filtrar datos del año 2022

datos_2022 = datosDengue_TABLA[which(datosDengue_TABLA$año == 2022), ]


# Generar secuencia de semanas, el formato es (Año, Mes, Día)

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

#datosDengue_TABLA$semana[which(datosDengue_TABLA$año == (2015))


conteo[53, c(1:5, 7)] = NA
conteo1 = ts(c(conteo))

library(imputeTS)
conteo2 = na_interpolation(conteo1, option = "spline")
                         
# Se añaden los casos a un vector                         
casos = c(conteo2)

plot(casos,pch=20)

semanas

which(semanas=="2014-12-28")

library(npregfast)


tabla = data.frame(semanas,casos[1:389])

tabla$conteo = 1:389  ##añade una nueva columna que cuenta los datos

plot(tabla$semanas, tabla$casos, pch=20, xlab="Mes-Año", ylab="No. de casos", col="blue")


## Aqui se hace una estimación no paramétrica 
##de la relación entre casos y conteo


np_casos = frfast(tabla$casos ~ tabla$conteo, 
                  
                  smooth = "kernel", 
                  
                  kernel = "gaussian", 
                  
                  p = 3, kbin = 389)

## se crea un nuevo data frame con la estimación realizada
np_df = data.frame(np_casos$p)

np_df  ## la segunda columna corresponde a la derivada (np_df$X2)


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


##-------------------------------------------

## Constantes (parametros iniciales)

#mu_h =2.01 --> tasa de mortalidad para los humanos
#mu_v = 0.6 --> tasa de mortaldiad para los vectores (mosquitos)
# lambda =  0.13 --> tasa de natalidad de los vectores
#beta_h = 0.1  --> tasa de transmisión de vector a humano
#beta_v = 0.5 --> tasa de transmisión de humano a vector
# gam  = 2.14 --> tasa de recuperación de los humanos
# Lambda 


library(deSolve)

sirsi <- function(t,x,parms){
  S_h <- x[1]  ## humanos susceptibles
  I_h <- x[2]  ## humanos infectados
  R <- x[3]  ##humanos recuperados
  S_v <- x[4] ## vectores susceptibles
  I_v <- x[5] ## Vectores infectados
  with(as.list(parms),
       {
         dS_h <- 141.90812/1000000 - parms[1]*S_h*I_v -(642.3/100000)*S_h #mu_h =0.067, beta_h = 0.1
         dI_h <- parms[1]*S_h*I_v - ((642.3/100000)+0.49994+0.5025)*I_h
         dR_h <- (642.3/100000)*I_h-(642.3/100000)*R
         dS_v <- 2800/1000000- 0.0011*S_v - parms[2]*S_v*I_h
         dI_v <- parms[2]*S_v*I_h - 0.0011*I_v
         res <- c(dS_h, dI_h, dR_h, dS_v,dI_v)
         list(res)
       })
}
ode(sirsi, times = 1:13, y = c(552922.50/1000000,77/1000000 ,38.49994/1000000,2384212/1000000, 1/1000000), parms = c(0.1715, 0.02744), method= "rk4")

# Condiciones iniciales y parámetros
initial_conditions <- c(S_h = 552922.50/1000000, I_h = 77/1000000, R = 38.49994/1000000, S_v = 2384212/1000000, I_v = 1/1000000)
params <- c(beta_h = 0.1715, beta_v =  0.012744)
times <- 0:13  # Definir el vector de tiempos

# Ejecutar el modelo
output <- ode(y = initial_conditions, times = times, func = sirsi, parms = params, method = "rk4")


# Visualización de resultados
print(output*1000000)

?ode
library(bbmle) 

verosimilitud <- function(lbetah, lbetav) { 
  x0 = c(552922.50/1000000, 77/1000000, 38.49994/1000000,2384212/1000000, 1/1000000) 
  par <- c(betah = lbetah, betav = lbetav) 
  out <- ode(func = sirsi, y =x0, times = 1:13, 
             parms = par) 
  SD <- sqrt(sum((np_df$X1[1:13]-out[,2]*1000000)^2)/ 
               length(1:13)) # - sum(dnorm(np_df$X1[1:13], mean=out[,2], 
                             #            sd=SD, log=TRUE)) 
} 
betah = seq(0, 0.5, by = 0.01)
betav = seq(0, 0.5, by = 0.01)

matriz_verosimilitud = matrix(0, nrow = length(betah), ncol = length(betav))
for (i in 1:length(betah)) {
  for (j in 1:length(betav)) {
    matriz_verosimilitud[i,j] = verosimilitud(lbetah = betah[i], 
                                              lbetav = betav[j])
  }
}

verosimilitud(lbetah = 0.1, lbetav = 0)

#install.packages("rgl")
library("rgl")
#install.packages("geometry")
library(geometry)

tabla_vero = data.frame(expand.grid(betah,betav), 0)
dim(tabla_vero)
for (i in 1:101) {
  tabla_vero[((i-1)*101):(i*101),3] <- c(matriz_verosimilitud)[((i-1)*101):(i*101)]
}
plot3d(betah, betav, tabla_vero[,3])

datos_de_prueba = seq(from= 0, to = 0.5, length.out =50) 
print(datos_de_prueba)

library(bbmle)
library(deSolve)
#?optim

for (i in 1:50){
ajuste <- mle2(verosimilitud,
               start=list(lbetah= datos_de_prueba[i], lbetav = datos_de_prueba[i]),  
               control=list(maxit=1E5,trace=0),
               trace=FALSE, lower = 0.002, upper = 1, 
               method = "L-BFGS-B")
theta <- as.numeric(c(coef(ajuste)))
print(paste("El valor de prueba es: ", datos_de_prueba[i]))
print(paste("\beta_h, beta_v= ", theta))
modelo = data.frame(ode(sirsi, times = 1:50, y = initial_conditions,parms= c(betah=theta[1],betav=theta[2]), method= "rk4"), 
                        parms = datos_de_prueba)
logLik(ajuste)

casos
plot(semanas[1:50],casos[1:50], pch = 20)
lines(semanas[1:50], np_df$X1[1:50], col = "red", lwd = 2)
lines(semanas[1:50], modelo[, 3], col = "blue2", lwd = 2)

}


 ### pasar a gráfico ggplot

x<- semanas[1:50]
y<- casos[1:50]

# convertir los datos en un dataframe

df <- data.frame(x=x,y=y)


library(ggplot2)
ggplot(df, aes(x = x, y = y)) +
  geom_point(color = "blue", size = 1)+xlab("Mes-Año")+ylab("No. De casos")+
  geom_line(aes(x=x,y=np_df$X1[1:50]), color="red",linetype="solid")+
  geom_line(aes(x=x,y=modelo[,3]), color= "blue2",linetype="solid")
  
###################### ESTIMACIÓN DE UN SOLO PARAMETRO ################
sirsi1 <- function(t,x,parms){
  S_h <- x[1]  ## humanos susceptibles
  I_h <- x[2]  ## humanos infectados
  R <- x[3]  ##humanos recuperados
  S_v <- x[4] ## vectores susceptibles
  I_v <- x[5] ## Vectores infectados
  with(as.list(parms),
       {
         dS_h <- 141.90812 - parms[1]*S_h*I_v - 0.006423*S_h#mu_h =0.067, beta_h = 0.1
         dI_h <- parms[1]*S_h*I_v - (0.006423+0.49994)*I_h 
         dR_h <-0.49994*I_h-0.006423*R
         dS_v <- 2800 - 0.001*S_v -parms[2] *S_v*I_h
         dI_v <- parms[2]*S_v*I_h - 0.001*I_v
         res <- c(dS_h, dI_h, dR_h, dS_v,dI_v)
         list(res)
       })
}
sirsi1(t = 0:10, x = c(10, 0, 0, 0, 0), parms = c(0.1, 0.2))

library(bbmle) 
verosimilitud <- function(lbetah, lbetav) { 
  x0 = c(552922.50, np_df$X1[1], 38.3583, 2384212, 1) 
  par <- c(betah = lbetah, betav = lbetav)
  out <- ode(func = sirsi1, y =x0, times = 1:13, 
             parms = par) 
  SD <- sqrt(sum((np_df$X1[1:13]-out[,2])^2)/ 
               length(1:13)) - 
    sum(dnorm(np_df$X1[1:13], mean=out[,2], 
              sd=sum((np_df$X1[1:13]-out[,2])^2)/ 
                length(1:13), log=TRUE)) 
}

ajuste <- mle2(verosimilitud,
               start=list(lbetah= 0.0001,lbetav=0.01),  
               control=list(maxit=1E5,trace=0),
               trace=FALSE, lower = 0, upper = 1, 
               method = "L-BFGS-B")
theta <- as.numeric(c(coef(ajuste)))
modelo = data.frame(ode(sirsi1, times = 1:13, y =  c(552922.50, np_df$X1[1], 38.3583, 2384212, 1), 
                        parms = theta))


casos
plot(semanas[1:13],casos[1:13], pch = 20)
lines(semanas[1:13], np_df$X1[1:13], col = "red", lwd = 2)
lines(semanas[1:13], modelo[, 3], col = "blue2", lwd = 2)

df <- data.frame(x=semanas[1:13],y=np_df$X1[1:13])
x= semanas[1:13]
y=casos[1:13]
v= modelo[,3]
library(ggplot2)
ggplot(df,aes(x=semanas[1:13],y=np_df$X1[1:13]))+
  geom_point(color = "blue") +
  geom_line(color="red",linetype="solid")+
  geom_line(aes(x=x,y=v), color="blue",linetype="solid")+
  xlab("Mes") +
  ylab("No. De casos") +
  ylim(30,10000)
