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
         dS_h <- 6428.833/365 - parms[1]*S_h*I_v - 0.067*S_h #mu_h =0.067, beta_h = 0.0245
         dI_h <- parms[1]*S_h*I_v - (0.067+0.07142)*I_h 
         dR_h <-0.07142*I_h-0.067*R
         dS_v <- 400 - 0.6*S_v - parms[2]*S_v*I_h
         dI_v <- parms[2]*S_v*I_h - 1/14*I_v
         res <- c(dS_h, dI_h, dR_h, dS_v,dI_v)
         list(res)
       })
}
ode(sirsi, times = 1:13, y = c(1000, 1,1,0,0), parms = c(0.1, 0.1), method= "rk4")

?ode
library(bbmle) 
verosimilitud <- function(lbetah, lbetav) { 
  x0 = c(581130, np_df$X1[1], 0.978*23, 9293/36000000, 65051/8) 
  par <- c(betah = lbetah, betav = lbetav) 
  out <- ode(func = sirsi, y =x0, times = 1:13, 
             parms = par) 
  SD <- sqrt(sum((np_df$X1[1:13]-out[,2])^2)/ 
               length(1:13)) 
  - sum(dnorm(np_df$X1[1:13], mean=out[,2], 
              sd=SD, log=TRUE)) 
} 
verosimilitud(lbetah = 0.2, lbetav = 0.2)

library(bbmle)
library(deSolve)
?optim
ajuste <- mle2(verosimilitud,
               start=list(lbetah= 0.030195, lbetav = 0.030065e-2),  
               control=list(maxit=1E5,trace=0),
               trace=FALSE, lower = 0.002, upper = 1, 
               method = "L-BFGS-B")
theta <- as.numeric(c(coef(ajuste)))
modelo = data.frame(ode(sirsi, times = 1:13, y =  c(581130, np_df$X1[1], 0.978*23, 9293/36000000, 65051/8), 
                        parms = theta))

casos
plot(semanas[1:13],casos[1:13], pch = 20)
lines(semanas[1:13], np_df$X1[1:13], col = "red", lwd = 2)
lines(semanas[1:13], modelo[, 3], col = "blue2", lwd = 2)

###################### ESTIMACIÓN DE UN SOLO PARAMETRO ################
sirsi1 <- function(t,x,parms){
  S_h <- x[1]  ## humanos susceptibles
  I_h <- x[2]  ## humanos infectados
  R <- x[3]  ##humanos recuperados
  S_v <- x[4] ## vectores susceptibles
  I_v <- x[5] ## Vectores infectados
  with(as.list(parms),
       {
         dS_h <- 6428.833/365 - parms[1]*S_h*I_v - 0.067*S_h#mu_h =2.01, beta_h = 0.1
         dI_h <- parms[1]*S_h*I_v - (0.067+0.07142)*I_h 
         dR_h <- 0.07142*I_h-0.067*R
         dS_v <- 400 - 1/14*S_v - 0.00392*S_v*I_h
         dI_v <- 0.00392*S_v*I_h - 0.6*I_v
         res <- c(dS_h, dI_h, dR_h, dS_v,dI_v)
         list(res)
       })
}

library(bbmle) 
verosimilitud <- function(lbetah) { 
  x0 = c(581130, np_df$X1[1], 0.978*23, 9293/36000000, 65051/8) 
  par <- c(betah = lbetah, betav = 0.0005)
  out <- ode(func = sirsi1, y =x0, times = 1:13, 
             parms = par) 
  SD <- sqrt(sum((np_df$X1[1:13]-out[,2])^2)/ 
               length(1:13)) 
  - sum(dnorm(np_df$X1[1:13], mean=out[,2], 
              sd=SD, log=TRUE)) 
}

ajuste <- mle2(verosimilitud,
               start=list(lbetah= 0.030188),  
               control=list(maxit=1E5,trace=0),
               trace=FALSE, lower = 0.3, upper = 1, 
               method = "L-BFGS-B")
theta <- as.numeric(c(coef(ajuste)))
modelo = data.frame(ode(sirsi1, times = 1:13, y =  c(581130, np_df$X1[1], 0.978*23, 9293/36000000, 65051/8), 
                        parms = theta))


casos
plot(semanas[1:13],casos[1:13], pch = 20)
lines(semanas[1:13], np_df$X1[1:13], col = "red", lwd = 2)
lines(semanas[1:13], modelo[, 3], col = "blue2", lwd = 2)



############## ESTIMACIÓN DE BETAV ##################
sirsi1 <- function(t,x,parms){
  S_h <- x[1]  ## humanos susceptibles
  I_h <- x[2]  ## humanos infectados
  R <- x[3]  ##humanos recuperados
  S_v <- x[4] ## vectores susceptibles
  I_v <- x[5] ## Vectores infectados
  with(as.list(parms),
       {
         dS_h <- 6428.833 - parms[1]*S_h*I_v - 2.01*S_h#mu_h =2.01, beta_h = 0.1
         dI_h <- parms[1]*S_h*I_v - (2.01+2.14)*I_h 
         dR_h <-2.14*I_h-2.01*R
         dS_v <- 0.13 - 0.6*S_v - 0.00392*S_v*I_h
         dI_v <- 0.00392*S_v*I_h - 0.6*I_v
         res <- c(dS_h, dI_h, dR_h, dS_v,dI_v)
         list(res)
       })
}

library(bbmle) 
verosimilitud <- function(lbetah) { 
  x0 = c(581130, np_df$X1[1], 0.978*23, 9293/36000000, 65051/8) 
  par <- c(betah = lbetah, betav = 0.0005)
  out <- ode(func = sirsi1, y =x0, times = 1:13, 
             parms = par) 
  SD <- sqrt(sum((np_df$X1[1:13]-out[,2])^2)/ 
               length(1:13)) 
  - sum(dnorm(np_df$X1[1:13], mean=out[,2], 
              sd=SD, log=TRUE)) 
}

ajuste <- mle2(verosimilitud,
               start=list(lbetah= 0.030188),  
               control=list(maxit=1E5,trace=0),
               trace=FALSE, lower = 0.3, upper = 1, 
               method = "L-BFGS-B")
theta <- as.numeric(c(coef(ajuste)))
modelo = data.frame(ode(sirsi1, times = 1:13, y =  c(581130, np_df$X1[1], 0.978*23, 9293/36000000, 65051/8), 
                        parms = theta))


casos
plot(semanas[1:13],casos[1:13], pch = 20)
lines(semanas[1:13], np_df$X1[1:13], col = "red", lwd = 2)
lines(semanas[1:13], modelo[, 3], col = "blue2", lwd = 2)
