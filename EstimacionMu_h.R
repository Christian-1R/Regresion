## Estimación de mu_h (TASA DE MORTALIDAD DE HUMANOS)


library(readxl)


## SE CARGAN LOS DATOS DE POBLACIÓN (SUSCEPTIBLES)

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

poblacion_NA = c(
                rep(NA,52), 548447, #2014 (484962)
                rep(NA,52), 554885, #2015 (554885)
                 rep(NA,52), 562123, #2016 (562123)
                 rep(NA,52), 570253, # 2017 (570253)
                rep(NA, 51), proyeccion_bga2[1], # 2018 
                 
                 rep(NA, 51), proyeccion_bga2[2], # 2019 
                 
                 rep(NA, 52), proyeccion_bga2[3], # 2020
                 
                 rep(NA, 51), proyeccion_bga2[4], # 2021
                 
                 rep(NA, 51), proyeccion_bga2[5], # 2022
                 
                 rep(NA, 51), proyeccion_bga2[6], # 2023
                 
                 rep(NA, 51), proyeccion_bga2[7]) # , # 2024

length(poblacion_NA)
plot(poblacion_NA, pch = 20, ylim = c(0, max(na.omit(poblacion_NA))))


datos_finales = data.frame(año = (2014:2023), poblacion = c(548447, 554885,562123,570253, proyeccion_bga2[1:6]))

datos_finales

library(npregfast)
regresion_np = frfast(datos_finales[, 2] ~ datos_finales[, 1], model = "np", 
                      
                      p = 3, smooth = "kernel", kbin = (577))

imputacion_pob = data.frame(regresion_np$p)

plot(regresion_np$x[53:441], poblacion_NA[53:441], pch = 20, 
     
     col = "blue", xlab= "Año", ylab= "Total de habitantes")

lines(regresion_np$x[53:441], imputacion_pob$X1[53:441], lwd = 2, col = "orchid")

### pasar a gráfico ggplot

z <- regresion_np$x[53:441]
w <- poblacion_NA[53:441]

# Convertir los datos en un data frame
df <- data.frame(x = z, y = w)

# Data frame para hacer la linea 
g <- regresion_np$x[53:441]
h <- imputacion_pob$X1[53:441]
df1 <- data.frame(x=g,y=h)

library(ggplot2)
# Gráfico con ggplot2
ggplot(df, aes(x = z, y = w)) +
  geom_point(color = "blue", size = 1) +  # Puntos personalizados
  #geom_line(aes(x=g, y= h), color = "orchid") +
  xlab("Año") +
  ylab("Total de habitantes")+
  scale_x_continuous(breaks = seq(2014, 2024, 1)) 
  

length(datos_finales$poblacion)
eta = c()
for (i in 1: 9){
  eta[i] = datos_finales$poblacion[i+1]- datos_finales$poblacion[i]
}
eta
length(eta)
promedio = mean(eta)
promedio
####################################

# SE CARGAN LOS DATOS DE PERSONAS INFECTADAS

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

################# ESTIMACIÓN mu_h ###################

#Lambda = mean(7379.222/365)*7
#t= regresion_np$x
t= tabla$semanas
length(t)

as.numeric(t[2]-t[1])
Lambda = (imputacion_pob$X1[53:442] - imputacion_pob$X1[52:441])
mu_j = (Lambda[2:390]-Lambda[1:389]- imputacion_pob$X1[53:442] + imputacion_pob$X1[52:441] + 0.5025*tabla$casos[52:441])/
  (imputacion_pob$X1[53:442])
plot(mu_j, pch = 20)


dataMu = data.frame(x=t, y=mu_j[1:389])

library(ggplot2)
ggplot(dataMu, aes(x=t,y=mu_j[1:389])) +
geom_line(color="red") +
xlab("Año") +
ylab(expression(hat(mu)[h])) 

#Delta = c() ##declaramos el vector Delta

#for (i in 1:314){
 # Delta = c(Delta,(as.numeric(t[i+1]-t[i])))
#}

#length(Delta)
#N_h= imputacion_pob$X1[53:442]


#N_h
#resultados = c()

#Se gráfica por intervalo, en este caso, hasta el primer intervalo:
  
#for (i in 1:400){
 # resultados = c(resultados,(Lambda -N_h[i+1]+N_h[i])/(N_h[i]*(Delta[i])))
#}
#print(resultados)

#length(resultados)


## pasamos a meses la estimación (PREGUNTAR??)

#Estimacion_final= resultados*30

#plot(t[1:314],Estimacion_final[1:314],col="blue",pch=20,ylab = "mu_h", xlab="Mes-Año")

## Pasamos la gráfica a GGplot

#n <- t[1:313]
#m <- Estimacion_final[1:313]

#DataEstimacion = data.frame(x=n, y=m)
# PAQUETE PARA ESCRIBIR EN LATEX
#install.packages("ggtext")
#library(ggtext)
#library(ggplot2)
#ggplot(DataEstimacion, aes(x=n, y=m))+geom_point(color = "blue2", size = 1)+
 # xlab("Año")+ylab(expression(mu[h]))


#####################Estimación Beta_h ##################

#gamma= 2.1426 meses^-1 = 0.49994

#Beta_h = 


Bh= c()
for (i in 1:389){
  Bh[i]=(np_df$X1[i+1]-np_df$X1[i]+((642.3/100000)+0.49994+0.5025)*np_df$X1[i])
}

plot(t[1:389], Bh[1:389],pch=20,xlab = "Mes-Año", ylab=
      "Beta_h", col="blue")

## Pasamos el gráfico a ggplot2
x <- t[1:389]
v <- Bh[1:389]

DataBeta = data.frame(x=t[1:389], y=Bh[1:389])

ggplot(DataBeta,aes(x=t,y=Bh[1:389]))+
  geom_line(color="blue", linewidth=1.4)+xlab("Año") +
  ylab(expression(hat(beta)[h]*S[h]*I[v])) 



############ Estimación R_h ######################

Rh = c()

t[1]
casos[1]
Rh[1]= casos[1]*0.99631951
## voy a asumir Rh(0)=5

for (i in 1:389){
  Rh = c(Rh, (0.49994*np_df$X1[i]-(642.3/100000)*Rh[i])+Rh[i])
}

plot(t[1:389], Rh[1:389], pch=20, xlab="Mes-Año",ylab="R_h", col="blue")


ggplot(DataBeta, aes(x=t[1:389],y=Rh[1:389])) +
  geom_line(color= "green", linewidth=1.4) + 
  xlab("Año") +
  ylab(expression(hat(R)[h]))

################ Estimación S_h ##############################

Sh = c()

casos[1]
Sh[1] =  imputacion_pob$X1[53]-Rh[1]-77
for (i in (2:388)){
  Sh[i] = Sh[i-1]+(Lambda[i-1]- (642.3/100000)*Sh[i-1]- Bh[i-1])
}
plot(Sh,pch=20)

DataSh = data.frame(x=t[1:388],y= Sh[1:388])

ggplot(DataSh, aes(x=t[1:388],y=Sh[1:388])) +
geom_line(color="blue", linewidth=1.4) +
xlab("Año") +
ylab(expression(hat(S)[h])) 

###################### Estimación para I_h y gráficas real vs estimación #######################
Ih = c()

Ih[1]=77

for (i in (2:388)){
  Ih[i] = Ih[i-1] -(642.3/100000+0.49994+0.5025)*Ih[i-1] + Bh[i-1]
}
plot(Ih,pch=20)

DataSh = data.frame(x=t[1:388],y= Ih[1:388])

ggplot(DataSh, aes(x=t[1:388],y=Ih[1:388])) +
  geom_line(color="turquoise3", linewidth=1.4) +
  xlab("Año") +
  ylab(expression(hat(I)[h])) 

 
colors()
## a continuación agregué el código con los datos del dengue 

library(readxl)
datos_DEN = data.frame(datosDengue_TABLA)


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
                         
                         
                         
                         ### pasar a gráfico ggplot
                         
                         x <- tabla$semanas
                         y <- tabla$casos.1.389.
                         
                         length(Ih)
                         # Convertir los datos en un data frame
                         df <- data.frame(x=x,y=y)
                         
                         
                         library(ggplot2)
                         # Gráfico con ggplot2
                         ggplot(df, aes(x = x, y = y)) +
                           geom_point(color = "blue", size = 1) +  
                           geom_line(aes(x=x,y = np_df$X1[1:nrow(df)]), color= "red", linetype="solid",size=1.5)+
                           geom_line(aes(x=x,y=Ih[1:nrow(df)]), color="turquoise", linetype="dashed",size=1.5)+
                           xlab("Mes-Año") +
                           ylab("No. de personas infectadas")+
                           scale_x_date(date_breaks = "3 months", date_labels = "%m-%y") 
                         





### Estimaciones para vectores ####

###########Estimación S_v(t_{j+1}) ###########3

Sv= c()
Sv[1]= 2384212
Iv = c()
Iv[1]=1
#mu_v= 0.5 semana^{-1} ---> tasa de mortalidad para vectores
Bv = 0.0012  #tasa de transmisión de humano a vector
lam = 2800 # tasa de natalidad de los vectores

for (i in 2:388){
  Iv[i] = (Bv*Sv[i-1]*casos[i-1]-0.001*Iv[i-1])+Iv[i-1]
  Sv[i]=  lam + (0.001*Sv[i-1]-Bv*Sv[i-1]*casos[i-1])+Sv[i-1]
}
plot(Sv,pch=20)
plot(Iv,pch=20)

dataIv = data.frame(x=t[1:388],y=Iv[1:388])


ggplot(dataIv, aes(x=t[1:388],y=Iv[1:388])) +
  geom_line(color="purple", size=1.4) +
  xlab("Año") +
  ylab(expression(hat(I)[v]))

betas2= c( 0.0078,0.0098)
vectores_infectados = c()


for(beta in betas2) {
  x = t[1:388]
  
  for(i in 2:388) {
    vectores_infectados[i] = (Bv*Sv[i-1]*casos[i-1]-0.001*Iv[i-1])+Iv[i-1]
  }
  
  y = vectores_infectados
  p = ggplot(data.frame(x, y), aes(x, y)) +
    geom_line(color="purple", size=1.4) + ggtitle(paste("βv  =", beta))+ylab(expression(hat(I)[v](t)))+
    #ggtitle(substitute(expression(beta[v] == val), list(val = beta))) +
    theme_minimal() + xlab("tiempo")
  
  print(p)  # Imprimir cada gráfico por separado
}



dataSv = data.frame(x=t[1:388],y=Sv[1:388])


ggplot(dataSv, aes(x=t[1:388],y=Sv[1:388])) +
  geom_line(color="brown", size=1.4) +
  xlab("Año") +
  ylab(expression(hat(S)[v]))+geom_hline(yintercept=250000,color="purple",size=1)


####### TABLA DE VALORES PARA BETA ####
betas = c(0.013, 0.014, 0.018, 0.021, 0.023, 0.0078, 0.0098)
vectores_susceptibles = c()

library(ggplot2)

for(beta in betas) {
  x = t[1:388]
  
  for(i in 2:388) {
    vectores_susceptibles[i] = lam + (0.001 * Sv[i-1] - beta * Sv[i-1] * casos[i-1]) + Sv[i-1]
  }
  
  y = vectores_susceptibles
  p = ggplot(data.frame(x, y), aes(x, y)) +
    geom_line(color="brown") + ggtitle(paste("βv  =", beta))+ylab(expression(hat(S)[v](t)))+
    #ggtitle(substitute(expression(beta[v] == val), list(val = beta))) +
    theme_minimal() + xlab("tiempo")
  
  print(p)  # Imprimir cada gráfico por separado
}


############# forma vieja de hacer la cuenta: ###############
for (j in seq(0,1000, by = 100)) {
  for (i in 1:100){
    Sv= c(Sv, (0.001*Sv[i]*casos[i]-0.001*Sv[i]-Bv*Sv[i]*casos[i])+Sv[i])
    # Graficar para cada valor de i
    #plot(Sv, type = "l", main = paste("Iteración", i), ylab = "Sv", xlab = "i")
    Sys.sleep(0.1)  # Pausa para ver la gráfica antes de pasar a la siguiente iteración (opcional)
  }
}

plot(Sv, type = "l", ylab = "Sv")
length(Sv)
plot(t[158:387], Sv[158:387], pch=20)

DataSv = data.frame(x=t[158:387],y=Sv[158:387])
ggplot(data=DataSv, aes(x=t[158:387],y=Sv[158:387])) +
geom_point(color="blue", size=1) +
xlab("tiempo") +
ylab(expression(hat(S)[v]))

which(t=="2018-01-07")



################ Minimos cuadrados ordinarios ###########################


#define function to minimize residual sum of squares
library(deSolve)


min_residuals <- function(data, par, state, t) {
  # Definir el sistema de ecuaciones diferenciales
  my_ode1 = function(t, state, par) {
    with(as.list(c(state, par)), {
      ##----------ecuaciones------
      mu_h = 642.3 / 100000 # Tasa de mortalidad para los humanos (días)
      Lambda = 141.90812 / 1000000
      lambda = 2800 / 1000000 # Tasa de natalidad de los vectores
      #gam = 0.49994 # Tasa de recuperación de los humanos
      betav =  0.012744
      mu_v = 0.0011
      dSh = Lambda - mu_h * Sh - par[1] * Sh * Iv
      dIh = par[1] * Sh * Iv - (mu_h + par[2]) * Ih - 0.5025*Ih
      dR = par[2] * Ih - mu_h * R
      dSv = lambda - mu_v * Sv -  betav* Sv * Ih
      dIv = 0.012744  * Sv * Ih - mu_v * Iv
      
      return(list(c(dSh, dIh, dR, dSv, dIv)))
    })
  }
  # Resolver el sistema de ODEs
  names(state) = c("Sh", "Ih", "R", "Sv", "Iv")
  # Ejecutar la simulación
  out <- ode(y = state, times = t, func = my_ode1, parms = par)
  
  # Verificar que el número de datos y resultados coincidan
  if (nrow(out) != length(data)) {
    stop("La longitud de 'data' no coincide con el número de filas de 'out'.")
  }
  
  # Asignar nombres a las columnas
  colnames(out) <- c("time", "Sh", "Ih", "R", "Sv", "Iv")
  
  # Calcular los residuos (comparar la columna 'Ih' con 'data')
  residuals <- sum((out[, "Ih"] - data)^2)
  
  return(residuals)
}



min_residuals(data = conteo2[1:13]/1000000, par = c(0.1715, 0.49994), 
              t = 1:13, state = c(552922.50/1000000, 77/1000000, 38.49994/1000000, 
                                  2384212/1000000, 1/1000000))

# Llama a la función
min_residuals(data = 1:10, par = c(0.1, 0.1), 
              state = c(1, 0, 0, 0, 0), t[1:13])

library(rgl)
library(geometry)

# Define x y y
x = seq(-1, 1, by = 0.01)
y = seq(-1, 1, by = 0.01)

# Inicializa la matriz z
z = matrix(0, nrow = length(x), ncol = length(y))

# Llenar la matriz z
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    z[i, j] = min_residuals(data = conteo2[1:13]/1000000, par = c(x[i], y[j]), 
                            t = 1:13, state = c(552922.50/1000000, 77/1000000, 38.49994/1000000, 
                                                2384212/1000000, 1/1000000))
  }
}

# Graficar
library(rgl)  # Asegúrate de tener la librería rgl cargada
plot3d(x, y, z, col = "lightblue", alpha = 0.5, axes = TRUE, box = TRUE)

# Encuentra el valor mínimo de z
min_value = min(z)


# Convierte la matriz z en un data frame
z_table <- as.data.frame(z)

# Añadir las columnas x y y a la tabla
z_table <- cbind(expand.grid(x = x, y = y), head(z_table[, 3], length(x)))

# Verifica el resultado
print(z_table)
plot3d(z_table[,1], z_table[,2], z_table[,3], col = "lightblue", alpha = 0.5, axes = TRUE, box = TRUE,
       xlab= expression(beta[h]), ylab=expression(gamma), zlab="SCE")
# Añadir nombres a los ejes
axes3d(edges = 'bbox', labels = TRUE)




#title3d(xlab = expression(beta[v]), ylab = expression(gamma), zlab = "Eje Z")
which(z_table[, 3] == min(z_table[, 3]))
z_table[201,]


min_index <- which(z == min_value, arr.ind = TRUE)
min_x <- x[min_index[1]]
min_y <- y[min_index[2]]

# Imprime el punto donde ocurre el mínimo
print(paste("El valor mínimo de los residuos es", min_value, 
            "y ocurre en x =", min_x, "y =", min_y))





################## variando solo beta_h y gammma #######

library(bbmle)
library(deSolve)
library(bbmle)
library(deSolve)

# Definir la función del ODE
my_ode1 = function(t, state, par) {
  with(as.list(c(state, par)), {
    ##----------ecuaciones------
    mu_h = 642.3 / 100000 # Tasa de mortalidad para los humanos (días)
    Lambda = 141.90812 / 1000000
    lambda = 2800 / 1000000 # Tasa de natalidad de los vectores
    betav =  0.012744
    mu_v = 0.0011
    
    # Definir ecuaciones diferenciales
    dSh = Lambda - mu_h * Sh - par[1] * Sh * Iv
    dIh = par[1] * Sh * Iv - (mu_h + par[2]) * Ih - 0.5025 * Ih
    dR = par[2] * Ih - mu_h * R
    dSv = lambda - mu_v * Sv - betav * Sv * Ih
    dIv = 0.012744 * Sv * Ih - mu_v * Iv
    
    # Devolver los cambios en el estado
    return(list(c(dSh, dIh, dR, dSv, dIv)))
  })
}

# Estado inicial
state = c(Sh = 1500, Ih = 0.2, R = 1, Sv = 1000, Iv = 10)

# Ejecutar la simulación
out2 <- ode(y = state, times = 1:20, func = my_ode1, parms = c(0.0019, 0.00197))


nuevos_datos_infectados = out2[,"Ih"]

for(i in 1:20){
  if(i%% 2==0){
    nuevos_datos_infectados[i]= out2[i,"Ih"]+10
  }
  else{
    nuevos_datos_infectados[i]= out2[i,"Ih"]-10
  }
}

plot(nuevos_datos_infectados, pch=20, main="ajuste de datos", xlab="Tiempo", ylab="Infectados")


x = t[1:20]
y = out2[,3]
z =  nuevos_datos_infectados

Nuevadata = data.frame(x=x,y=y)


library(ggplot2)

ggplot(Nuevadata,aes(x=x,y=y))+
  geom_point(aes(x=x,y=nuevos_datos_infectados[1:20]), color="red",linetype="solid")+
  geom_line(aes(x=x,y=out2[,3]), color= "blue2",linetype="solid")+
  xlab("Tiempo")+
  ylab(expression(I[h](t)))



##### MINIMOS CUADRADOS ORDINARIOS- SIMULACION #############


min_residuals <- function(data, par, state, t) {
  # Definir el sistema de ecuaciones diferenciales
  my_ode1 = function(t, state, par) {
    with(as.list(c(state, par)), {
      ##----------ecuaciones------
      mu_h = 642.3 / 100000 # Tasa de mortalidad para los humanos (días)
      Lambda = 141.90812 / 1000000
      lambda = 2800 / 1000000 # Tasa de natalidad de los vectore
      betav =  0.012744
      mu_v = 0.0011
      dSh = Lambda - mu_h * Sh - par[1] * Sh * Iv
      dIh = par[1] * Sh * Iv - (mu_h + par[2]) * Ih - 0.5025 * Ih
      dR = par[2] * Ih - mu_h * R
      dSv = lambda - mu_v * Sv - betav * Sv * Ih
      dIv = 0.012744 * Sv * Ih - mu_v * Iv
      return(list(c(dSh, dIh, dR, dSv, dIv)))
    })
    }
  names(state) = c("Sh", "Ih", "R", "Sv", "Iv")
  out <- ode(y = state, times = t, func = my_ode1, parms = par)
  if (nrow(out) != length(data)) {
    stop("La longitud de 'data' no coincide con el número de filas de 'out'.")
    }
  colnames(out) <- c("time", "Sh", "Ih", "R", "Sv", "Iv")
  residuals <- sum((out[, "Ih"] - data)^2)
  return(residuals)
  }
min_residuals(data = nuevos_datos_infectados, par = c(0.0019, 0.00197), 
              t = 1:20, state = c(2000, 0.4,2, 1000, 10))



# Llama a la función
#min_residuals(data = nuevos_datos_infectados, par = c(0.003, 0.002), 
            #  t = 1:20, state = c(1000, 0.1,0, 100, 0.1))


library(rgl)
library(geometry)

# Define x y y
x = seq(0, 0.002, length = 100)
y = seq(0, 0.002, length = 100)

# Inicializa la matriz z
z = matrix(0, nrow = length(x), ncol = length(y))

# Llenar la matriz z
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    z[i, j] = min_residuals(data = nuevos_datos_infectados, par = c(x[i], y[j]), 
                            t = 1:20, state = c(2000, 3,2, 1000, 10))
  }
}

# Graficar
#library(rgl)  # Asegúrate de tener la librería rgl cargada
#plot3d(x, y, z, col = "lightblue", alpha = 0.5, axes = TRUE, box = TRUE)

# Encuentra el valor mínimo de z
min_value = min(z)


# Convierte la matriz z en un data frame
z_table <- as.data.frame(z)

# Añadir las columnas x y y a la tabla
z_table <- cbind(expand.grid(x = x, y = y), head(z_table[, 3], length(x)))
which(z_table$`head(z_table[, 3], length(x))` == min(z_table$`head(z_table[, 3], length(x))`))
# Verifica el resultado
print(z_table)
plot3d(z_table[,1], z_table[,2], z_table[,3], col = "lightblue", alpha = 0.5, axes = TRUE, box = TRUE,
       xlab= expression(beta[h]), ylab=expression(gamma), zlab="SCE")
# Añadir nombres a los ejes
axes3d(edges = 'bbox', labels = TRUE)


min_index <- which(z == min_value, arr.ind = TRUE)
min_x <- x[min_index[1]]
min_y <- y[min_index[2]]

# Imprime el punto donde ocurre el mínimo
print(paste("El valor mínimo de los residuos es", min_value, 
            "y ocurre en x =", min_x, "y =", min_y))






#################################


# Definir la función de verosimilitud
verosimilitud <- function(lbetah, lgammah) { 
  x0 = c(Sh = 1000, Ih = 0.1, R = 0, Sv = 100, Iv = 0.1) 
  par <- c(betah = lbetah, gammah = lgammah) 
  out <- ode(func = my_ode1, y = x0, times = 1:13, parms = par) 
  
  # Asegúrate de definir `np_df` antes de usarlo
  SD <- sqrt(sum((np_df$X1[1:13] - out[, "Ih"])^2) / length(1:13)) 
  return(-sum(dnorm(np_df$X1[1:13], mean = out[, "Ih"], sd = SD, log = TRUE)))
} 

# Crear la matriz de verosimilitud
betah = seq(-1, 1, by = 0.01)
gammah = seq(-1, 1, by = 0.01)

matriz_verosimilitud = matrix(0, nrow = length(betah), ncol = length(gammah))
for (i in 1:length(betah)) {
  for (j in 1:length(gammah)) {
    matriz_verosimilitud[i,j] = verosimilitud(lbetah = betah[i], 
                                              lgammah = gammah[j])
  }
}

verosimilitud(lbetah = 0.1, lgamma = 0)



#install.packages("rgl")
library("rgl")
#install.packages("geometry")
library(geometry)

tabla_vero = data.frame(expand.grid(betah,gammah), 0)
dim(tabla_vero)
for (i in 1:101) {
  tabla_vero[((i-1)*101):(i*101),3] <- c(matriz_verosimilitud)[((i-1)*101):(i*101)]
}
plot3d(betah, gammah, tabla_vero[,3])

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



###############3


out3 = matrix(0, nrow = 20, ncol = 5)
out3[, 1] = out[,2] + 10