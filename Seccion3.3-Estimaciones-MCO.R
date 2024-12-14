##Se cargan las librerias ###

library(rgl)
library(geometry)
library(bbmle)
library(deSolve)
library(ggplot2)


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
out2 <- ode(y = state, times = 1:20, func = my_ode1, 
            parms = c(0.0019, 0.00197))


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



min_residuals <- function(data, par, state, t) {
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
              t = 1:20, state = c(1500,0.2,1,1000,10))
x = seq(0, 0.0008, length = 100)
y = seq(0, 0.004, length = 100)
z = matrix(0, nrow = length(x), ncol = length(y))
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    z[i, j] = min_residuals(data = nuevos_datos_infectados, par = c(x[i], y[j]), 
                            t = 1:20, state = c(1500,0.2,1,1000,10))
  }
}
min_value = min(z)
z_table <- as.data.frame(z)
z_table <- cbind(expand.grid(x = x, y = y), head(z_table[, 3], length(x)))
print(z_table)
plot3d(z_table[,1], z_table[,2], z_table[,3], col = "lightblue", alpha = 0.5, 
       axes = TRUE, box = TRUE,
       xlab= expression(beta[h]), ylab=expression(gamma), zlab="SCE")
axes3d(edges = 'bbox', labels = TRUE)

filas = which(z_table[,3] == min(z_table[,3]) & round(z_table[,2],3) == 0.0019 & 
                round(z_table[,1],5) == 0.00197)

z_table[filas, ]

## Se optimiza la SCE ####  
# Optimización
opt <- optim(par = c(0.0004040404, 0.002020202),
  fn = min_residuals,
  data = nuevos_datos_infectados,
  state = c(1500,0.2,1,1000,10),
  t = 1:20)
print(opt)

## se aproxima la solución de la EDO con el método RK4 ##

out3 <- ode(y = state, times = 1:20, func = my_ode1, 
            parms = c(0.001915672, 0.001233069))

library(ggplot2)


x = t[1:20]
y = out3[,3]
z =  nuevos_datos_infectados

colors()
ggplot(Nuevadata,aes(x=x,y=y))+
  geom_point(aes(x=x,y=nuevos_datos_infectados[1:20]), color="red")+
  geom_line(aes(x=x,y=out2[,3]), color= "purple",linetype="solid",size=1.6)+
  geom_line(aes(x=x,y=out3[,3]), color= "turquoise",linetype="dashed",size=1.4)+
  xlab("Tiempo")+
  ylab(expression(I[h](t)))
