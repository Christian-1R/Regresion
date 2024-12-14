library(ggplot2)
library(deSolve)
library(readxl)
datos_DEN = data.frame(datosDengue_TABLA)
semanas = seq(as.Date("2015-01-04"),as.Date("2022-06-18"), by = "week")
length(semanas)
## Constantes

#mu_h =2.01 meses-1 --> tasa de mortalidad para los humanos
#mu_v = 0.6 --> tasa de mortaldiad para los vectores (mosquitos)
# lambda =  0.13 --> tasa de natalidad de los vectores
#beta_h = 0.1  --> tasa de transmisión de vector a humano
#beta_v = 0.5 --> tasa de transmisión de humano a vector
# gam  = 2.14 --> tasa de recuperación de los humanos

####-------------------------###########3

## Variables
# Sh --> población de humanos susceptibles
# Sv --> población de vectores susceptibles
# N --> población total de humanos (N=S+I+R)
# R --> población de humanos recuperados
#Iv --> población de vectores infectados
#Ih --> población de humanos infectados


my_ode = function(t, state, parms){
  with(as.list(state),{
    dsdt = rep(0,length(state))
    ##----------ecuaciones------
    dsdt[1] = Lambda- mu_h*Sh- beta_h*Sh*Iv
    dsdt[2] = beta_h*Sh*Iv - (mu_h+gam)*Ih-mu*Ih
    dsdt[3] = gam*Ih-mu_h*R
    dsdt[4] = lambda - mu_v*Sv- beta_v*Sv*Ih
    dsdt[5] = beta_v*Sv*Ih - mu_v*Iv
    
    ##--------
    return(list(dsdt)) #return
  })
}

### CONDICIONES INICIALES 
Sh = 552922.50  ##S(0) = 
Ih = 77  ## I(0)=20
N = 553038
R = 38.49994
Iv = 1
Sv = 2384212
## constantes
mu = 0.5025 ##--> tasa de mortalidad general (semanas^-1)
mu_h = 642.3/100000 #--> tasa de mortalidad para los humanos (días)
mu_v = 0.5 #--> tasa de mortaldiad para los vectores (mosquitos)
Lambda = 141.90812 ##
 lambda =  2800  #--> tasa de natalidad de los vectores
beta_h = 0.1715  #--> tasa de transmisión de vector a humano
beta_v = 0.012744 #-> tasa de transmisión de humano a vector
gam  = 0.49994 #--> tasa de recuperación de los humanos

init= c(Sh=Sh,Ih=Ih,R=R,Iv=Iv,Sv=Sv)
t=seq(0,53,1) ##se ejecuta para 53 pasos
out = ode(y=init, times=t,func= my_ode, parms=NULL)

head(out) ##muestra los primeros 6 valores guardados en out

length(out[,2])
plot(semanas[1:2],out[,2], type="l",xlab= "Tiempo (días)",
     ylab= "No. de humanos susceptibles") ##gráfica para S(t)

plot(semanas[1:2],out[,3], type="l",xlab= "Tiempo (días)",
     ylab= "No. de humanos infectados") ##gráfica para I(t)

plot(semanas[1:2],out[,4],type="l",xlab="Tiempo (días)", ylab=" No. de humanos recuperados")
##gráfica para R(t)

plot(semanas[1:2],out[,5],type="l",xlab="Tiempo (días)", ylab="No. de vectores infectados")
##gráfica para Iv(t).

plot(semanas[1:2],out[,6],type="l",xlab="Tiempo (días)",ylab="No. de vectores susceptibles")



####-----------------------------------------------------------

x <- semanas[1:2]
y <- out[,2]

# Convertir los datos en un data frame
df <- data.frame(x = x, y = y)

# Gráfico con ggplot2
ggplot(df, aes(x = x, y = y)) +
  #geom_point(color = "blue", size = 1) +  # Puntos personalizados
  geom_line(color = "blue", linetype = "solid") +
  xlab("Tiempo") +
  ylab("No. de humanos susceptibles") 


######-------------------------------------------------

x <- semanas[1:2]
y <- out[,3]

# Convertir los datos en un data frame
df <- data.frame(x = x, y = y)

# Gráfico con ggplot2
ggplot(df, aes(x = x, y = y)) +
  #geom_point(color = "red", size = 1) +  # Puntos personalizados
  geom_line(color = "red", linetype = "solid") +
  xlab("Tiempo ") +
  ylab("No. de humanos infectados") 



#### -------------------------------------

x <- semanas[1:2]
y <- out[,4]
# Convertir los datos en un data frame
df <- data.frame(x = x, y = y)

# Gráfico con ggplot2
ggplot(df, aes(x = x, y = y)) +
  #geom_point(color = "red", size = 1) +  # Puntos personalizados
  geom_line(color = "red", linetype = "solid") +
  xlab("Tiempo ") +
  ylab("No. de humanos recuperados") 


##### -------Vectores infectados ------
x <- semanas[1:2]
y <- out[,5]
# Convertir los datos en un data frame
df <- data.frame(x = x, y = y)

# Gráfico con ggplot2
ggplot(df, aes(x = x, y = y)) +
  #geom_point(color = "red", size = 1) +  # Puntos personalizados
  geom_line(color = "red", linetype = "solid") +
  xlab("Tiempo ") +
  ylab("No. de vectores infectados") 


###---- Vectotes susceptibles 


x <- semanas[1:2]
y <- out[,6]
# Convertir los datos en un data frame
df <- data.frame(x = x, y = y)

# Gráfico con ggplot2
ggplot(df, aes(x = x, y = y)) +
  #geom_point(color = "red", size = 1) +  # Puntos personalizados
  geom_line(color = "red", linetype = "solid") +
  xlab("Tiempo ") +
  ylab("No. de vectores susceptibles") 



