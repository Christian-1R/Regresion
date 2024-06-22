library(ggplot2)
library(deSolve)

## Constantes

#mu_h =2.01 --> tasa de mortalidad para los humanos
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
    dsdt[1] = mu_h*(N-Sh)- beta_h*Sh*Iv
    dsdt[2] = beta_h*Sh*Iv - (mu_h+gam)*Ih
    dsdt[3] = gam*Ih-mu_h*R
    dsdt[4] = lambda - mu_v*Sv- beta_v*Sv*Ih
    dsdt[5] = beta_v*Sv*Ih - mu_v*Iv
    
    ##--------
    return(list(dsdt)) #return
  })
}

### CONDICIONES INICIALES 
Sh = 40020  ##S(0) = 40000
Ih =20     ## I(0)=20
N = 40020
R = 5
Iv = 20
Sv = 4000
## constantes
mu_h =2.01 #--> tasa de mortalidad para los humanos
mu_v = 0.6 #--> tasa de mortaldiad para los vectores (mosquitos)
 lambda =  0.13 #--> tasa de natalidad de los vectores
beta_h = 0.1  #--> tasa de transmisión de vector a humano
beta_v = 0.5 -#-> tasa de transmisión de humano a vector
gam  = 2.14 #--> tasa de recuperación de los humanos

init= c(Sh=Sh,Ih=Ih,R=R,Iv=Iv,Sv=Sv)
t=seq(0,53,1) ##se ejecuta para 53 pasos
out = ode(y=init, times=t,func= my_ode, parms=NULL)

head(out) ##muestra los primeros 6 valores guardados en out

plot(out[,1],out[,2], type="l",xlab= "Tiempo (meses)",
     ylab= "No. de humanos susceptibles") ##gráfica para S(t)

plot(out[,1],out[,3], type="l",xlab= "Tiempo (meses)",
     ylab= "No. de humanos infectados") ##gráfica para I(t)




####-----------------------------------------------------------

x <- out[,1]
y <- out[,2]

# Convertir los datos en un data frame
df <- data.frame(x = x, y = y)

# Gráfico con ggplot2
ggplot(df, aes(x = x, y = y)) +
  #geom_point(color = "blue", size = 1) +  # Puntos personalizados
  geom_line(color = "blue", linetype = "dashed") +
  xlab("Tiempo (días)") +
  ylab("No. de humanos susceptibles") 


######-------------------------------------------------

x <- out[,1]
y <- out[,3]

# Convertir los datos en un data frame
df <- data.frame(x = x, y = y)

# Gráfico con ggplot2
ggplot(df, aes(x = x, y = y)) +
  #geom_point(color = "red", size = 1) +  # Puntos personalizados
  geom_line(color = "red", linetype = "dashed") +
  xlab("Tiempo (días)") +
  ylab("No. de humanos infectados") 


