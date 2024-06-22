#install.packages("ggplot2")
library(ggplot2)
library(deSolve)



my_ode = function(t, state, parms){
  with(as.list(state),{
    dsdt = rep(0,length(state))
    ##----------ecuaciones------
    dsdt[1] = -k*S*I
    dsdt[2] = k*S*I-g*I
    ##--------
    return(list(dsdt)) #return
  })
}

### CONDICIONES INICIALES 
S = 40000  ##S(0) = 40000
I =20     ## I(0)=20
k = 0.1  ## k= \beta =0.1
g=2.14  ## g= \gamma = 2.14
init= c(S=S,I=I)
t=seq(0,53,1) ##se ejecuta para 53 pasos
out = ode(y=init, times=t,func= my_ode, parms=NULL)
 
head(out) ##muestra los primeros 6 valores guardados en out

plot(out[,1],out[,2], type="l",xlab= "Tiempo (meses)",
     ylab= "No. de personas susceptibles") ##gráfica para S(t)

plot(out[,1],out[,3], type="l",xlab= "Tiempo (meses)",
     ylab= "No. de infectados") ##gráfica para I(t)


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
  ylab("No. de personas susceptibles") 


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
  ylab("No. de personas infectadas") 

