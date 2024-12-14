
library(ggplot2)
library(deSolve)

ODE = function(t,y,parms){
  with(as.list(c(y,parms)),{
    dydt= k*y
    return(list(c(dydt))) #return
  })
}

### CONDICIONES INICIALES 

k=0.05
y=1000

init= c(y=y)
parms = c(k = k)
t=seq(0,53,1)
out = ode(y=init, times=t,func= ODE, parms=parms,method = "rk4")

head(out) ##muestra los primeros 6 valores guardados en out

plot(out[,1],out[,2], type="l",xlab= "Tiempo",
     ylab= "Población total") ##gráfica para S(t)


x= out[,1]
y=out[,2]
solucion = data.frame(out)
ggplot(solucion, aes(x=time, y=y))+
  geom_line(color = "blue", size=1.5) +
  xlab("Tiempo (días)") +
  ylab("Población total") 

