install.packages("deSolve")
library(deSolve)


my_ode = function(t,state,parms){
  with(as.list(state),{dndt=rep(0,length(state))
  
  ##mis ecuaciones
  dndt[1]= -k*N    #dN/dt
  dndt[2] = k*N-r*B #dB/dt
  ##----------
  return(list(dndt)) ##return
  })
}


## fin de la función

N=70 ## valor inicial de N, es decir, N(0)=70
B=0
#init = c(N=N) ## ceamos un vector con valores iniciales
init = c(N=N,B=B)

#constantes del problema
k=0.2
r=0.1
t=seq(0,30,1) ## se ejecuta por 30 pasos
out= ode(y=init,times=t,func= my_ode,parms=NULL)
head(out)  ##muestra los primeros 6 datos de out
#plot(out,type="l",xlab="Tiempo (h)",ylab="Cafeina (mg)", main="Curva solución ")

plot(out[,1],out[,2],type="l",xlab="tiempo (h)",ylab="Cafeina (mg)")
lines(out[,1],out[,3],col="red")
legend("topright",c("N","B"),col=c("black","red"),lty=1)