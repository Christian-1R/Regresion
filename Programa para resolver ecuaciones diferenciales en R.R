install.packages("deSolve")

library(deSolve)

## Sistema de ecuaciones diferenciales SIR-SI

## el "vector" de parametros iniciales es state
my_ode = function(t, state, parms){
  with(as.list(state),{
    dndt = rep(0,length(state))
    ##----------ecuaciones------
    dndt[1] = -k*N
    ##--------
    return(list(dndt)) #return
  })
}

### CONDICIONES INICIALES 

N=70 ##N(0)=70
init = c(N=N)
k=0.2
t=seq(0,30,1) ##se ejecuta para 30 pasos
out = ode(y=init, times=t,func= my_ode, parms=NULL)
plot(out,type="l",xlab="Tiempo (h)", ylab="Cafeina (mg)", main="Solución ODE")




