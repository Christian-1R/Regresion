lista_de_listas = vector(mode='list',length=10)
lista_1 = vector(mode = 'list',length=10)
lista_2 = vector(mode = 'list', length=10)


for(k in 1:10){
  lista_de_listas[[k]][1] = 0
  lista_1[[k]][1] =  1
  lista_2[[k]][2] =2
}

lista_de_listas


##################################################

Sv = vector(mode='list',length=11)
Iv = vector(mode='list',length=11)
tasa_natalidad = c()
#for( i in 1:100){
 # tasa_natalidad[i]=i
#}

for (k in 1:11){
  if(k==1){
    Sv[[k]][1]= 1000 ## dato inicial de susceptibles en el tiempo 0
    Iv[[k]][1]=100 ## dato inicial de vectores infectados en el tiempo 0
  }
 else{
   Sv[[k]][1]= (k-1)*100 ## dato inicial de susceptibles en el tiempo 1,2,...
   Iv[[k]][1] =  (k)*100 ## dato inicial de vectores infectados en el tiempo 1,2,.... 
 }
}

for (k in 1:11){
  for(i in 2:100){
    Sv[[k]][i] = 0.5*(Iv[[k]][i-1]+Sv[[k]][i-1])-0.5*Sv[[k]][i-1]-0.02744*Sv[[k]][i-1]*casos[i]+Sv[[k]][i-1]
    Iv[[k]][i] = 0.02744*Sv[[k]][i-1]*casos[i]+Iv[[k]][i-1] -0.5*Iv[[k]][i-1]
  }
}

# Graficar Sv
par(mfrow=c(3,4))  # Configurar la pantalla para múltiples gráficos
for (k in 1:11) {
  plot(t[1:100],Sv[[k]], type='l', main=paste("Simulación tomando  k =", k), xlab="Año-Mes", ylab=expression(hat(S)[v](t)), col='blue')
}


# Graficar Iv
par(mfrow=c(3,4))  # Configurar la pantalla para múltiples gráficos
for (k in 1:11) {
  plot(t[1:100],Iv[[k]], type='l', main=paste("Simulación tomando k =", k), xlab="Año-Mes", ylab=expression(hat(I)[v](t)), col='blue')
}
