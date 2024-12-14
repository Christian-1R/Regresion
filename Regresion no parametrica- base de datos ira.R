## Regresión no paramétrica para Imputación de datos

datos_IRA = data.frame(asriosgu_datos_IRA)

head(datos_IRA)

dim(datos_IRA)

datos_IRA1 = t(datos_IRA[, 2:53])

dim(datos_IRA1)

datos_IRA2 = c(datos_IRA1)

datos_IRA2



casos = datos_IRA2[1:(10*52)]

plot(casos, pch = 20)


## generar secuencia de semanas 
fechas <- seq(as.Date("2010-01-03"), 
              
              as.Date("2019-12-28"), 
              
              by = "week")

fechas



which(fechas == "2014-12-28")

tabla = data.frame(fechas, 
                   
                   casos = c(casos[1:260], NA,
                             
                             casos[261:520]))



plot(tabla$fechas, tabla$casos, pch = 20)



#install.packages("imputeTS")

library(imputeTS)

tabla$casos = na_interpolation(tabla$casos, 
                               
                               option = "spline")

tabla$casos[261]

tabla$conteo = 1:521



#install.packages("npregfast")

library(npregfast)

?frfast

np_casos = frfast(tabla$casos ~ tabla$conteo, 
                  
                  smooth = "kernel", 
                  
                  kernel = "gaussian", 
                  
                  p = 3, kbin = 521)

np_df = data.frame(np_casos$p)

np_df



plot(tabla$fechas, tabla$casos, pch = 20)

lines(tabla$fechas, np_df$X1, col = "red", lwd = 2)