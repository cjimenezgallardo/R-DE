
#Librerias
library (car)
library (EnvStats)
library (psych)
library (nortest)
library (doBy)
library (MASS)
library (tidyverse)




#ANALISIS NORMALIDAD 
shapiro.test(datos$vd)
ad.test(datos$vd)
lillie.test(datos$vd)
pearson.test(datos$vd)
sf.test(datos$vd)
cvm.test(datos$vd)




#Prueba t para una muestra parametrica
t.test(datos$vd,mu=2,alternative="less" )# "less" o "greater" o "two.sided" sin alternative es a 2 colas.

#prueba No parametrica una poblacion alternativa a la prueba t cuando los datos no son normales.
wilcox.test(datos$vd,mu=Valor,exact=T, alternative="t",conf.int=0.95) #t two.sided. l =less, g = greater

# Prueba Ji2 para la varianza de una poblacion cuando  s = s0
vartest(datos$vd,null.value= 50000, alternative="two.sided", conf.level=0.95)


#prueba Binomial para la proporcion de un grupo
#x=numero de exitos en el grupo
#n=tama?o de la muestra
#p0= proporcion esperada, teorica, historica, norma
prop.test(x, n, p=p0 ,conf.level = 0.95, alternative = "two.sided")





