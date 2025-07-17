#--- anova  


# datos$y   representa la variable dependiente
# datos$x   representa la variable independiente.

#####names#####


names(datos)[1]=c('y')
names(datos)[2]=c('x')


#activar las siguientes librerias

library(ggplot2)# grafica
library(doBy) #Estadisticas agrupadas
library(nortest) #pruebas de normalidad
library(car) #contiene prueba de levene 
library(agricolae) #algunas pruebas posthoc
library(DescTools) #algunas pruebas posthoc
library(PMCMRplus) #algunas pruebas posthoc
library(onewaytests) #


#probando Normalidad en varios grupos

by(data = datos,INDICES = datos$x, FUN = function(datos){shapiro.test(datos$y)}) #shapiro wilk test
by(data = datos,INDICES = datos$x, FUN = function(datos){ad.test(datos$y)}) #Anderson-Darling
by(data = datos,INDICES = datos$x, FUN = function(datos){cvm.test(datos$y)}) # Cramer
by(data = datos,INDICES = datos$x, FUN = function(datos){lillie.test(datos$y)}) #Kolmogorov-Smirnov
by(data = datos,INDICES = datos$x, FUN = function(datos){sf.test(datos$y)}) #shapiro Francia
by(data = datos,INDICES = datos$x, FUN = function(datos){pearson.test(datos$y)}) #Pearson



#Probando Homocedasticidad

leveneTest(datos$y,datos$x, center=mean) # mejor cuando los ni no son de igual tamaño, centrar en la mediana si los datos no son normales

bartlett.test(datos$y,datos$x) # necesariamente los ni deben iguales.

fligner.test(datos$y,datos$x) # alternativa a LEVENE,el ni >=5

bf.test(datos$y~as.factor(datos$x),data=datos) # 



# Anova Completamente Aleatorizado o a Una Via (oneWay)

ResAnova<-aov(datos$y~as.factor(datos$x)) # si la VI es numerica utilice as.factor, si no puede crear una variable factor
summary(ResAnova) 

#Post Hoc Parametrico Varianzas IGUALES

#tukey
ResTukey<-tukeyTest(ResAnova) #Library(PMCMRplus)
summary(ResTukey)

TukeyHSD(ResAnova,"as.factor(datos$x)", ordered = TRUE) #en R basico
plot(TukeyHSD(ResAnova))

#----------------------------------------------------------------------------- SNK 
ResSNK<-SNK.test(ResAnova,"as.factor(datos$x)", console=TRUE, main="titulo") #test SNK Libreria Agricolae
plot(ResSNK)

#----------------------------------------------------------------------------- Scheffe
ResScheffe<-scheffe.test(ResAnova,"as.factor(datos$x)",group = TRUE,console = TRUE,main = "resultado ejemplo") #libreria agricolae
plot(ResScheffe)


ResScheffe<-scheffeTest(ResAnova) #library PMCMRPLUS
summary(ResScheffe)
plot(ResScheffe,main="Scheffe")

#####LSD#####
#-----------------------------------------------------------------------------

ResLSD<-lsdTest(ResAnova,"as.factor(datos$x)",group = TRUE, console = TRUE) #Library PMCMRPLUS
summary(ResLSD)
plot(ResLSD,main = "Test LSD para Temperaturas")


ResLSD<-LSD.test(ResAnova,"as.factor(datos$x)", p.adj = c("holm"),group =TRUE, console = TRUE) # library agricolae
#c("none","holm","hommel", "hochberg", "bonferroni", "BH", "BY", "fdr")

plot(ResLSD,main="TITULO")





#Post Hoc Parametrico Varianzas DISTINTAS

ResGames<-gamesHowellTest(ResAnova) #library PMCMRPLUS
summary(ResGames)
plot(ResGames,main="Test Games-Howell para Temperaturas")





#anova si supuestos de normalidad es rechazado
kruskal.test(datos$y,datos$x)

wilcox.test(x=datos$y[datos$x==1],y=datos$y[datos$x==2],alternative="two.sided",mu=0,paried=FALSE)
wilcox.test(x=datos$y[datos$x==1],y=datos$y[datos$x==3],alternative="two.sided",mu=0,paried=FALSE)
wilcox.test(x=datos$y[datos$x==1],y=datos$y[datos$x==4],alternative="two.sided",mu=0,paried=FALSE)
wilcox.test(x=datos$y[datos$x==1],y=datos$y[datos$x==5],alternative="two.sided",mu=0,paried=FALSE)
wilcox.test(x=datos$y[datos$x==2],y=datos$y[datos$x==3],alternative="two.sided",mu=0,paried=FALSE)
wilcox.test(x=datos$y[datos$x==2],y=datos$y[datos$x==4],alternative="two.sided",mu=0,paried=FALSE)
wilcox.test(x=datos$y[datos$x==2],y=datos$y[datos$x==5],alternative="two.sided",mu=0,paried=FALSE)
wilcox.test(x=datos$y[datos$x==3],y=datos$y[datos$x==4],alternative="two.sided",mu=0,paried=FALSE)
wilcox.test(x=datos$y[datos$x==3],y=datos$y[datos$x==5],alternative="two.sided",mu=0,paried=FALSE)
wilcox.test(x=datos$y[datos$x==4],y=datos$y[datos$x==5],alternative="two.sided",mu=0,paried=FALSE)



