


#anova si supuestos de normalidad es rechazado
kruskal.test(datos1$ind_polvo,datos1$trat)


wilcox.test(x=datos1$ind_polvo[datos1$eqN==1],y=datos1$ind_polvo[datos1$eqN==2],alternative="two.sided",mu=0,paried=FALSE)
wilcox.test(x=datos1$ind_polvo[datos1$eqN==1],y=datos1$ind_polvo[datos1$eqN==3],alternative="two.sided",mu=0,paried=FALSE)
wilcox.test(x=datos1$ind_polvo[datos1$eqN==1],y=datos1$ind_polvo[datos1$eqN==4],alternative="two.sided",mu=0,paried=FALSE)
wilcox.test(x=datos1$ind_polvo[datos1$eqN==1],y=datos1$ind_polvo[datos1$eqN==5],alternative="two.sided",mu=0,paried=FALSE)
wilcox.test(x=datos1$ind_polvo[datos1$eqN==2],y=datos1$ind_polvo[datos1$eqN==3],alternative="two.sided",mu=0,paried=FALSE)
wilcox.test(x=datos1$ind_polvo[datos1$eqN==2],y=datos1$ind_polvo[datos1$eqN==4],alternative="two.sided",mu=0,paried=FALSE)
wilcox.test(x=datos1$ind_polvo[datos1$eqN==2],y=datos1$ind_polvo[datos1$eqN==5],alternative="two.sided",mu=0,paried=FALSE)
wilcox.test(x=datos1$ind_polvo[datos1$eqN==3],y=datos1$ind_polvo[datos1$eqN==4],alternative="two.sided",mu=0,paried=FALSE)
wilcox.test(x=datos1$ind_polvo[datos1$eqN==3],y=datos1$ind_polvo[datos1$eqN==5],alternative="two.sided",mu=0,paried=FALSE)
wilcox.test(x=datos1$ind_polvo[datos1$eqN==4],y=datos1$ind_polvo[datos1$eqN==5],alternative="two.sided",mu=0,paried=FALSE)




