


var1g.test<-function(vd,vc,alternative,conf){
  options(warn = -1)
  options(scipen = 999)
  
  print(alternative)
  
  gl<- length(vd)-1
  alfa<- 1-conf
 
  ji<- gl * var(vd)/vc
  
  if (alternative=="less"){
    pvalue<- pchisq(ji,gl,lower.tail = TRUE)

  }else if (alternative=="greater"){
    pvalue<- pchisq(ji,gl,lower.tail = FALSE)
  }else{
    pvalue<- pchisq(ji,gl,lower.tail = TRUE)/2
  }
  paste("Chi-t:")
  paste(" Ji2 calculado ", ji)
  paste("pvalue:",pvalue)

  
}



da<-as.data.frame(c(6.2,5.8,5.7,6.3,5.9,5.8,6.0))
names(da)[1]=c("y")
var1g.test(da$y,0.1,"less",0.95)
