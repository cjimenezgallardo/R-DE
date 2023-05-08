


var1g.test<-function(vd,vc,alternative,conf){
  options(warn = -1)
  options(scipen = 999)
  
  print(alternative)
  
  gl<- length(vd)-1
  alfa<- 1-conf
  
  ji<- gl * (var(vd)*gl/(gl+1))/vc
  
  if (alternative=="less"){
    pvalue<- pchisq(ji,gl,lower.tail = TRUE)
  }else if (alternative=="greater"){
    pvalue<- pchisq(ji,gl,lower.tail = FALSE)
  }else{
    pvalue<- pchisq(ji,gl,lower.tail = TRUE)/2
  }
  
  print(pvalue)
  paste(" Ji2 calculado ", ji)
  
}



da<-as.data.frame(c(7.96,7.9,7.98,8.01,7.97,7.96,8.03,8.02,8.04,8.04))
names(da)[1]=c("y")
var1g.test(da$y,0.01,"less",0.95)
