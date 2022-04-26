library(ez)

anova.pwr <- function(ssims,alpha,MOP,type,return_aov,detailed){
  k1=MOP[[1]]
  names1=MOP[[2]]
  n1=MOP[[3]]
  meanlog1=MOP[[4]]
  sdlog1=MOP[[5]]
  i = 1
  p.values <- c()

  repeat{
  data1 <-lnormdm(k=k1,names=names1,n=n1,meanlog=meanlog1,sdlog=sdlog1)$anovaoutput
  data1 <- data.frame(data1)
  data1 <- na.omit(data1)
  p <<- ezANOVA(
  data = data1,
  wid = as.factor(wid),
  dv = Values,
  between = as.factor(Treatment),
  type = type,
  return_aov = return_aov,
  detailed = detailed)

  p2 <- p[["ANOVA"]][["p"]]

  p.values[i] <- p2

if (i==ssims) break
i = i+1

pwr <- length(which(p.values<alpha))/ssims

}
return(pwr)
}

anova.pwr(ssims=10,alpha=0.01,MOP=MOP,type=1,return_aov=T,detailed=T)


