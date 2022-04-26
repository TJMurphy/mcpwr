t.pwr <- function(ssims,alpha,MOP,alternative,paired,var.equal,conf.level){

  k=MOP[[1]]
  names=MOP[[2]]
  n1=MOP[[3]][[1]]
  n2=MOP[[3]][[2]]
  meanlog1=MOP[[4]][[1]]
  sdlog1=MOP[[5]][[1]]
  meanlog2=MOP[[4]][[2]]
  sdlog2=MOP[[5]][[2]]
  p.values <- c()
  i <- 1

  repeat{
    data1 <<-lnormdm(k=k, names=names,n=c(n1,n2), meanlog=c(meanlog1,meanlog2), sdlog=c(sdlog1,sdlog2))$df
    p <- t.test(x=data1$Ctrl,
                y=data1$Treat1,
                alternative=alternative,
                paired=paired,
                var.equal=var.equal,
                conf.level=conf.level)$p.value
    p.values[i] <- p

    if (i==ssims) break
    i = i+1

    pwr <- length(which(p.values<alpha))/ssims
  }
  return(pwr)
}


t.pwr(ssims=10,alpha=0.05,MOP=MOP,alternative="two.sided", paired=F,var.equal=T,conf.level=0.95)

