
t.pwr <- function(sims,MOP,alternative,paired,var.equal,conf.level){

  n1=MOP[[3]][[1]]
  n2=MOP[[3]][[2]]
  meanlog1=MOP[[4]][[1]]
  sdlog1=MOP[[5]][[1]]
  meanlog2=MOP[[4]][[2]]
  sdlog2=MOP[[5]][[2]]
  alpha=0.05
  ssims=100
  p.values <- c()
  i <- 1

  repeat{
    data1 <-lnormdm(k=2, names=c("Ctrl","Treat1"),n=c(n1,n2), meanlog=c(meanlog1,meanlog2), sdlog=c(sdlog1,sdlog2))$df
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


t.pwr(sims=10,MOP=MOP,alternative="two.sided", paired=F,var.equal=T,conf.level=0.95)

