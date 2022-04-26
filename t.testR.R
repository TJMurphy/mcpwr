
t.pwr <- function(sims,optimalparameters,paired,var.equal,conf.level){

  n1=optimalparameters[[3]][[1]]
  n2=optimalparameters[[3]][[2]]
  meanlog1=optimalparameters[[4]][[1]]
  sdlog1=optimalparameters[[5]][[1]]
  meanlog2=optimalparameters[[4]][[2]]
  sdlog2=optimalparameters[[5]][[2]]
  alpha=0.05
  ssims=100
  p.values <- c()
  i <- 1

  repeat{
    x=lnormdm(n1, meanlog1, sdlog1);
    y=lnormdm(n2, meanlog2, sdlog2);
    p <- t.test(x,
                y,
                paired,
                var.equal,
                conf.level)$p.value
    p.values[i] <- p

    if (i==ssims) break
    i = i+1

    pwr <- length(which(p.values<alpha))/ssims
  }
  return(pwr)
}


t.pwr(sims=10,optimalparameters=optimalparameters,paired=F,var.equal=T,conf.level=0.95)

######Megan's format

t.pwr <- function(){
  ssims=100
  p.values <- c()
  i <- 1

  repeat{
    x=lnormdm(n1, meanlog1, sdlog1);
    y=lnormdm(n2, meanlog2, sdlog2);
    p <- pairwise.t.test(simdata$Treatment,simdata$Value)
    p.values[i] <- p

  }
}

pwr <- length(which(p.values<alpha))/ssims
return(pwr)
