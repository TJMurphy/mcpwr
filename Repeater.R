library(rstatix)
library(dplyr)
library(tidyverse)

myoptimdm <- get(nbinomdm(3,c("C","T1","T2"),c(50,50,40),c(5,5,5),c(1,2,3),F,F)$dist)
myoptimparams <- nbinomdm(3,c("C","T1","T2"),c(50,50,40),c(5,5,5),c(1,2,3),F,F)$params

repeater_nbinom <- function(myoptimdm,myoptimparams,sims,pair,alt,ci){
  i <- 1
  p.values <- c()
  repeat{
    data <- myoptimdm(nrow(myoptimparams),myoptimparams$groups,myoptimparams$n,myoptimparams$size,myoptimparams$mu,F,F)$df
    data_p <- pivot_longer(data, cols = everything(), names_to = "groups",values_to = "samples")
    data_p$groups <- factor(data_p$groups)
    p <- pairwise_t_test(data_p, samples ~ groups, pool.sd = FALSE,
                         p.adjust.method = "bonferroni", 
                paired=pair, 
                alternative=alt, 
                conf.level=1-ci)$p.adj
    p.values[i] <- p
    
    if (i==sims) break
    i <- i+1
    
    pwr <- length(which(p.values<ci))/sims
  }
  return(pwr)
}

repeater_nbinom(myoptimdm,myoptimparams,100,F,"two.sided",0.05)
