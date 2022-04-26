#' Wilcoxon Rank Sum & Signed Rank Tests Repeater Function
#'
#' @param sims number of simulations
#' @param param parameters from datamaker output
#' @param alternative "greater", "less", or "two.sided"
#' @param mu null hypothesis
#' @param paired paired test, T/F
#' @param exact should exact pvalue be computed, T/F
#' @param correct should continuity correction be applied to pvalue, T/F
#' @param conf.int should confidence interval be computed, T/F
#' @param conf.level confidence level of the interval
#' @param tol.root if conf.int=T, positive numeric tolerance
#' @param digits.rank used to computed ranks of test statistic
#'
#' @return a power value
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' wl.pwr(sims=10000,param=d1$param,alternative="two.sided",mu=0,paired=FALSE,exact=NULL,correct=TRUE,conf.int=FALSE,conf.level=0.95,tol.root=1e-4,digits.rank=Inf)

##### above requires for dm to be run first
##### I want to find a way to make it use defaults for optional args


wl.pwr <- function(sims, param, alternative, mu, paired, exact
                   , correct, conf.int, conf.level, tol.root
                   , digits.rank){

  group1=param[[1]][[1]]
  group2=param[[1]][[2]]
  n1=param[[2]][[1]]
  n2=param[[2]][[2]]
  lambda1=param[[3]][[1]]
  lambda2=param[[3]][[2]]
  alpha=0.05
  ssims=100
  p.values <- c()
  i <- 1

  repeat{
    data1 <- poisdm(c(n1, n2), c(lambda1,lambda2)
                    , c(group1, group2), FALSE)$simdata

    p <- stats::wilcox.test(x = data1$group1
                     , y = data1$group2
                     , alternative = alternative
                     , mu = mu
                     , paired = paired
                     , exact = exact
                     , correct = correct
                     , conf.int = conf.int
                     , conf.level = conf.level
                     , tol.root = tol.root
                     , digits.rank = digits.rank
                     )$p.value

    p.values[i] <- p

    if (i==ssims) break
    i = i+1

    pwr <- length(which(p.values < alpha))/ssims
  }
  return(pwr)
}


