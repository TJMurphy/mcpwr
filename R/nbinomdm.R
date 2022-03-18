#' Negative Binomial Data Maker
#'
#' nbinomdm simulates over-dispersed frequency data using the stats::rnbinom function.
#' It creates a data frame containing different samples for different treatment groups 
#' following the negative binomial distribution. Currently it uses the size and mu parameters
#' following the rnbinom function, but not the prob parameter.
#'
#' @param k A single value corresponding to the number of treatment groups.
#' @param names A vector of character strings specifying the names of the treatment groups.
#' @param n A vector specifying the sample sizes of the treatment groups.
#' @param size A vector specifying the dispersion parameters of the negative binomial distributions for the treatment groups. Strictly have to be positive values, not necessarily integers.
#' @param mu A vector specifying the means of the negative binomial distributions for the treatment groups.
#' @param hist TRUE or FALSE to return a histogram plot
#' @param scatter TRUE or FALSE to return a scatter plot
#'
#' @return Histogram and jitter plots for the treatment groups and a data frame containing the random samples.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' nbinomdm(3,c("C","T1","T2"),c(50,50,60),c(3,3,3),c(1,5,10),TRUE,FALSE)
#'
#'

nbinomdm <- function(k,names,n,size,mu,hist,scatter){
  
  if(length(n) != k
     | length(size) != k
     | length(mu) != k
     | length(names) != k
  )
  {
    print("Length of sample size or size or prob doesn't match the number of groups")
    stop()
  }
  
  list_of_values <- lapply(1:k,
                           FUN = function(c){
                             stats::rnbinom(
                               n=n[c]
                               ,size=size[c]
                               ,mu=mu[c]
                             )
                           }
  )
  
  Data <- as.data.frame(t(
    purrr::map_dfr(.x = list_of_values, .f = ~ as.data.frame(t(.)))
  ))
  colnames(Data) <- names
  
  DataFrame_p <- tidyr::pivot_longer(data = Data
                                     ,cols = dplyr::everything()
                                     ,names_to = "Groups"
                                     ,values_to = "Value")
  DataFrame_p <- stats::na.omit(DataFrame_p)
  
  Input_parameters <- data.frame(groups = names
                                 , n = n
                                 , size = size
                                 , mu = mu)
  
  p1 <- ggplot2::ggplot(DataFrame_p,
                        ggplot2::aes(x=.data$Value,fill = .data$Groups)) +
    ggplot2::geom_histogram(bins = 15,position = "identity", alpha = .8) +
    ggplot2::theme_bw()
  p2 <- ggplot2::ggplot(DataFrame_p,
                        ggplot2::aes(x=.data$Groups,y = .data$Value,color=.data$Groups)) +
    ggplot2::geom_jitter() +
    ggplot2::theme_bw()
  
  if (hist == T){
  print(p1)
  }
  if (scatter == T){
  print(p2)
  }
  
  return(list(Input_parameters,Data))
  
}


