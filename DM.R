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
  
  return(list(params=Input_parameters,df=Data,dist="nbinomdm"))
  
}


