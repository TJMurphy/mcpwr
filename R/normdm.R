#' normdm
#' Simulates groups of normally distributed data and produces graphic
#' visualization of simulated data
#'
#' @param k number of groups to be simulated
#' @param names a vector of character strings representing group names
#' @param n a vector of n values representing group sample sizes
#' @param mean a vector of means
#' @param sd a vector of standard deviations
#'
#' @return
#' @export
#'
#' @examples normdm(k=5, names= c("control", "low", "medium", "high", "positive"),n= c(50,50,50,50,40), mean = c(10, 5, 6, 7,8), sd= c(1,5,5,5,5))
#'
#'
normdm <- function(k, names= c(...), n= c(...), mean= c(...), sd= c(...)){

  if(k != (length(n)+length(mean)+length(sd)+length(names))/4){
    print("k value must match length of names, n, mean, and sd")
    stop()
  }


  df1 <- data.frame(n , mean , sd)
  df2 <- apply(df1, 1, function(x) stats::rnorm(x[1]
                                                , mean=x[2]
                                                , sd=x[3]))


  df3 <- as.data.frame(t(purrr::map_dfr(.x = df2, .f= ~as.data.frame(t(.)))))

  colnames(df3) <- names

  df4 <- tidyr::pivot_longer(data=df3
                             , cols= c(1:ncol(df3))
                             , names_to = "groups"
                             , values_to = "values")

  simdata <- stats::na.omit(df4)

  p <- ggplot2:: ggplot(data= simdata
                        , mapping = ggplot2::aes(x=groups, y=values)) +
    ggplot2:: geom_jitter(mapping = ggplot2::aes(color=groups))

  parameters <<- list(names=names
                      , n=n
                      , mean=mean
                      , sd=sd)

  print(p)
  print(parameters)
  print(df2)

}
