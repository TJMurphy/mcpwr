#' normdm
#'
#' @param k number of groups to be simulated
#' @param names names for each group
#' @param n sample size of each group
#' @param mean mean of simulated data for each group
#' @param sd standard deviation of simulated data for each group
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

  parameters <<- list(names=names
                      , n=n
                      , mean=mean
                      , sd=sd)

  df1 <- as.data.frame(n , mean , sd)
  df2 <- apply(df1, 1, function(x) stats::rnorm(x[1]
                                                , mean=x[2]
                                                , sd=x[3]))
  df2 <- purrr::map_dfr(.x = list, .f= ~as.data.frame(t(.)))

  colnames(df2) <- names

  df3 <- tidyr::pivot_longer(data=df2
                             , cols= c(1:ncol(df2))
                             , names_to = "groups"
                             , values_to = "values")
  simdata <<- as.data.frame(df3)

  simdata <- stats::na.omit(simdata)

  p <- ggplot2:: ggplot(data= simdata
                        , mapping = ggplot2::aes(x=name, y=value)) +
    ggplot2:: geom_jitter(mapping = ggplot2::aes(color=name))

  print(p)
  print(parameters)
  print(df2)

}
