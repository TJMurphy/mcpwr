binomdm <- function(k, n, groupnames, size, prob){
  if(length(n) != k
     | length(size) != k
     | length(prob) != k
     | length(groupnames) != k)
  {
    print("STOP! Length of one of the variables (n, size, prob) does not match k")
    stop()
  }
  parameters <<- list(groupnames = groupnames
                      , n = n
                      , size = size
                      , prob = prob)

  value_list <- lapply(1:k
                       , FUN = function(b){
                         stats::rbinom(n=n[b]
                                       , size = size[b]
                                       , prob = prob[b])
                       }
  )

  data <- as.data.frame(t(
    purrr::map_dfr(.x = value_list
                   , .f = ~ as.data.frame(t(.)))
  ))
  colnames(data) <- groupnames

  datap <- stats::na.omit(tidyr::pivot_longer(data = data
                                              , cols = everything()
                                              , names_to = "group"
                                              , values_to = "values"))


  plot1 <- ggplot2::ggplot(datap,
                           ggplot2::aes(x=group
                                        , y = values
                                        , color=group)) +
    ggplot2::geom_jitter() +
    ggplot2::theme_bw()

  print(plot1)

  return(list(parameters,data))
}

