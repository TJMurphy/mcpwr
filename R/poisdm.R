#' Poisson DataMaker
#'
#' @param n Sample size
#' @param lambda Mean of sample
#' @param groupnames Names of groups
#'
#' @return Table with input parameters, histogram of simulated values, and scatterpoint of simulated values
#' @export
#'
#' @examples
#' poisdm(c(40, 50), c(10, 12), c("group1", "group2"))
#' poisdm(c(40, 50), c(10, 12), c("group2"))
#' poisdm(c(40, 50), c(-10, 12), c("group1", "group2"))
#' poisdm(rep(40, 5), rep(10, 5), c("group", "group2", "group3", "group4", "group5"

poisdm <- function(n, lambda, groupnames){

  # make data frame of input values
  input <- data.frame(
    groupnames
    , n
    , lambda
  )

  # error for unequal input lengths
  if(length(n) != length(lambda) || length(n) != length(groupnames)){
    print("specify an equal number of sample sizes (n), means (lambda), and group names")
    stop()
  }

  # error for negative input
  if (!all((lambda >= 0) && (n >= 0))) {
    print("lambda (vector of means) and n (sample size) must be non-negative")
    stop()
  }

  # generate random samples - list of length(n), int[1:n[i]]
  df1 <- lapply(1:length(n)
                , FUN = function(i){rpois(n = n[i]
                                          , lambda = lambda[i]
                )
                }
  )

  # makes data frame with length(n) obs of max(n) variables
  # fills difference in output lengths with NAs
  df2 <- purrr::map_dfr(
    .x = df1
    , .f = ~ as.data.frame(
      t(.)
    )
  )

  # make it a data frame and flips
  df2 <- as.data.frame(t(df2))

  # rename columns with group names
  colnames(df2) <- groupnames

  # prep data to plot (make it tidy!)
  dfp <- stats::na.omit(tidyr::pivot_longer(data = df2
                                            , cols = everything()
                                            , names_to = "group"
                                            , values_to = "value"))

  # print a histogram
  print(ggplot2::ggplot(dfp
                        , ggplot2::aes(
                          x = value
                          , fill = group
                        )
  )
  + ggplot2::geom_histogram(bins = 15
                            , alpha = .8
  )
  + ggplot2::theme_bw())

  # print a jitterplot
  print(ggplot2::ggplot(dfp
                        , ggplot2::aes(
                          x = group
                          , y = value
                          , color = group
                        )
  )
  + ggplot2::geom_jitter()
  + ggplot2::theme_bw()
  )

  # output the input values
  print("input")
  print(input)

  # save input values to environment
  assign("input"
         , input
         , envir = globalenv())

  # save generated data to environment
  assign("samples"
         , df1
         , envir = globalenv())
}
