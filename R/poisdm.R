#' Poisson DataMaker: simulates groups of Poisson-distributed count data.
#'
#' @param n sample size
#' @param lambda group mean
#' @param groupnames names of the groups
#' @param plot TRUE or FALSE; generate histogram and jitterplot of output
#'
#' @return a data frame with input values; if plot == T, also a histogram, and a jitterplot
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' poisdm(c(40, 50), c(10, 12), c("group1", "group2"), TRUE)

poisdm <- function(n, lambda, groupnames, plot){
  input <- data.frame(
    groupnames
    , n
    , lambda
  )

  if(length(n) != length(lambda) | length(n) != length(groupnames)){
    print("specify an equal number of sample sizes (n), means (lambda), and group names")
    stop()
  }

  if (!all((lambda >= 0) & (n >= 0))) {
    print("lambda (vector of means) and n (sample size) must be non-negative")
    stop()
  }

  df1 <- lapply(1:length(n)
                , FUN = function(i){stats::rpois(n = n[i]
                                          , lambda = lambda[i]
                )
                }
  )

  df2 <- purrr::map_dfr(
    .x = df1
    , .f = ~ as.data.frame(
      t(.)
    )
  )

  df2 <- as.data.frame(t(df2))

  colnames(df2) <- groupnames

  pielou <- stats::na.omit(
    tidyr::pivot_longer(
      data = df2
      , cols = dplyr::everything()
      , names_to = "group"
      , values_to = "value"))

  if (plot == T) {
    print(ggplot2::ggplot(pielou
                          , ggplot2::aes(
                            x = .data$value
                            , fill = .data$group
                            )
                          )
          + ggplot2::geom_histogram(bins = 15
                                    , alpha = .8
                                    )
          + ggplot2::theme_bw())

    print(ggplot2::ggplot(pielou
                          , ggplot2::aes(
                            x = .data$group
                            , y = .data$value
                            , color = .data$group
                            )
                          )
          + ggplot2::geom_jitter()
          + ggplot2::theme_bw()
    )
    }

  print("input")
  print(input)
  print("generated_data")
  print(pielou)

}
