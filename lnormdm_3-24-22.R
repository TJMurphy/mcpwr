#' A sandbox to create and plot lognormal data.
#' Enter any number of treatment groups, their names, their respective log means, and their log standard deviations.
#' Treatment groups can be uneven sizes.
#' k must correspond with the number of values in names, meanlog, and sdlog.
#'
#' @param k a vector of character strings of length k defining the treatment groups.
#' @param names a vector of names defining the treatment groups.
#' @param n a vector of sample sizes of each treatment group.
#' @param meanlog a vector of log means for each treatment group.
#' @param sdlog a vector of log sds for each treatment group.
#'
#' @return ggplot,data
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' lnormdm(k=2,names=c("Ctrl","Treat1"),n=c(30,50),meanlog=c(3,4),sdlog=c(1,1))
#'
#'
#'
lnormdm <- function(k, names, n, meanlog, sdlog){


  if(k != (length(n)
           +length(meanlog)
           +length(sdlog))/3)
  {

    print("Stop, k is incorrect length!")

    stop()
  }


  df1 = data.frame(n , meanlog , sdlog )

  df2 = apply(df1, 1, function(x) stats::rlnorm(x[1], meanlog=x[2], sdlog=x[3]))

  df2 = t(purrr::map_dfr(df2, ~as.data.frame(t(.))))

  df2 = as.data.frame(df2)

  simdata = tidyr::pivot_longer(data=df2, cols= c(1:ncol(df2)))

  simdata = stats::na.omit(simdata)

  P = ggplot2::ggplot(simdata, mapping = ggplot2::aes(x=.data$name,
                                                      y=.data$value,
                                                      color = .data$name)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(alpha=0.4) +
    ggthemes::theme_tufte() +
    ggplot2::ggtitle("Simulated Data") +
    ggplot2::labs(x="Group",y="Value",color="Treatment Groups")


  colnames(df2) = names

  simulateddata = df2

  parameters = list(k=k
                     ,names=names
                     , n=n
                     , meanlog=meanlog
                     , sdlog=sdlog)

  MOP <<- parameters

  return(list(params=parameters,df=simulateddata,dist="lnormdm"))

}



