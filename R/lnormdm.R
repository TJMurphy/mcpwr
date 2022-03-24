#' Lognormal Data Maker : lnormdm
#'
#' A sandbox to create and plot lognormal data.
#' Enter any number of treatment groups, their names, their respective log means, and their log standard deviations.
#' Treatment groups can be uneven sizes.
#' k must correspond with the number of values in names, meanlog, and sdlog.
#'
#' @param k is the number of treatment groups.
#' @param names are the names of the treatment groups.
#' @param n is the size of each treatment group.
#' @param meanlog is the log mean of each treatment group.
#' @param sdlog is the log standard deviation of each treatment group.
#'
#' @return ggplot,data
#' @export
#'
#' @example
#'
#' lnormdm(5, names = c("Control", "Treatment 1", "Treatment 2", "Treatment 3", "Treatment 4"), n= c(30,50,50,50,50), meanlog = c(3,4,3,2,3), sdlog= c(1,1,1,1,1))
#'
#' example above will generate a tibble and ggplot for an experiment with 1 control and 4 treatment groups, with the above sample sizes, log means, and log sds.
#'
#'
#'


lnormdm <- function(k, names=c(...), n= c(...), meanlog= c(...), sdlog= c(...)){


if(k != (length(n)
         +length(meanlog)
         +length(sdlog))/3)
{

  print("Stop, k is incorrect length!")

  stop()
}

parameters <<- list(names=names
                    , n=n
                    , meanlog=meanlog
                    , sdlog=sdlog)


df1 <<- data.frame(n , meanlog , sdlog )

df2 <- apply(df1, 1, function(x) stats::rlnorm(x[1], meanlog=x[2], sdlog=x[3]))

df2 <- purrr::map_dfr(df2, ~as_tibble(t(.)))

df2 <- tibble::as_tibble(t(df2))

df2 <<- as.data.frame(df2)

colnames(df2) <- names

simulateddata <<- df2

simdata <- tidyr::pivot_longer(data=df2, cols= c(1:ncol(df2)))

simdata <- data.frame(simdata)


p <- simdata %>% stats::na.omit() %>% ggplot2::ggplot(simdata, mapping = aes(x=name,
                                                                             y=value,
                                                                             color = name)) +
  ggplot2::geom_boxplot() +
  ggplot2::geom_jitter(alpha=0.4) +
  ggthemes::theme_tufte() +
  ggplot2::ggtitle("Simulated Data") +
  ggplot2::labs(x="Group",y="Value",color="Treatment Groups")


print(p)
print(parameters)
print(simulateddata)

}
