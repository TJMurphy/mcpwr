ordinaldm<- function(scale, k, size, replace, prob){

  if(length(prob) != length(scale)
            | sum(prob) != 1
            | replace !=T
     )
  {
    print("Number of probabilities doesn't match number of ordinal values, or replacement is set to False")
    stop()
  }

  list_of_values <- lapply(1:k,
                           FUN = function(c){
                             stats::sample(
                               x=c(#is ordinal scale always integers from 1- something?#)
                               ,size= size[c]
                               ,prob= probc(1:length(x))
                             )
                           }
  )

  Data <<- as.data.frame(t(
    purrr::map_dfr(.x = list_of_values, .f = ~ as.data.frame(t(.)))
     ))


  colnames(Data) <- values


  Inputs <<- data.frame( groups = values
                        , replicates = size
                        , prob = prob)

  DataFrame <- sample::na.omit(tidyr::pivot_longer(data = Data
                                                    ,cols = everything()
                                                    ,names_to = "Groups"
                                                    ,values_to = "Value"))

return(list(Inputs, Data))

}


Reprex: ordinaldm(scale = 1:10, k = 2, size = 100, replace = T, prob = c(0.1, 0.4, 0.08, 0.02, 0.05, 0.15, 0.03, 0.05, 0.06, 0.06))
