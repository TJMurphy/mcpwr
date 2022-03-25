ordinaldm<- function(scale, k, groups, size, replace, prob){

  if(length(prob) != length(scale)
            | sum(prob) != 1
            | replace !=T
            | length(k) != length(groups)
     )
  {
    print("Number of probabilities doesn't match number of ordinal values, or groups are not equal to k, or replacement is set to False")
    stop()
  }


  list_of_values <- lapply(1:k,
                           FUN = function(c){
                             stats::sample(x = x[c],
                                           size = size[c],
                                           prob = matrix[c])
                           }
                           )

  Data <<- as.data.frame(t(
    purrr::map_dfr(.x = list_of_values, .f = ~ as.data.frame(t(.)))
     ))


  colnames(Data) <- groups


  Inputs <- data.frame( groups = k
                        , replicates = size
                        , replace = T
                        , prob = prob)

  DataFrame <- sample::na.omit(tidyr::pivot_longer(data = Data
                                                    ,cols = everything()
                                                    ,names_to = "Groups"
                                                    ,values_to = "Values"))

return(list(Inputs, Data))

}


#Reprex:
  
ordinaldm(scale = 1:3, k = 2, groups = c("males", "females"), size = 100, replace = T, prob = c(0.2, 0.3, 0.5, 0.5, 0.3, 0.2) )
