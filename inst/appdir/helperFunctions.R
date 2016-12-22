###helper functions
##functions for population table in scatter plot tab 
#shinyInput 
#shinyValue 
shinyInput <- function(FUN, len, id, ...) {
                  inputs <- len
                      for (i in seq(len)) {
                          inputs[i] <- as.character(FUN(id, ...))
                   }
                  return(inputs)
               }

shinyValue <- function(id, len) {
                  unlist(lapply(seq_len(len), function(i) {
                      value <- input[[id]]
                         if (is.null(value)) NA else value
                     }))
                }     