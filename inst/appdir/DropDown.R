####Function to define drop down lists in server via renderUI 

DropDown <- function(identifier,input,toSelect,multiState,allPlates){

  renderUI({
    if(is.null(input)) return()
      else 
    selectizeInput(   identifier,
                      label = h6(toSelect),
                      choices = input,
                      multiple = multiState,
                      selected = input[1],
                      width="auto")

      })
}
