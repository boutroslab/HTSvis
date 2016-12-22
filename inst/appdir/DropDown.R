####Function to plot heatmaps with tooltip 
DropDown <- function(identifier,input,toSelect,multiState){
  
  renderUI({
    if(is.null(input)) return()

    selectizeInput(   identifier,
                      label = h6(toSelect),
                      choices = input,
                      multiple = multiState,
                      selected = input[1],
                      width="auto")

      })
}