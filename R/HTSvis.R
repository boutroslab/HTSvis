#' Launch the HTSvis app
#'
#' Executing this function will launch the HTSvis application in
#' the user's default web browser.
#' HTSvis is a shiny application for the visulaization and analysis
#' of data from arrayed high-throughput screens.
#' @author Christian Scheeder \email{c.scheeder@dkfz.de}
#' @author Florian Heigwer \email{f.heigwer@dkfz.de}
#' @author Michael Boutros \email{m.boutros@dkfz.de}
#' @examples
#' \dontrun{
#' HTSvis()
#' }

#' @export

HTSvis <- function(){
    runHTSvis()
    return(invisible())
}


runHTSvis <- function(){
    filename <-  base::system.file("appdir", package = "HTSvis")
    runApp(filename, launch.browser = TRUE)
}
