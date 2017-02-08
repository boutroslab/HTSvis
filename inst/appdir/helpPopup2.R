##Function for help popups
#modified from:
# Title: Client data and query string
# URK: https://gist.github.com/RAPLER/a84f5d3896356c5a09eb
# License: MIT
# Author: Winston Chang <winston@rstudio.com>
#     AuthorUrl: http://www.rstudio.com/



helpPopup <- function(title, content,id,
                      placement='right',
                      trigger='hover',
                      iconClass) {
  

  
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { 
                    $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#",class = "btn btn-mini", `data-toggle` = "popover", id = id,
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      
      tags$i(class=iconClass)
    )
  )
}

