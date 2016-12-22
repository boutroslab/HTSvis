####Function for help popups
## original function written by Joe Cheng (@jcheng5), slighty edited 


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

