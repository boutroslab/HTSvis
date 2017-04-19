                 fixedPage(
                     HTML("<div style='width:1048px;'>"),
                     conditionalPanel(condition='output.showPanels',
                     fixedRow(
  
                                                
                         column( 3,
                                 style="vertical-align: middle;",
                                 downloadButton( 'downloadParms',                     
                                                 label = h6("click to download")),
                                 tags$style(
                                     type='text/css', 
                                     "#downloadParms { margin-top:25px;
                                     margin-left:-10px;}")
                                 )
        )
),#end of conditional panel 
HTML("</div>")
)#end of fixedPage  