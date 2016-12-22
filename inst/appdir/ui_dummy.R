conditionalPanel(condition="output.hidePanels",
                 fixedRow(
                   HTML("<div style='height: 300px;'>"), 
                   HTML("</div>"),
                   column( 12,
                           align = "center",
                           h5("Data Input has to be submitted by the 
                              \"Explore Data\" button on the overview panel")
                   )
                 )
)