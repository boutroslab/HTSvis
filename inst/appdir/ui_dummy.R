conditionalPanel(condition="output.hidePanels",
                 fixedRow(
                   HTML("<div style='height: 300px;'>"), 
                   HTML("</div>"),
                   column( 12,
                           align = "center",
                           h5("First, data has to be uploaded and submitted by the \'Explore Data\' button on the \'Data Input\' panel.\n The \'Explore Data\' button will occur once data has been uploaded and specific columns were selected.")
                   )
                 )
)