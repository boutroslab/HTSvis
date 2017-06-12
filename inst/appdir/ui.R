source("./helperFunctions.R")
source("./global.R")
#################################################################################################################################
##Start of UI
ui <- function(request) {
  fixedPage(
            theme = "bootstrap.css",
            title = "HTS-Vis",
            HTML('<style type="text/css">
                 .row-fluid { width: 50%; }
                 .well { background-color: #99CCFF; }
                 </style>'),
            HTML('<link rel="stylesheet"
                        type="text/css"
                        href="//fonts.googleapis.com/css?family=Open+Sans" />
                 <link rel="stylesheet" type="text/css" href="button.css" />
                 ') ,


            #initialize shiny js (for html tags in selectize dropdown)
            shinyjs::useShinyjs(),

            ##set font size  for all dropdown lists
            tags$style(   type='text/css',
                          ".selectize-input {
                                        font-size:12px;line-height: 12px;}",
                          ".selectize-dropdown {
                                        font-size:12px;line-height: 12px;}"
            ),
            tags$style(HTML('.selectize-dropdown {
                                    width:250px!important;highlight=T;}')
            ),

            ##supress error messages in app
            tags$style(    type='text/css',
                           ".shiny-output-error{ visibility: hidden;}",
                           ".shiny-output-error:before { visibility:hidden; }"
            ),

            ##custom settings for popTable in scatter plot tab
            tags$head(tags$style(HTML("#popTable tr {
                                        background-color:#ddd!important;}"))),
            tags$head(tags$style(HTML("#popTable th {
                                        display: none!important;}"))),
            tags$head(tags$style(HTML('#popTable {
                                        margin-top:10px;}'))),
            tags$head(tags$style(HTML('#popTable  {
                                        border-collapse:collapse;}'))),



            ##to send error message as pop up
            tags$head(
                tags$script(
                    HTML(
                        'Shiny.addCustomMessageHandler(
                            "jsCode",function(message) {
                                eval(message.value);});')
                    )
                ),


            ##to reset input values upon data change
            tags$head(
                tags$script(
                    HTML(
                        "Shiny.addCustomMessageHandler(
                            'resetValue',function(variableName) {
                                Shiny.onInputChange(variableName, null);});")
                    )
                ),

            ##set window size of help popup to fixed value
            tags$style(    type='text/css',
                           ".popover{width:300px!important;}"
                        ),
            
    HTML("<div style='height: 40px;'>"),
    HTML("</div>"),

            # ##Bind logo as .png
            HTML("<div>"),
            HTML(paste(img(src='logo.png',
                           align = "left",
                           style='height: 80px;',
                           hspace="15",
                           vspace="15"), "")
                 ),
            HTML("</div>"),

            tags$head(tags$style( HTML(' .nav {margin-top:30px;}'))),
    
            ##Tab for Data Input
            tabsetPanel(
                    tabPanel(title = "Home",
                             HTML("<div style='height: 25px;'>"),
                             HTML("</div>"),
                             source("./ui_welcome.R",local=T)[1]),
                    tabPanel("Data Input",
                       HTML("<div style='height: 25px;'>"),
                       HTML("</div>"),
                            source("./ui_dataInput1.R",local=T)[1],

                            source("./ui_dataInput2.R",local=T)[1],
                       
                            source("./ui_dataInput3.R",local=T)[1],

                       HTML("<div style='height: 25px;'>"),
                       HTML("</div>"),

                            source("./ui_dataInput4.R",local=T)[1],

                       HTML("<div style='height: 25px;'>"),
                       HTML("</div>"),

                           source("./ui_dataInput5.R",local=T)[1],

                       HTML("<div style='height: 200px;'>"),
                       HTML("</div>")

                ),

                #Plate Viewer Tab
                tabPanel("Plate Viewer",
                        source("./ui_dummy.R",local=T)[1],
                        source("./ui_plateViewer.R",local=T)[1]
                ),


              #Feature Table Tab
              tabPanel("Feature Table",
                       source("./ui_dummy.R",local=T)[1],
                       source("./ui_featureTable.R",local=T)[1]
              ),



              #Quality Control Tab
              tabPanel("Quality Control",
                       source("./ui_dummy.R",local=T)[1],
                       source("./ui_qualityControl.R",local=T)[1]
              ),


              #Scatter Plot Tab
              tabPanel("Scatter Plot",
                       source("./ui_dummy.R",local=T)[1],
                       source("./ui_scatterPlot.R",local=T)[1]
              ),
              
              #Options Tab
              tabPanel("Options",
                       source("./ui_dummy.R",local=T)[1],
                       source("./ui_options.R",local=T)[1]
              ),

              #Help Tab
              tabPanel("Help Page",
                       source("./ui_helpPage.R",local=T)[1]
              )


        ),  style='width: 1048px; height: auto;' #end of tabsetPanel
    )#end of global fluidPage
}#end of shiny UI
