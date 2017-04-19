conditionalPanel(condition="output.showPanels",         
    HTML("<div style='height: 50px;'>"), 
    HTML("</div>"),
         
    fixedRow(
        column (    3, 
                   align = "left",
                   h6("Define Color Scale")
        ),
        column(    1,
                   helpPopup("Help: Color Scale",
                             id="helpColor",
                             content = "Set a custom color scale for the 
                             plate heatmap below by typing the lower and upper 
                             limits in the boxes. The default color scale 
                             ranges from the minimum to maximum value
                             on each plate. The default color scale can be restored 
                            with the reset button.",
                             iconClass = "fa fa-question")
        ),
       column(     2,
                   offset=1,
                   align="center",
                   helpPopup("Help: Synchronize button",
                             id="helpSync",
                             content = "Click to open the synchronization panel
                             which allows you to synchronize the four plate 
                             heatmaps. 
                            Selections and settings (selected plate 
                            and color scale) made on the upper left plate can be 
                            transferred to the remaining plates by using the 
                            buttons on the panel.
",
                             iconClass = "fa fa-question")
       ),
       column (    3, 
                   offset = 2,
                   h6("Define Color Scale"),
                   align = "right"
       )
    ),
         
    fixedRow(
        column(   1,
                 textInput(    "LoScaleLim1", 
                               label = "",
                               placeholder = "min"),
                 tags$style(
                 type='text/css',
                 "#LoScaleLim1{ font-size:10px;font-style:italic; color:gray}"
                ),
         HTML("<div style='height: 10px;'>"), 
         HTML("</div>"),
                align = "left",
                actionButton('resetScale1',
                             label = "Click to reset color key"),
                             tags$style(
                             type='text/css', "#resetScale1{ font-size:10px}")
                   ),
                   column(   2,
                             align = 'center',
                             plotOutput(   "key1",
                                           height = "50px")  
                   ),
                   column(   1, 
                             textInput(    "HiScaleLim1", 
                                           label = "",
                                           placeholder = "max"),
                             tags$style(
                             type='text/css',
                             "#HiScaleLim1{
                                font-size:10px;font-style:italic; color:gray}")
                   ), 
                   conditionalPanel(condition = "output.showSync",
                                    column(4,
                                           align="center",
                                           actionButton('showPanel',
                                                label = "Click to synchronize"),
                                                tags$style(type='text/css',
                                                "#hidePanel{ font-size:12px;}")
                                    )
                   ),
                   conditionalPanel(condition = "output.hideSync" , 
                                    column(4,
                                           align="center",
                                           actionButton('syncFeature',
                                                label = "Synchronize channel"),
                                                tags$style(type='text/css',
                                                        "#syncFeature{
                                                        font-size:12px;}"),
                                       HTML("<div style='height: 10px;'>"), 
                                       HTML("</div>"),
                                           actionButton('syncPlate',
                                                    label="Synchronize plate"),
                                                    tags$style(
                                                    type='text/css',
                                                        "#syncPlate{
                                                            font-size:12px;}"),
                                       HTML("<div style='height: 10px;'>"), 
                                       HTML("</div>"),
                                           actionButton('syncScreen',
                                             label = "Synchronize experiment"),
                                             tags$style(type='text/css',
                                                        "#syncScreen{
                                                            font-size:12px;}"),
                                       HTML("<div style='height: 10px;'>"), 
                                       HTML("</div>"),
                                           actionButton('syncCol',
                                             label = "Synchronize color scale"),
                                             tags$style(type='text/css',
                                                        "#syncCol{
                                                            font-size:12px;}"),
                                       HTML("<div style='height: 10px;'>"), 
                                       HTML("</div>"),
                                            actionButton('hidePanel',
                                            label="Hide Syncronization panels"),
                                            tags$style(type='text/css',
                                                       "#hidePanel{
                                                            font-size:10px;}")
                                       )
                    ),
                    column(4,
                        fixedRow(
                            column( 3,
                                    textInput("LoScaleLim2", 
                                                label = "",
                                                placeholder = "min"),
                                         tags$style(type='text/css',
                                                    "#LoScaleLim2{
                                                        font-size:10px;
                                                        font-style:italic;
                                                        color:gray}")
                                 ),
                             column(  6, 
                                      align = "center",
                                      plotOutput(   "key2",
                                                    height = "50px")  
                                 ),
                             column( 3,
                                     align = "right",
                                     textInput(      "HiScaleLim2", 
                                                     label = "",
                                                     placeholder = "max"),
                                     tags$style(type='text/css',
                                                "#HiScaleLim2{
                                                    font-size:10px;
                                                    font-style:italic;
                                                    color:gray}")
                                 )
                            ),
                       HTML("<div style='height: 10px;'>"), 
                       HTML("</div>"),
                            fixedRow(
                                 column( 12,
                                         align = "right",
                                         actionButton('resetScale2',
                                             label="Click to reset color key"),
                                         tags$style(type='text/css',
                                                    "#resetScale2{
                                                        font-size:10px}")
                                 )
                           )
                    )
            ),
                
#dropdown lists for plates 1 & 2
    fixedRow(
        column(12,
               helpPopup("Help: Plate Heatmaps",
                         id="helpDropDown",
                         content = "Select a plate from the data set 
                         to be shown as a heatmap. If a single 
                        experiment is investigated, the 'select experiment'
                         option is inactive. The measured value for which
                         the plate heatmap will be shown is defined from
                         the 'select feature' list",
                         iconClass = "fa fa-question"),
               tags$style(type='text/css', "#helpDropDown {margin-top: 35px;}"),
               height="50"
        )
    ),

    fixedRow(
            column( 2,
                    uiOutput("featuresPlate1")
               ),
            column( 2,
                    uiOutput("platesPlate1")
               ),         
            column( 2,        
                    uiOutput("screensPlate1")
               ),
            column( 2,
                    uiOutput("featuresPlate2")
               ),
            column( 2, 
                    uiOutput("platesPlate2")
               ),
            column( 2,
                    uiOutput("screensPlate2")             
            )
    ), 
             
#Heatmaps for plates 1 & 2
    fixedRow(
        column( 6,
                ggvisOutput("heatmap1")), 
                column(  6, 
                         ggvisOutput("heatmap2") 
                        )
     ),  
     HTML("<div style='height: 20px;'>"), 
     HTML("</div>"),
                 
##lower heatmaps
#display settings for plates 3 & 4
    fixedRow( 
            column( 2,
                    uiOutput("featuresPlate3")
           ),
            column( 2,
                    uiOutput("platesPlate3")
           ),
           column( 2,
                   uiOutput("screensPlate3")
           ),
           column( 2,   
                   uiOutput("featuresPlate4")
           ),
           column( 2,             
                   uiOutput("platesPlate4")
           ),
           column( 2, 
                   uiOutput("screensPlate4")
           )
    ),

#Heatmaps for plates 3 & 4
    fixedRow(                              
       column( 6, 
               ggvisOutput("heatmap3") 
       ),
       column( 6, 
               ggvisOutput("heatmap4") 
       )     
    ),

#Color key settings for 3nd Plate
    fixedRow(
       column (  3, 
                 align = "left",
                 h6("Define Color Scale")
       ),
       column (  3, 
                 offset = 6,
                 h6("Define Color Scale"),
                 align = "right"
       )
    ),
    fixedRow(
       column(   1,
                 textInput("LoScaleLim3", 
                            label = "",
                            placeholder = "min"),
                 tags$style(type='text/css',
                                "#LoScaleLim3{
                                    font-size:10px;
                                    font-style:italic;
                                    color:gray}")
        ),
        column(   2,
                 align = 'center',
                 plotOutput(    "key3",
                                height = "50px")
       ),
       column(   1,
                 textInput(     "HiScaleLim3", 
                                label = "",
                                placeholder = "max"),
                 tags$style(type='text/css',
                                "#HiScaleLim3{font-size:10px;
                                    font-style:italic;
                                        color:gray}")
       ),  
       column(   4
        ),
        #Color key settings for 4th Plate
       column( 1,
               textInput("LoScaleLim4", 
                            label = "",
                            placeholder = "min"),
                            tags$style(type='text/css',
                                        "#LoScaleLim4{
                                            font-size:10px;
                                                font-style:italic;
                                                    color:gray}")
       ),
       column( 2, 
               align = "center",
               plotOutput("key4",
                           height = "50px")      
       ),
       column( 1,
               textInput("HiScaleLim4", 
                            label = "",
                            placeholder = "max"),
                            tags$style(type='text/css',
                                       "#HiScaleLim4{
                                            font-size:10px; 
                                                font-style:italic;
                                                    color:gray}"),
                   align = "right"
        ) 
    ),
#Reset buttons for color key for plates 3 & 4
    fixedRow(
        column(   2,
                 align = "left",
                 actionButton( 'resetScale3',
                                label = "Click to reset color key"),
                 tags$style(type='text/css',
                                "#resetScale3{
                                    font-size:10px}")
        ),
        column(   2,
                 offset = 8,
                 align = "right",
                 actionButton( 'resetScale4',
                                label = "Click to reset color key"),
                 tags$style(type='text/css',
                                "#resetScale4{
                                    font-size:10px}")
        )
    ),
    HTML("<div style='height: 50px;'>"), 
    HTML("</div>")
)