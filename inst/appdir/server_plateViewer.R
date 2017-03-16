source("./setLimits.R",local=T)

##calculate limits for heatmaps
limits <- reactiveValues()

observe({
    validate(need(input$featuresPlate1, message=FALSE))
    validate(need(input$platesPlate1, message=FALSE))
    validate(need(input$screensPlate1, message=FALSE))

    
    if(!isTRUE(IsSingleExperimentTabs$state)) {
        limits$limits1 <- setLimits(data_lim=feature_table2$data,
                                     lo_scale_lim=input$LoScaleLim1,
                                     hi_scale_lim=input$HiScaleLim1,
                                     curr_feature_lim=input$featuresPlate1,
                                     curr_plate_lim=input$platesPlate1,
                                     curr_screen_lim=input$screensPlate1)
        if(is.finite(limits$limits1[1]) & is.finite(limits$limits1[2])) {
            warnEmpty_js_string <- ''
        return(limits$limits1)
        } else {warnEmpty_js_string <- 'alert("Selected column combination does not exist");'
        session$sendCustomMessage(type='jsCode',
                                  list(value = warnEmpty_js_string ))
        return(NULL)}
    } else {
        limits$limits1 <- setLimits(data_lim=feature_table2$data,
                                     lo_scale_lim=input$LoScaleLim1,
                                     hi_scale_lim=input$HiScaleLim1,
                                     curr_feature_lim=input$featuresPlate1,
                                     curr_plate_lim=input$platesPlate1,
                                     curr_screen_lim=F)
        if(is.finite(limits$limits1[1]) & is.finite(limits$limits1[2])) {
            warnEmpty_js_string <- ''
            return(limits$limits1)
        } else {warnEmpty_js_string <- 'alert("Selected column combination does not exist");'
        session$sendCustomMessage(type='jsCode',
                                  list(value = warnEmpty_js_string ))
        return(NULL)}
    }
})


observe({
    validate(need(input$featuresPlate2, message=FALSE))
    validate(need(input$platesPlate2, message=FALSE))
    validate(need(input$screensPlate2, message=FALSE))

    if(!isTRUE(IsSingleExperimentTabs$state)) {
        limits$limits2 <- setLimits(data_lim=feature_table2$data,
                                     lo_scale_lim=input$LoScaleLim2,
                                     hi_scale_lim=input$HiScaleLim2,
                                     curr_feature_lim=input$featuresPlate2,
                                     curr_plate_lim=input$platesPlate2,
                                     curr_screen_lim=input$screensPlate2)
        return(limits$limits2)
    } else {
        limits$limits2 <- setLimits(data_lim=feature_table2$data,
                                     lo_scale_lim=input$LoScaleLim2,
                                     hi_scale_lim=input$HiScaleLim2,
                                     curr_feature_lim=input$featuresPlate2,
                                     curr_plate_lim=input$platesPlate2,
                                     curr_screen_lim=F)
        return(limits$limits2)
    }

})


observe({
    validate(need(input$featuresPlate3, message=FALSE))
    validate(need(input$platesPlate3, message=FALSE))
    validate(need(input$screensPlate3, message=FALSE))

    if(!isTRUE(IsSingleExperimentTabs$state)) {
        limits$limits3 <- setLimits(data_lim=feature_table2$data,
                                     lo_scale_lim=input$LoScaleLim3,
                                     hi_scale_lim=input$HiScaleLim3,
                                     curr_feature_lim=input$featuresPlate3,
                                     curr_plate_lim=input$platesPlate3,
                                     curr_screen_lim=input$screensPlate3)
        return(limits$limits3)
    }else{
        limits$limits3 <- setLimits(data_lim=feature_table2$data,
                                     lo_scale_lim=input$LoScaleLim3,
                                     hi_scale_lim=input$HiScaleLim3,
                                     curr_feature_lim=input$featuresPlate3,
                                     curr_plate_lim=input$platesPlate3,
                                     curr_screen_lim=F)
        return(limits$limits3)
    }
})


observe({
    validate(need(input$featuresPlate4, message=FALSE))
    validate(need(input$platesPlate4, message=FALSE))
    validate(need(input$screensPlate4, message=FALSE))

    if(!isTRUE(IsSingleExperimentTabs$state)) {
        limits$limits4 <- setLimits(data=feature_table2$data,
                                     lo_scale_lim=input$LoScaleLim4,
                                     hi_scale_lim=input$HiScaleLim4,
                                     curr_feature_lim=input$featuresPlate4,
                                     curr_plate_lim=input$platesPlate4,
                                     curr_screen_lim=input$screensPlate4)
        return(limits$limits4)
    } else {
        limits$limits4 <- setLimits(data=feature_table2$data,
                                     lo_scale_lim=input$LoScaleLim4,
                                     hi_scale_lim=input$HiScaleLim4,
                                     curr_feature_lim=input$featuresPlate4,
                                     curr_plate_lim=input$platesPlate4,
                                     curr_screen_lim=F)
        return(limits$limits4)
    }
})




##plot color keys for heatmaps (function plotKey is sourced in header)

observe({
    output$key1 <- renderPlot({
        validate(need(input$featuresPlate1, message=FALSE))
        validate(need(input$platesPlate1, message=FALSE))
        validate(need(input$screensPlate1, message=FALSE))

        plotKey(col_palette = col_palette, limits = limits$limits1)
    }, width = 150, height = 50)
})


observe({
    output$key2 <- renderPlot({
        validate(need(input$featuresPlate2, message=FALSE))
        validate(need(input$platesPlate2, message=FALSE))
        validate(need(input$screensPlate2, message=FALSE))

        plotKey(col_palette,limits$limits2)
    }, width = 150, height = 50)
})


observe({
    output$key3 <- renderPlot({
        validate(need(input$featuresPlate3, message=FALSE))
        validate(need(input$platesPlate3, message=FALSE))
        validate(need(input$screensPlate3, message=FALSE))

        plotKey(col_palette,limits$limits3)
    }, width = 150, height = 50)
})


observe({
    output$key4 <- renderPlot({
        validate(need(input$featuresPlate4, message=FALSE))
        validate(need(input$platesPlate4, message=FALSE))
        validate(need(input$screensPlate4, message=FALSE))

        plotKey(col_palette,limits$limits4)
    }, width = 150, height = 50)
})




##Calculate heatmaps (function plotHeatmap is sourced in header)
#1st heatmap

df1 <- reactive({
    validate(need(input$featuresPlate1, message=FALSE))
    validate(need(input$platesPlate1, message=FALSE))
    validate(need(input$screensPlate1, message=FALSE))

    if(!isTRUE(IsSingleExperimentTabs$state)) {
        plotHeatmap(data_table=feature_table2$data,
                           limits=limits$limits1,
                           curr_plate=input$platesPlate1,
                           curr_screen=input$screensPlate1,
                           curr_feature=input$featuresPlate1,
                           plateDim=TabDimensions$plate,
                           expDim=TabDimensions$experiment,
                           wellDim=TabDimensions$well,
                           annoDim=TabDimensions$annotation)
        } else {
            plotHeatmap(data_table=feature_table2$data,
                               limits=limits$limits1,
                               curr_plate=input$platesPlate1,
                               curr_screen=F,
                               curr_feature=input$featuresPlate1,
                               plateDim=TabDimensions$plate,
                               expDim=TabDimensions$experiment,
                               wellDim=TabDimensions$well,
                               annoDim=TabDimensions$annotation)
    }
})


hover_df1 <-    function(x) {
    if(is.null(x)) return(NULL)
    if(TabDimensions$well == TabDimensions$annotation) {
        paste(df1()[df1()$CSidB110 == x$CSidB110,TabDimensions$well],
                    df1()[df1()$CSidB110 == x$CSidB110,"value" ],
                    sep= "<br />")
        } else {
            paste(df1()[df1()$CSidB110 == x$CSidB110,TabDimensions$well],
                  df1()[df1()$CSidB110 == x$CSidB110,"value" ],
                  df1()[df1()$CSidB110 == x$CSidB110,TabDimensions$annotation],
                  sep= "<br />")
            }
}

testInputDf1 <- reactiveValues(state=F)

observe({
    validate(need(input$featuresPlate1, message=FALSE))
    validate(need(input$platesPlate1, message=FALSE))
    validate(need(input$screensPlate1, message=FALSE))
    testInputDf1$state = T
})

observe({
    if(isTRUE(testInputDf1$state)){
        df1 %>%
            ggvis(~column, ~row ,
                  fill:=~def.color,
                  stroke := "white",
                  key :=~CSidB110)%>%
                add_tooltip(hover_df1, "hover")%>%
                    layer_rects(width = band(), height = band())%>%
                        scale_nominal("x", padding = 0, points = FALSE)%>%
                            scale_nominal("y", padding = 0, points = FALSE)%>%
                                add_axis("x",
                                         title = "",
                                         tick_size_major = 0,
                                         properties = axis_props(
                                             axis = list(stroke="white",
                                                         strokeWidth=0),
                                             grid = list(strokeWidth=0))) %>%
                                    add_axis("y",
                                             title = "",
                                             tick_size_major = 0,
                                             properties = axis_props(
                                                 axis = list(stroke="white",
                                                             strokeWidth=0),
                                                 grid = list(strokeWidth=0)))%>%
                                        set_options(height=333, width=500) %>%
                                            bind_shiny("heatmap1")

    }
})


## 2nd heatmap
df2 <- reactive({
    validate(need(input$featuresPlate2, message=FALSE))
    validate(need(input$platesPlate2, message=FALSE))
    validate(need(input$screensPlate2, message=FALSE))


    if(!isTRUE(IsSingleExperimentTabs$state)) {
        plotHeatmap(data_table=feature_table2$data,
                           limits=limits$limits2,
                           curr_plate=input$platesPlate2,
                           curr_screen=input$screensPlate2,
                           curr_feature=input$featuresPlate2,
                           plateDim=TabDimensions$plate,
                           expDim=TabDimensions$experiment,
                           wellDim=TabDimensions$well,
                           annoDim=TabDimensions$annotation)
    }else{
        plotHeatmap(data_table=feature_table2$data,
                           limits=limits$limits2,
                           curr_plate=input$platesPlate2,
                           curr_screen=F,
                           curr_feature=input$featuresPlate2,
                           plateDim=TabDimensions$plate,
                           expDim=TabDimensions$experiment,
                           wellDim=TabDimensions$well,
                           annoDim=TabDimensions$annotation)
    }
})


hover_df2 <- function(x) {
    if(is.null(x)) return(NULL)
    if(TabDimensions$well == TabDimensions$annotation) {
        paste(df2()[df2()$CSidB110 == x$CSidB110,TabDimensions$well],
              df2()[df2()$CSidB110 == x$CSidB110,"value" ],
              sep="<br />")
    } else {
        paste(df2()[df2()$CSidB110 == x$CSidB110,TabDimensions$well],
              df2()[df2()$CSidB110 == x$CSidB110,"value" ],
              df2()[df2()$CSidB110 == x$CSidB110,TabDimensions$annotation],
              sep="<br />") }
}

testInputDf2 <- reactiveValues(state=F)

observe({
    validate(need(input$featuresPlate2, message=FALSE))
    validate(need(input$platesPlate2, message=FALSE))
    validate(need(input$screensPlate2, message=FALSE))

    testInputDf2$state=T
})

observe({
    if(isTRUE(testInputDf2$state)){
        df2 %>%
            ggvis(~column,
                  ~row,
                  fill:=~def.color,
                  stroke := "white",
                  key :=~CSidB110)%>%
                add_tooltip(hover_df2, "hover")%>%
                layer_rects(width=band(), height=band())%>%
                    scale_nominal("x", padding=0, points=FALSE)%>%
                        scale_nominal("y", padding=0, points=FALSE)%>%
                            add_axis("x",
                                     title="",
                                     tick_size_major = 0,
                                     properties=axis_props(
                                         axis=list(stroke="white",
                                                   strokeWidth=0),
                                         grid=list(strokeWidth=0))) %>%
                            add_axis("y",
                                     title="",
                                     tick_size_major=0,
                                     properties=axis_props
                                        (axis = list(stroke="white",
                                                     strokeWidth=0),
                                            grid = list(strokeWidth=0))) %>%
                                set_options(height=333, width=500) %>%
                                    bind_shiny("heatmap2")

    }
})



## 3rd heatmap

df3 <- reactive({
    validate(need(input$featuresPlate3, message=FALSE))
    validate(need(input$platesPlate3, message=FALSE))
    validate(need(input$screensPlate3, message=FALSE))

    if(!isTRUE(IsSingleExperimentTabs$state)) {
        plotHeatmap(data_table=feature_table2$data,
                           limits=limits$limits3,
                           curr_plate=input$platesPlate3,
                           curr_screen=input$screensPlate3,
                           curr_feature=input$featuresPlate3,
                           plateDim=TabDimensions$plate,
                           expDim=TabDimensions$experiment,
                           wellDim=TabDimensions$well,
                           annoDim=TabDimensions$annotation)
        } else {
            plotHeatmap(data_table=feature_table2$data,
                               limits=limits$limits3,
                               curr_plate=input$platesPlate3,
                               curr_screen=F,
                               curr_feature=input$featuresPlate3,
                               plateDim=TabDimensions$plate,
                               expDim=TabDimensions$experiment,
                               wellDim=TabDimensions$well,
                               annoDim=TabDimensions$annotation)
    }
})


hover_df3 <- function(x) {
    if(is.null(x)) return(NULL)
    if(TabDimensions$well == TabDimensions$annotation) {
        paste(df3()[df3()$CSidB110 == x$CSidB110,TabDimensions$well],
              df3()[df3()$CSidB110 == x$CSidB110,"value" ],
              sep="<br />")
    } else {
        paste(df3()[df3()$CSidB110 == x$CSidB110,TabDimensions$well],
              df3()[df3()$CSidB110 == x$CSidB110,"value" ],
              df3()[df3()$CSidB110 == x$CSidB110,TabDimensions$annotation],
              sep="<br />") }
}


testInputDf3 <- reactiveValues(state=F)

observe({
    validate(need(input$featuresPlate3, message=FALSE))
    validate(need(input$platesPlate3, message=FALSE))
    validate(need(input$screensPlate3, message=FALSE))
    testInputDf3$state = T
})

observe({
    if(isTRUE(testInputDf3$state)){

        df3 %>%
            ggvis(~column,
                  ~row,
                  fill:=~def.color,
                  stroke := "white",
                  key :=~CSidB110)%>%
                add_tooltip(hover_df3, "hover")%>%
                    layer_rects(width=band(), height=band())%>%
                        scale_nominal("x", padding=0, points=FALSE) %>%
                            scale_nominal("y", padding=0, points=FALSE) %>%
                                add_axis("x",
                                         title="",
                                         tick_size_major=0,
                                         properties=axis_props(
                                             axis=list(stroke="white",
                                                       strokeWidth=0),
                                             grid = list(strokeWidth=0)))%>%
                                add_axis("y",
                                         title="",
                                         tick_size_major=0,
                                         properties=axis_props(
                                             axis = list(stroke="white",
                                                         strokeWidth=0),
                                             grid = list(strokeWidth=0)))%>%
                                    set_options(height=333, width =500)%>%
                                        bind_shiny("heatmap3")
    }
})


## 4th heatmap

df4 <- reactive({
    validate(need(input$featuresPlate4, message=FALSE))
    validate(need(input$platesPlate4, message=FALSE))
    validate(need(input$screensPlate4, message=FALSE))


    if(!isTRUE(IsSingleExperimentTabs$state)) {
        plotHeatmap(data_table=feature_table2$data,
                           limits=limits$limits4,
                           curr_plate=input$platesPlate4,
                           curr_screen=input$screensPlate4,
                           curr_feature=input$featuresPlate4,
                           plateDim=TabDimensions$plate,
                           expDim=TabDimensions$experiment,
                           wellDim=TabDimensions$well,
                           annoDim=TabDimensions$annotation)
        } else {
            plotHeatmap(data_table=feature_table2$data,
                               limits=limits$limits4,
                               curr_plate=input$platesPlate4,
                               curr_screen=F,
                               curr_feature=input$featuresPlate4,
                               plateDim=TabDimensions$plate,
                               expDim=TabDimensions$experiment,
                               wellDim=TabDimensions$well,
                               annoDim=TabDimensions$annotation)
    }
})


hover_df4 <- function(x) {
    if(is.null(x)) return(NULL)
    if(TabDimensions$well == TabDimensions$annotation) {
        paste(df4()[df4()$CSidB110 == x$CSidB110,TabDimensions$well],
              df4()[df4()$CSidB110 == x$CSidB110,"value" ],
              sep="<br />")
    } else {
        paste(df4()[df4()$CSidB110 == x$CSidB110,TabDimensions$well],
              df4()[df4()$CSidB110 == x$CSidB110,"value" ],
              df4()[df4()$CSidB110 == x$CSidB110,TabDimensions$annotation],
              sep="<br />") }
}
testInputDf4 <- reactiveValues(state=F)

observe({
    validate(need(input$featuresPlate4, message=FALSE))
    validate(need(input$platesPlate4, message=FALSE))
    validate(need(input$screensPlate4, message=FALSE))
    testInputDf4$state = T
})

observe({
    if(isTRUE(testInputDf4$state)){

        df4 %>%
            ggvis(~column,
                  ~row,
                  fill:=~def.color,
                  stroke := "white",
                  key :=~CSidB110)%>%
                add_tooltip(hover_df4, "hover") %>%
                    layer_rects(width=band(), height=band()) %>%
                        scale_nominal("x", padding=0, points=FALSE) %>%
                            scale_nominal("y", padding=0, points=FALSE) %>%
                                add_axis("x",
                                         title="",
                                         tick_size_major=0,
                                         properties=axis_props(
                                             axis=list(stroke="white",
                                                       strokeWidth=0),
                                             grid=list(strokeWidth=0))) %>%
                                add_axis("y",
                                         title="",
                                         tick_size_major=0,
                                         properties=axis_props(
                                             axis=list(stroke="white",
                                                       strokeWidth=0),
                                             grid=list(strokeWidth=0))) %>%
                                    set_options(height=333, width=500) %>%
                                        bind_shiny("heatmap4")
    }
})


##synchronization option for plateviewer
#conditional panel control for synchronozation buttons
#value "show" is assigned to reactive variable (showSync) if action button is clicked to switch conditional panel
#reverse conditions with (hideSync)
#outputOptions() required because shiny suspends all output reactives if they're not shown in the UI
#dynamic min and max values of plates are calculated with function "setLimits.R" defined in global.R

output$showSync <- reactive({
    if(input$showPanel == 0) {
        return(TRUE)
    } else {
        if(input$showPanel == input$hidePanel) {
            return(TRUE)
            }
    }
})
outputOptions(output, "showSync", suspendWhenHidden=FALSE)

output$hideSync <- reactive({
    if(input$hidePanel != input$showPanel) {
        return(TRUE)
    }
})
outputOptions(output, "hideSync", suspendWhenHidden=FALSE)


observe({
    if(input$resetScale1)
        updateTextInput(session,"LoScaleLim1",value ="")
    updateTextInput(session,"HiScaleLim1",value ="")
})

observe({
    if(input$resetScale2)
        updateTextInput(session,"LoScaleLim2",value ="")
    updateTextInput(session,"HiScaleLim2",value ="")
})

observe({
    if(input$syncCol) {
        validate(need(input$featuresPlate1, message=FALSE))
            isolate(updateTextInput(session,
                                    "LoScaleLim2",
                                    value=limits$limits1[1]))
                isolate(updateTextInput(session,
                                        "HiScaleLim2",
                                        value=limits$limits1[2])
                        )
                }
})


observe({
    if(input$resetScale3)
        updateTextInput(session,"LoScaleLim3",value="")
            updateTextInput(session,"HiScaleLim3",value="")
})

observe({
    if(input$syncCol) {
        validate(need(input$featuresPlate1, message=FALSE))
            isolate(updateTextInput(session,
                                    "LoScaleLim3",
                                    value=limits$limits1[1]))
                isolate(updateTextInput(session,
                                        "HiScaleLim3",
                                        value=limits$limits1[2])
                        )
                }
})

observe({
    if(input$resetScale4)
        updateTextInput(session,"LoScaleLim4",value="")
            updateTextInput(session,"HiScaleLim4",value="")
})

observe({
    if(input$syncCol) {
        validate(need(input$featuresPlate1, message=FALSE))
            isolate(updateTextInput(session,
                                    "LoScaleLim4",
                                    value=limits$limits1[1]))
                isolate(updateTextInput(session,
                                        "HiScaleLim4",
                                        value=limits$limits1[2])
                        )
                }
})

observe({
    if(input$syncFeature)
        isolate(updateSelectInput(session,
                                  "featuresPlate2",
                                  selected=input$featuresPlate1))
            isolate(updateSelectInput(session,
                                      "featuresPlate3",
                                      selected=input$featuresPlate1))
                isolate(updateSelectInput(session,
                                          "featuresPlate4",
                                          selected=input$featuresPlate1))

    if(input$syncPlate)
        isolate(updateSelectInput(session,
                                  "platesPlate2",
                                  selected=input$platesPlate1))
            isolate(updateSelectInput(session,
                                      "platesPlate3",
                                      selected=input$platesPlate1))
                isolate(updateSelectInput(session,
                                          "platesPlate4",
                                          selected=input$platesPlate1))

    if(input$syncScreen)
        isolate(updateSelectInput(session,
                                  "screensPlate2",
                                  selected=input$screensPlate1))
            isolate(updateSelectInput(session,
                                      "screensPlate3",
                                      selected=input$screensPlate1))
                isolate(updateSelectInput(session,
                                          "screensPlate4",
                                          selected=input$screensPlate1))
})
