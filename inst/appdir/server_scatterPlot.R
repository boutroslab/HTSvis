####Scatter Plott##############################################################


#create dataframe for scatter plot
observe({
    validate(need(input$feature_selection_sp_x, message=FALSE))
    validate(need(input$feature_selection_sp_y, message=FALSE))
    validate(need(input$screens_selection_sp_x, message=FALSE))
    validate(need(input$screens_selection_sp_y, message=FALSE))


    updateSelectizeInput(session,
        'GenesToColor',
            choices=feature_table2$data %>%
                select_(TabDimensions$annotation) %>%
                    filter_(lazyeval::interp(quote(!is.na(x)),
                                x=as.name(TabDimensions$annotation))) %>%
                        distinct() %>% unlist(use.names = F),

            selected=feature_table2$data  %>%
                select_(TabDimensions$annotation) %>%
                    filter_(lazyeval::interp(quote(!is.na(x)),
                                x=as.name(TabDimensions$annotation))) %>%
                        distinct() %>% unlist(use.names = F) %>% head(,n=1),
           server = TRUE,
           shinyjs::html("labelGTC", "Color by ID")
        )
})



#final dataframe which will be handed over to scatter plot function
featureFrame <- reactive({
    validate(need(input$feature_selection_sp_x, message=FALSE))
    validate(need(input$feature_selection_sp_y, message=FALSE))
    validate(need(input$screens_selection_sp_x, message=FALSE))
    validate(need(input$screens_selection_sp_y, message=FALSE))

    if(!isTRUE(IsSingleExperimentTabs$state)) {
        df_featureFrame <-  data.frame(
            feature_table2$data %>%
                filter_(lazyeval::interp(quote(x == y),
                                         x=as.name(TabDimensions$experiment),
                                         y=input$screens_selection_sp_x)) %>%
                    select_(input$feature_selection_sp_x),

            feature_table2$data %>%
                filter_(lazyeval::interp(quote(x == y),
                                         x=as.name(TabDimensions$experiment),
                                         y=input$screens_selection_sp_y)) %>%
            select_(input$feature_selection_sp_y)
        )  %>% drop_na_(c(input$feature_selection_sp_x,input$feature_selection_sp_y))

       if(nrow(df_featureFrame) > 0) {
       warnEmpty_js_string <- ''
        row.names(df_featureFrame) <- seq(1:nrow(df_featureFrame))
       return(df_featureFrame)
       } else {
           warnEmpty_js_string <- 'alert("Selected column combination does not exist");'
           session$sendCustomMessage(type='jsCode',
                                     list(value = warnEmpty_js_string ))
           return(NULL)
           }

    } else {
        df_featureFrame <- data.frame(
                feature_table2$data %>%
                    filter_(lazyeval::interp(quote(x == y),
                                             x=as.name(TabDimensions$plate),
                                             y=input$screens_selection_sp_x))%>%
                        select_(input$feature_selection_sp_x),

                feature_table2$data %>%
                    filter_(lazyeval::interp(quote(x == y),
                                             x=as.name(TabDimensions$plate),
                                             y=input$screens_selection_sp_y))%>%
            select_(input$feature_selection_sp_y)
    ) %>% drop_na_(c(input$feature_selection_sp_x,input$feature_selection_sp_y))

    if(nrow(df_featureFrame) > 0) {
        row.names(df_featureFrame) <- seq(1:nrow(df_featureFrame))
        return(df_featureFrame)
            }else{(return(NULL))}
  }
})



#create dataframe from feature table to map the brushed data points back
#(IMPORTANT: same subsetting as for featureFrame have to be performed)
feature_table_scatter <- reactive({

    validate(need(input$screens_selection_sp_x, message=FALSE))
    validate(need(input$screens_selection_sp_y, message=FALSE))

    if(!isTRUE(IsSingleExperimentTabs$state)) {
        df_ft_scatter <- data.frame(
            feature_table2$data %>%
                filter_(lazyeval::interp(quote(x == y),
                                         x=as.name(TabDimensions$experiment),
                                         y=input$screens_selection_sp_x)),

            feature_table2$data %>%
                filter_(lazyeval::interp(quote(x == y),
                                         x=as.name(TabDimensions$experiment),
                                         y=input$screens_selection_sp_y))
        ) %>% drop_na_(c(input$feature_selection_sp_x,input$feature_selection_sp_y)) %>%
            select_(TabDimensions$annotation,
                    TabDimensions$well,
                    TabDimensions$plate,
                    TabDimensions$experiment)
        if(nrow(df_ft_scatter)>0) {
        row.names(df_ft_scatter) <- seq(1:nrow(df_ft_scatter))
        return(df_ft_scatter)
        } else {return(NULL)}
  } else {
      df_ft_scatter <-   data.frame (
                feature_table2$data %>%
                    filter_(lazyeval::interp(quote(x == y),
                                             x=as.name(TabDimensions$plate),
                                             y=input$screens_selection_sp_x)),

                feature_table2$data %>%
                    filter_(lazyeval::interp(quote(x == y),
                                             x=as.name(TabDimensions$plate),
                                             y=input$screens_selection_sp_y))
                ) %>% drop_na_(c(input$feature_selection_sp_x,input$feature_selection_sp_y)) %>%
                  select_(TabDimensions$annotation,
                          TabDimensions$well,
                          TabDimensions$plate)
      if(nrow(df_ft_scatter)>0) {
          row.names(df_ft_scatter) <- seq(1:nrow(df_ft_scatter))
          return(df_ft_scatter)
      } else {return(NULL)}
    }
})



#define reactive value variable (i.e. a reactive list)
brush_container <- reactiveValues()
pop_table <- reactiveValues(data = data.frame())


#control of conditional panels for text info messages
#observer is a reactive values object defined in server_launchApp.R


observeEvent(input$plotSP_brush,{
  observer$click <- input$plotSP_brush$ymin
})


observe(
  if(is.null(input$plotSP_brush$ymin)) {
    observer$click <- NULL
  }
)


observeEvent(input$plotSP_dblclick,{
  observer$click <- NULL
  observer$click2 <- NULL
})


observeEvent(input$Zoomer,{
  observer$click <- NULL
  observer$click2 <- input$plotSP_brush$ymin
})


output$showZoomer <- reactive({
  if(is.null(observer$click) ) {return("")} else {
    return ("show")}
})
outputOptions(output, "showZoomer", suspendWhenHidden=FALSE)


output$showDoubleClick <- reactive({
  if(is.null(observer$click2) ) {return("")}
  else {return("show")}
})
outputOptions(output, "showDoubleClick", suspendWhenHidden=FALSE)


output$startMessage <- reactive({
  if(is.null(observer$click) &  is.null(observer$click2)) {
      return("show")
      } else {
            return ("")
        }
})
outputOptions(output, "startMessage", suspendWhenHidden=FALSE)


firstTime <- reactiveValues()
observe(
  if(!is.null(input$plotSP_brush$xmax)) {
    firstTime$state <- isolate(input$plotSP_brush$xmax)
    }
)

output$controlSB <- reactive({
  if(!is.null(firstTime$state)) {return("show")} else{return("")}
})
outputOptions(output, "controlSB", suspendWhenHidden=FALSE)


#control of conditional panels for dropdown lists (plot and population manager)
#showPopMan is a reactiveValues object defined in server_lauchApp.R


observeEvent(input$plotSP_brush,{
  showPopMan$state <- input$plotSP_brush$ymin
})


output$showPopMan <- reactive({
  if(is.null(showPopMan$state) |
     input$popman == F |
     !is.null(input$plotSP_dblclick)) {
      return("")
  } else {
          return("show")
      }
})
outputOptions(output, "showPopMan", suspendWhenHidden=FALSE)

output$hidePopMan <- reactive({
  if(is.null(showPopMan$state)|
     input$popman == F |
     !is.null(input$plotSP_dblclick)) {
      return("show")
  } else {
          return("")
      }
})
outputOptions(output, "hidePopMan", suspendWhenHidden=FALSE)





testGeneTable <- reactiveValues(state=NULL)
observe({
  if(is.null(nrow(geneTable$data))){
      testGeneTable$state=NULL
  } else {
    if(nrow(geneTable$data) == 0){
        testGeneTable$state=NULL
    } else {
        testGeneTable$state=1
        }
      }
})


#control of gene table visibility
showGeneTable <- reactiveValues()

output$showGeneTable <- reactive({
  if(is.null(testGeneTable$state)) {
      return("")
  } else {
      return("show")
          }
})
outputOptions(output, "showGeneTable", suspendWhenHidden=FALSE)

observe({
  if(is.null(testGeneTable$state)) {
      return("hide")
  } else {
          return("show")
      }
})


# zoom function with reactive x/z axis limits controlled by brus and double clicks
lims <- reactiveValues(xlims=NULL,
                       ylims=NULL)

observeEvent(input$Zoomer,{
  brush = input$plotSP_brush
  lims$xlims <-  as.vector(c(brush$xmin,brush$xmax))
  lims$ylims <-  as.vector(c(brush$ymin,brush$ymax))
})

observeEvent(input$plotSP_dblclick,{
  lims$xlims <-  NULL
  lims$ylims <-  NULL
})





##scatter plot

#scatter plot has to be stored as a function to enable the download
#default coloring of scatter plot, all points black
#dependency on observeEvent(input$makeSubpopulation... (see below) to brush colored points
#data frame is ordered to render smallest population last

#downlaod function for scatter plot
output$downloadPlot <- downloadHandler(
  filename = function() {
    paste(input$fileNamePlot,input$fileFormatPlot,sep="")
  },
  content = function(file) {
    if(input$fileFormatPlot %in% ".png"){
      ggsave(filename=file,
             plot=scatter_plot(df=featureFrame()),
             device="png")
    }else{
      if(input$fileFormatPlot %in% ".tiff"){
        ggsave(filename=file,
               plot=scatter_plot(df=featureFrame()),
               device="tiff")
      }else{
        if(input$fileFormatPlot %in% ".jpeg"){
          ggsave(filename=file,
                 plot=scatter_plot(df=featureFrame()),
                 device="jpeg")
        } else {
            if(input$fileFormatPlot %in% ".pdf"){
                ggsave(filename=file,
                       plot=scatter_plot(df=featureFrame()),
                       device="pdf")
            }
        }
      }
    }
  }
)#end of download function


popCounter <- reactiveValues(count=0)

observeEvent(input$makeSubpopulation,{
    popCounter$count=popCounter$count+1
})

observeEvent(input$ColorGenes,{
    popCounter$count=popCounter$count+1
})



observe({
  inter_counter <-  popCounter$count
  tail <- as.numeric(substr(inter_counter,
                            nchar(inter_counter),
                            nchar(inter_counter)))
  if(length(inter_counter) > 0) {
    if(tail != 0) {
      popCounter$count <- tail
    } else {
        return(10)
        }
  }
})







#action button to create Population from brushed Points
#color and name of population are defined by user in app)
#row indices from brush are accesible via input$plot_brush
observeEvent(input$makeSubpopulation,{
    populationName <- isolate(input$namePopulation)

    if(length(input$plotSP_brush)  == 0) {
        warnPop <- paste("No population defined - please define a population",
                         "by drwaing a rectangle on the plot",
                         sep=" ")
            warnPop_js_string <- 'alert("SOMETHING");'
                warnPop_js_string <- sub("SOMETHING",warnPop,warnPop_js_string )
                    session$sendCustomMessage(type='jsCode',
                                              list(value = warnPop_js_string ))
    } else if (nchar(input$namePopulation) ==  0) {
        populationName <-  "Population_1"
            row_ind <-
              isolate(
                as.numeric(
                  row.names(
                    brushedPoints(featureFrame(),
                                  brush=input$plotSP_brush)
          )
        )
      )

    brush_container[[as.character(isolate(populationName))]] <-
      list(isolate(as.character(populationName)),
           isolate(row_ind),
           2,
           isolate(popCounter$count),
           isolate(population_colors[popCounter$count])
           )
    } else {
        row_ind <-
          isolate(
            as.numeric(
              row.names(
                brushedPoints(featureFrame(),
                              brush=input$plotSP_brush)
              )
            )
          )

    brush_container[[as.character(isolate(populationName))]] <-
      list(isolate(as.character(populationName)),
           isolate(row_ind),
           2,
           isolate(popCounter$count),
           isolate(population_colors[popCounter$count]))
    print(brush_container[[as.character(isolate(populationName))]])
    }
})


#action button to delete a population
observeEvent(input$delete_button, {
  popToDelete <- gsub(pattern = "@.*",
                      replacement = "",
                      x = input$delete_button)
  brush_container[[popToDelete]] <- NULL
  testCheckbox[[popToDelete]] <- "off"
  geneTable$data <- data.frame()
})


#make population via "color by id" 
observeEvent(input$ColorGenes,{
    if(!isTRUE(any(
        feature_table_scatter() %>%
            select_(TabDimensions$annotation) %>%
                unlist(use.names=F) %in% input$GenesToColor
            ))
    ) {
        warnGenes_js_string <- 'alert("Id not found in selected experiments");'
        session$sendCustomMessage(type='jsCode',
                                  list(value = warnGenes_js_string ))
    } else {
    warnGenes_js_string <- ''
    feature_table_scatter() %>%
        rownames_to_column() %>%
            filter_(lazyeval::interp(quote(x == y),
                                     x=as.name(TabDimensions$annotation),
                                     y = input$GenesToColor)) %>%
                `[[`("rowname") %>%
                    as.numeric() -> row_ind_genes

    GenePopulationName <-input$GenesToColor
    brush_container[[as.character(isolate(GenePopulationName))]] <-
        list(isolate(as.character(GenePopulationName)),
             isolate(row_ind_genes),
             2,
             isolate(popCounter$count),
             isolate(population_colors[popCounter$count]))
    }
})


orderAndColors <- reactiveValues()

observe({
    order_no <- data.frame()
    for(i in names(brush_container)) {
        if(!is.null(brush_container[[i]][[2]])) {
            order_no = rbind.data.frame(
                order_no,
                    data.frame(
                        name=i,
                            order=brush_container[[as.character(i)]][[4]],
                                col=as.character(
                                    brush_container[[as.character(i)]][[5]])
                    ,stringsAsFactors = F)
                )
            }
    }
    orderAndColors$data = order_no
        if(ncol(order_no)>0) {
            order_no = arrange(order_no,order_no[,2])
                orderAndColors$data = order_no
            }
})


pop_table <- reactiveValues()

observe ({
    pop_df <- data.frame()
        if( ncol(orderAndColors$data) > 0) {
            for(i in 1:nrow(orderAndColors$data)) {
                j <- as.character(orderAndColors$data$name[i])
                time=gsub("-|:| ","",Sys.time())
                idi <- paste0(j,"@",time)
                pop_df=rbind.data.frame(
                    pop_df,
                    data.frame(
                        name=j,
                        delete=shinyInput(actionButton,
                                            1,
                                            j,
                                            label="Delete",
onclick='Shiny.onInputChange(\"delete_button\", (this.id + \"@\" + Date()))' ),
                        gene=shinyInput(actionButton,
                                        1,
                                        j,
                                        label="Get Info",
onclick = 'Shiny.onInputChange(\"gene_button\", (this.id + \"@\" + Date()))' ),
                        highlight=shinyInput(checkboxInput,
                                             len=1,
                                             id=idi,
                                             value=FALSE,
                                             label="",
                                             width="25px"),
                        info_text="check to highlight in plot",
                        id = idi,
                        color=as.character(orderAndColors$data$col[i])
                    )
                )
            }
        }
  pop_table$data <- pop_df
})


testTable <- reactiveValues()
observe(
    if(is.null(ncol(pop_table$data)) |ncol(pop_table$data) == 0) {
        testTable$state=NULL
        } else {
            testTable$state = 1
            }
)


#print population table
observe(
    if(!is.null(testTable$state)) {
        df <- pop_table$data %>% select(1,2,3,4,5)
        output$popTable <- DT::renderDataTable(
                                DT::datatable({df},
                                    escape=FALSE,
                                    selection='none',
                                    rownames=F,
                                    colnames=c("","","","",""),
                                    options = list( server = FALSE,
                                                    paging = F,
                                                    searching = F,
                                                    bFilter = FALSE,
                                                    bSort=FALSE,
                                                    bInfo = F,
                                                    aLengthMenu = 5,
                                                    iDisplayLength = 5,
                                    preDrawCallback=JS(
'function() { Shiny.unbindAll(this.api().table().node()); }'),
                                    drawCallback=JS(
'function() { Shiny.bindAll(this.api().table().node()); } ')
                                                )
                                    ) %>% formatStyle(columns=1,
                                                     backgroundColor=styleEqual(
                                                          pop_table$data$name,
                                                          pop_table$data$color)
                                                    ) %>%
                                        formatStyle(columns=c(1,2,3,4,5),
                                                borderBottom="2px solid white")
                                )
  } else{ output$popTable <- DT::renderDataTable(
                                    DT::datatable({
                                            data.frame()}
                                    )
                                )
  }
)


testCheckbox <- reactiveValues()

observe({
    if(nrow(pop_table$data)>0) {
        for(i in 1:nrow(pop_table$data)){
            idi_cb <- as.character(pop_table$data[i,6])
            checkBoxHighlight <- shinyValue(as.character(idi_cb),1)
            if(!is.na(checkBoxHighlight)) {
                popToHighlight <- gsub("@.*","",idi_cb)
                    if(isTRUE(checkBoxHighlight)) {
                    testCheckbox[[as.character(popToHighlight)]] <- list("on",
                                             as.character(pop_table$data[i,7]))
                    } else if (!isTRUE(checkBoxHighlight)) {
                testCheckbox[[as.character(popToHighlight)]] <-list("off",
                                                                    "white")
                    }
                }
            }
        }
})


hlines <- reactiveValues(hline_frame = data.frame(name=1,cutX=1,cutY=1,thickness=0,color="white"))


observeEvent(input$makeSubpopulation,{
  hlines$hline_frame <- data.frame(name=1,
                                   cutX=1,
                                   cutY=1,
                                   thickness=0,
                                   color = "white")
})


observe({testCheckbox
  frame = data.frame(name=1,cutX=1,cutY=1,thickness=0,color="white")
  for(i in names(testCheckbox)) {
    if(  testCheckbox[[as.character(i)]][1] == "on" ) {
      if(length(unlist(brush_container[[i]][2])) == 1 ) {
          
      singelSample_val <- featureFrame() %>%
                    slice(unlist(brush_container[[i]][2])) %>%
                        unlist(use.names=F)
      
        frame <- rbind.data.frame(
                    frame,
                    cbind.data.frame( name=i,
                        cutX=as.vector(singelSample_val)[1],
                        cutY=as.vector(singelSample_val)[2],
                        thickness=0.5,
                        color=as.character(testCheckbox[[as.character(i)]][2]))
                    )
      } else {
            mean_values <- featureFrame() %>%
                        slice(unlist(brush_container[[i]][2])) %>%
                            summarise_each(funs(mean)) %>% unlist(use.names=F)


             cut_index <- vector(length=2)


            cut_index[1] <- as.vector(mean_values)[1]#x1[x2][1]
            cut_index[2] <-  as.vector(mean_values)[2]#y1[y2][1]


            frame <- rbind.data.frame(
                        frame,
                            cbind.data.frame(name=i,
                                cutX=as.vector(cut_index[1]),
                                cutY=as.vector(cut_index[2]),
                                thickness=0.5,
                                color=as.character(
                                    testCheckbox[[as.character(i)]][2]))
                        )
            }
        }
    }
  hlines$hline_frame <- frame
})


geneTable <- reactiveValues()

#action button to get gene list of selected population
#(last clicked population is shown)
observeEvent(input$gene_button,{
    popSelection <- gsub(pattern="@.*",
                         replacement="",
                         x=input$gene_button)
        genes_final <- c()
        for(selectedPop in as.character(popSelection)) {
            if(length(brush_container[[selectedPop]][[2]]) == 0) {
                warnPop_js_string <- 'alert("Your population is empty");'
                session$sendCustomMessage(type='jsCode',
                                          list(value = warnPop_js_string ))
            } else {
                indices <- brush_container[[selectedPop]][[2]]
                if(isTRUE(IsSingleExperimentTabs$state)) {
                    geneTableNames <- c(TabDimensions$annotation,
                                        TabDimensions$well,
                                        TabDimensions$plate)
                } else {
                            geneTableNames <- c(TabDimensions$annotation,
                                                TabDimensions$well,
                                                TabDimensions$plate,
                                                TabDimensions$experiment)
                            }

        if(TabDimensions$annotation == TabDimensions$well) {
          geneTableNames <- c(TabDimensions$well,
                              geneTableNames[-which(
                                  geneTableNames==TabDimensions$annotation)])
        }

        tmp <- feature_table_scatter()[indices,geneTableNames]
        tmp <- cbind.data.frame(rep(gsub("@.*","",selectedPop),nrow(tmp)),tmp)
        names(tmp) <- c("Population",geneTableNames)
        genes_final = rbind.data.frame(genes_final,tmp)
        names(genes_final) <- c("Population",geneTableNames)
        geneTable$data <- genes_final
      }
    }
})




gene_table_save <- function(gene_table) {return(gene_table)}

output$geneTable <- DT::renderDataTable({
    gene_table_save(gene_table=geneTable$data)},
        rownames=F,
        options = list(
                      paging = TRUE,
                      searching = FALSE,
                      bFilter = FALSE,
                      #bInfo = FALSE,
                      bLengthChange = FALSE,
                      bSort = FALSE,
                      aLengthMenu = 5,
                      iDisplayLength = 5,
                      server=T)
            )



fileNameGeneTable <-  reactive({
  if(nchar(input$filnameGeneTable) == 0) {return("filename")}else{
    return(input$filnameGeneTable)}
})

output$downloadGeneTable <- downloadHandler (
  filename = function() {paste(fileNameGeneTable(),"csv",sep=".") },
  content = function(file) {write.table(
    gene_table_save(gene_table=geneTable$data),
    file,
    row.names = F,
    sep=",",
    dec =".")  }
) # end of downloadHandler





orderAndColors_SP <- reactiveValues()

observe({
    if(!is.null(featureFrame())) {
      if(ncol(orderAndColors$data)>0) {
        pop_order_sp <- orderAndColors$data[,"name"]
        pop_order_sp <- append("all",pop_order_sp)
        colors_sp <- orderAndColors$data[,3]
        colors_sp <- append("#000000",colors_sp)
      }else{
        pop_order_sp = "all"
        colors_sp =  "#000000"
      }
      df=featureFrame()
      Populations=rep("all",times=nrow(df))

      for(i in pop_order_sp){
        if(is.null(length(brush_container[[i]][[2]]))) {
          Populations=rep("all",times=nrow(df))
        } else {
          indices <- brush_container[[i]][[2]]
          Populations[unlist(indices)] <-i
        }
      }
      orderAndColors_SP$pops = Populations
      orderAndColors_SP$pop_order = pop_order_sp
      orderAndColors_SP$colors=colors_sp
      
      #this is a quick fix, a more elgant way should be implemented
      if(length(unique(Populations)) != length(pop_order_sp)){
          if(ncol(orderAndColors$data)>0) {
          pop_order_sp <-orderAndColors$data[grep(
                                paste(unique(Populations),collapse="|"), 
                                    orderAndColors$data[,"name"]),"name"]
          pop_order_sp <- append("all",pop_order_sp)
          
          colors_sp <-orderAndColors$data[grep(
                                        paste(unique(Populations),collapse="|"), 
                                            orderAndColors$data[,"name"]),3]
          colors_sp <- append("#000000",colors_sp)
          orderAndColors_SP$pops = Populations
          orderAndColors_SP$pop_order = pop_order_sp
          orderAndColors_SP$colors=colors_sp
          }}
    }
})


lmModel <- reactiveValues(coefs=NULL,r2=NULL,line=0,corr=0)

observe({
    if(!is.null(featureFrame())) {
        linMod <- lm(as.formula(paste(names(featureFrame())[1],
                                      names(featureFrame())[2],
                                      sep="~")
                                ),
                     na.omit(featureFrame()))
        lmModel$coefs <- paste(format(coef(linMod)[1], digits = 2),
                               format(coef(linMod)[2], digits = 2),
                               sep = " + ")
        lmModel$r2 <- format(suppressWarnings(summary(linMod))$r.squared, digits = 3)
        lmModel$cor1 <- format(
                        cor(na.omit(featureFrame())[1],
                            na.omit(featureFrame())[2]),
                        digits=3)
        lmModel$cor2 <- format(
            cor(na.omit(featureFrame())[1],
                na.omit(featureFrame())[2],method = "spearman"),
            digits=3)

        if(isTRUE(input$getCorr)) {
            lmModel$line = 1
        } else{ lmModel$line = 0 }
    }
})


output$corrText <- renderUI({
    if(isTRUE(input$getCorr)) {
        HTML(paste0("<i> y = </i>",
                   lmModel$coefs,
                   "* <i> x </i> ",
                   "<br/> <i> r <sup>2</sup> = </i> ",
                   lmModel$r2,
                   "<br/> <i> Pearson's R  = </i> ",
                   lmModel$cor1,
                   "<br/> <i> Spearman's R  = </i> ",
                   lmModel$cor2)
                )
    }
})




scatter_plot <- function(df){
  validate(need(input$feature_selection_sp_x, message=FALSE))
  validate(need(input$feature_selection_sp_y, message=FALSE))
  validate(need(input$screens_selection_sp_x, message=FALSE))
  validate(need(input$screens_selection_sp_y, message=FALSE))
  
  if(length(input$pointSizeSPin)>0)
  {pointSizeSP<- input$pointSizeSPin} else {pointSizeSP<-1}
  
  if(length(input$opaSPin)>0)
  {opaSP<- input$opaSPin} else {opaSP<-1}
  
  cross <- hlines$hline_frame
  

  df_ordered <-  df %>%
    mutate(Pops=
        factor(orderAndColors_SP$pops,levels=orderAndColors_SP$pop_order)) %>%
            arrange(Pops)

  ggplot(df_ordered,
         aes_string(x=names(df_ordered)[1],
                    y=names(df_ordered)[2],
                    color="Pops")) +
          geom_point(size=pointSizeSP,alpha = opaSP,na.rm=TRUE) +
          theme_classic() +
          scale_colour_manual(values= orderAndColors_SP$colors) +
          coord_cartesian(xlim = lims$xlims, ylim = lims$ylims) +
          geom_hline(yintercept=cross[,"cutY"],
                     size = cross[,"thickness"],
                     colour = cross[,"color"],na.rm=TRUE) +
          geom_vline(xintercept=cross[,"cutX"],
                     size = cross[,"thickness"],
                     colour = cross[,"color"],na.rm=TRUE) +
          theme(legend.position="none")+
          xlab(paste(input$feature_selection_sp_x,
                    "for",
                    input$screens_selection_sp_x,
                    sep = "  ")) +
          ylab(paste(input$feature_selection_sp_y,
                     "for",
                     input$screens_selection_sp_y,
                     sep = "  ")) +
          geom_line(stat="smooth",method = "lm",
                    size = 0.5,
                    color ="red",
                    alpha = lmModel$line)
}




#render plot by calling the function
output$scatterPlot <- renderPlot({
  validate(need(input$feature_selection_sp_x, message=FALSE))
  validate(need(input$feature_selection_sp_y, message=FALSE))
  validate(need(input$screens_selection_sp_x, message=FALSE))
  validate(need(input$screens_selection_sp_y, message=FALSE))
  if(!is.null(featureFrame())) {
    scatter_plot(df=featureFrame())
  }
},
height = function() {
  session$clientData$output_scatterPlot_width
})
