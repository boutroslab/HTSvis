### server of quality control tab
## reactiveValues object 'TabDimensions' defined in lauchApp.R 
## 3 quality control plots are generated: plate plot, desity plot and box plot

## Control populations are selected by clicking on corresponding wells of a 
## plotted heatmap 

## 'col' is a reactiveValues object to store the clicked wells 
col <- reactiveValues()

#Heatmap to define control populations by clickin'
df_qc <- reactive({
    validate(need(input$feature_selection_qc, message=FALSE))
    validate(need(input$screen_selection_qc, message=FALSE))

    if(!isTRUE(IsSingleExperimentTabs$state)) {
        limits_qc <- feature_table2$data %>%
                        select_(TabDimensions$plate,
                                TabDimensions$experiment,
                                input$feature_selection_qc) %>%
                            filter_(lazyeval::interp(quote(x == y),
                                    x = as.name(TabDimensions$experiment),
                                    y = input$screen_selection_qc)) %>%
                                filter_(lazyeval::interp(quote(x == y),
                                             x = as.name(TabDimensions$plate),
                                            y = feature_table2$data[1,TabDimensions$plate])) %>%
                                    select_(input$feature_selection_qc) %>%
                                        do(funs=c(min(.,na.rm=T),
                                                  max(.,na.rm=T))) %>%
                                            unlist(use.names = F)
        if(is.finite(limits_qc)[1] & is.finite(limits_qc)[2]) {
            warnEmpty_js_string <- ''

        plotHeatmap(data_table=feature_table2$data,
                           limits=limits_qc,
                           curr_plate=feature_table2$data[1,TabDimensions$plate],
                           curr_screen=input$screen_selection_qc,
                           curr_feature=input$feature_selection_qc,
                           plateDim=TabDimensions$plate,
                           expDim=TabDimensions$experiment,
                           wellDim=TabDimensions$well,
                           annoDim=TabDimensions$annotation)
                } else {
                    warnEmpty_js_string <- 'alert("Selected column combination does not exist");'
                    session$sendCustomMessage(type='jsCode',
                                              list(value = warnEmpty_js_string ))
                    return(NULL)}
        } else {
            limits_qc <- feature_table2$data %>%
                            select_(TabDimensions$plate,
                                    "value"=input$feature_selection_qc) %>%
                                filter_(lazyeval::interp(quote(x == y),
                                        x = as.name(TabDimensions$plate),
                                        y = feature_table2$data[1,TabDimensions$plate])) %>%
                                    select_("value") %>%
                                        do(funs=c(min(.,na.rm=T)
                                                  ,max(.,na.rm=T))) %>%
                                            unlist(use.names = F)

            if(is.finite(limits_qc)[1] & is.finite(limits_qc)[2]) {
                warnEmpty_js_string <- ''

    plotHeatmap(data_table=feature_table2$data,
                       limits=limits_qc,
                       curr_plate=feature_table2$data[1,TabDimensions$plate],
                       curr_screen=F,
                       curr_feature=input$feature_selection_qc,
                       plateDim=TabDimensions$plate,
                       expDim=TabDimensions$experiment,
                       wellDim=TabDimensions$well,
                       annoDim=TabDimensions$annotation)
            } else {
                warnEmpty_js_string <- 'alert("Selected column combination does not exist");'
                session$sendCustomMessage(type='jsCode',
                                          list(value = warnEmpty_js_string ))
                return(NULL)}

  }
})


#action button to kill the clicked population(s)
observeEvent(input$resetControls,{
  col$population = rep("undefined",
                       length(isolate(df_qc()[,TabDimensions$well]) ))
  col$well = isolate(df_qc()[,TabDimensions$well])
  col$color = isolate(df_qc()$def.color)
  col$opa = rep(0.3, length(isolate(df_qc()[,TabDimensions$well]) ))
})



observe({
  if(!is.null(input$screen_selection_qc)) {
    if(nchar(input$screen_selection_qc)>0){
      col$color[which(
          col$population == "undefined")] <- df_qc()$def.color[which(
                                                col$population == "undefined")]
    }}
})


observe({
  if(!is.null(input$feature_selection_qc)) {
    if(nchar(input$feature_selection_qc)>0){
      col$color[which(
          col$population == "undefined")] <- df_qc()$def.color[which(
                                                col$population == "undefined")]
    }}
})



#function to select controls by clicking 
#input$radio is a radio button to switch between the contol populations 
fu_qc_click <- reactive({print_out <- function(x) {
  if(is.null(x)) return(NULL) else
    isolate(
      if(col$population[x$CSidB110] == "undefined") {
        if(input$radio == "pos") {
          col$population[x$CSidB110] = "positive"
          col$color[x$CSidB110] = pp_col
          col$opa[x$CSidB110] = 1
        } else {
          if(input$radio == "neg") {
            col$color[x$CSidB110] = pn_col
            col$population[x$CSidB110] = "negative"
            col$opa[x$CSidB110] = 1 } else {
              if(input$radio == "nt") {
                col$population[x$CSidB110] = "nt"
                col$color[x$CSidB110] = nt_col
                col$opa[x$CSidB110] = 1
              }
            }
        }
      }else{
        col$population[x$CSidB110] = "undefined"
        col$color[x$CSidB110] = df_qc()$def.color[x$CSidB110]
        col$opa[x$CSidB110] = 0.3
      }
    )
  return(NULL)
}
})


#function for hover over heatmap 
fu_qc_hover <- reactive({print_out <- function(x) {
  if(is.null(x)) return(NULL)
    if(TabDimensions$well == TabDimensions$annotation) {
      paste(df_qc()[df_qc()$CSidB110 == x$CSidB110,TabDimensions$well],
            df_qc()[df_qc()$CSidB110 == x$CSidB110,"value" ],sep="<br />")
    } else {
      paste(df_qc()[df_qc()$CSidB110 == x$CSidB110,TabDimensions$well],
            df_qc()[df_qc()$CSidB110 == x$CSidB110,"value" ],
            df_qc()[df_qc()$CSidB110 == x$CSidB110,TabDimensions$annotation],sep="<br />") }
}
})


#reactive values object to buffer reactivity before heatmap is plotted 
test_df_qc <- reactiveValues(state1=F,state2=F)

observe({
  validate(need(input$feature_selection_qc, message=FALSE))
  test_df_qc$state1 = T
})


observe({
  validate(need(input$screen_selection_qc, message=FALSE))
  test_df_qc$state2 = T
})

observe({
    if(nrow(df_qc()) == length(col$color )) {
    warnMissingQC_js_string <- ''
     
     } else {
         test_df_qc$state2 = F
         test_df_qc$state2 = F
           warnMissingQC_js_string <- 'alert("The well dimensions is incomplete");'
           session$sendCustomMessage(type='jsCode',
                                    list(value = warnMissingQC_js_string ))}
})
        

#plot the reactive heatmap 
observe({
  if(isTRUE(test_df_qc$state1) | isTRUE(test_df_qc$state2)) {
        df_qc %>%
          ggvis(~column,
                ~row,
                fill:=~col$color,
                fillOpacity:=~col$opa,
                stroke :="black",
                key :=~CSidB110)%>%
            add_tooltip(fu_qc_click(), "click") %>%
                add_tooltip(fu_qc_hover(), "hover")%>%
                    layer_rects(width = band(), height = band())%>%
                        scale_nominal("x", padding = 0, points = FALSE)%>%
                            scale_nominal("y", padding = 0, points = FALSE)%>%
                                add_axis("x",
                                         title="",
                                         tick_size_major=0,
                                         properties=axis_props(
                                             axis=list(stroke="white",
                                                       strokeWidth=0),
                                             grid=list(strokeWidth=0)))%>%
                                add_axis("y",
                                         title="",
                                         tick_size_major=0,
                                         properties=axis_props(
                                             axis=list(stroke="white",
                                                       strokeWidth=0),
                                             grid=list(strokeWidth = 0)))%>%
                                        set_options(height = 300, width = 450) %>%
                                            bind_shiny("heatmap_qc")
    }
})



pos_wells <- reactive({
  validate(need(input$feature_selection_qc, message=FALSE))
  validate(need(input$screen_selection_qc, message=FALSE))
    df_qc()[which(col$population == "positive"),TabDimensions$well]})

neg_wells <- reactive({
  validate(need(input$feature_selection_qc, message=FALSE))
  validate(need(input$screen_selection_qc, message=FALSE))
    df_qc()[which(col$population == "negative"),TabDimensions$well] })

nt_wells <- reactive({
  validate(need(input$feature_selection_qc, message=FALSE))
  validate(need(input$screen_selection_qc, message=FALSE))
    df_qc()[which(col$population == "nt"),TabDimensions$well] })



#definition of control populations
final <- reactive ({
  validate(need(input$feature_selection_qc, message=FALSE))
  validate(need(input$screen_selection_qc, message=FALSE))
  if(!isTRUE(IsSingleExperimentTabs$state)) {
      feature_table2$data %>%
        filter_(lazyeval::interp(quote(x == y),
                                 x = as.name(TabDimensions$experiment),
                                 y = input$screen_selection_qc)) %>%
          select_(TabDimensions$well,
                  TabDimensions$plate,
                  "value"=input$feature_selection_qc)
    } else {
      feature_table2$data %>%
        select_(TabDimensions$well,
                TabDimensions$plate,
                "value"=input$feature_selection_qc)
  }
})


nt_controls <- reactive({
    validate(need(input$feature_selection_qc, message=FALSE))
    validate(need(input$screen_selection_qc, message=FALSE))
    final()[with(final(),
                 which(final()[,TabDimensions$well] %in% nt_wells()) ),"value"]
})



p_controls_n <- reactive({
  validate(need(input$feature_selection_qc, message=FALSE))
  validate(need(input$screen_selection_qc, message=FALSE))
    final()[with(final(),
                 which(final()[,TabDimensions$well] %in% neg_wells()) ),"value"]
})


p_controls_p <- reactive({
  validate(need(input$feature_selection_qc, message=FALSE))
  validate(need(input$screen_selection_qc, message=FALSE))
    final()[with(final(),
                 which(final()[,TabDimensions$well] %in% pos_wells()) ),"value"]
})


rest <- reactive ({
  validate(need(input$feature_selection_qc, message=FALSE))
  validate(need(input$screen_selection_qc, message=FALSE))

  if(!is.null(input$IsSingleExperiment)){
    if(!isTRUE(input$IsSingleExperiment)) {
      feature_table2$data %>%
        filter_(lazyeval::interp(quote(x == y),
                                 x = as.name(TabDimensions$experiment),
                                 y = input$screen_selection_qc))%>%
          select_(TabDimensions$well,
                  TabDimensions$plate,
                  "value"=input$feature_selection_qc)%>%
            filter_(lazyeval::interp(quote(!(x %in% y)),
                                     x = as.name(TabDimensions$well),
                                     y = c(input$var_ntControls_qc,
                                           input$var_postiveControls_qc,
                                           input$var_negativeControls_qc)))

    }else{
      feature_table2$data%>%
        select_(TabDimensions$well,
                TabDimensions$plate,
                "value"=input$feature_selection_qc)%>%
          filter_(lazyeval::interp(quote(!(x %in% y)),
                                   x = as.name(TabDimensions$well),
                                   y = c(input$var_ntControls_qc,
                                         input$var_postiveControls_qc,
                                         input$var_negativeControls_qc)))
    }
  }
})

#assign reactive values object to contorl density and box plot
#those plots will only be drwan when the contorl populations are defined
DummyPlots <- reactiveValues(dens=F,box=F)

observe({
    if(length(p_controls_p()) < 10 |
       length(p_controls_n()) < 10) {
        DummyPlots$dens <- F
    } else {
        DummyPlots$dens <- T
    }
})



#Density Plot
output$densityPlot <- renderPlot({
    validate(need(input$feature_selection_qc, message=FALSE))
    validate(need(input$screen_selection_qc, message=FALSE))
    densPlotOut()
})#end of renderPlot

densPlotOut <- function(){  
    if(!isTRUE(DummyPlots$dens)){
        plot(1, type="n", axes=F, xlab="", ylab="" ,main = NA)
    }else{
        if(length(p_controls_p())>0 &length(p_controls_n())>0) {
            dis_p <- suppressWarnings(density(p_controls_p(),na.rm=T))
            dis_n <- suppressWarnings(density(p_controls_n(),na.rm=T))
            if(length(nt_controls()) < 10 ){
                dis_nt <- list()
                dis_nt$x <- 0
                dis_nt$y <- 0
            } else {
                dis_nt <- suppressWarnings(density(nt_controls(),na.rm=T))}
            xmin <-  min(c(dis_n$x,dis_p$x,dis_nt$x),na.rm = T)
            +(0-abs(min(c(dis_n$x,dis_p$x,dis_nt$x),na.rm = T)*0.5))
            xmax <-  max(c(dis_n$x,dis_p$x,dis_nt$x),na.rm = T)
            +(max(c(dis_n$x,dis_p$x,dis_nt$x),na.rm = T)*0.5)
            ymin <- min(c(dis_n$y,dis_p$y,dis_nt$y),na.rm = T)
            +(0-abs(min(c(dis_n$y,dis_p$y,dis_nt$y),na.rm = T)*0.2))
            ymax <- max(c(dis_n$y,dis_p$y,dis_nt$y),na.rm=T)
            +max(c(dis_n$y,dis_p$y,dis_nt$y,na.rm=T)*0.2)
            sd_p <- sd(p_controls_p(),na.rm=T)
            sd_n <- sd(p_controls_n(),na.rm=T)
            mean_p <- mean(p_controls_p(),na.rm=T)
            mean_n <- mean(p_controls_n(),na.rm=T)
            z_factor <- round(
                (1-((3*(sd_n+sd_p))/(abs(mean_n-mean_p)))),
                2)
            z_factor_printout <- paste("Z'-factor",
                                       z_factor,
                                       sep=" : ")
            feature <- input$feature_selection_qc
            plot(dis_p,
                 ylim=c(ymin,ymax),
                 xlim =c(xmin,xmax),
                 main=NA,
                 col=pp_col,xlab=feature)
            lines(dis_n,col = pn_col)
            lines(dis_nt,col = nt_col)
            title(main=substitute(
                paste(
                    "Density Distributions of Controls for",
                    "\n",italic(feature) ) ) )
            
            if(length(nt_controls()) > 10  ){
                legend_cols <- c(pp_col,pn_col,nt_col,"black")
                legend_string <- c("positive controls",
                                   "negative controls",
                                   "non-targeting controls",
                                   z_factor_printout)
            } else {
                legend_cols <- c(pp_col,pn_col,"black")
                legend_string <- c("positive controls",
                                   "negative controls",
                                   z_factor_printout)
            }
            legend("topleft",
                   pch=c(15,15,16),
                   col=legend_cols,
                   legend=legend_string)
        }
    }
}



#control conditional panel for dummy plots (when populations are undefined)
output$showDensPlots <- reactive({
    return(DummyPlots$dens)
})
outputOptions(output, "showDensPlots", suspendWhenHidden=FALSE)


output$hideDensPlots <- reactive({
    return(!DummyPlots$dens)
})
outputOptions(output, "hideDensPlots", suspendWhenHidden=FALSE)


#dummy text for density plot
output$textDensDummy <- renderUI(
    HTML(paste("
                <b> Density distribution (KDE) plot </b>",
               "Select control wells
               to create a density distribution (KDE) plot.",
               "Minimum 10 data points for positve and negative controls are
               required",
               sep="<br/>"))
)

##Plate Plot #################################################

output$platePlot <- renderPlot({
  validate(need(input$feature_selection_qc, message=FALSE))
  validate(need(tabInput$inputPlates, message=FALSE))
  platePlotOut()
})#end of renderPlot

platePlotOut <- function(){
  plates <- tabInput$inputPlates


  if(is.null(pos_wells() )) {
      meds1 <- cbind.data.frame(plate=0,med=0,status=0,color=0)
  } else {
            meds1 <- cbind.data.frame(plate=plates,
                                       med=seq(1:length(plates)),
                                       status=seq(1:length(plates)),
                                       color = seq(1:length(plates)))

            sapply(seq_along(meds1$plate), function(ind, p) {
                x <- final()[with(final(),which(
                    final()[,TabDimensions$plate] %in% p[ind])),]
                meds1[ind,"med"] <<- mean(x[with(x, which(
                    x[,TabDimensions$well] %in% pos_wells()) ),"value"] )
                meds1[ind,"status"] <<- "positive"
                meds1[ind,"color"] <<- pp_col
            },p = plates)

            meds1[,"plate"] <- as.numeric(as.factor(meds1[,"plate"]))
    }


  if(is.null(neg_wells() )) {
      meds2 <- cbind.data.frame(plate=0,med=0,status=0,color=0)
  } else {
            meds2 <- cbind.data.frame(plate=plates,
                                       med=seq(1:length(plates)),
                                       status=seq(1:length(plates)),
                                       color=seq(1:length(plates)))
            sapply(seq_along(meds2$plate), function(ind, p) {
              x <- final()[with(final(), which(
                  final()[,TabDimensions$plate] %in% p[ind])),]
              meds2[ind,"med"] <<- mean(x[with(x, which(
                  x[,TabDimensions$well] %in% neg_wells()) ),"value"] )
              meds2[ind,"status"] <<- "negative"
              meds2[ind,"color"] <<- pn_col
            },p = plates)
            meds2[,"plate"] <- as.numeric(as.factor(meds2[,"plate"])) }


  if(is.null(nt_wells() )){
      meds3 <- cbind.data.frame(plate=0,med=0,status=0,color=0)
    } else {
        meds3 <- cbind.data.frame(plate=plates,
                                   med=seq(1:length(plates)),
                                   status=seq(1:length(plates)),
                                   color=seq(1:length(plates))
                                   )

        sapply(seq_along(meds3$plate), function(ind, p) {
            x <- final()[with(final(), which(
                final()[,TabDimensions$plate] %in% p[ind])) ,]
            meds3[ind,"med"] <<- mean(x[with(x, which(
              x[,TabDimensions$well] %in% nt_wells()) ),"value"] )
            meds3[ind,"status"] <<- "non-targeting"
            meds3[ind,"color"] <<- nt_col
        },p = plates)

    meds3[,"plate"] <- as.numeric(as.factor(meds3[,"plate"]))
    }


  sample_values = cbind.data.frame(plate = rest()[,TabDimensions$plate],
                                   med = rest()$value,
                                   color = rep("black",nrow(rest())),
                                   status = rep("sample",nrow(rest()))
                                    )

  sample_values[,"plate"] <- as.numeric(as.factor(sample_values[,"plate"]))


  ymax <- max(
            sample_values$med,na.rm = T)+(max(
                                            sample_values$med,na.rm = T)*0.2)
  ymin <- min(
    sample_values$med,na.rm = T)+(0-abs(
                                        min(
                                            sample_values$med,na.rm = T)*0.2))
  feature_mp <- as.character(input$feature_selection_qc)

  meds_total <- rbind.data.frame(meds1,meds2,meds3)


  plot(med ~as.numeric(plate),
       data=meds_total,
       col=meds_total$color,
       xaxt="n",
       pch=16,
       ylim=c(ymin,ymax),
       type="p",
       ylab=substitute(italic(feature_mp)~"per plate"),
       xlab = "plate index")
  axis(1, at = 1:(length(unique(meds_total$plate))))
  points(sample_values,col = "black", pch = 19,cex = 0.1)

}




##Box Plot

pp <- reactive({
  if( length(p_controls_p()) == 0) {
    p <- NULL
    return(p)} else {
      p <- cbind.data.frame(value=as.numeric(p_controls_p()),
                            status=as.factor("positive")
                            )
      return(p)}
})

pn <- reactive({
  if( length(p_controls_n()) == 0) {
    n <- NULL
    return(n)} else {
      n <- cbind.data.frame(value=as.numeric(p_controls_n()),
                            status=as.factor("negative") )
      return(n)}
})

nt <- reactive({
  if( length(nt_controls()) == 0    ) {
    t <- NULL
    return(t)} else {
      t <- cbind.data.frame(value=as.numeric(nt_controls()),
                            status=as.factor("non-targeting") )
      return(t)}
})

total <- reactive({
  tot <- rbind.data.frame(pp(),pn(),nt())
  return(tot) })

#control conditional panel for box plot
observe({
    if(length(na.omit(p_controls_p())) == 0 &
       length(na.omit(p_controls_n())) == 0  &
       length(na.omit(nt_controls())) == 0) {
        DummyPlots$box <- F
    } else {
        DummyPlots$box <- T
    }
})


output$boxPlot <- renderPlot({
  validate(need(input$feature_selection_qc, message=FALSE))
  validate(need(input$screen_selection_qc, message=FALSE))
  boxPlotOut()
})#end of renderPlot

boxPlotOut <- function(){
    if(!isTRUE(DummyPlots$box)){
        plot(1, type="n", axes=F, xlab="", ylab="" ,main = NA)
    } else {

        x_pos_nt <- as.numeric(which(
                                levels(total()$status) == "non-targeting") )
            x_pos_pp <- as.numeric(which(
                                    levels(total()$status) == "positive") )
                x_pos_pn <- as.numeric(which(
                                        levels(total()$status) == "negative") )
                    feature_bp <- as.character(input$feature_selection_qc)

    if(nrow(total())>0) {
    bp_ctrls <- boxplot(value~as.factor(status),
                        data=total(),
                        ylab=substitute(italic(feature_bp)~"per screen"),
                        labels=levels(total()$status),
                        plot = F)
    bxp(bp_ctrls,show.names=T)
        points(rep(x_pos_nt,length(nt()$value)),nt()$value,col = nt_col)
            points(rep(x_pos_pn,length(pn()$value)),pn()$value,col = pn_col)
                points(rep(x_pos_pp,length(pp()$value)),pp()$value,col = pp_col)
    }
  }
}


#downlaod function for qc plot 

output$downloadPlotQC <- downloadHandler(
    filename = function() {
        paste(input$fileNamePlotQC,".pdf",sep="")
    },
    content = function(file) {
        pdf(file)
             platePlotOut() 
             densPlotOut() 
             boxPlotOut() 
        dev.off()
    }
)#end of download function




#contorl condictional panel for dummy plots (when populations are undefined)
output$showBoxPlots <- reactive({
    return(DummyPlots$box)
})
outputOptions(output, "showBoxPlots", suspendWhenHidden=FALSE)


output$hideBoxPlots <- reactive({
    return(!DummyPlots$box)
})
outputOptions(output, "hideBoxPlots", suspendWhenHidden=FALSE)


#dummy text for box plot
output$textBoxDummy <- renderUI(
    HTML(paste("
               <b> Box plot </b>",
               "Select control wells
               to create a box plot.",
               sep="<br/>"))
)
