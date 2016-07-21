# ------------------------------------------------------------------------------
# Name of Quantlet: MMSTATvis_distr
# ------------------------------------------------------------------------------
# Published in:     MMSTAT
# ------------------------------------------------------------------------------
# Description:      Shows an interactive interface with 4 plots for univariate data (dotplot, histogram, boxplot, ECDF).
#                   The user can choose the dotplot type (overplot, jitter, stack), the number of bins in the histogram,
#                   and whether additional lines are shown to indicate mean and variance. The lower panel summarizes robust 
#                   and non-robust location and dispersion parameters for the selected variable. Furthermore, the user can choose variables of the data sets USCRIME, CARS and DECATHLON.
# ------------------------------------------------------------------------------
# Keywords:         plot, histogram, boxplot, mean, variance, median, quantile, visualization, data visualization,
#                   parameter, interactive, uscrime, binwidth, standard deviation, univariate, empirical,
#                   cdf, US crime data set
# ------------------------------------------------------------------------------
# Usage:            MMSTAThelper_function
# ------------------------------------------------------------------------------
# Output:           Interactive shiny application
# ------------------------------------------------------------------------------
# Example:          Shows variable visualizations for the variable POPULATION of the data set USCRIME.     
# ------------------------------------------------------------------------------
# See also:         BCS_Boxplot, BCS_Hist1, BCS_Hist2, MVAboxcar, empcdf, MMSTATtime_series_1, MMSTATlinreg, MMSTATconfmean, 
#                   MMSTATconfi_sigma, MMSTATassociation, MMSTAThelper_function
# ------------------------------------------------------------------------------
# Author :          Sigbert Klinke
# ------------------------------------------------------------------------------
# Code Editor:      Yafei Xu
# ------------------------------------------------------------------------------
# Datafiles:        CARS.rds, USCRIME.rds, DECATHLON.rds
# ------------------------------------------------------------------------------

# please use "Esc" key to jump out of the Shiny app
rm(list = ls(all = TRUE))
graphics.off()

# please set working directory setwd('C:/...') 
# setwd('~/...')    # linux/mac os
setwd("C:/Users/nazaretl.sfb649/Documents/GitHub/MMSTAT-ToDo/MMSTATvis_distr")
source("MMSTAThelper_function.r")

############################### SUBROUTINES ##################################
############################ assistant helper ################################

mmstat.html = function(file, ...) {
  stopif (!file.exists(file), sprintf("File '%s' does not exist", file))
  html = paste0(readLines(file), collapse="")
  stopif (!is.ASCII(html), sprintf("File '%s' contains non-ASCII symbols", file))
  args = list(...)
  cond = sapply(args, length)
  if (length(args)) {
    stopif (any(sapply(args, is.null)), 'One or more arguments contain a NULL')
    args$fmt = html
    html     = do.call("sprintf", args)
  }
  return(html)
}

############################### SUBROUTINES ##################################
################################# server #####################################

dpc = gettext(c("overplot", "jitter", "stack"), "name")

mmstat.ui.elem("method", "selectInput",
               label    = gettext("Select a Dotplot type"),
               choices  = dpc,
               selected = "overplot")
mmstat.ui.elem("bins", "sliderInput",
               label   = gettext("Histogram: choose number of bins"),
               min     = 1,
               max     = 50,
               value   = 30)
mmstat.ui.elem("addmean", "checkboxInput",
               label = gettext("Add mean (aquamarine, dotted)"),
               value = FALSE)
mmstat.ui.elem("addmedian", "checkboxInput",
               label = gettext("Add median (blue, dashed)"),
               value = FALSE)               
mmstat.ui.elem("obs", "checkboxInput",
               label = gettext("Show observations"),
               value = FALSE)               
mmstat.ui.elem("dataset", "dataSet",     
               choices=mmstat.getDataNames("USCRIME", "CARS", "DECATHLON"))
mmstat.ui.elem("variable", "variable1", vartype="numeric")
mmstat.ui.elem("cex", "fontSize")

server = shinyServer(function(input, output, session) {

  output$methodUI    = renderUI({ mmstat.ui.call('method') })
  output$binsUI      = renderUI({ mmstat.ui.call('bins') })
  output$addmeanUI   = renderUI({ mmstat.ui.call('addmean') })
  output$addmedianUI = renderUI({ mmstat.ui.call('addmedian') })  
  output$obsUI       = renderUI({ mmstat.ui.call('obs') })  
  output$datasetUI   = renderUI({ mmstat.ui.call('dataset') })
  output$cexUI       = renderUI({ mmstat.ui.call('cex')  })
  output$variableUI  = renderUI({
                                 inp = mmstat.getValues(NULL, dataset = input$dataset)
                                 mmstat.ui.call('variable',
                                                choices = mmstat.getVarNames(inp$dataset, 'numeric')
                                 ) 
  })  
  
  readHTML = reactive({
    var  = getVar()
    html = mmstat.html(gettext("distText.html"), 
                       100*input$cex,
                       var$xlab, var$dataname,
                       var$decimal, var$mean,
                       var$decimal, var$median,
                       var$decimal, var$min,
                       var$decimal, var$quart[1],
                       var$decimal, var$max,
                       var$decimal, var$quart[2],
                       var$decimal, var$sd,
                       var$decimal, var$iqr,
                       var$decimal, var$var,
                       var$decimal, diff(var$range)
                      )
    html
  })
  
  getVar = reactive({
    var              = mmstat.getVar(isolate(input$dataset), 
                                   input$variable, 'numeric')
    dec              = mmstat.dec(c(var$mean, var$median))
    var[['decimal']] = dec$decimal
    var[['pos']]     = 2*(var$mean<var$median)
    var
  })
  
  output$plotStrip = renderPlot({
    inp = mmstat.getValues(NULL,
                           method    = input$method,
                           addmean   = input$addmean,
                           addmedian = input$addmedian,
                           cex       = input$cex)
    var = getVar()
    par(mar = c(3.1, 4.1, 4.1, 2.1))
    stripchart(var$values, 
               inp$method, 
               xlim     = var$range, 
               axes     = F, 
               main     = sprintf(gettext("Dotplot (%s)"), 
                                  tolower(gettext(inp$method))), 
               cex.axis = inp$cex, 
               cex.lab  = inp$cex, 
               cex.main = 1.2*inp$cex, 
               cex.sub  = inp$cex,
               xlab     = var$xlab)
    usr = par("usr")
    mmstat.axis(1, usr[1:2], cex.axis = inp$cex)  
    box()
    if (inp$addmean) abline(v   = var$mean, 
                            col = mmstat$col[[1]], 
                            lwd = 3, 
                            lty = "dotted")
    if (inp$addmedian) abline(v   = var$median, 
                              col = mmstat$col[[3]], 
                              lwd = 3, 
                              lty = "dashed")
  })

  output$plotHist = renderPlot({
    var  = getVar()
    inp  = mmstat.getValues(NULL,
                            bins      = input$bins,
                            addmean   = input$addmean,
                            addmedian = input$addmedian,
                            obs       = input$obs,
                            cex       = input$cex)
    bins = seq(var$min, var$max, length.out = as.numeric(inp$bins) + 1)
    par(mar = c(3.1, 4.1, 4.1, 2.1))
    hist(var$values, 
         xlim     = var$range, 
         breaks   = bins, 
         col      = "grey", 
         xlab     = var$xlab, 
         main     = gettext("Histogram"), 
         ylab     = gettext("Absolute frequency"), 
         cex.axis = inp$cex, 
         cex.lab  = inp$cex, 
         cex.main = 1.2 * inp$cex, 
         cex.sub  = inp$cex, 
         axes     = F)
    usr = par("usr")
    mmstat.axis(1, usr[1:2], cex.axis = inp$cex)   
    mmstat.axis(2, usr[3:4], cex.axis = inp$cex)
    box()
    if (inp$addmean) abline(v   = var$mean, 
                            col = mmstat$col[[1]], 
                            lwd = 3, 
                            lty = "dotted")
    if (inp$addmedian) abline(v   = var$median, 
                              col = mmstat$col[[3]], 
                              lwd = 3, 
                              lty = "dashed")
    if (inp$obs) rug(var$values, lwd = 1)
  })
   
  output$plotBox = renderPlot({
    inp = mmstat.getValues(NULL,
                           bins      = input$bins,
                           addmean   = input$addmean,
                           addmedian = input$addmedian,
                           obs       = input$obs,
                           cex       = input$cex)
    var = getVar()
    par(mar = c(3.1, 4.1, 4.1, 2.1))
    boxplot(var$values, 
            horizontal = T, 
            ylim       = var$range, 
            axes       = F, 
            xlab       = var$xlab,
            cex.axis   = inp$cex,
            cex.lab    = inp$cex,
            cex.main   = 1.2*inp$cex,
            cex.sub    = inp$cex,
            main       = gettext("Boxplot"))
    usr = par("usr")
    mmstat.axis(1, usr[1:2], cex.axis = inp$cex)
    box()
    if (inp$addmean) abline(v   = var$mean, 
                            col = mmstat$col[[1]], 
                            lwd = 3, 
                            lty = "dotted")
    if (inp$addmedian) abline(v   = var$median, 
                              col = mmstat$col[[3]], 
                              lwd = 3, 
                              lty = "dashed")
    if (inp$obs) rug(var$values, lwd = 1)
  })
     
  output$plotEcdf = renderPlot({
    inp = mmstat.getValues(NULL,
                           bins      = input$bins,
                           addmean   = input$addmean,
                           addmedian = input$addmedian,
                           obs       = input$obs,
                           cex       = input$cex)
    var = getVar()
    par(mar = c(3.1, 4.1, 4.1, 2.1))
    plot(ecdf(var$values), 
         xlim     = var$range, 
         main     = gettext(" Ecdf "),
         xlab     = var$xlab,
         cex.axis = inp$cex,
         cex.lab  = inp$cex,
         cex.main = 1.2*inp$cex,
         cex.sub  = inp$cex,
         axes     = F)
    usr = par("usr")
    mmstat.axis(1, usr[1:2], cex.axis = inp$cex)
    mmstat.axis(2, usr[3:4], cex.axis = inp$cex)
    box()
    if (inp$addmean) abline(v   = var$mean, 
                            col = mmstat$col[[1]], 
                            lwd = 3, 
                            lty = "dotted")
    if (inp$addmedian) abline(v   = var$median, 
                              col = mmstat$col[[3]], 
                              lwd = 3, 
                              lty = "dashed")
  })
     
  output$distText = renderText({
    mmstat.log("called 'distText'")
    html = readHTML()
    html
  })
  
  output$logText = renderText({
    mmstat.getLog(session)
  })
  
})

############################### SUBROUTINES ##################################
#################################### ui ######################################

ui = shinyUI(fluidPage(
  div(class = "navbar navbar-static-top",
      div(class = "navbar-inner", 
          fluidRow(column(4, div(class = "brand pull-left", 
                                 gettext("Variable visualizations"))),
                   column(2, checkboxInput("showplot", 
                                           gettext("Plot parameter"), 
                                            TRUE)),
                   column(2, checkboxInput("showparam", 
                                           gettext("Descriptives"), 
                                           TRUE)),
                   column(2, checkboxInput("showdata", 
                                           gettext("Data choice"), 
                                           FALSE)),
                   column(2, checkboxInput("showoptions", 
                                           gettext("Options"), 
                                           FALSE))))),

  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = 'input.showplot',
        uiOutput("methodUI"),
        uiOutput("binsUI")
      ),
      conditionalPanel(
        condition = 'input.showparam',
        hr(),
        uiOutput("addmeanUI"),
        uiOutput("addmedianUI")
      ),
      conditionalPanel(
        condition = 'input.showdata',
        hr(),
        uiOutput("datasetUI"),
        uiOutput("variableUI")
      ),
      conditionalPanel(
        condition = 'input.showoptions',
        hr(),
        uiOutput("cexUI")
      )
    ),    
    mainPanel(fluidRow(
      column(width = 6, height = 3, plotOutput("plotStrip", height = "300px")),
      column(width = 6, height = 3, plotOutput("plotHist", height = "300px"))),
    fluidRow(
      column(width = 6, height = 3,  plotOutput("plotBox", height = "300px")),
      column(width = 6, height = 3,  plotOutput("plotEcdf", height = "300px"))),
    HTML('<hr>'),
    htmlOutput("distText"))),  
    htmlOutput("logText")
))

############################### SUBROUTINES ##################################
################################ shinyApp ####################################

shinyApp(ui = ui, server = server)
