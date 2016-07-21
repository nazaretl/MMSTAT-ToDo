# ------------------------------------------------------------------------------
# Name of Quantlet: MMSTATconfi_pi
# ------------------------------------------------------------------------------
# Published in:     MMSTAT
# ------------------------------------------------------------------------------
# Description:      Shows an interactive interface to show the coverage of confidence intervals for proportions.
#                   Samples are drawn from the whole population and the mean with corresponding
#                   confidence interval is estimated. The user can check whether the estimated
#                   confidence interval covers the population mean (green dashed line).
#                   One can adjust the confidence level (1-alpha) and the sample size (n).
#                   The upper panel shows the previously estimated confidence intervals and
#                   the lower panel shows the distribution of the whole population and sample.
#                   The user can choose variables from the data sets CREDIT, BOSTONHOUSING
#                   and TITANIC.
# ------------------------------------------------------------------------------
# Keywords:         distribution, plot, confidence interval, sampling, mean,
#                   visualization, data visualization, estimation, parameter,
#                   interactive
# ------------------------------------------------------------------------------
# Usage:            MMSTAThelper_function
# ------------------------------------------------------------------------------
# Output:           Interactive shiny application
# ------------------------------------------------------------------------------
# Example:          Shows the one confidence interval of the variable AGE of data set TITANIC.               
# ------------------------------------------------------------------------------
# See also:         KI, MVAdrafthousing, MMSTATtime_series_1, MMSTATlinreg, 
#                   MMSTATconfmean, MMSTATconfi_sigma, MMSTATassociation, 
#                   MMSTAThelper_function, MMSTATlinreg
# ------------------------------------------------------------------------------
# Author :          Sigbert Klinke
# ------------------------------------------------------------------------------
# Code Editor:      Yafei Xu
# ------------------------------------------------------------------------------
# Datafiles:        CREDIT.rds, BOSTONHOUSING.rds, TITANIC.rds
# ------------------------------------------------------------------------------

# please use "Esc" key to jump out of the Shiny app
rm(list = ls(all = TRUE))
graphics.off()

# please set working directory setwd('C:/...') 
# setwd('~/...')    # linux/mac os
 setwd("C:/Users/nazaretl.sfb649/Documents/GitHub/MMSTAT-ToDo/MMSTATconfi_pi") # windows

source("MMSTAThelper_function.r")

############################### SUBROUTINES ##################################
### server ###################################################################

mmstat$vartype = "binvars"
shiny = sessionInfo()$otherPkgs$shiny
mmstat.ui.elem("conflevel", "confidenceLevel")
mmstat.ui.elem("go", "drawSample")
mmstat.ui.elem("reset", "actionButton", label = gettext("Reset"), value = 0)
mmstat.ui.elem("speed", "speedSlider")
mmstat.ui.elem("dataset", "dataSet", 
               choices = mmstat.getDataNames("TITANIC", 
                                             "CREDIT", 
                                             "BOSTONHOUSING"))
mmstat.ui.elem("variable", "variable1", vartype = "binary")
mmstat.ui.elem("size", "sampleSize")
mmstat.ui.elem("cex", "fontSize")

confint = vector("list", 0)

CIpi = function(x, oma, xlim = NULL) {
  n     = length(x) 
  pi    = prop.table(table(x))[1]
  dist  = qnorm(1 - (1 - oma)/2) * sqrt(pi * (1 - pi)/n)
  upper = pi + dist
  lower = pi - dist
  if (is.null(xlim)) {
    lim = c(lower, upper)
  } else {
    lim = mmstat.merge(xlim, c(lower, upper))
  }
  list(upper = upper, lower = lower, pi = pi, oma = oma, n = n, xlim = lim)
}

drawVariableValues = function(x, jitter, ylim, 
                              box.param    = NULL, 
                              points.param = NULL, 
                              param        = NULL) {
  if (is.numeric(x) || is.integer(x)) {
    if (is.list(points.param) || is.null(points.param) || points.param) {
      points.param$x = x
      points.param$y = ylim[1] + diff(ylim) * jitter
      suppressWarnings(do.call("points", points.param))
    }
    if (is.list(box.param) || is.null(box.param) || box.param) {
      q                 = quantile(x, c(0.25, 0.5, 0.75))
      box.param$xleft   = q[1]
      box.param$xright  = q[3]
      box.param$ybottom = ylim[1]
      box.param$ytop    = ylim[2]
      suppressWarnings(do.call("rect", box.param))
      box.param$x       = c(q[2], q[2])
      box.param$y       = ylim
      if (!is.null(box.param$border)) 
        box.param$col = box.param$border
      suppressWarnings(do.call("lines", box.param))
    }
  }
  if (is.factor(x)) {
    tab = prop.table(table(x))
    xp  = as.numeric(c(0, cumsum(tab)))
    for (i in seq(tab)) {
      args          = param
      args$xleft    = xp[i]
      args$xright   = xp[i + 1]
      args$ybottom  = ylim[1]
      args$ytop     = ylim[2]
      args$col      = mmstat$col[[i + param$col]]
      suppressWarnings(do.call("rect", args))
      if (!is.null(param$level) && param$level) {
        args        = param
        args$x      = mean((xp[i:(i + 1)]))
        args$y      = mean(ylim)
        args$labels = gettext(names(tab)[i])
        suppressWarnings(do.call("text", args))
      }
    }
  }
}

server = shinyServer(function(input, output, session) {
  
  output$conflevelUI = renderUI({
    mmstat.ui.call("conflevel")
  })
  output$goUI = renderUI({
    mmstat.ui.call("go")
  })
  output$resetUI = renderUI({
    mmstat.ui.call("reset")
  })
  output$speedUI = renderUI({
    mmstat.ui.call("speed")
  })
  output$datasetUI = renderUI({
    mmstat.ui.call("dataset")
  })
  output$cexUI = renderUI({
    mmstat.ui.call("cex")
  })
  output$variableUI = renderUI({
    inp = mmstat.getValues(NULL, dataset = input$dataset)
    mmstat.ui.call("variable", 
                   choices = mmstat.getVarNames(inp$dataset, "binary"))
  })
  output$sizeUI = renderUI({
    var = getVar()
    mmstat.ui.call("size", ticks = var$ticks, max = length(var$ticks))
  })
  
  getVar = reactive({
    inp = mmstat.getValues(NULL, 
                           dataset  = isolate(input$dataset), 
                           variable = input$variable)
    var = mmstat.getVar(inp$dataset, inp$variable)
    var$ticks = mmstat.ticks(var$n, nmin = 30)
    var
  })
  
  getSize = reactive({
    var = getVar()
    inp = mmstat.getValues(NULL, size = input$size)
    var$ticks[inp$size]
  })
  
  resetCI = reactive({
    input$reset
    confint <<- vector("list", 0)
  })
  
  drawSample = reactive({
    mmstat.log(sprintf("drawSample"))
    inp = mmstat.getValues(NULL, 
                           go        = input$go, 
                           reset     = input$reset, 
                           speed     = input$speed, 
                           conflevel = input$conflevel)
    if (inp$speed > 0) 
      invalidateLater(500 / inp$speed, session)
      var    = getVar()
      index  = sample(length(var$values), size = getSize(), replace = T)
      sample = var$values[index]
      nci    = length(confint)
    if (nci > 0) 
      xlim = confint[[nci]]$xlim else xlim = NULL
      confint[[nci + 1]] <<- CIpi(sample, mmstat$UI$conflevel$ticks[inp$conflevel]/100, 
                                xlim)
      nci  = length(confint)
      index
  })
  
  output$outputConfPlot = renderPlot({
    mmstat.log(sprintf("outputConfPlot"))
    resetCI()
    var   = getVar()
    index = drawSample()
    inp   = mmstat.getValues(NULL, cex = input$cex)
    nci   = length(confint)
    if (nci) {
      par(mar = c(2, 0, 2, 0))
      plot(c(0, 1), c(0, 0), 
           type     = "n", 
           ylim     = c(1.5, 2 + 0.2 * nci), 
           axes     = F, 
           cex      = 0.5, 
           col      = mmstat$col[[1]], 
           main     = sprintf(gettext("%i confidence interval(s)"), nci), 
           cex.axis = inp$cex, 
           cex.lab  = inp$cex, 
           cex.main = 1.2 * inp$cex, 
           cex.sub  = inp$cex)
      text(var$prop[1], 1.5, sprintf("%.3f", var$prop[1]), 
           col = mmstat$col[[1]], 
           pos = 4, 
           cex = inp$cex)
      
      usr  = par("usr")
      mmstat.axis(1, usr[1:2], cex.axis = inp$cex)
      seqi = 1:nci
      lwd  = 2 - (nci > 10)
      for (i in seqi) {
        yi  = 1.95 + 0.2 * i
        rng = c(confint[[i]]$lower, confint[[i]]$upper)
        col = ifelse((rng[1] - var$prop[1]) > 0 || (rng[2] - var$prop[1]) < 
                      0, "black", mmstat$col[[2]])
        lines(rng, c(yi, yi), lwd = lwd, lty = "solid", col = col)
        lines(c(rng[1], rng[1]), c(yi - 0.05, yi + 0.05), 
              lwd = lwd, 
              lty = "solid", 
              col = col)
        lines(c(rng[2], rng[2]), c(yi - 0.05, yi + 0.05), 
              lwd = lwd, 
              lty = "solid", 
              col = col)
        lines(c(confint[[i]]$pi, confint[[i]]$pi), c(yi - 0.05, yi + 0.05), 
              lwd = lwd, 
              lty = "solid", 
              col = col)
      }
      size  = sapply(confint, "[[", "n")
      index = 1 + c(0, which(diff(size) != 0))
      posx  = mmstat.pos(usr[1:2], c(0.05, 0.95))
      text(posx[1], 1.95 + 0.2 * index, 
           labels = sprintf("%.0f", size[index]), 
           cex    = inp$cex)
           oma    = sapply(confint, "[[", "oma")
           index  = 1 + c(0, which(diff(oma) != 0))
      text(posx[2], 1.95 + 0.2 * index, 
           labels = sprintf("%.3f", oma[index]), 
           cex    = inp$cex)
      axis(3, 
           at       = posx, 
           labels   = c(expression("n"), expression(alpha)), 
           cex.axis = inp$cex)
      abline(v = var$prop[1], col = mmstat$col[[1]], lwd = 3, lty = "dotted")
      
      box()
    }
  })
  
  output$outputSamplePlot = renderPlot({
    mmstat.log(sprintf("outputSamplePlot"))
    var   = getVar()
    index = drawSample()
    inp   = mmstat.getValues(NULL, cex = input$cex)
    par(mar = c(5, 0, 2, 0))
    plot(c(0, 1), c(-0.05, 1),
         type     = "n", 
         axes     = F, 
         main     = gettext("Population and sample"), 
         xlab     = var$xlab, 
         sub      = var$sub, 
         cex.axis = inp$cex, 
         cex.lab  = inp$cex, 
         cex.main = 1.2 * inp$cex, 
         cex.sub  = inp$cex)
    usr = par("usr")
    mmstat.axis(1, usr[1:2], cex.axis = inp$cex)
    drawVariableValues(var$values, 
                       jitter = NULL, 
                       ylim   = c(0, 0.45), 
                       param  = list(cex    = inp$cex, 
                                     col    = 0, 
                                     border = NA, 
                                     level  = T))
    drawVariableValues(var$values[index], 
                       jitter = NULL, 
                       ylim   = 0.5 + c(0, 0.45 * sqrt(length(index)/var$n)), 
                       param  = list(cex    = inp$cex, 
                                     col    = 8, 
                                     border = NA, 
                                     level  = F))
    
    box()
  })
  
  output$logText = renderText({
    mmstat.getLog(session)
  })
})

############################### SUBROUTINES ##################################
### ui #######################################################################

ui = shinyUI(fluidPage(
    
  div(class="navbar navbar-static-top",
      div(class = "navbar-inner", 
          fluidRow(column(4, div(class = "brand pull-left", 
                                 gettext("Confidence intervals for the proportion"))),
                   column(2, checkboxInput("showtest", 
                                           gettext("Confidence interval parameter"),
                                           TRUE)),
                   column(2, checkboxInput("showsample", 
                                           gettext("Sample drawing"), 
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
        condition = 'input.showtest',
        uiOutput("conflevelUI"),
        br(),
        uiOutput("sizeUI")
      ),
      conditionalPanel(
        condition = 'input.showsample',
        hr(),
        uiOutput("goUI"),
        uiOutput("resetUI"),
        uiOutput("speedUI")
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
  mainPanel(plotOutput("outputConfPlot"),
            plotOutput("outputSamplePlot", height = "200px"))),

  htmlOutput("logText")
  )
)

############################### SUBROUTINES ##################################
### shinyApp #################################################################

shinyApp(ui = ui, server = server)
