# ------------------------------------------------------------------------------
# Name of Quantlet: MMSTATdistribution_exponential
# ------------------------------------------------------------------------------
# Published in:     MMSTAT
# ------------------------------------------------------------------------------
# Description:      Shows the pdf and cdf of the exponential distribution. The parameter lambda can be chosen by the user.
#                   One can also interactively plot the normal distribution with exchangeable mean and variance.
# ------------------------------------------------------------------------------
# Keywords:         distribution, plot, cdf, pdf, exponential, visualization, parameter, interactive, parametric,
#                   normal, normal-distribution, mean, variance, density
# ------------------------------------------------------------------------------
# Usage:            MMSTAThelper_function
# ------------------------------------------------------------------------------
# Output:           Interactive user choice.
# ------------------------------------------------------------------------------
# Example:          Shows the density of the exponential distribution with lambda = 1.
# ------------------------------------------------------------------------------
# See also:         clt, BCS_Weibull, mixexprnd, BCS_ExpPdfCdf, MMSTATtime_series_1, MMSTATlinreg, MMSTATconfmean, 
#                   MMSTATconfi_sigma, MMSTATassociation, MMSTAThelper_function
# ------------------------------------------------------------------------------
# Author :          Sigbert Klinke
# ------------------------------------------------------------------------------
# Code Editor:      Yafei Xu
# ------------------------------------------------------------------------------

# please use "Esc" key to jump out of the Shiny app
rm(list = ls(all = TRUE))
graphics.off()

# please set working directory setwd('C:/...') 
# setwd('~/...')    # linux/mac os
setwd("C:/Users/nazaretl.sfb649/Documents/GitHub/MMSTAT-ToDo/MMSTATdistribution_exponential") # windows

source("MMSTAThelper_function.r")

############################### SUBROUTINES ##################################
### assistant function #######################################################

mmstat.getValues = function (local, ...) {
  ret <<- list(...)
  for (name in names(ret)) {
    if (is.null(ret[[name]]) || (length(ret[[name]]) == 0)) {
      if (is.null(local[[name]])) {
        stopif (is.null(mmstat$UI[[name]]$value), 
                        paste0('no default value(s) for "', name, '"'))
        ret[[name]] = mmstat$UI[[name]]$value
      } else {
        ret[[name]] = local[[name]]
      }
    }
  }
  ret
}

############################### SUBROUTINES ##################################
### server ###################################################################

gv     = list(dist = "mmstat", xlim = NULL, ylim = NULL)
distrc = gettext(c("EXP", "NORM"), "name")
distrd = "EXP"
radioc = gettext(c("PDF", "CDF"), "name")

mmstat$UI = list(distribution = list(inputId  = "distribution", 
                                     label    = gettext("Select a probability 
                                                        distribution"), 
                                     choices  = distrc, 
                                     selected = distrd, 
                                     value    = distrd), 
                 exp.lambda   = list(inputId = "exp.lambda", 
                                     label   = HTML(gettext("Parameter (&lambda;)")), 
                                     min     = 0.1, 
                                     max     = 5, 
                                     value   = 1, 
                                     step    = 0.1), 
                 norm.mu      = list(inputId = "norm.mu", 
                                     label   = HTML(gettext("Mean (&mu;)")), 
                                     min     = -3, 
                                     max     = +3, 
                                     value   = 0, 
                                     step    = 0.1), 
                 norm.sigma2  = list(inputId = "norm.sigma2", 
                                     label   = HTML(gettext("Variance (&sigma;<sup>2</sup>)")), 
                                     min     = 0.1, 
                                     max     = 5, 
                                     value   = 1, 
                                     step    = 0.1), 
                 pdforcdf     = list(inputId = "pdforcdf", 
                                     label   = gettext("Show"), 
                                     choices = radioc, 
                                     value   = "PDF"), 
                 refit        = list(inputId = "refit", 
                                     label   = gettext("Refit axes")), 
                                     cex     = list(inputId = "cex", 
                                     label   = gettext("Font size"),
                                     min     = 1, 
                                     max     = 1.5, 
                                     step    = 0.05, 
                                     value   = 1))

server = shinyServer(function(input, output, session) {
  
  output$EXPUI = renderUI({
    HTML(mmstat.html(gettext("INTRO_EXP")))
  })
  
  output$NORMUI = renderUI({
    HTML(mmstat.html(gettext("INTRO_NORM")))
  })
  
  output$exp.lambdaUI = renderUI({
    mmstat.log("exp.lambdaUI")
    args = mmstat$UI$exp.lambda
    do.call("sliderInput", args)
  })
  
  output$norm.muUI = renderUI({
    mmstat.log("norm.muUI")
    args = mmstat$UI$norm.mu
    do.call("sliderInput", args)
  })
  
  output$norm.sigma2UI = renderUI({
    mmstat.log("norm.sigma2UI")
    args = mmstat$UI$norm.sigma2
    do.call("sliderInput", args)
  })
  
  output$distributionUI = renderUI({
    mmstat.log("distributionUI")
    args = mmstat$UI$distribution
    args$value = NULL
    do.call("selectInput", args)
  })
  
  output$pdforcdfUI = renderUI({
    args = mmstat$UI$pdforcdf
    args$value = NULL
    do.call("radioButtons", args)
  })
  
  output$refitUI = renderUI({
    args = mmstat$UI$refit
    do.call("actionButton", args)
  })
  
  output$cexUI = renderUI({
    mmstat.log("cexUI")
    args = mmstat$UI$cex
    do.call("sliderInput", args)
  })
  
  refit = reactive({
    input$refit
    gv$dist <<- "mmstat"
  })
  
  output$distPlot = renderPlot({
    refit()
    inp = mmstat.getValues(NULL, 
                           cex          = input$cex, 
                           distribution = input$distribution, 
                           exp.lambda   = input$exp.lambda, 
                           norm.sigma2  = input$norm.sigma2, 
                           norm.mu      = input$norm.mu, 
                           pdforcdf     = input$pdforcdf)
    
    if (gv$dist != inp$distribution) {
      if ((gv$dist == "NORM") && (inp$distribution == "EXP")) {
        # switch from NORM to EXP
        updateSliderInput(session, inputId = "exp.lambda", value = inp$norm.sigma2)
      }
      if ((gv$dist == "EXP") && (inp$distribution == "NORM")) {
        # switch from EXP to NORM
        updateSliderInput(session, inputId = "norm.sigma2", value = inp$exp.lambda)
        updateSliderInput(session, inputId = "norm.mu", value = 0)
      }
      gv$xlim <<- c(0, 0)
      gv$ylim <<- c(0, 0)
      gv$dist <<- inp$distribution
    }
    switch(inp$distribution, EXP = {
      gv$xlim <<- mmstat.merge(gv$xlim, c(0, qexp(0.999, inp$exp.lambda)))
      x = (0:300)/300 * gv$xlim[2]
      if (inp$pdforcdf == "PDF") {
        height = dexp(x, inp$exp.lambda)
        gv$ylim <<- mmstat.merge(gv$ylim, c(0, height))
        plot(x, height, 
             type     = "l", 
             xlim     = gv$xlim, 
             ylim     = gv$ylim, 
             ylab     = "", 
             xlab     = "x", 
             main     = mmstat.math(sprintf(gettext("Density function f(x) of EX(&lambda;=%.1f)"), 
                                            inp$exp.lambda)), 
             cex.axis = inp$cex, 
             cex.lab  = inp$cex, 
             cex.main = 1.2 * inp$cex, 
             cex.sub  = inp$cex)
      } else {
        gv$ylim <<- mmstat.merge(gv$ylim, c(0, 1))
        height = pexp(x, inp$exp.lambda)
        plot(x, height, 
             type     = "l", 
             xlim     = gv$xlim, 
             ylim     = gv$ylim, 
             ylab     = "", 
             xlab     = "x", 
             main     = mmstat.math(sprintf(gettext("Cumulative distribution function F(x) of EX(&lambda;=%.1f)"), 
                                            inp$exp.lambda)), 
             cex.axis = inp$cex, 
             cex.lab  = inp$cex, 
             cex.main = 1.2 * inp$cex, 
             cex.sub  = inp$cex)
        abline(h = 0, col = "gray70", lty = "dashed")
        abline(h = 1, col = "gray70", lty = "dashed")
      }
    }, NORM = {
      sd = sqrt(inp$norm.sigma2)
      gv$xlim <<- mmstat.merge(gv$xlim, c(qnorm(0.001, inp$norm.mu, sd), 
                                          qnorm(0.999, inp$norm.mu, sd)))
      x = gv$xlim[1] + (0:300)/300 * diff(gv$xlim)
      if (inp$pdforcdf == "PDF") {
        height = dnorm(x, inp$norm.mu, sd)
        gv$ylim <<- mmstat.merge(gv$ylim, c(0, height))
        plot(x, height, 
             type     = "l", 
             xlim     = gv$xlim, 
             ylim     = gv$ylim, 
             ylab     = "", 
             xlab     = "x", 
             main     = mmstat.math(sprintf(gettext("Density function f(x) of N(&mu;=%.1f,&sigma^2;=%.1f)"), 
                                            inp$norm.mu, 
                                            inp$norm.sigma2)), 
             cex.axis = inp$cex, 
             cex.lab  = inp$cex, 
             cex.main = 1.2 * inp$cex, 
             cex.sub  = inp$cex)
        abline(v = 0, col = "gray70")
      } else {
        gv$ylim <<- mmstat.merge(gv$ylim, c(0, 1))
        height = pnorm(x, inp$norm.mu, sd)
        plot(x, height, 
             type     = "l", 
             xlim     = gv$xlim, 
             ylim     = gv$ylim, 
             ylab     = "", 
             xlab     = "x", 
             main     = mmstat.math(sprintf(gettext("Cumulative distribution function F(x) of N(&mu;=%.1f,&sigma^2;=%.1f)"), 
                                            inp$norm.mu, 
                                            inp$norm.sigma2)), 
             cex.axis = inp$cex, 
             cex.lab  = inp$cex, 
             cex.main = 1.2 * inp$cex,
             cex.sub  = inp$cex)
        abline(h = 0, col = "gray70", lty = "dashed")
        abline(h = 1, col = "gray70", lty = "dashed")
      }
    })
  })
  
  output$logText = renderText({
    mmstat.getLog(session)
  })
})

############################### SUBROUTINES ##################################
### ui #######################################################################

ui = shinyUI(fluidPage(
  
  div(class = "navbar navbar-static-top",
      div(class = "navbar-inner", 
          fluidRow(column(4, div(class = "brand pull-left", 
                                 gettext("Continuous probability 
                                         distributions"))),
                   column(2, checkboxInput("showdist", 
                                           gettext("Choose distribution"), 
                                           TRUE)),
                   column(2, checkboxInput("showparameter", 
                                           gettext("Distribution parameter"), 
                                           TRUE)),
                   column(2, checkboxInput("showfunction", 
                                           gettext("Choose function"), 
                                           TRUE)),
                   column(2, checkboxInput("showoptions", 
                                           gettext("Options"), 
                                           FALSE))))),
    

    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition = 'input.showdist',
          uiOutput("distributionUI")
        ),
        conditionalPanel(
          condition = "input.distribution=='EXP'",
          conditionalPanel(
            condition = 'input.showparameter',
            uiOutput("exp.lambdaUI")
          )
        ),
        conditionalPanel(
          condition = "input.distribution=='NORM'",
          conditionalPanel(
            condition = 'input.showparameter',
            uiOutput("norm.muUI"),
            uiOutput("norm.sigma2UI")
          )
        ),
        conditionalPanel(
          condition = 'input.showfunction',
          br(),
          uiOutput("pdforcdfUI"),
          uiOutput("refitUI")
        ),
        conditionalPanel(
          condition = 'input.showoptions',
          hr(),
          uiOutput("cexUI")
        )        
      ),
 
      mainPanel(plotOutput("distPlot"))
      ),

      htmlOutput("logText")
  ))

############################### SUBROUTINES ##################################
### shinyApp #################################################################

shinyApp(ui = ui, server = server)
