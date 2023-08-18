library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(hablar)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(GGally)

require(betareg)

# Load data:
load("load_vars.RData")
options(shiny.fullstacktrace=TRUE)
options(shiny.autoreload=TRUE)

# Define UI
header <- dashboardHeader(
  title = "",
  titleWidth = 230
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home",
             tabName = "home",
             icon = icon("house")),
    menuItem("Model predictions",
             tabName = "3dplot",
             icon = icon("chart-line")),
    menuItem("Quality metric comparisons",
             tabName = "multiplot",
             icon = icon("chart-bar"))
  )
)

homepage_content = "## An overlooked phenomenon: complex interactions of potential error sources on the quality of bacterial *de novo* genome assemblies

Rádai et al., 2023 (doi...)

**Online supplementary material** for visualizing the results of multiplicative models, in which the effects of error sources (error rate, sequencing depth, optical duplacte ratio, and PCR duplicate ratio) are assessed on genome assembly quality metrics.

### Model predictions

3D scatterplot visualization of model predictions on quality metric values, across a defined range of sequencing depth and error rate values, and at defined optical and PCR duplicate ratio values. Predicted quality metric values can ba drawn separately for each bacterial species used in the analyses.

The \"best\" observed value highlighted in the plot title refers to those values that represented the highest quality in the given set of assemblies for the given metric, in the given bacterium.

### Quality metric comparisons

Pairwise comparisons of quality metric values, across the defined range of sequencing depth and error rate, and at defined values of optical and PCR duplicate ratios. (Rendering of these figures when comparing 3 or more metrics may take some time, because for each quality metric the prediction values have to be calculated separately.)

**NOTE:** visualized results serve the purpose of helping interpretation of complex effects arising from interactions of the assessed sample parameters (error sources), model parameter estimates and model predictions should be interpreted with caution, and be taken as broad guidelines towards understanding the modeled associations, rather than as accurate estimations (see article Discussion)!

"

body <- dashboardBody(
  
  tabItems(
    
    # main page
    tabItem(
      tabName = "home",
      markdown(homepage_content)
    ),
    
    # 3D plot
    tabItem(tabName = "3dplot",
      fluidRow(
        column(width=4,
          selectInput("bacterium_3d", "Bacterium:", choices = unique(bacs$species)),
          selectInput("qualityMetric_3d", "Quality metric:", choices = unique(qvar.labels$label)),
          sliderInput(inputId = "minCov_3d", label = "Min sequencing depth:", min = 1, max = 150, value = 25, step = 1),
          sliderInput(inputId = "maxCov_3d", label = "Max sequencing depth:", min = 1, max = 150, value = 150, step = 1),
          numericInput("numCov_3d", "Number of values for sequencing depth:", value = 10),
          sliderInput(inputId = "minErr_3d", label = "Min error rate:", min = 0, max = 1, value = 0, step = 0.01),
          sliderInput(inputId = "maxErr_3d", label = "Max error rate:", min = 0, max = 1, value = 0.05, step = 0.01),
          numericInput("numErr_3d", "Number of values for error rate:", value = 10),
          sliderInput(inputId = "opticalDuplicateRatio_3d", label = "Optical duplicate ratio:", min = 0, max = 1, value = 0, 0.01),
          sliderInput(inputId = "pcrDuplicateRatio_3d", label = "PCR duplicate ratio:", min = 0, max = 1, value = 0, 0.01),

          actionButton("plotBtn_3d", "Generate plot")
        ),
        ##### Plotting ####
        column(
          width = 8,
          box(
            title = "Model predictions",
            status = "primary",
            width = NULL,
            plotlyOutput(
              "plot_out_3d",
              height = "600px"
              #height = "31.6em"
            )
          )
        )
      )
    ),
    
    tabItem(tabName = "multiplot",
      fluidRow(
        column(width=4,
          selectInput("bacterium_MP", "Bacterium:", choices = unique(bacs$species)),
          selectInput(
            "qualityMetrics_MP", "Quality metric:", 
            choices = unique(qvar.labels$label), multiple = T, selected = qvar.labels$label[qvar.labels$qvar %in% c("contigs.all", "NG50")]),
          sliderInput(inputId = "minCov_MP", label = "Min sequencing depth:", min = 1, max = 150, value = 25, step = 1),
          sliderInput(inputId = "maxCov_MP", label = "Max sequencing depth:", min = 1, max = 150, value = 150, step = 1),
          numericInput("numCov_MP", "Number of values for sequencing depth:", value = 10),
          sliderInput(inputId = "minErr_MP", label = "Min error rate:", min = 0, max = 1, value = 0, step = 0.01),
          sliderInput(inputId = "maxErr_MP", label = "Max error rate:", min = 0, max = 1, value = 0.05, step = 0.01),
          numericInput("numErr_MP", "Number of values for error rate:", value = 10),
          
          sliderInput(inputId = "opticalDuplicateRatio_MP", label = "Optical duplicate ratio:", min = 0, max = 1, value = 0, 0.01),
          sliderInput(inputId = "pcrDuplicateRatio_MP", label = "PCR duplicate ratio:", min = 0, max = 1, value = 0, 0.01),

          actionButton("plotBtn_mp", "Generate plot")
        ),
        ##### Plotting ####
        column(
          width = 8,
          box(
            title = "Quality metric comparisons",
            status = "primary",
            #solidHeader = TRUE,
            width = NULL,
            plotlyOutput(
              "plot_out_MP",
              height = "600px"
            )
          )
        )
      )
    )
  )
)


## DashboardPage ####
ui <- dashboardPage(
  title = "Interactive online Shiny application",
  header,
  sidebar,
  body
)

# Define server
server <- function(input, output) {
  
  # Generate 3D scatterplot
  observeEvent(input$plotBtn_3d, {
    
    # create data for plot
    b <- bacs$working.ID[bacs$species == input$bacterium_3d]
    y <- qvar.labels$qvar[qvar.labels$label == input$qualityMetric_3d]
    pdup <- input$pcrDuplicateRatio_3d
    odup <- input$opticalDuplicateRatio_3d
    cov.range <- seq(input$minCov_3d, input$maxCov_3d, length.out = input$numCov_3d)
    err.range <- seq(input$minErr_3d, input$maxErr_3d, length.out = input$numErr_3d)
    
    qvar.modifier <- c(-1, -1, -1, 1, 1, 1, -1, -1, -1, -1, -1, -1, 1, 1)
    names(qvar.modifier) <- qvar.labels$qvar
    
    # 3D Scatterplot code
    m <- models.mult[[paste0(b, "_", y)]]
    
    # build data table for prediction
    rr <- expand.grid(cov=cov.range, err=err.range)
    rr.2 <- cbind(
      rr,
      data.frame(
        odup = odup,
        pdup = pdup
      )
    )
    
    for(x in colnames(rr.2)){
      rr.2[,paste0(x, ".rsc")] <- (rr.2[, x] - mean(asq[, x], na.rm=T))/sd(asq[, x], na.rm=T)
    }
    preds.df <- cbind(
      rr.2,
      data.frame(pred.y = predict(m, type = "response", newdata = rr.2))
    )
    
    # the prediction iteslf is still in the [0,1] standard scale,
    # so we have to back-transform it to the original scale
    N_y <- length(na.omit(asq[, y]))
    preds.df$pred.y.backtransformed <- (preds.df$pred.y * N_y - 0.5)/(N_y - 1)
    preds.df$pred.y.backtransformed <- preds.df$pred.y.backtransformed * max(asq[, y], na.rm=T)
    
    # for informative purposes, visualize how the predicted values are associated to
    # the "best" values of the original data;
    # the twist is that for some metrics, low values are considered good,
    # while for others the large values
    if(qvar.modifier[y] == (-1) ){
      best.y <- min(asq[asq$bact.ID==b, y], na.rm=T)
      preds.df$py.to.best.ratio <- preds.df$pred.y.backtransformed/best.y
    }else{
      best.y <- max(asq[asq$bact.ID==b, y], na.rm=T)
      preds.df$py.to.best.ratio <- preds.df$pred.y.backtransformed/best.y
    }
    preds.df$txt <- paste0("predicted ", tolower(qvar.labels$label[qvar.labels$qvar==y]), ": ", round(preds.df$pred.y.backtransformed, 3))
    preds.df$predicted <- preds.df$pred.y.backtransformed
    
    pl.3d <- plot_ly(preds.df, x = ~cov, y = ~err, z = ~predicted, color = ~predicted, text = ~txt)
    pl.3d <- pl.3d %>%
      layout(
        title = paste0(
          qvar.labels$label[qvar.labels$qvar==y], 
          " (", bacs$species[bacs$working.ID==b], ")", 
          "\n", 'observed "best" value: ', best.y,
          "\n", "ODUP = ", odup,", PDUP = ", pdup
        ),
        scene = list(
          xaxis = list(title = "sequencing depth (x)"),
          yaxis = list(title = "error rate (y)"),
          zaxis = list(title = "predicted value (z)"),
          camera = list(
            eye = list(x = -1.5, y = -1, z = 0.65)
          ),
          aspectratio = list(x = 0.65, y = 0.65, z = 0.65)
        )
      )
    
    # generate & return plot
    output$plot_out_3d <- renderPlotly({
      pl.3d
    })
    
  })

  # Generate multi-plot
  observeEvent(input$plotBtn_mp, {
    
    yy <- qvar.labels$qvar[qvar.labels$label %in% input$qualityMetrics_MP]
    b <- bacs$working.ID[bacs$species == input$bacterium_MP]
    pdup <- input$pcrDuplicateRatio_MP
    odup <- input$opticalDuplicateRatio_MP
    cov.range <- seq(input$minCov_MP, input$maxCov_MP, length.out = input$numCov_MP)
    err.range <- seq(input$minErr_MP, input$maxErr_MP, length.out = input$numErr_MP)
    rr <- expand.grid(cov=cov.range, err=err.range)
    rr.2 <- cbind(
      rr,
      data.frame(
        odup = odup,
        pdup = pdup
      )
    )
    for(x in colnames(rr.2)){
      rr.2[,paste0(x, ".rsc")] <- (rr.2[, x] - mean(asq[, x], na.rm=T))/sd(asq[, x], na.rm=T)
    }
    
    for(y in yy){
      
      m <- models.mult[[paste0(b, "_", y)]]
      py <- predict(m, type = "response", newdata = rr.2)
      py <- (py*length(resid(m)) - 0.5)/(length(resid(m))-1)
      py <- py * max(asq[asq$bact.ID==b, y], na.rm=T)
      
      preds.df <- cbind(
        rr.2,
        data.frame(pred.y = py)
      )
      
      N_y <- length(na.omit(asq[, y]))
      
      preds.df$y <- y
      
      if(y==yy[1]){
        multi.preds <- preds.df
      }else{
        multi.preds <- rbind(
          multi.preds,
          preds.df
        )
      }
      
    }
    k <- 0
    for(y in yy){
      if(k==0){
        mpdf <- eval(parse(text=paste0("data.frame(", y, "=multi.preds$pred.y[multi.preds$y==y])")))
      }else{
        dy <- eval(parse(text=paste0("data.frame(", y, "=multi.preds$pred.y[multi.preds$y==y])")))
        mpdf <- cbind(mpdf, dy)
      }
      k <- k+1
    }
    
    df.to.pairplot <- cbind(mpdf, rr.2)
    df.to.pairplot$cov <- factor(x = round(df.to.pairplot$cov), levels = sort(unique(round(df.to.pairplot$cov))))
    df.to.pairplot$err <- factor(x = round(df.to.pairplot$err, 2), levels = sort(unique(round(df.to.pairplot$err, 2))))
    for(x in colnames(rr)){
      colnames(df.to.pairplot)[colnames(df.to.pairplot) %in% x] <- evar.labels$label[evar.labels$evar==x]
    }
    yy.labels <- yy
    for(y in colnames(mpdf)){
      colnames(df.to.pairplot)[colnames(df.to.pairplot) %in% y] <- qvar.labels$label[qvar.labels$qvar==y]
      yy.labels[yy.labels==y] <- qvar.labels$label[qvar.labels$qvar==y]
    }
    g.pairs <- ggpairs(df.to.pairplot, aes(color = `Sequencing depth`, label = `Error rate`), columns = yy.labels) +
      theme_bw(base_size = 8)
    
    output$plot_out_MP <- renderPlotly({
      ggplotly(g.pairs)
    })
    
  })
}

# Run the application
options(shiny.host = '127.0.0.1')
options(shiny.port = 6186)
shinyApp(ui = ui, server = server)

