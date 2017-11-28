#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyTime)
library(simmer)
library(reshape2)
library(dplyr)
library(simmer.plot)
source("module.R")
source("simmer-module.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  original <- callModule(generateMu, "original", reactive(input$go), reactive(input$mu), reactive(input$lambda))
  originalk <- callModule(generateMu, "originalk", reactive(input$go), reactive(input$mu), reactive(input$lambda))
  originalg <- callModule(generateMu, "originalg", reactive(input$go), reactive(input$mu), reactive(input$lambda))
  originalgk <- callModule(generateMu, "originalgk", reactive(input$go), reactive(input$mu), reactive(input$lambda))
  
  cola_caja <- callModule(formulae, "cola_caja", reactive(input$go), reactive(input$mu), reactive(input$lambda))  
  cola_caja_mmk <- callModule(mmk, "cola_caja_mmk", reactive(input$go), reactive(input$mu), reactive(input$lambda), reactive(input$prepper))
  cola_caja_mmg <- callModule(mmg, "cola_caja_mmg", reactive(input$go), reactive(input$mu), reactive(input$lambda), reactive(input$mu_sd))
  cola_caja_mmgk <- callModule(mgk, "cola_caja_mmgk", reactive(input$go), reactive(input$mu), reactive(input$lambda), reactive(input$prepper), reactive(input$mu_sd))
  
  observeEvent(input$go, {
    # Simmer para mm1
    df <- callModule(simmergenerator, "plot-mm1", reactive(input$lambda), reactive(input$lambda_sd), reactive(input$mu), reactive(input$mu_sd), reactive(input$prepper), reactive(input$time_init), reactive(input$time_unit))
    mu_l <- callModule(datafr, "simmer-mm1", reactive(df()), reactive(input$time_init), reactive(input$time_unit))
    
    output$server_plot <- renderPlot({
      plot(df(), "resources", "usage", "servidor", items="system")
    })
    
    mu1_sim <- mean(mu_l()$Mu1, na.rm = TRUE)
    lambda_sim <- mean(mu_l()$Lambda, na.rm = TRUE)
    
    simulado <- callModule(generateMu, "simulado", reactive(input$go), reactive(mu1_sim), reactive(lambda_sim))
    
    cola_caja_simulado <- callModule(formulae, "cola_caja_simulado", reactive(input$go), reactive(mu1_sim), reactive(lambda_sim))
    
    
    output$cambio_lambda <- renderText({
      cambio <- round(((input$lambda - lambda_sim)/input$lambda) * 100, digits = 2)
      paste("Cambio en lambda: ",cambio,"%")
      
    })
    output$cambio_mu1 <- renderText({
      cambio <- round(((input$mu - mu1_sim)/input$mu) * 100, digits = 2)
      paste("Cambio en mu1: ",cambio,"%")
      
    })
    
    
    # Simmer para mmk
    
    dfk <- callModule(simmergenerator, "plot-mmk", reactive(input$lambda), reactive(input$lambda_sd), reactive(input$mu), reactive(input$mu_sd), reactive(input$prepper), reactive(input$time_init), reactive(input$time_unit))
    mu_lk <- callModule(datafr, "simmer-mmk", reactive(df()), reactive(input$time_init), reactive(input$time_unit))
    
    output$server_plotk <- renderPlot({
      plot(df(), "resources", "usage", "servidor", items="system")
    })
    
    mu1_simk <- mean(mu_lk()$Mu1, na.rm = TRUE)
    lambda_simk <- mean(mu_lk()$Lambda, na.rm = TRUE)
    
    cola_caja_mmk_sim <- callModule(mmk, "cola_caja_mmk_sim", reactive(input$go), reactive(mu1_simk), reactive(lambda_simk), reactive(input$prepper))
    
    simulado_mmk <- callModule(generateMu, "simulado_mmk", reactive(input$go), reactive(mu1_simk), reactive(lambda_simk))
    
    
    output$cambio_lambdak <- renderText({
      cambio <- round(((input$lambda - lambda_simk)/input$lambda) * 100, digits = 2)
      paste("Cambio en lambda: ",cambio,"%")
      
    })
    output$cambio_mu1k <- renderText({
      cambio <- round(((input$mu - mu1_simk)/input$mu) * 100, digits = 2)
      paste("Cambio en mu1: ",cambio,"%")
      
    })
    
    # Simmer para mg1
    
    dfg <- callModule(simmergenerator, "plot-mmg", reactive(input$lambda), reactive(input$lambda_sd), reactive(input$mu), reactive(input$mu_sd), reactive(input$prepper), reactive(input$time_init), reactive(input$time_unit))
    mu_lg <- callModule(datafr, "simmer-mmg", reactive(df()), reactive(input$time_init), reactive(input$time_unit))
    
    output$server_plotg <- renderPlot({
      plot(dfg(), "resources", "usage", "servidor", items="system")
    })
    
    mu1_simg <- mean(mu_lg()$Mu1, na.rm = TRUE)
    lambda_simg <- mean(mu_lg()$Lambda, na.rm = TRUE)
    
    cola_caja_mmg_sim <- callModule(mmg, "cola_caja_mmg_sim", reactive(input$go), reactive(mu1_simg), reactive(lambda_simg), reactive(input$mu_sd))
    
    simulado_mmg <- callModule(generateMu, "simulado_mmg", reactive(input$go), reactive(mu1_simg), reactive(lambda_simg))
    
    
    
    output$cambio_lambdag <- renderText({
      cambio <- round(((input$lambda - lambda_simg)/input$lambda) * 100, digits = 2)
      paste("Cambio en lambda: ",cambio,"%")
      
    })
    output$cambio_mu1g <- renderText({
      cambio <- round(((input$mu - mu1_simg)/input$mu) * 100, digits = 2)
      paste("Cambio en mu1: ",cambio,"%")
      
    })
    
    # Simmer para mgk
    
    dfgk <- callModule(simmergenerator, "plot-mmgk", reactive(input$lambda), reactive(input$lambda_sd), reactive(input$mu), reactive(input$mu_sd), reactive(input$prepper), reactive(input$time_init), reactive(input$time_unit))
    mu_lgk <- callModule(datafr, "simmer-mmgk", reactive(df()), reactive(input$time_init), reactive(input$time_unit))
    
    output$server_plotgk <- renderPlot({
      plot(dfgk(), "resources", "usage", "servidor", items="system")
    })
    
    mu1_simgk <- mean(mu_lgk()$Mu1, na.rm = TRUE)
    lambda_simgk <- mean(mu_lgk()$Lambda, na.rm = TRUE)
    
    cola_caja_mmgk_sim <- callModule(mgk, "cola_caja_mmgk_sim", reactive(input$go), reactive(mu1_simgk), reactive(lambda_simgk), reactive(input$prepper), reactive(input$mu_sd))
    
    simulado_mmgk <- callModule(generateMu, "simulado_mmgk", reactive(input$go), reactive(mu1_simgk), reactive(lambda_simgk))
    
    
    
    output$cambio_lambdagk <- renderText({
      cambio <- round(((input$lambda - lambda_simgk)/input$lambda) * 100, digits = 2)
      paste("Cambio en lambda: ",cambio,"%")
      
    })
    output$cambio_mu1gk <- renderText({
      cambio <- round(((input$mu - mu1_simgk)/input$mu) * 100, digits = 2)
      paste("Cambio en mu1: ",cambio,"%")
      
    })
    
    
    
  })
  
  
  
  
  
  
  
})

