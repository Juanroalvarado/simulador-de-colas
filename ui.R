#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyTime)
source("module.R")

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Queue Simulator dashboard"),
    dashboardSidebar(
      sidebarMenu(
        actionButton("go","Correr Simulacion"),
        numericInput("lambda","Lambda input",0),
        numericInput("lambda_sd","Lambda SD",0),
        numericInput("mu","Mu input",0),
        numericInput("mu_sd","Mu SD",0),
        numericInput("time_init","Tiempo Inicial",120),
        numericInput("prepper","Agregar Canales",1),
        selectInput("dist", "Distribucion:",
                    c("Normal" = "rnorm",
                      "Poisson" = "rpois",
                      "Exponential" = "rexp")),
        menuItem("MM1", tabName = "MM1", icon = icon("dashboard")),
        menuItem("MMK", tabName = "MMK", icon = icon("th")),
        menuItem("MG1", tabName = "MG1", icon = icon("th")),
        menuItem("MGK", tabName = "MGK", icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "MM1",
                h2("M/M/1"),
                fluidRow(
                  box(
                    h1("Original"),
                    displayMu("original"),
                    h2("De cola a caja"),
                    displayInput("cola_caja")
                  ),
                  box(
                    h1("Simulado"),
                    displayMu("simulado"),
                    h2("De cola a caja sim"),
                    displayInput("cola_caja_simulado")
                  ),
                  
                  h1("Diferencias"),
                  h2("Cambios Porcentuales"),
                  box(
                    textOutput("cambio_lambda"),
                    textOutput("cambio_mu1"),
                    width = 12
                  ),
                  box(
                    plotOutput("server_plot"),
                    width = 12
                  )
                )
        ),
        
        # Second tab content
        tabItem(tabName = "MMK",
                h2("M/M/k"),
                fluidRow(
                  box(
                    h1("Original"),
                    displayMu("originalk"),
                    h2("De cola a caja"),
                    displayInput("cola_caja_mmk")
                  ),
                  box(
                    h1("Simulado"),
                    displayMu("simulado_mmk"),
                    h2("De cola a caja sim"),
                    displayInput("cola_caja_mmk_sim")
                  ),
                  
                  h1("Diferencias"),
                  h2("Cambios Porcentuales"),
                  box(
                    textOutput("cambio_lambdak"),
                    textOutput("cambio_mu1k"),
                    width = 12
                  ),
                  box(
                    plotOutput("server_plotk"),
                    width = 12
                  )
                )
                
        ),
        
        tabItem(tabName = "MG1",
                h2("M/G/1"),
                fluidRow(
                  box(
                    h1("Original"),
                    displayMu("originalg"),
                    h2("De cola a caja"),
                    displayInput("cola_caja_mmg")
                  ),
                  box(
                    h1("Simulado"),
                    displayMu("simulado_mmg"),
                    h2("De cola a caja sim"),
                    displayInput("cola_caja_mmg_sim")
                  ),
                  
                  h1("Diferencias"),
                  h2("Cambios Porcentuales"),
                  box(
                    textOutput("cambio_lambdag"),
                    textOutput("cambio_mu1g"),
                    width = 12
                  ),
                  box(
                    plotOutput("server_plotg"),
                    width = 12
                  )
                )
        ),
        
        tabItem(tabName = "MGK",
                h2("M/G/k"),
                fluidRow(
                  box(
                    h1("Original"),
                    displayMu("originalgk"),
                    h2("De cola a caja"),
                    displayInput("cola_caja_mmgk")
                  ),
                  box(
                    h1("Simulado"),
                    displayMu("simulado_mmgk"),
                    h2("De cola a caja sim"),
                    displayInput("cola_caja_mmgk_sim")
                  ),
                  
                  h1("Diferencias"),
                  h2("Cambios Porcentuales"),
                  box(
                    textOutput("cambio_lambdagk"),
                    textOutput("cambio_mu1gk"),
                    width = 12
                  ),
                  box(
                    plotOutput("server_plotgk"),
                    width = 12
                  )
                )
        )
      )
    )
  )
)
