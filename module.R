

displayMu <- function(id){
  ns <- NS(id)
  fluidRow(
    box(
      h3("Lambda"),
      textOutput(ns("origin_lambda")),
      width = 4
    ),
    box(
      h3("Mu"),
      textOutput(ns("origin_mu")),
      width = 4
    )
  )
}




displayInput <- function(id){
  ns <- NS(id)
  
  fluidRow(
    box(
      h3("Promedio"),
      h5("Unidades en linea de espera"),
      textOutput(ns("waiting_units")),
      h5("Unidades en el sistema"),
      textOutput(ns("avg_units")),
      width = 4
    ),
    box(
      h3("Tiempos"),
      h5("Tiempo promedio que la unidad pasa en la linea de espera"),
      textOutput(ns("avg_waiting_time")),
      h5("Tiempo que la unidad pasa en el sistema"),
      textOutput(ns("time_in_system")),
      width = 4
    ),
    box(
      h3("Probabilidad"),
      h5("Que no tenga que esperar"),
      textOutput(ns("no_wait")),
      h5("Probabilidad que no haya unidades"),
      textOutput(ns("no_units")),
      width = 4
    )
  )
}



generateMu <- function(input, output, session, go, mu, lambda){
  observeEvent(go(), {
    output$origin_mu <- renderText({
      mu()
    })
    output$origin_lambda <- renderText({
      lambda()
    })
    
  })
  
  
}

formulae <- function(input, output, session, go, mu, lambda){
  
  observeEvent(go(), {
    
    output$no_units <- renderText({
      1 - (lambda()/mu())
    })
    
    unit_wait <- reactive({
      (lambda()^2)/(mu() * (mu()-lambda()))
    })
    
    output$waiting_units <- reactive({
      unit_wait()
    })
    
    output$avg_units <- renderText({
      unit_wait() + (lambda()/mu())
    })
    
    avg_time_wait <- reactive({
      unit_wait()/lambda()
    })
    
    output$avg_waiting_time <- renderText({
      avg_time_wait()
    })
    
    output$time_in_system <- renderText({
      avg_time_wait() + (1/mu())
    }) 
    
    output$no_wait <- renderText({
      lambda()/mu()
    })
    
    
  })
  
}

mmk <- function(input, output, session, go, mu, lambda, k){
  
  observeEvent(go(), {
    
    no_units_react <- reactive({
      i = 0:k()-1
      1 / (sum(((lambda()/mu())^i / factorial(i)), na.rm = TRUE) + ((lambda()/mu())^k() / factorial(k())) * (k() * mu() / (k()*mu() - lambda())))
      
    })
    
    output$no_units <- renderText({
      no_units_react()
    })
    
    unit_wait <- reactive({
      
      ((((lambda()/mu())^k()) * (lambda() * mu()))/(factorial(k()-1) * ((k()*mu()) - lambda())^2) ) * no_units_react()
    })
    
    output$waiting_units <- reactive({
      unit_wait()
    })
    
    output$avg_units <- renderText({
      unit_wait() + (lambda()/mu())
    })
    
    avg_time_wait <- reactive({
      unit_wait()/lambda()
    })
    
    output$avg_waiting_time <- renderText({
      avg_time_wait()
    })
    
    output$time_in_system <- renderText({
      avg_time_wait() + (1/mu())
    }) 
    
    output$no_wait <- renderText({
      (1/factorial(k())) * (lambda()/mu()) ^k() * ((k() * mu()) / ((k() * mu()) - lambda())) * no_units_react()
    })
    
    
  })
  
}

mmg <- function(input, output, session, go, mu, lambda, sig){
  
  observeEvent(go(), {
    
    no_units_react <- reactive({
      1-(lambda()/mu())
    })
    
    output$no_units <- renderText({
      no_units_react()
    })
    
    unit_wait <- reactive({
      
      (((lambda()^2) * (sig()^2)) + (lambda()/mu())^2 )/ (2 * (1-lambda()/mu()))
    })
    
    output$waiting_units <- reactive({
      unit_wait()
    })
    
    output$avg_units <- renderText({
      unit_wait() + (lambda()/mu())
    })
    
    avg_time_wait <- reactive({
      unit_wait()/lambda()
    })
    
    output$avg_waiting_time <- renderText({
      avg_time_wait()
    })
    
    output$time_in_system <- renderText({
      avg_time_wait() + (1/mu())
    }) 
    
    output$no_wait <- renderText({
      0
    })
    
    
  })
  
}

mgk <- function(input, output, session, go, mu, lambda, k, sig){
  
  observeEvent(go(), {
    
    no_units_react <- reactive({
      i = 0:k()-1
      1 / (sum(((lambda()/mu())^i / factorial(i)), na.rm = TRUE) + ((lambda()/mu())^k() / factorial(k())) * (k() * mu() / (k()*mu() - lambda())))
      
    })
    
    output$no_units <- renderText({
      no_units_react()
    })
    
    unit_wait <- reactive({
      
      (((lambda()^2) * (sig()^2)) + (lambda()/mu())^2 )/ (2 * (1-lambda()/mu()))
    })
    
    output$waiting_units <- reactive({
      unit_wait()
    })
    
    output$avg_units <- renderText({
      unit_wait() + (lambda()/mu())
    })
    
    avg_time_wait <- reactive({
      unit_wait()/lambda()
    })
    
    output$avg_waiting_time <- renderText({
      avg_time_wait()
    })
    
    output$time_in_system <- renderText({
      avg_time_wait() + (1/mu())
    }) 
    
    output$no_wait <- renderText({
      (1/factorial(k())) * (lambda()/mu()) ^k() * ((k() * mu()) / ((k() * mu()) - lambda())) * no_units_react()
    })
    
    
  })
  
}