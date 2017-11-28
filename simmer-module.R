library(simmer)
library(reshape2)
library(dplyr)
library(simmer.plot)

simmergenerator <- function(input, output, session, lambda, lambda_sd, mu, mu_sd, canal, time_in, disttype){
  mu_l <- reactive({
    lambda = lambda()
    lambda_sd = lambda_sd()
    mu1 = mu()
    mu1_sd = mu_sd()
    
    rho = lambda/mu1
    sub.N <- rho/(1-rho)
    
    numero_de_panes = canal()
    
    tiempo_a_correr = time_in()
    
    subway <- simmer()
    
    if (disttype()=="rexp"){
      print("rexp")
      cliente <-
        trajectory() %>%
        seize("servidor") %>%
        timeout(function() (rexp(1,mu1))) %>%
        release("servidor") 
      
      subway <-
        simmer("subway") %>%
        add_resource("servidor", capacity = numero_de_panes) %>%
        add_generator("Cliente", cliente, function() (rexp(1,lambda))) %>%
        run(until = tiempo_a_correr)
    }
    else if(disttype()=="rnorm"){
      print("rnorm")
      cliente <-
        trajectory() %>%
        seize("servidor") %>%
        timeout(function() (rnorm(1,mu1,mu1_sd))) %>%
        release("servidor") 
      
      subway <-
        simmer("subway") %>%
        add_resource("servidor", capacity = numero_de_panes) %>%
        add_generator("Cliente", cliente, function() (rnorm(1,lambda,lambda_sd))) %>%
        run(until = tiempo_a_correr)
    }
    else if(disttype()=="rpois"){
      print("rpois")
      cliente <-
        trajectory() %>%
        seize("servidor") %>%
        timeout(function() (rpois(1,mu1))) %>%
        release("servidor") 
      
      subway <-
        simmer("subway") %>%
        add_resource("servidor", capacity = numero_de_panes) %>%
        add_generator("Cliente", cliente, function() (rpois(1,lambda))) %>%
        run(until = tiempo_a_correr)
    }
    
    subway
    
    
  })
  
}

datafr <- function(input, output, session, subway, disttype ){
  mu_l <- reactive({
    
    if(disttype()=="rpois"){
      subway.att <- get_mon_arrivals(subway())
      
      mu_lambda <- data.frame()
      mu_lambda <- rbind(mu_lambda,
                         c(mean(diff(subset(subway.att)$start_time)),
                           mean(subway.att$activity_time)))
      
    }
    else if(disttype()=="rexp"){
      subway.att <- get_mon_arrivals(subway())
      
      mu_lambda <- data.frame()
      mu_lambda <- rbind(mu_lambda,
                         c(1/mean(diff(subset(subway.att)$start_time)),
                           1/mean(subway.att$activity_time)))
    }
    else if(disttype()=="rnorm"){
      subway.att <- get_mon_arrivals(subway())
      
      mu_lambda <- data.frame()
      mu_lambda <- rbind(mu_lambda,
                         c(mean(diff(subset(subway.att)$start_time)),
                           mean(subway.att$activity_time)))
    }
    
    colnames(mu_lambda) <- c("Lambda","Mu1")
    
    mu_lambda
    
  })
  
}

backup <- function(input, output, session, subway, tiempo_a_correr ){
  mu_l <- reactive({
    subway.att <- get_mon_attributes(subway()) %>% 
      select(name, key, time) %>%
      dcast(name ~ key, value.var="time") %>%
      mutate(tasa_de_pan = (salio_sistema - start_time_sub))
    
    mu_lambda <- data.frame()
    
    while(upper_bound <= tiempo_a_correr()){
      a <- subway.att[subway.att$start_time_sub >= lower_bound & subway.att$start_time_sub < upper_bound ,]
      mu_lambda <- rbind(mu_lambda, 
                         c(nrow(a), mean(a$tasa_de_pan, na.rm = TRUE))
      )
      lower_bound = upper_bound
      upper_bound = upper_bound + div_tiempo()
      print(lower_bound)
      print(upper_bound)
    }
    
    colnames(mu_lambda) <- c("Lambda","Mu1")
    
    mu_lambda
    
  })
  
}

