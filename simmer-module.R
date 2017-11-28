library(simmer)
library(reshape2)
library(dplyr)
library(simmer.plot)

simmergenerator <- function(input, output, session, lambda, lambda_sd, mu, mu_sd, canal, time_in, time_div){
  mu_l <- reactive({
    lambda = lambda()
    lambda_sd = lambda_sd()
    mu1 = mu()
    mu1_sd = mu_sd()
    
    rho = lambda/mu1
    sub.N <- rho/(1-rho)
    
    numero_de_panes = canal()
    
    tiempo_a_correr = time_in()
    div_tiempo = time_div()
    
    
    
    set.seed(1234)
    
    subway <- simmer()
    
    cliente <-
      trajectory("Camino de Cliente") %>%
      log_("Llego") %>%
      
      ## Llega al primer paso de hacer su subway
      set_attribute("start_time_sub", function() {now(subway)}) %>%
      seize("servidor") %>%
      log_(function() {paste("Waited for sub: ", now(subway) - get_attribute(subway, "start_time_sub"))}) %>%
      timeout(function() abs(rnorm(1,mu1,mu1_sd))) %>%
      release("servidor") %>%
      set_attribute("salio_sistema", function() {now(subway)}) %>%
      log_(function() {paste("Finished: ", now(subway))})
    
    subway <-
      simmer("subway") %>%
      add_resource("servidor", capacity = numero_de_panes) %>%
      add_generator("Cliente", cliente, function() abs(rnorm(1,lambda,lambda_sd)), mon = 2)
    
    subway %>% run(until = tiempo_a_correr)
    
    subway
    
    
  })
  
}

datafr <- function(input, output, session, subway, tiempo_a_correr, div_tiempo ){
  mu_l <- reactive({
    subway.att <- get_mon_attributes(subway()) %>% 
      select(name, key, time) %>%
      dcast(name ~ key, value.var="time") %>%
      mutate(tasa_de_pan = (salio_sistema - start_time_sub))
    
    lower_bound = 0
    upper_bound = div_tiempo()
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

