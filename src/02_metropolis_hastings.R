rm(list = ls())
source("lib/helpers.R")


# Implementación M-H Ber con a priori cos ---------------------------------

set.seed(937)
# Muestra observada
muestra_ber <- rbinom(100, 1, 0.4)
r <- sum(muestra_ber)
n <- length(muestra_ber)

simula_posterior <- function(nsim, x_obs = muestra_ber){
  # Declarar la cadena
  cadena <- vector()
  rechazos <- 0
  # Generar punto inicial
  set.seed(836)
  (cadena[1] <- runif(1, 0, 1/2))
  
  # Cociente de M-H
  rho <- function(p, q){
    cos(pi*q)/cos(pi*p)*(q>0)*(q<1/2)
  }
  
  # Simulaciones
  for(i in 1:nsim){
    y_t <- rbeta(1, r+1, n-r+1) 
    if(runif(1)< rho(cadena[i],y_t)){
      cadena[i+1] <- y_t
    }else{
      rechazos <- rechazos + 1
      cadena[i+1] <- cadena[i]
    }
  }
  out <- list(cadena = cadena, rechazos = rechazos)
  return(out)
}


# Prueba con n = 10000 ----------------------------------------------------

# Monitorear la convergencia
sims <- simula_posterior(10000)
sims_1 <- sims$cadena
# La dn estacionaria se alcanza de inmediato,
# Entonces no necesitamos burn-in

# Monitorear la dependencia
x11()
plot(acf(sims_1))
# tau = 3 sería una buena opción

indexes <- seq(1,length(sims_1), by = 3)
indexes %>% length()

pseudo_sims_post <- sims_1[indexes]

pseudo_sims_post %>% summary

x11()
pseudo_sims_post %>% 
  acf %>% plot

x11()
pseudo_sims_post %>% 
  hist(freq = F)
lines(density(pseudo_sims_post), col = "darkblue")


# Ver proba de rechazo ----------------------------------------------------

sims$rechazos/length(sims$cadena)


# Otra forma de simular la posterior --------------------------------------

cuasi_post <- function(p, x = muestra_ber){
  p^sum(x)*(1-p)^{length(x)-sum(x)}*cos(pi*p)*(p>0)*(p<0.5)
}

posterior <- function(p, x = muestra_ber){
  cte <- integrate(cuasi_post, lower = 0, upper = 0.5)$value
  post <- cuasi_post(p, x = x)/cte
  return(post)
}

pseudo_sims_post %>% 
  hist(freq = F)
curve(posterior(x), 0, 0.5, add = T, col = "blue", lwd = 2)

                      