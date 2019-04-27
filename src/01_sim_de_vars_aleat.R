# Limpiar la consola
rm(list = ls())

# Cargar archivos de ayuda
source("lib/helpers.R")

lista_bayesiana <- readRDS("data/alumnos_bayesiana.RDS")

# Simulador de uniformes --------------------------------------------------

# ¿Quién nos hablará del simulador de uniformes de R?
set.seed(37)
sample(lista_bayesiana, 1)

# Número de simulaciones
Nsim <- 10^4 

# Simular
x <- runif(Nsim)

# Pares adyacentes
x1 <- x[-Nsim] 
x2 <- x[-1] 

# Graficar
par(mfrow = c(1,3))
hist(x, freq = F, main = "Histograma de las simulaciones")
plot(x1,x2, main = "x vs. x[-1]")
acf(x, main = "Correlograma")

# No olvidar correr dev.off cuando se han modificado los parámetros del par
dev.off()

# Guardar la gráfica
pdf("plots/01_generador_unifs.pdf", width = 10, height = 6)
par(mfrow = c(1,3))
hist(x, freq = F, main = "Histograma de las simulaciones")
plot(x1,x2, main = "x vs. x[-1]")
acf(x, main = "Correlograma")
dev.off()

# Verificar función de set.seed
runif(5)
runif(5)
runif(5)

set.seed(1)
runif(5)

set.seed(1)
runif(5)

set.seed(7)
runif(5)


# Método de la transformada inversa ---------------------------------------

# Recordar que para la densidad Exp(1)
# x = F^{-1}(u) = -log(1-u), con U ~ Unif(0, 1)

generador_exponenciales <- function(n){
  unifs <- runif(n)
  exps <- -log(1-unifs)
  return(exps)
}

set.seed(2019)
generador_exponenciales(10)

# Mil simulaciones con la transformada inversa
set.seed(298)
mil_exps <- generador_exponenciales(1000) 

# Con el simulador de R
set.seed(334)
exps_r <- rexp(1000)

hist(mil_exps, freq = F, col = add_alpha("mistyrose", 0.2), 
     border = "mistyrose4", breaks = 17)
hist(exps_r, freq = F, col = add_alpha("royalblue", 0.2), 
     border = "royalblue4", add = T, 
     breaks = 17)


# Ejercicios --------------------------------------------------------------

# generador_logistica y comparar vs. rlogis

# generador_cauchy y comparar vs. rcauchy


# Generar v.a's por transformación ----------------------------------------
# A partir de v.a's Exp(1)

#   Ji cuadrada con 2 nu grados de libertad

# Usando for
generador_jis_0 <- function(n, nu){
  # n es el número de simulaciones a generar
  jis <- vector()
  for(i in 1:n)
  jis[i] <- 2*sum(generador_exponenciales(nu))
  return(jis)
}

# Vectorial
generador_jis <- function(n, nu){
  # n es el número de simulaciones a generar
  unifs <- runif(n*nu) %>% 
    matrix(nrow = nu) 
  exps <- -log(1-unifs)
  jis <- 2*apply(exps,2,sum)
  return(jis)
}

# Mil simulaciones con los generadores programados
set.seed(29)
mil_jis_0 <- generador_jis_0(1000, 2)
set.seed(94)
mil_jis_v <- generador_jis(1000, 2)

# Con el simulador de R
set.seed(124)
jis_r <- rchisq(1000, 4)

hist(mil_jis_0, freq = F, col = add_alpha("mistyrose", 0.2), 
     border = "mistyrose4")
hist(mil_jis_v, freq = F, col = add_alpha("tomato", 0.2), 
     border = "tomato4", add = T)
hist(jis_r, freq = F, col = add_alpha("royalblue", 0.2), 
     border = "royalblue4", add = T)

# Ejercicios --------------------------------------------------------------

# Programar simuladores para Gamma y Beta a partir de exponenciales y comparar
# histogramas con rgamma y rbeta



# Generador de Normales (Box-Muller) --------------------------------------

genera_normales <- function(n){
  if(n%%2 == 0){
    m <- n/2
  } else m <- ceiling(n/2)
  unifs <- matrix(runif(2*m), ncol = 2, nrow = m)
  normales <- matrix(0, ncol = 2, nrow = m)
  for(i in 1:m){
    u1 <- unifs[i,1]
    u2 <- unifs[i,2]
    x1 <- sqrt(-2*log(u1))*cos(2*pi*u2)
    x2 <- sqrt(-2*log(u1))*sin(2*pi*u2)
    normales[i,] <- c(x1,x2)
  }
  norm_vec <- as.vector(normales)[1:n]
  return(norm_vec)
}


# Generar normales por acept-rechazo --------------------------------------

genera_dob_exp <- function(n, alpha = 1){
  unifs <- runif(n, min = -0.5, max = 0.5)
  transf_inv <- function(u){
   -(1/alpha)*sign(u)*log(1-2*abs(u))
  }
  return(transf_inv(unifs))
}

dlaplace <- function(x, alpha = 1){
  (alpha/2)*exp(-alpha*abs(x))
}

# Densidad Laplace
curve(dlaplace(x),-3,3)
curve(dlaplace(x, alpha = 3), col = "tomato", add = T)

# Laplace vs. N(0,1)
curve(dlaplace(x),-3,3, lwd = 3)
curve(dnorm(x), col = "blue", add = T, lwd = 3)

# Envolvente
M <- sqrt(2*exp(1)/pi)
x11()
curve(M*dlaplace(x),-3,3, lwd = 3)
curve(dnorm(x), col = "blue", add = T, lwd = 3)

genera_norm_AR <- function(n){
  iter <- 0
  acep <- 0
  normales <- vector()
  while(acep < n){
   x <- genera_dob_exp(1)
   if(runif(1) < dnorm(x)/(M*dlaplace(x))){
     acep <- acep + 1
     normales[acep] <- x
   } 
   iter <- iter + 1
  }
  out <- list(sims = normales, prop_acep = acep/iter, iter = iter)
  return(out)
}

genera_norm_AR(10)

genera_norm_AR(100)

normales_AR <- genera_norm_AR(5000)

x11()
normales_AR$sims %>% hist(freq = F)
curve(dnorm(x), col = "blue", lwd = 2, add = T)

# Medir tiempos de ejecución ----------------------------------------------

# Box-Muller

start_time <-  Sys.time()
norms1 <- genera_normales(10000000)
end_time <- Sys.time()
tiempo_box_muller <- end_time - start_time

# Aceptación y rechazo

start_time <-  Sys.time()
norms_2 <- genera_norm_AR(10000000)
end_time <- Sys.time()
tiempo_acep_rech <- end_time - start_time

# Generador de R

start_time <-  Sys.time()
norms_3 <- rnorm(10000000)
end_time <- Sys.time()
tiempo_simulador_r <- end_time - start_time

