rm(list = ls())
source("lib/helpers.R")

# Función que regresa número de evaluaciones necesarias -------------------

# Input: u es un vector de uniformes (0,1), vec_cdf es un vector de evaluaciones
# de la fde (si es de soporte infinito, vec_cdf evalúa en los primeros n valores)
# Output:

evals <- function(u, vec_cdf){
  n_evals <- rep(0, length(u))
  for(i in 1:length(u)){
    n_evals[i] <- which.max(u[i]<vec_cdf)
  }
  return(n_evals)
}


# a) Un dado equilibrado --------------------------------------------------

evals_dado <- function(n){
  cdf_dado <- 1:6/6
  u <- runif(n)
  n_evals <- evals(u, cdf_dado)
  return(mean(n_evals))
}

evals_dado(1000000)
# La respuesta correcta es 3.5 evaluaciones


# b) Poisson --------------------------------------------------------------

evals_pois <- function(n, tasa = 4){
  # Evaluación en 0:100
  cdf_pois <- ppois(0:100, tasa)
  u <- runif(n)
  n_evals <- evals(u, cdf_pois)
  return(mean(n_evals))
}

evals_pois(1000000, 4)
# La respuesta correcta es 5 evaluaciones


# c) Geométrica -----------------------------------------------------------

evals_geom <- function(n, p = 0.5){
  # Evaluación en 0:100
  pgeom_2 <- function(x, p) 1-(1-p)^x 
  cdf_geom <- pgeom_2(1:100, p)
  u <- runif(n)
  n_evals <- evals(u, cdf_geom)
  return(mean(n_evals))
}

evals_geom(1000000, 0.5)
# La respuesta correcta es 2 evaluaciones


