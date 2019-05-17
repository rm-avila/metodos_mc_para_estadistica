rm(list = ls())
source("lib/helpers.R")


# Cargar los datos --------------------------------------------------------

cargar_paquetes("wooldridge")

data("wage2")
?wage2
head(wage2)

wage_db <- wage2[1:500,] %>% 
  mutate(pareduc = meduc + feduc)

# Base de datos con datos de salarios mensuales de 935 individuos en 1980
# Las variables que utilizaremos son:
# wage: Salario mensual
# educ: Años de educación
# exper: Años de experiencia trabajando
# tenure: Permanencia en el trabajo actuan (en años)
# meduc: Nivel de educación de la madree
# feduc: Nivel de educación del padre
# pareduc: Suma de meduc y feduc
# married: 1 si casado, 0 si no
# black: 1 si es de raza negra, 0 si no
# south: 1 si vive en el sur
# urban: 2 si vive en zona metropolitana, 0 si no
# age: edad del individuo

# Modelos candidatos ------------------------------------------------------

# Probaremos distintas regresiones propuestas en Wooldridge(2016) 
# "Introductory Econometrics: A modern approach" para modelar el salario

# Modelo 1: salario explicado por nivel de eduación, experiencia y permanencia
# en trabajo actual
mod_1 <- lm(log(wage)~educ+exper+tenure, data = wage_db) 
summary(mod_1)

# Modelo 2: salario explicado por educación, educación de los padres (interactuando
# con educación del individuo), experiencia y permanencia en el trabajo actual
mod_2 <- lm(log(wage)~educ+educ*pareduc+exper+tenure, data = wage_db) 
summary(mod_2)

# Modelo 3: salario explicado por educación y experiencia en forma cuadrática 
# (para modelar efecto decreciente del aumento de experiencia)
mod_3 <- lm(log(wage)~educ+exper+exper^2, data = wage_db) 
summary(mod_3)

# Modelo 4: salario explicado por educación, experiencia, interacción entre ambas
# y edad
mod_4 <- lm(log(wage)~educ+exper+educ*exper+age+age^2, data = wage2)
summary(mod_4)

# Modelo 5: Modelo 3 agregando estado civil
mod_5 <- lm(log(wage)~educ+exper+exper^2+married,data = wage_db)
summary(mod_5)

# Modelo 6: Modelo 1 agregando estado civil, raza, si vive en el sur y 
# si vive en zona metropolitana
mod_6 <- lm(log(wage)~educ+exper+tenure+married+black+south+urban, data = wage_db)
summary(mod_6)

# Estimar m gorro para cada modelo candidato ------------------------------

# De la ecuación (37) del artículo de Wasserman:
fun_log_m_hat <- function(model){
  n <- nobs(model)
  sigma_hat <- sqrt(sum(residuals(model)^2)/n)
  log_m_hat <- -n*log(sigma_hat)-(n/2)*log(2*pi)-(n/2)
  return(log_m_hat)
}

models_list <- list(mod_1, mod_2, mod_3, mod_4, mod_5, mod_6)
names(models_list) <- paste("mod_", 1:6, sep = "")

# Vector de log m gorros para cada modelo (mantener logs para evitar problemas
# numéricos)
log_m_hat <- sapply(models_list, fun_log_m_hat)

# Probabilidades posteriores de los modelos
(probs_post <- exp(log_m_hat-log(sum(exp(log_m_hat)))))

# Comparar conclusiones con AIC y BIC
sapply(models_list, AIC) # Elegir el de menor AIC
sapply(models_list, BIC) # Elegir el de menor BIC

