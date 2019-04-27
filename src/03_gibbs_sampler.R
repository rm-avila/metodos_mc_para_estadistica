rm(list = ls())
source("lib/helpers.R")

# Muestreador de Gibbs de la conjunta de binomial con previa Beta ----------

# Inicializar valores
nsim <- 5000
n <- 15
a <- 3
b <- 7

# Inicializar arreglos
x <- th <- array(0, dim = c(nsim, 1))

# Inicializar cadenas
th[1] <- rbeta(1, a, b)
x[1] <- rbinom(1, n, th[1])

# Ciclo para simular de las condicionales
for(i in 2:nsim){
  x[i] <- rbinom(1, n, th[i-1])
  th[i] <- rbeta(1, x[i]+a, n-x[i]+b)
}

# Programar una función con la conjunta -----------------------------------

conjunta <- function(xval, thval){
  fxt <- choose(n, xval)/beta(a, b)*thval^(xval+a-1)*(1-thval)^(n-xval+b-1)
  return(fxt)
  }

# Marginal de x

betabi <- function(x){
  fx <-gamma(n+1)/(gamma(x+1)*gamma(n-x+1)*beta(a, b))*beta(x+a, n-x+b)
  return(fx)
}

df <-  data.frame(cbind(x, th)) %>% 
  setNames(c("x", "theta"))

# Comparar simulaciones con distribuciones teóricas -----------------------

df %>% 
  gather(variable, value) %>% 
  ggplot(aes(x = value, colour = variable, fill = variable)) +
  geom_histogram() +
  facet_wrap(~variable, scales = "free_x")

par(mfrow = c(1, 2))

# Marginal de x
range_x <- sort(unique(x))
hist(df$x, freq = F, breaks = c(range_x, max(x)+1)-0.5, main = "Marginal de x")
lines(range_x, betabi(range_x), col = "blue", pch = 16, lwd = 2)
points(range_x, betabi(range_x), col = "blue", pch = 16)

# Marginal de theta
hist(df$theta, freq = F, main = "Marginal de theta")
curve(dbeta(x, shape1 = a, shape2 = b), col = "blue", add = T, lwd = 2)
