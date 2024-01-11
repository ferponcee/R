# Ejercicio 1
x <- c(10, 72, 18, -11, 6, -34, 291)
cat("Ejercicio 1:\n", "Vector x:", x, "\n\n")

# Ejercicio 2
media_x <- mean(x)
desviacion_estandar_x <- sd(x)
varianza_x <- var(x)
est.x <- c(media_x, desviacion_estandar_x, varianza_x)
cat("Ejercicio 2:\n", "Media:", est.x[1], "\nDesviación Estándar:", est.x[2], "\nVarianza:", est.x[3], "\n\n")

# Ejercicio 3
secuencia <- seq(10, 80)
media_25_95 <- mean(secuencia[secuencia >= 25 & secuencia <= 95])
suma_51_93 <- sum(secuencia[secuencia >= 51 & secuencia <= 93])
cat("Ejercicio 3:\n", "Media de números del 25 al 95:", media_25_95, "\nSuma de números del 51 al 93:", suma_51_93, "\n\n")

# Ejercicio 4
vector_aleatorio <- sample(-100:500, 10, replace = TRUE)
cat("Ejercicio 4:\n", "Vector aleatorio:", vector_aleatorio, "\n")

# Ejercicio 5 
fb <- numeric(10)
fb[1] <- fb[2] <- 1
for (i in 3:10) {
  fb[i] <- fb[i-1] + fb[i-2]
}
cat("Los primeros 10 números de Fibonacci son:", fb, "\n")



