# Ejercicio 1
DNA_random <- function(n){
  DNA <- c("A", "T", "C", "G")
  DNA_random <- sample(DNA, n, replace = TRUE)
  return(DNA_random)
}

DNA_random(50)

# Ejercicio 2
DNA_size <- function(DNA){
  DNA <- c("A", "T", "C", "G")
  DNA_random <- sample(DNA, n, replace = TRUE)
  DNA_random <- paste(DNA_random, collapse = "")
  return(DNA_random)
}
