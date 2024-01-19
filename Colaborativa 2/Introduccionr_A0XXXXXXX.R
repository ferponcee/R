# Ejercicio 1
DNA_random <- function(n){
  DNA <- c("A", "T", "C", "G")
  DNA_random <- sample(DNA, n, replace = TRUE)
  return(DNA_random)
}

Cadena <- DNA_random(50)
cat("Ejercicio 1:
    Secuencia aleatoria de DNA: ", Cadena, sep = "")

# Ejercicio 2
DNA_size <- function(DNA){
  return(length(DNA))
}

cat("Ejercicio 2:
    Longitud de la secuencia de DNA: ", DNA_size(Cadena))

# Ejercicio 3
Cadena90 <- DNA_random(90)
porcentaje <- function(Cadena90){
  contador_A <- 0
  contador_T <- 0
  contador_C <- 0
  contador_G <- 0
  for (i in 1:length(Cadena90)){
    if (Cadena90[i] == "A"){
      contador_A <- contador_A + 1
    } else if (Cadena90[i] == "T"){
      contador_T <- contador_T + 1
    } else if (Cadena90[i] == "C"){
      contador_C <- contador_C + 1
    } else if (Cadena90[i] == "G"){
      contador_G <- contador_G + 1
    }
  }
  porcentaje_A <- (contador_A/length(Cadena90))*100
  porcentaje_T <- (contador_T/length(Cadena90))*100
  porcentaje_C <- (contador_C/length(Cadena90))*100
  porcentaje_G <- (contador_G/length(Cadena90))*100
  return(c(porcentaje_A, porcentaje_T, porcentaje_C, porcentaje_G))
}
cat("Ejercicio 3:
    Cadena de DNA: ", Cadena90, "
    Porcentaje de A: ", porcentaje(Cadena90)[1], "%", "
    Porcentaje de T: ", porcentaje(Cadena90)[2],"%", "
    Porcentaje de C: ", porcentaje(Cadena90)[3],"%", "
    Porcentaje de G: ", porcentaje(Cadena90)[4], "%", sep = "")


# Ejercicio 4
RNA <- function(Cadena90){
  RNA <- Cadena90
  for (i in 1:length(Cadena90)){
    if (Cadena90[i] == "T"){
      RNA[i] <- "U"
    }
  }
  return(RNA)
}
cat("Ejercicio 4:
    Cadena de DNA: ", Cadena90, "
    Cadena de RNA: ", RNA(Cadena90), sep = "")

# Ejercicio 5
sec_p <- function(rna) {
  prot <- switch(rna,
    "UUU", "UUC", "Phe",
    "UUA", "UUG", "Leu",
    "UCU", "UCC", "UCA", "UCG", "Ser",
    "UAU", "UAC", "Tyr",
    "UGU", "UGC", "Cys",
    "UGG", "Trp",
    "CUU", "CUC", "CUA", "CUG", "Leu",
    "CCU", "CCC", "CCA", "CCG", "Pro",
    "CAU", "CAC", "His",
    "CAA", "CAG", "Gin",
    "CGU", "CGC", "CGA", "CGG", "Arg",
    "AUU", "AUC", "AUA", "Ile",
    "AUG", "Met",
    "ACU", "ACC", "ACA", "ACG", "Thr",
    "AAU", "AAC", "Asn",
    "AAA", "AAG", "Lys",
    "AGU", "AGC", "Ser",
    "AGA", "AGG", "Arg",
    "GUU", "GUC", "GUA", "GUG", "Val",
    "GCU", "GCC", "GCA", "GCG", "Ala",
    "GAU", "GAC", "Asp",
    "GAA", "GAG", "Glu",
    "GGU", "GGC", "GGA", "GGG", "Gly",
    "UAA", "UAG", "UGA", "Stop",
    "Unknown"
  )
  
  return(prot)
}

# Crear variables para probar
Cadena90 <- DNA_random(90)
Cadena89 <- DNA_random(89)
rna_ejercicio4 <- RNA(Cadena90)
rna_nuevo <- RNA(Cadena89)

# Ejecutar la función sec_p con las variables creadas
resultado_ejercicio4 <- sec_p(rna_ejercicio4)
resultado_nuevo <- sec_p(rna_nuevo)

# Mostrar resultados
cat("Ejercicio 4:
    Cadena de DNA: ", Cadena90, "
    Cadena de RNA: ", rna_ejercicio4, "
    Aminoácido correspondiente: ", resultado_ejercicio4, "\n")

cat("\nNueva variable con n = 89:
    Cadena de DNA: ", Cadena89, "
    Cadena de RNA: ", rna_nuevo, "
    Aminoácido correspondiente: ", resultado_nuevo, "\n")

#Ejercicio 6 
invertir_hebra <- function(hebra_directa) {
  invertida <- rev(strsplit(hebra_directa, "")[[1]])
  return(paste(invertida, collapse = ""))
}
hebra_directa <- "5'-TGCGATAC-3'"
invertir_hebra(hebra_directa)

#Ejercicio 7 
obtener_hebra_complementaria <- function(hebra_directa) {
  complemento <- c('A'='T', 'T'='A', 'C'='G', 'G'='C')
  hebra_complementaria <- sapply(strsplit(hebra_directa, ''), function(base) complemento[base])
  return(paste(hebra_complementaria, collapse = ''))
}

hebra_directa <- 'TGCGATAC'
hebra_complementaria <- obtener_hebra_complementaria(hebra_directa)

cat("Hebra directa: ", hebra_directa, "\n")
cat("Hebra complementaria: ", hebra_complementaria, "\n")


#Ejercicio 8 
obtener_complementaria_inversa <- function(hebra_directa) {
  complemento <- c('A'='T', 'T'='A', 'C'='G', 'G'='C')
  hebra_complementaria <- sapply(strsplit(hebra_directa, ''), function(base) complemento[base])
  hebra_complementaria_inversa <- rev(hebra_complementaria)
  return(paste(hebra_complementaria_inversa, collapse = ''))
}

hebra_directa <- 'TGCGATAC'
hebra_complementaria_inversa <- obtener_complementaria_inversa(hebra_directa)

cat("Hebra directa: ", hebra_directa, "\n")
cat("Hebra complementaria inversa: ", hebra_complementaria_inversa, "\n")
