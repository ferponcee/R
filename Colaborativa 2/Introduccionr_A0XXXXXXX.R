# Ejercicio 1 
adn = function(n){
  cadena = c("A","T","G","C")
  cadena_adn = sample(cadena, n, replace = TRUE)
  return(cadena_adn)
}
secuencia_adn = adn(50)
print(secuencia_adn)

# Ejercicio 2
tamanio = function(t){
  n = length(t)
  return(n)
}
tamanio_secuencia = tamanio(secuencia_adn)
print(tamanio_secuencia)

# Ejercicio 3
Cadena90 <- adn(90)
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
transcripcion = function(d){
  if (length(d) %% 3 != 0) {
    cat("La longitud de la secuencia de RNA no es un múltiplo de 3. Se agregarán bases adicionales al final.\n")
    d = c(d, sample(c("A", "U", "C", "G"), length(d) %% 3, replace = TRUE))
  }
  
  rna = character(length(d))
  for (i in 1:length(d)){
    if (d[i] == "A") rna[i] = "U"
    else if (d[i] == "T") rna[i] = "A"  # Aquí estaba el error, se debe cambiar T por U
    else if (d[i] == "C") rna[i] = "G"
    else if (d[i] == "G") rna[i] = "C"
  }
  return(paste(rna, collapse = ""))
}


# Transcribir la secuencia del Ejercicio 3 de DNA a RNA
transcripcion_resultado = transcripcion(Cadena90)
print(transcripcion_resultado)


# Ejercicio5
traduccion_lista = function(lista_codones) {
  proteinas = character(length(lista_codones)/3)
  
  for (i in seq(1, length(lista_codones), by = 3)) {
    if (i + 2 <= length(lista_codones)) {
      codon = paste(lista_codones[i:(i+2)], collapse = "")
      
      aminoacido = ifelse(codon == "UUU" | codon == "UUC", "Phe",
        ifelse(codon == "UUA" | codon == "UUG", "Leu",
          ifelse(codon == "UCU" | codon == "UCC" | codon == "UCA" | codon == "UCG", "Ser",
            ifelse(codon == "UAU" | codon == "UAC", "Tyr",
              ifelse(codon == "UAA" | codon == "UAG" | codon == "UGA", "STOP",
                ifelse(codon == "UGU" | codon == "UGC", "Cys",
                  ifelse(codon == "UGG", "Trp",
                    ifelse(codon == "CUU" | codon == "CUC" | codon == "CUA" | codon == "CUG", "Leu",
                      ifelse(codon == "CCU" | codon == "CCC" | codon == "CCA" | codon == "CCG", "Pro",
                        ifelse(codon == "CAU" | codon == "CAC", "His",
                          ifelse(codon == "CAA" | codon == "CAG", "Gln",
                            ifelse(codon == "CGU" | codon == "CGC" | codon == "CGA" | codon == "CGG", "Arg",
                              ifelse(codon == "AUU" | codon == "AUC" | codon == "AUA", "Ile",
                                ifelse(codon == "AUG", "Met",
                                  ifelse(codon == "ACU" | codon == "ACC" | codon == "ACA" | codon == "ACG", "Thr",
                                    ifelse(codon == "AAU" | codon == "AAC", "Asn",
                                      ifelse(codon == "AAA" | codon == "AAG", "Lys",
                                        ifelse(codon == "AGU" | codon == "AGC", "Ser",
                                          ifelse(codon == "AGA" | codon == "AGG", "Arg",
                                            ifelse(codon == "GUU" | codon == "GUC" | codon == "GUA" | codon == "GUG", "Val",
                                              ifelse(codon == "GCU" | codon == "GCC" | codon == "GCA" | codon == "GCG", "Ala",
                                                ifelse(codon == "GAU" | codon == "GAC", "Asp",
                                                  ifelse(codon == "GAA" | codon == "GAG", "Glu",
                                                    ifelse(codon == "GGU" | codon == "GGC" | codon == "GGA" | codon == "GGG", "Gly", "UNK")
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      
      proteinas[(i+2)/3] = aminoacido
    }
  }
  
  return(proteinas)
}

# Traducir la secuencia de RNA del Ejercicio 4 a proteínas
traduccion_resultado = traduccion_lista(transcripcion_resultado)
print(traduccion_resultado)

# Prueba del Ejercicio 5 con la variable del Ejercicio 4
secuencia_adn_4 = adn(90)
rna_secuencia_4 = transcripcion(secuencia_adn_4)
proteinas_secuencia_4 = traduccion_lista(rna_secuencia_4)
print(proteinas_secuencia_4)

# Nueva variable del Ejercicio 1 con n = 89
secuencia_adn_nuevo = adn(89)
rna_secuencia_nuevo = transcripcion(secuencia_adn_nuevo)

if (!is.null(rna_secuencia_nuevo)) {
  proteinas_secuencia_nuevo = traduccion_lista(rna_secuencia_nuevo)
  cat("\nProteínas de la nueva variable del Ejercicio 1 con n = 89:\n")
  print(proteinas_secuencia_nuevo)
} else {
  cat("La longitud de la secuencia de RNA no es un múltiplo de 3. No se pueden traducir proteínas.\n")
}

#Ejercicio 6

inversa = function(h){
  hebra_inversa = rev(h)
return(hebra_inversa)
}
hebra_directa = c("5'","T","G","C","G","A","T","A","C","3'")
inversa(hebra_directa)


#Ejercicio 7
complementaria = function(h){
  hebra_complementaria = c(1:length(h))
  i = 1
  while (i <= length(h)){
   if (h[i] == "5'"){
     c_5 = "3'"
     hebra_complementaria = replace(hebra_complementaria,i,c_5)
   } 
   if (h[i] == "A"){
     c_A = "T"
     hebra_complementaria = replace(hebra_complementaria,i,c_A)
   }
   if (h[i] == "T"){
     c_T = "A"
     hebra_complementaria = replace(hebra_complementaria,i,c_T)
   }
   if (h[i] == "C"){
     c_C = "G"
     hebra_complementaria = replace(hebra_complementaria,i,c_C)
   }
   if (h[i] == "G"){
     c_G = "C"
     hebra_complementaria = replace(hebra_complementaria,i,c_G)
   }
   if (h[i] == "3'"){
     c_3 = "5'"
     hebra_complementaria = replace(hebra_complementaria,i,c_3)
   }
    i = i + 1
  }
return(hebra_complementaria)
}
hebra_directa = c("5'","T","G","C","G","A","T","A","C","3'")
complementaria(hebra_directa)


#Ejercicio 8 
complementaria_inversa = function(h){
   hebra_complementaria_inversa = rev(h)
return(hebra_complementaria_inversa)
}
hebra_complementaria = c("3'","A", "C", "G", "C", "T", "A", "T", "G", "5'")
complementaria_inversa(hebra_complementaria)
