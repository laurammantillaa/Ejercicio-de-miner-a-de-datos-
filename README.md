# Ejercicio-de-miner-a-de-datos-
Ejemplo academico de mineria de texto en R studio

##Ejercicio acádemico de minería de texto 
##librería necesarias 
install.packages("tm")
install.packages("Snowvallc")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("tidytext")
install.packages("wordcloud2")
install.packages("cluster")
install.packages("readr")

library(tm)
library(SnowvallC)
library(dplyr) #manipulacion de datos
library(ggplot2) #visualizacion
library(gridExtra) 
library(tidytext) #extraer textos 
library(wordcloud2) 
library(cluster)
library(readr)

##Se carga el documento al directorio de trabajo con import Dataset 
##Se lee el documento usando la función read_lines desde la línea 4 hasta la 17 para omitir autor y fecha de publicación
relato <- read_lines("relato.txt", skip = 3, n_max = 58-3)

##Se prepara el texto 
str(relato)

##creación de vector parrafo 
##se crean parrafos de 10 renglones, desde la línea 1 hasta el número de renglones
parrafo <- rep(1:ceiling(length(relato)/10), each = 10)
##del vector parrafo con quedamos con el número de elementos igual al número de renglones
parrafo <- parrafo[1:length(relato)]

##Se combina parrafo con relato y los asignamos al objeto texto. Así tenemos una columna con los renglones de texto y otra con un número que identifica a qué grupo de diez renglones pertenece
##además convertimos a data.frame para que las columnas estén identificadas con un nombre
texto <- cbind(parrafo, relato) 
data.frame(texto)

##Se usa aggregate para concatenar los renglones (FUN = paste, con collapse = " " para preservar el espacio entre palabras)
texto <- aggregate(formula = relato ~ parrafo,data = texto,FUN = paste,collapse = " ")
##Para dejar solo la columna con los ahora párrafos de texto se transformar "texto" en una matrix
texto <- select((texto))
as.matrix(texto)
dim(texto)

##Se hace limpieza de texto de caracteres especiales saltos de linea y tabulaciones 
texto <- gsub("[[:cntrl:]]", " ", texto)

##Se convierte todo a minuscula y se eliminan palabras vacias 
texto <- tolower(texto)
texto <- removeWords(texto, words = stopwords("spanish"))

##Se elimina la puntuación, números y vacios excesivos 
texto <- removePunctuation(texto)
texto <- removeNumbers(texto)
texto <- stripWhitespace(texto)

##Se crea el acervo de documentos a analizar.
cuerpo <- Corpus(VectorSource(texto))
cuerpo

##Se crea la nube de palabras que más se repite en el relato
nube <- tm_map(cuerpo, PlainTextDocument)
wordcloud(nube, max.words = 80, random.order = F, colors = brewer.pal(name = "Dark2", n = 8))

##Se mapea el cuerpo indicando que es una matriz de terminos 
tdm <- TermDocumentMatrix(cuerpo)
tdm

##Se transforma el objeto en una matriz y se busca la frecuencia 
mat <- as.matrix(tdm)
dim(mat)

mat <- mat %>% rowSums() %>% sort(decreasing = TRUE)
mat <- data.frame(palabra = names(mat), frec = mat)

wordcloud(
  words = mat$palabra, 
  freq = mat$frec, 
  max.words = 80, 
  random.order = F, 
  colors=brewer.pal(name = "Dark2", n = 8)
)

##Se determina la asociación entre palabras 
findAssocs(tdm, terms = c("ejercito", "masacre", "trabajadores", "company"), corlimit = .25)
