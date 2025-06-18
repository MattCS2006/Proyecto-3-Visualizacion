#Red de Palabras
# Instalar y cargar paquetes necesarios
if (!require("tm")) install.packages("tm")
if (!require("igraph")) install.packages("igraph")
if (!require("SnowballC")) install.packages("SnowballC")
if (!require("tokenizers")) install.packages("tokenizers")

library(tm)
library(igraph)
library(SnowballC)
library(tokenizers)

# 1. Leer y unificar el texto del archivo
texto <- tolower(readLines("El pozo y el péndulo.txt", encoding = "UTF-8"))
texto <- paste(texto, collapse = " ")

# 2. Dividir el texto en oraciones
oraciones <- unlist(tokenize_sentences(texto))

# 3. Función para procesar el texto
procesar_texto <- function(texto) {
  corpus <- VCorpus(VectorSource(texto))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
  corpus <- tm_map(corpus, stemDocument, language = "spanish")
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

corpus_procesado <- procesar_texto(oraciones)

# 4. Crear la matriz de co-ocurrencias
crear_matriz_coocurrencias <- function(corpus) {
  tdm <- TermDocumentMatrix(corpus)
  tdm <- as.matrix(tdm)
  tdm <- tdm %*% t(tdm)
  diag(tdm) <- 0
  return(tdm)
}

matriz_coocurrencias <- crear_matriz_coocurrencias(corpus_procesado)

# 5. Visualizar red de palabras
visualizar_red_palabras <- function(matriz, titulo = "Red de palabras") {
  g <- graph.adjacency(matriz, weighted = TRUE, mode = "undirected")
  g <- simplify(g)
  E(g)$weight <- ifelse(E(g)$weight < 1, 0, E(g)$weight)
  g <- delete_edges(g, E(g)[weight == 0])
  
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  V(g)$label.color <- "darkblue"
  V(g)$frame.color <- NA
  V(g)$color <- rgb(0.7, 0.8, 1, alpha = 0.8)
  
  E(g)$width <- 0.8 * E(g)$weight / max(E(g)$weight) + 0.5
  E(g)$color <- "gray70"
  
  set.seed(123)
  layout <- layout_with_fr(g)
  
  par(mar = c(0, 0, 2, 0))
  plot(g,
       layout = layout,
       main = titulo,
       vertex.label.dist = 1.3,
       vertex.label.cex = 0.8)
}

# 6. Ejecutar análisis y mostrar la red
visualizar_red_palabras(matriz_coocurrencias, "Red de palabras en 'El pozo y el péndulo'")

