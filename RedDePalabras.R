# --- Paquetes necesarios ---
if (!require("tm")) install.packages("tm")
if (!require("igraph")) install.packages("igraph")
if (!require("SnowballC")) install.packages("SnowballC")
if (!require("tokenizers")) install.packages("tokenizers")

library(tm)
library(igraph)
library(SnowballC)
library(tokenizers)

# --- Leer y procesar texto ---
texto <- tolower(readLines("El pozo y el péndulo.txt", encoding = "UTF-8"))
texto <- paste(texto, collapse = " ")
oraciones <- unlist(tokenize_sentences(texto))

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

crear_matriz_coocurrencias <- function(corpus) {
  tdm <- TermDocumentMatrix(corpus)
  m <- as.matrix(tdm)
  m <- m %*% t(m)
  diag(m) <- 0
  return(m)
}

matriz <- crear_matriz_coocurrencias(corpus_procesado)

# --- Limitar a las 30 palabras más conectadas ---
top_palabras <- sort(rowSums(matriz), decreasing = TRUE)[1:30]
matriz_reducida <- matriz[names(top_palabras), names(top_palabras)]

# --- Crear grafo ---
g <- graph.adjacency(matriz_reducida, mode = "undirected", weighted = TRUE, diag = FALSE)
g <- simplify(g)

# --- Ajustar visual ---
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
V(g)$size <- 6 + V(g)$degree
V(g)$label.cex <- 0.9
V(g)$color <- "#81C784"
V(g)$frame.color <- NA
V(g)$label.color <- "black"

E(g)$width <- 1 + E(g)$weight / max(E(g)$weight) * 2
E(g)$color <- "gray70"

# --- Layout más espacioso ---
set.seed(42)
layout <- layout_with_kk(g)

# --- Graficar ---
par(mar = c(0, 0, 2, 0))
plot(g,
     layout = layout,
     main = "Red de Palabras (Top 30) - El pozo y el péndulo",
     vertex.label.family = "sans")
