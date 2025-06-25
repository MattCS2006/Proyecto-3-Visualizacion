# --- Cargar o instalar paquetes necesarios ---
if (!require("syuzhet")) {
  install.packages("syuzhet")
  library("syuzhet")
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}

# --- Leer el texto y preprocesarlo ---
texto <- tolower(readLines("El pozo y el péndulo.txt", encoding = "UTF-8"))
texto <- paste(texto, collapse = " ")
oraciones <- get_sentences(texto)

# --- Obtener emociones por oración ---
emociones <- get_nrc_sentiment(oraciones, language = "spanish")
conteo <- colSums(emociones[, 1:8])

# --- Traducir nombres de emociones al español ---
nombres_es <- c(
  "anger" = "Ira",
  "anticipation" = "Anticipación",
  "disgust" = "Asco",
  "fear" = "Miedo",
  "joy" = "Alegría",
  "sadness" = "Tristeza",
  "surprise" = "Sorpresa",
  "trust" = "Confianza"
)

# --- Crear data frame traducido ---
df_emociones <- data.frame(
  emocion = nombres_es[names(conteo)],
  frecuencia = as.numeric(conteo)
)

# --- Visualización ---
print(
  ggplot(df_emociones, aes(x = reorder(emocion, -frecuencia), y = frecuencia, fill = emocion)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_brewer(palette = "Set3") +
    labs(
      title = "Conteo de emociones: El pozo y el péndulo",
      subtitle = "Método: NRC (español)",
      x = "Emoción",
      y = "Frecuencia"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
)
