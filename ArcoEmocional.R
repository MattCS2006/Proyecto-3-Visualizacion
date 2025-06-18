#Aquí va el Arco emocional

if (require("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# por si ocupa instalarse algo
if (!require("syuzhet")) {
  install.packages("syuzhet")
  library("syuzhet")
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}
# --- Leer el texto y preprocesarlo ---
#el archivo "El pozo y el péndulo.txt" en la misma carpeta del script
texto <- tolower(readLines("El pozo y el péndulo.txt", encoding = "UTF-8"))
texto <- paste(texto, collapse = " ")
print(texto)

#Obtener oraciones y calcular las emociones
oraciones <- get_sentences(texto)
sentimientos <- get_sentiment(oraciones, method = "nrc", language = "spanish")

#  Transformar a arco emocional 
arco <- get_dct_transform(sentimientos, scale_range = TRUE)

# Preparar datos para ggplot2
df_arco <- data.frame(segmento = seq_along(arco), emocion = arco)

#Visualización del arco emocional con ggplot2
print(
  ggplot(df_arco, aes(x = segmento, y = emocion)) +
  geom_line(color = "#9C27B0", size = 1.2) +
  geom_point(color = "#9C27B0", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Arco emocional de El pozo y el péndulo",
    x = "Segmentos del texto",
    y = "Intensidad emocional"
  ) +
  theme_minimal()
)
