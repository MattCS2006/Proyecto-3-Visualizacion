#Análisis de Polaridad del cuento

# Cargar o instalar paquetes necesarios
if (!require("syuzhet")) {
  install.packages("syuzhet")
  library("syuzhet")
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}

# Leer el texto y preprocesarlo
# Asegúrate de tener el archivo "El pozo y el péndulo.txt" en la misma carpeta del script
texto <- tolower(readLines("El pozo y el péndulo.txt", encoding = "UTF-8"))
texto <- paste(texto, collapse = " ")

#Procesamiento de oraciones y análisis de sentimiento
oraciones <- get_sentences(texto)
sentimientos <- get_sentiment(oraciones, method = "nrc", language = "spanish")

#Crear dataframe con resultados
datos <- data.frame(
  oracion = 1:length(sentimientos),
  valor = sentimientos,
  texto = oraciones
)
#print(texto)
#Visualización con ggplot2
print(
ggplot(datos, aes(x = oracion, y = valor)) +
  geom_line(color = "#1E88E5", linewidth = 1.2) +
  geom_point(color = "#D81B60", size = 1) +
  labs(
    title = "Análisis de polaridad: El pozo y el péndulo",
    subtitle = "Método: NRC (español)",
    x = "Oración",
    y = "Sentimiento (de negativo a positivo)"
  ) +
  theme_minimal()
)
