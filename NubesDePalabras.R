if (!require("ggwordcloud")) {install.packages("ggwordcloud")}
if (!require("tidytext")) {install.packages("tidytext")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("stringr")) {install.packages("stringr")}
library("ggwordcloud")
library("tidytext")
library("dplyr")
library("stringr")

texto_completo <- readLines("El pozo y el péndulo.txt", encoding = "UTF-8")

# Une todas las líneas en un solo string
texto_unido <- paste(texto_completo, collapse = " ")

# Limpia el texto: conviértelo a minúsculas, elimina números y puntuación
texto_limpio <- tolower(texto_unido)
texto_limpio <- str_replace_all(texto_limpio, "[0-9]", "") # Eliminar números
texto_limpio <- str_replace_all(texto_limpio, "[[:punct:]]", "") # Eliminar puntuación
texto_limpio <- str_replace_all(texto_limpio, "…", "") # Eliminar elipsis que no son puntuación estándar

datos_texto <- tibble(texto = texto_limpio)

datos_palabras <- datos_texto %>%
  unnest_tokens(palabra, texto)

# Carga las 'stop words' en español de tidytext
data("stop_words")
spanish_stop_words <- stop_words %>%
  filter(lexicon == "snowball") %>%
  select(word)

# Define una lista extendida de palabras a excluir manualmente.
# Incluye pronombres, verbos comunes, conectores y palabras genéricas que
# no aportan significado temático relevante, basándonos en tu lista.
palabras_a_excluir_extendida <- c(
  "cuanto",
  "como", "soy", "solo", "puede", "ser", "mas", "más", "bien", "así", "esto",
  "muy", "tan", "uno", "todo", "mis", "sus", "cada", "algún", "había", "habían",
  "dijo", "hacer", "hacía", "sido", "estar", "estaba", "estaban", "sentía", "era",
  "eran", "tiempo", "luego", "ahora", "vez", "siempre",
  "poco", "mucho", "casi", "cuando", "donde", "mientras", "parte", "cosas",
  "mismo", "misma", "suficiente", "después", "durante", "aunque", "hasta",
  "sin", "ni", "hacia", "sobre", "entre", "entre", "cual", "cuales", "aquella",
  "aquel", "aquellas", "aquellos", "esa", "ese", "esas", "esos", "ella", "ello",
  "ellos", "ellas", "él", "me", "te", "se", "nos", "os", "le", "les", "lo", "la",
  "los", "las", "mi", "tu", "su", "nuestro", "vuestro", "sus", "mis", "tus", "sus",
  "nuestros", "vuestros", "vuestras", "cierto", "cierta", "ciertos", "ciertas",
  "demás", "otro", "otra", "otros", "otras", "fueron", "había", "puedo", "poder",
  "debe", "deber", "podía", "pudiera", "hubiera", "quería", "parecía", "tenía", "tenían",
  "hecho", "hacerlo", "saber", "decir", "ir", "volver", "quedar", "ver", "oír",
  "gran", "grande", "pequeño", "pequeña", "primero", "segundo", "tercero", "cuarto",
  "final", "último", "principio", "medio", "punto", "línea", "lado", "manera",
  "modo", "claro", "oscuro", "difícil", "fácil", "real", "verdad", "falso", "posible",
  "imposible", "diferente", "igual", "propio", "propia", "que", "del", "por", "una", "con", "pero", "para", "fue", "nada", "antes", "paso", "menos", "todas", "dos", "haber", "qué", "fin", "pues", "entonces", "acababa", "completamente", "esta", "eso", "algo"
)

# Filtra las 'stop words' y las palabras irrelevantes adicionales
datos_finales <- datos_palabras %>%
  filter(!palabra %in% palabras_a_excluir_extendida) %>% # Usa la lista extendida
  filter(nchar(palabra) > 3) %>% # Filtra palabras muy cortas (de 3 caracteres o menos)
  count(palabra, name = "frecuencia") %>%
  arrange(desc(frecuencia)) # Ordena por frecuencia para ver las más comunes

# Limita el número de palabras a mostrar para evitar una nube demasiado densa.
num_palabras_a_mostrar <- 100
datos_para_nube <- head(datos_finales, num_palabras_a_mostrar)
print(
ggplot(datos_para_nube, aes(
  label = palabra,
  size = frecuencia,
  color = frecuencia
)) +
  geom_text_wordcloud(
    shape = "circle",
    rm_outside = TRUE,
    fontface = "bold",
    eccentricity = 0.5
  ) +
  scale_size_area(max_size = 15) + # Aumenta ligeramente el tamaño máximo para mayor visibilidad
  scale_color_gradient(
    low = "#2166ac", # Azul oscuro
    high = "#b2182b" # Rojo intenso
  ) +
  theme_void() +
  labs(
    title = "Nube de Palabras: 'El pozo y el péndulo' de Edgar Allan Poe",
    subtitle = paste0("Las ", num_palabras_a_mostrar, " palabras clave más frecuentes")
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 10))
  )

)