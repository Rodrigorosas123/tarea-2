# Instalar si no tienes (solo la primera vez)
# install.packages(c("dplyr", "ggplot2", "ggpubr", "psych", "summarytools", "tidyr"))

# Cargar librerías
library(dplyr)
library(ggplot2)
library(ggpubr)
library(psych)
library(summarytools)
library(tidyr)

# Crear los datos
datos <- data.frame(
  GRADO = c("3° PRIMARIA", "5° PRIMARIA"),
  CANTIDADES = I(list(
    c(2,3,1,1,5,3,1,4,3,1,2,1,2,4,1,4,3,4,5,2,2,3,3,2,1,4),
    c(3,1,1,4,1,5,4,1,1,1,1,5,1,5,1,2,2,3,1,5,2,5,3,1,4,2)
  ))
)

# Desenrollar los datos
df <- datos %>%
  unnest(cols = c(CANTIDADES)) %>%
  group_by(GRADO) %>%
  mutate(ESTUDIANTE = row_number()) %>%
  rename(CEPILLADAS = CANTIDADES)

# 1. Promedio, desviación y coeficiente de variación
resumen <- df %>%
  group_by(GRADO) %>%
  summarise(
    MEDIA = mean(CEPILLADAS),
    DESVIACION = sd(CEPILLADAS),
    CV = round(DESVIACION / MEDIA, 3)
  )

# 2. Tabla de frecuencia por grado y cepilladas
tabla_frec <- df %>%
  group_by(GRADO, CEPILLADAS) %>%
  summarise(CANTIDAD = n(), .groups = "drop")

# -------------------- GRÁFICOS --------------------

# Gráfico 1: Promedio
g1 <- ggplot(resumen, aes(x = GRADO, y = MEDIA, fill = GRADO)) +
  geom_col() +
  geom_text(aes(label = round(MEDIA, 2)), vjust = -0.5, size = 5) +
  labs(title = "Promedio de cepilladas por día", y = "Promedio", x = "") +
  theme_minimal()

# Gráfico 2: Frecuencia por cantidad de cepilladas
g2 <- ggplot(tabla_frec, aes(x = factor(CEPILLADAS), y = CANTIDAD, fill = GRADO)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = CANTIDAD), position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
  labs(title = "Frecuencia de cepilladas por día", x = "Cepilladas por día", y = "Cantidad de estudiantes") +
  theme_minimal()

# Gráfico 3: Desviación estándar
g3 <- ggplot(resumen, aes(x = GRADO, y = DESVIACION, fill = GRADO)) +
  geom_col() +
  geom_text(aes(label = round(DESVIACION, 2)), vjust = -0.5, size = 5) +
  labs(title = "Desviación estándar por grado", y = "Desviación estándar", x = "") +
  theme_minimal()

# Gráfico 4: Coeficiente de variación
g4 <- ggplot(resumen, aes(x = GRADO, y = CV, fill = GRADO)) +
  geom_col() +
  geom_text(aes(label = CV), vjust = -0.5, size = 5) +
  labs(title = "Coeficiente de variación por grado", y = "CV", x = "") +
  theme_minimal()

# -------------------- ESTADÍSTICAS COMPLETAS --------------------

# Estadísticas detalladas
stats_detalladas <- stby(df$CEPILLADAS, df$GRADO, descr, stats = "common")

# Mostrar por consola
print("📊 Estadísticas descriptivas completas:")
print(stats_detalladas)

# Exportar tabla a PDF
print(stats_detalladas, method = "render", file = "Estadisticas_Cepilladas.pdf")

# -------------------- MOSTRAR GRÁFICOS --------------------

# Si quieres mostrarlos todos juntos en RStudio:
ggarrange(g1, g2, g3, g4, 
          ncol = 2, nrow = 2, 
          labels = c("A", "B", "C", "D"),
          common.legend = TRUE, legend = "bottom")

