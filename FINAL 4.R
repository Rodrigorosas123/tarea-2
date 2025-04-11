# Instalar paquetes si no están instalados (solo una vez)
# install.packages(c("ggplot2", "dplyr", "tidyr", "ggpubr"))

# Cargar librerías
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

# Crear los datos
datos <- data.frame(
  GRADO = c("3° PRIMARIA", "5° PRIMARIA"),
  CANTIDADES = I(list(
    c(2,3,1,1,5,3,1,4,3,1,2,1,2,4,1,4,3,4,5,2,2,3,3,2,1,4),
    c(3,1,1,4,1,5,4,1,1,1,1,5,1,5,1,2,2,3,1,5,2,5,3,1,4,2)
  ))
)

# Expandir datos en formato largo
df <- datos %>%
  unnest(cols = c(CANTIDADES)) %>%
  group_by(GRADO) %>%
  mutate(ESTUDIANTE = row_number()) %>%
  rename(CEPILLADAS = CANTIDADES)

# Resumen estadístico
resumen <- df %>%
  group_by(GRADO) %>%
  summarise(
    MEDIA = mean(CEPILLADAS),
    DESVIACION = sd(CEPILLADAS),
    CV = round((DESVIACION / MEDIA), 3)
  )

# Calcular diferencias
diferencia_media <- diff(resumen$MEDIA)
diferencia_sd <- diff(resumen$DESVIACION)
diferencia_cv <- diff(resumen$CV)

# Gráfico 1: Promedio
g1 <- ggplot(resumen, aes(x = GRADO, y = MEDIA, fill = GRADO)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = round(MEDIA,2)), vjust = -0.5) +
  annotate("segment", x = 1, xend = 2, y = resumen$MEDIA[1], yend = resumen$MEDIA[2],
           colour = "red", arrow = arrow(length = unit(0.2,"cm")), linetype = "dashed") +
  annotate("text", x = 1.5, y = max(resumen$MEDIA) + 0.5,
           label = paste0("Δ = ", round(diferencia_media, 2)), color = "red") +
  labs(title = "Promedio de cepilladas por día", y = "Promedio", x = "") +
  theme_minimal()

# Gráfico 2: Desviación estándar
g2 <- ggplot(resumen, aes(x = GRADO, y = DESVIACION, fill = GRADO)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = round(DESVIACION,2)), vjust = -0.5) +
  annotate("segment", x = 1, xend = 2, y = resumen$DESVIACION[1], yend = resumen$DESVIACION[2],
           colour = "blue", arrow = arrow(length = unit(0.2,"cm")), linetype = "dashed") +
  annotate("text", x = 1.5, y = max(resumen$DESVIACION) + 0.5,
           label = paste0("Δ = ", round(diferencia_sd, 2)), color = "blue") +
  labs(title = "Desviación estándar por grado", y = "Desviación estándar", x = "") +
  theme_minimal()

# Gráfico 3: Coeficiente de variación
g3 <- ggplot(resumen, aes(x = GRADO, y = CV, fill = GRADO)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = round(CV,2)), vjust = -0.5) +
  annotate("segment", x = 1, xend = 2, y = resumen$CV[1], yend = resumen$CV[2],
           colour = "darkgreen", arrow = arrow(length = unit(0.2,"cm")), linetype = "dashed") +
  annotate("text", x = 1.5, y = max(resumen$CV) + 0.1,
           label = paste0("Δ = ", round(diferencia_cv, 3)), color = "darkgreen") +
  labs(title = "Coeficiente de variación por grado", y = "CV", x = "") +
  theme_minimal()

# Mostrar los gráficos por separado
print(g1)
print(g2)
print(g3)
