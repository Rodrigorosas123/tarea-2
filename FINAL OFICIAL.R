library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(psych)
library(summarytools)

datos <- data.frame(
  GRADO = c("3° PRIMARIA", "5° PRIMARIA"),
  CANTIDADES = I(list(
    c(2,3,1,1,5,3,1,4,3,1,2,1,2,4,1,4,3,4,5,2,2,3,3,2,1,4),
    c(3,1,1,4,1,5,4,1,1,1,1,5,1,5,1,2,2,3,1,5,2,5,3,1,4,2)
  ))
)

df <- datos %>%
  unnest(cols = CANTIDADES) %>%
  group_by(GRADO) %>%
  mutate(ESTUDIANTE = row_number()) %>%
  rename(CEPILLADAS = CANTIDADES)

resumen <- df %>%
  group_by(GRADO) %>%
  summarise(
    MEDIA = mean(CEPILLADAS),
    DESVIACION = sd(CEPILLADAS),
    CV = round(DESVIACION / MEDIA, 3)
  )

tabla_frec <- df %>%
  group_by(GRADO, CEPILLADAS) %>%
  summarise(CANTIDAD = n(), .groups = "drop")

diferencia_media <- diff(resumen$MEDIA)
diferencia_sd <- diff(resumen$DESVIACION)
diferencia_cv <- diff(resumen$CV)


g1 <- ggplot(resumen, aes(x = GRADO, y = MEDIA, fill = GRADO)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(MEDIA,2)), vjust = -0.5, size = 5) +
  annotate("segment", x = 1, xend = 2, y = resumen$MEDIA[1], yend = resumen$MEDIA[2],
           colour = "red", arrow = arrow(length = unit(0.2,"cm")), linetype = "dashed") +
  annotate("text", x = 1.5, y = max(resumen$MEDIA) + 0.5,
           label = paste0("Δ = ", round(diferencia_media, 2)), color = "red") +
  labs(title = "Promedio de cepilladas por día", y = "Promedio", x = "") +
  theme_minimal()

g2 <- ggplot(tabla_frec, aes(x = factor(CEPILLADAS), y = CANTIDAD, fill = GRADO)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = CANTIDAD), position = position_dodge(0.7), vjust = -0.5, size = 4.5) +
  labs(title = "Frecuencia de cepilladas por día", x = "Cepilladas", y = "Cantidad de estudiantes") +
  theme_minimal()

g3 <- ggplot(resumen, aes(x = GRADO, y = DESVIACION, fill = GRADO)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(DESVIACION, 2)), vjust = -0.5, size = 5) +
  annotate("segment", x = 1, xend = 2, y = resumen$DESVIACION[1], yend = resumen$DESVIACION[2],
           colour = "blue", arrow = arrow(length = unit(0.2,"cm")), linetype = "dashed") +
  annotate("text", x = 1.5, y = max(resumen$DESVIACION) + 0.5,
           label = paste0("Δ = ", round(diferencia_sd, 2)), color = "blue") +
  labs(title = "Desviación estándar por grado", y = "Desviación", x = "") +
  theme_minimal()

g4 <- ggplot(resumen, aes(x = GRADO, y = CV, fill = GRADO)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = CV), vjust = -0.5, size = 5) +
  annotate("segment", x = 1, xend = 2, y = resumen$CV[1], yend = resumen$CV[2],
           colour = "darkgreen", arrow = arrow(length = unit(0.2,"cm")), linetype = "dashed") +
  annotate("text", x = 1.5, y = max(resumen$CV) + 0.05,
           label = paste0("Δ = ", round(diferencia_cv, 3)), color = "darkgreen") +
  labs(title = "Coeficiente de variación por grado", y = "CV", x = "") +
  theme_minimal()

resumen_largo <- resumen %>% pivot_longer(cols = -GRADO, names_to = "METRICA", values_to = "VALOR")
g5 <- ggplot(resumen_largo, aes(x = METRICA, y = VALOR, group = GRADO, color = GRADO)) +
  geom_line(aes(linetype = GRADO), size = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label = round(VALOR, 2)), vjust = -1, size = 5) +
  labs(title = "Comparación de métricas", x = "Métrica", y = "Valor") +
  theme_minimal()

stats_detalladas <- stby(df$CEPILLADAS, df$GRADO, descr, stats = "common")
print("📊 Estadísticas descriptivas completas:")
print(stats_detalladas)

print(stats_detalladas, method = "render", file = "Estadisticas_Cepilladas.pdf")


ggarrange(g1, g2, g3, g4, g5,
          ncol = 2, nrow = 3,
          labels = c("A", "B", "C", "D", "E"),
          heights = c(1, 1, 1.2),
          common.legend = TRUE, legend = "bottom")
