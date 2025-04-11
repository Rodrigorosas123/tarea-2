library(dplyr)
library(ggplot2)
library(ggpubr)
library(psych)
library(summarytools)
library(tidyr)

datos <- data.frame(
  GRADO = c("3° PRIMARIA", "5° PRIMARIA"),
  CANTIDADES = I(list(
    c(2,3,1,1,5,3,1,4,3,1,2,1,2,4,1,4,3,4,5,2,2,3,3,2,1,4),
    c(3,1,1,4,1,5,4,1,1,1,1,5,1,5,1,2,2,3,1,5,2,5,3,1,4,2)
  ))
)

df <- datos %>%
  unnest(cols = c(CANTIDADES)) %>%
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

g1 <- ggplot(resumen, aes(x = GRADO, y = MEDIA, fill = GRADO)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(MEDIA, 2)), vjust = -0.5, size = 6) +
  labs(title = "Promedio de cepilladas por día", y = "Promedio", x = "") +
  theme_minimal(base_size = 14) +
  ylim(0, max(resumen$MEDIA) + 1)

g2 <- ggplot(tabla_frec, aes(x = factor(CEPILLADAS), y = CANTIDAD, fill = GRADO)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = CANTIDAD), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 5) +
  labs(title = "Frecuencia de cepilladas por día", x = "Cepilladas por día", y = "Cantidad de estudiantes") +
  theme_minimal(base_size = 14)

g3 <- ggplot(resumen, aes(x = GRADO, y = DESVIACION, fill = GRADO)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(DESVIACION, 2)), vjust = -0.5, size = 6) +
  labs(title = "Desviación estándar por grado", y = "Desviación", x = "") +
  theme_minimal(base_size = 14) +
  ylim(0, max(resumen$DESVIACION) + 1)

g4 <- ggplot(resumen, aes(x = GRADO, y = CV, fill = GRADO)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = CV), vjust = -0.5, size = 6) +
  labs(title = "Coeficiente de variación por grado", y = "CV", x = "") +
  theme_minimal(base_size = 14) +
  ylim(0, max(resumen$CV) + 0.1)

resumen_largo <- resumen %>%
  pivot_longer(cols = -GRADO, names_to = "METRICA", values_to = "VALOR")

g5 <- ggplot(resumen_largo, aes(x = METRICA, y = VALOR, group = GRADO, color = GRADO)) +
  geom_line(aes(linetype = GRADO), size = 1.3) +
  geom_point(size = 4) +
  geom_text(aes(label = round(VALOR, 2)), vjust = -1, size = 5) +
  labs(title = "Comparación de métricas entre grados", x = "Métrica", y = "Valor") +
  theme_minimal(base_size = 14)

stats_detalladas <- stby(df$CEPILLADAS, df$GRADO, descr, stats = "common")

print("📊 Estadísticas descriptivas completas:")
print(stats_detalladas)

print(stats_detalladas, method = "render", file = "Estadisticas_Cepilladas.pdf")

ggarrange(g1, g2, g3, g4, g5,
          ncol = 2, nrow = 3,
          labels = c("A", "B", "C", "D", "E"),
          heights = c(1, 1, 1.2),
          common.legend = TRUE, legend = "bottom")
