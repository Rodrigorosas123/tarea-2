# CARGAR LIBRERÍAS NECESARIAS
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(psych)
library(summarytools)
library(car)
library(multcomp)
library(report)  # <- Nueva librería para interpretación automática

# DATOS COMPLETOS
datos <- data.frame(
  GRADO = c("3° PRIMARIA", "4° PRIMARIA", "5° PRIMARIA"),
  CANTIDADES = I(list(
    c(2,3,1,1,5,3,1,4,3,1,2,1,2,4,1,4,3,4,5,2,2,3,3,2,1,4),
    c(5,4,3,1,2,1,3,4,5,3,1,2,2,4,3,3,1,5,3,4,2,2,3,2,1,4),
    c(3,1,1,4,1,5,4,1,1,1,1,5,1,5,1,2,2,3,1,5,2,5,3,1,4,2)
  ))
)

# TRANSFORMAR A FORMATO LARGO
df <- datos %>%
  unnest(cols = CANTIDADES) %>%
  group_by(GRADO) %>%
  mutate(ESTUDIANTE = row_number()) %>%
  rename(CEPILLADAS = CANTIDADES)

# CALCULO DE MEDIDAS ESTADÍSTICAS
resumen <- df %>%
  group_by(GRADO) %>%
  summarise(
    MEDIA = mean(CEPILLADAS),
    DESVIACION = sd(CEPILLADAS),
    CV = round(DESVIACION / MEDIA, 3)
  )

# TABLA DE FRECUENCIA
tabla_frec <- df %>%
  group_by(GRADO, CEPILLADAS) %>%
  summarise(CANTIDAD = n(), .groups = "drop")

# DIFERENCIAS ENTRE GRUPOS (SE USA PARA ANOTACIONES EN GRÁFICOS)
diferencia_media <- diff(range(resumen$MEDIA))
diferencia_sd <- diff(range(resumen$DESVIACION))
diferencia_cv <- diff(range(resumen$CV))

# GRAFICO 1 - PROMEDIO
g1 <- ggplot(resumen, aes(x = GRADO, y = MEDIA, fill = GRADO)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(MEDIA,2)), vjust = -0.5, size = 5) +
  labs(title = "Promedio de cepilladas por día", y = "Promedio", x = "") +
  theme_minimal()

# GRAFICO 2 - FRECUENCIA
g2 <- ggplot(tabla_frec, aes(x = factor(CEPILLADAS), y = CANTIDAD, fill = GRADO)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = CANTIDAD), position = position_dodge(0.7), vjust = -0.5, size = 4.5) +
  labs(title = "Frecuencia de cepilladas por día", x = "Cepilladas", y = "Cantidad de estudiantes") +
  theme_minimal()

# GRAFICO 3 - DESVIACIÓN ESTÁNDAR
g3 <- ggplot(resumen, aes(x = GRADO, y = DESVIACION, fill = GRADO)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(DESVIACION, 2)), vjust = -0.5, size = 5) +
  labs(title = "Desviación estándar por grado", y = "Desviación", x = "") +
  theme_minimal()

# GRAFICO 4 - COEFICIENTE DE VARIACIÓN
g4 <- ggplot(resumen, aes(x = GRADO, y = CV, fill = GRADO)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = CV), vjust = -0.5, size = 5) +
  labs(title = "Coeficiente de variación por grado", y = "CV", x = "") +
  theme_minimal()

# GRAFICO 5 - LINEAL COMPARATIVO DE LAS MÉTRICAS
resumen_largo <- resumen %>%
  pivot_longer(cols = -GRADO, names_to = "METRICA", values_to = "VALOR")

g5 <- ggplot(resumen_largo, aes(x = METRICA, y = VALOR, group = GRADO, color = GRADO)) +
  geom_line(aes(linetype = GRADO), size = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label = round(VALOR, 2)), vjust = -1, size = 5) +
  labs(title = "Comparación de métricas", x = "Métrica", y = "Valor") +
  theme_minimal()

# MOSTRAR TODOS LOS GRAFICOS JUNTOS
ggarrange(g1, g2, g3, g4, g5,
          ncol = 2, nrow = 3,
          labels = c("A", "B", "C", "D", "E"),
          heights = c(1, 1, 1.2),
          common.legend = TRUE, legend = "bottom")

# ESTADISTICAS DETALLADAS
stats_detalladas <- stby(df$CEPILLADAS, df$GRADO, descr, stats = "common")
print(stats_detalladas)

# ANÁLISIS DE VARIANZA (ANOVA)
anova_model <- aov(CEPILLADAS ~ GRADO, data = df)
summary(anova_model)

# PRUEBA POST HOC: TUKEY HSD
tukey_result <- TukeyHSD(anova_model)
print(tukey_result)

# GRAFICO DE TUKEY (OPCIONAL)
plot(tukey_result)

# INTERPRETACIÓN AUTOMÁTICA CON REPORT
reporte_anova <- report(anova_model)
print(reporte_anova)