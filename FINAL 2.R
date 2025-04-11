library(tidyverse)      
library(janitor)        
library(psych)          
library(summarytools)   
library(ggpubr)         


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
    CV = round((DESVIACION / MEDIA), 3)
  )

print("📊 Resumen estadístico por grado:")
print(resumen)


print("📘 Estadísticas descriptivas con 'psych':")
describeBy(df$CEPILLADAS, group = df$GRADO)


tabla_frec <- df %>%
  group_by(GRADO, CEPILLADAS) %>%
  summarise(CANTIDAD = n(), .groups = "drop")


ggbarplot(resumen, x = "GRADO", y = "MEDIA", fill = "GRADO",
          title = "Promedio de cepilladas por día", ylab = "Promedio")


2: Frecuencia por grupoggbarplot(tabla_frec, x = "CEPILLADAS", y = "CANTIDAD", fill = "GRADO",
          position = "dodge", title = "Frecuencia de cepilladas por grado",
          xlab = "Cepilladas por día", ylab = "Cantidad")


ggbarplot(resumen, x = "GRADO", y = "CV", fill = "GRADO",
          title = "Coeficiente de variación por grado", ylab = "CV")

