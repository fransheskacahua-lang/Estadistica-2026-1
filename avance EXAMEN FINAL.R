
# ============================================================
# ANALISIS ESTADISTICO - COMPLEJO RESPIRATORIO INFECCIOSO CANINO
# Base teorica para Sistematizacion y Metodos Estadisticos
# ============================================================
# Tema:
# Factores asociados al complejo respiratorio infeccioso canino
# en perros atendidos en clinicas veterinarias.
#
# Articulo de referencia:
# Schulz BS, Kurz S, Weber K, Balzer HJ, Hartmann K.
# Detection of respiratory viruses and Bordetella bronchiseptica
# in dogs with acute respiratory tract infections. The Veterinary Journal. 2014.
#
# IMPORTANTE:
# La base es teorica/simulada con fines docentes.
# El objetivo es que el estudiante practique analisis descriptivo,
# graficos, pruebas de asociacion, Odds Ratio y regresion logistica.
# ============================================================


# ============================================================
# 1. INSTALAR Y CARGAR PAQUETES
# ============================================================

# Ejecutar esta parte solo si no tienen los paquetes instalados.
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("janitor")
# install.packages("gtsummary")
# install.packages("broom")
# install.packages("epitools")
# install.packages("writexl")
# install.packages("forcats")

library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)
library(gtsummary)
library(broom)
library(epitools)
library(writexl)
library(forcats)


# ============================================================
# 2. CARGAR LA BASE DE DATOS
# ============================================================

# Coloca este script en la misma carpeta donde esta el Excel.
# Luego ejecuta todo el codigo.

datos <- read_excel("base_complejo_respiratorio_canino_teorica.xlsx",
                    sheet = "base_datos")

# Ver primeras filas
head(datos)

# Ver estructura general
str(datos)

# Ver nombres de variables
names(datos)


# ============================================================
# 3. PREPARAR VARIABLES
# ============================================================
# En esta seccion convertimos las variables categoricas a factor.
# Esto ayuda a que R las reconozca correctamente en tablas y modelos.

datos <- datos %>%
  mutate(
    sexo = factor(sexo),
    raza = factor(raza),
    vacunado_bordetella = factor(vacunado_bordetella, levels = c("Si", "No")),
    vacunado_multiple = factor(vacunado_multiple, levels = c("Si", "No")),
    guarderia_canina = factor(guarderia_canina, levels = c("No", "Si")),
    convivencia_otros_perros = factor(convivencia_otros_perros, levels = c("No", "Si")),
    visitas_parques = factor(visitas_parques, levels = c("Nunca", "Ocasional", "Frecuente")),
    procedencia = factor(procedencia, levels = c("Hogar", "Criadero", "Albergue")),
    hacinamiento = factor(hacinamiento, levels = c("No", "Si")),
    tos = factor(tos, levels = c("No", "Si")),
    descarga_nasal = factor(descarga_nasal, levels = c("No", "Si")),
    fiebre = factor(fiebre, levels = c("No", "Si")),
    complejo_respiratorio = factor(complejo_respiratorio, levels = c("No", "Si"))
  )

# Crear carpeta para guardar resultados
if(!dir.exists("resultados_CIRDC")){
  dir.create("resultados_CIRDC")
}


# ============================================================
# 4. OBJETIVOS DE INVESTIGACION
# ============================================================

# Objetivo general:
# Determinar los factores asociados a la presentacion del
# complejo respiratorio infeccioso canino.

# Objetivos especificos:
# OE1: Describir las caracteristicas demograficas, clinicas y de manejo.
# OE2: Determinar la frecuencia del complejo respiratorio.
# OE3: Evaluar la asociacion entre vacunacion frente a Bordetella y enfermedad.
# OE4: Evaluar la asociacion entre guarderia canina y enfermedad.
# OE5: Evaluar la asociacion entre procedencia, hacinamiento y enfermedad.
# OE6: Identificar factores asociados mediante regresion logistica.


# ============================================================
# 5. OE1 - ANALISIS DESCRIPTIVO
# ============================================================

# 5.1 Resumen de variables numericas
resumen_numerico <- datos %>%
  summarise(
    n = n(),
    edad_media = mean(edad_meses, na.rm = TRUE),
    edad_sd = sd(edad_meses, na.rm = TRUE),
    edad_mediana = median(edad_meses, na.rm = TRUE),
    edad_min = min(edad_meses, na.rm = TRUE),
    edad_max = max(edad_meses, na.rm = TRUE)
  )

resumen_numerico

# 5.2 Tablas de frecuencia de variables categoricas

tabla_sexo <- datos %>% tabyl(sexo) %>% adorn_pct_formatting()
tabla_raza <- datos %>% tabyl(raza) %>% adorn_pct_formatting()
tabla_vacuna_bordetella <- datos %>% tabyl(vacunado_bordetella) %>% adorn_pct_formatting()
tabla_guarderia <- datos %>% tabyl(guarderia_canina) %>% adorn_pct_formatting()
tabla_procedencia <- datos %>% tabyl(procedencia) %>% adorn_pct_formatting()
tabla_hacinamiento <- datos %>% tabyl(hacinamiento) %>% adorn_pct_formatting()

tabla_sexo
tabla_raza
tabla_vacuna_bordetella
tabla_guarderia
tabla_procedencia
tabla_hacinamiento

# 5.3 Tabla descriptiva general lista para informe
tabla_descriptiva <- datos %>%
  select(
    edad_meses,
    sexo,
    raza,
    vacunado_bordetella,
    vacunado_multiple,
    guarderia_canina,
    convivencia_otros_perros,
    visitas_parques,
    procedencia,
    hacinamiento,
    tos,
    descarga_nasal,
    fiebre,
    complejo_respiratorio
  ) %>%
  tbl_summary(
    by = complejo_respiratorio,
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_overall() %>%
  add_p()

tabla_descriptiva


# ============================================================
# 6. GRAFICOS DESCRIPTIVOS
# ============================================================

# Grafico 1: Histograma de edad
g1 <- ggplot(datos, aes(x = edad_meses)) +
  geom_histogram(bins = 25) +
  labs(
    title = "Distribucion de edad de los perros evaluados",
    x = "Edad (meses)",
    y = "Frecuencia"
  ) +
  theme_minimal()

g1
ggsave("resultados_CIRDC/grafico_01_histograma_edad.png", g1,
       width = 8, height = 5, dpi = 300)


# Grafico 2: Frecuencia de complejo respiratorio
g2 <- ggplot(datos, aes(x = complejo_respiratorio)) +
  geom_bar() +
  labs(
    title = "Frecuencia del complejo respiratorio infeccioso canino",
    x = "Complejo respiratorio",
    y = "Numero de perros"
  ) +
  theme_minimal()

g2
ggsave("resultados_CIRDC/grafico_02_frecuencia_enfermedad.png", g2,
       width = 7, height = 5, dpi = 300)


# Grafico 3: Enfermedad segun vacunacion frente a Bordetella
g3 <- ggplot(datos, aes(x = vacunado_bordetella,
                        fill = complejo_respiratorio)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proporcion de enfermedad segun vacunacion frente a Bordetella",
    x = "Vacunado frente a Bordetella",
    y = "Proporcion",
    fill = "Complejo respiratorio"
  ) +
  theme_minimal()

g3
ggsave("resultados_CIRDC/grafico_03_vacunacion_bordetella.png", g3,
       width = 8, height = 5, dpi = 300)


# Grafico 4: Enfermedad segun asistencia a guarderia
g4 <- ggplot(datos, aes(x = guarderia_canina,
                        fill = complejo_respiratorio)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proporcion de enfermedad segun asistencia a guarderia canina",
    x = "Asiste a guarderia canina",
    y = "Proporcion",
    fill = "Complejo respiratorio"
  ) +
  theme_minimal()

g4
ggsave("resultados_CIRDC/grafico_04_guarderia_canina.png", g4,
       width = 8, height = 5, dpi = 300)


# Grafico 5: Enfermedad segun procedencia
g5 <- ggplot(datos, aes(x = procedencia,
                        fill = complejo_respiratorio)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proporcion de enfermedad segun procedencia del perro",
    x = "Procedencia",
    y = "Proporcion",
    fill = "Complejo respiratorio"
  ) +
  theme_minimal()

g5
ggsave("resultados_CIRDC/grafico_05_procedencia.png", g5,
       width = 8, height = 5, dpi = 300)


# Grafico 6: Enfermedad segun hacinamiento
g6 <- ggplot(datos, aes(x = hacinamiento,
                        fill = complejo_respiratorio)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proporcion de enfermedad segun hacinamiento",
    x = "Hacinamiento",
    y = "Proporcion",
    fill = "Complejo respiratorio"
  ) +
  theme_minimal()

g6
ggsave("resultados_CIRDC/grafico_06_hacinamiento.png", g6,
       width = 8, height = 5, dpi = 300)


# Grafico 7: Boxplot de edad segun enfermedad
g7 <- ggplot(datos, aes(x = complejo_respiratorio,
                        y = edad_meses)) +
  geom_boxplot() +
  labs(
    title = "Edad de los perros segun presencia de complejo respiratorio",
    x = "Complejo respiratorio",
    y = "Edad (meses)"
  ) +
  theme_minimal()

g7
ggsave("resultados_CIRDC/grafico_07_boxplot_edad.png", g7,
       width = 7, height = 5, dpi = 300)


# ============================================================
# 7. OE2 - FRECUENCIA / PREVALENCIA DE ENFERMEDAD
# ============================================================

tabla_prevalencia <- datos %>%
  tabyl(complejo_respiratorio) %>%
  adorn_pct_formatting()

tabla_prevalencia

# Interpretacion:
# El porcentaje de perros con "Si" corresponde a la frecuencia
# de complejo respiratorio infeccioso canino en esta base teorica.


# ============================================================
# 8. FUNCION PARA CHI-CUADRADO/FISHER Y ODDS RATIO
# ============================================================
# Esta funcion permite analizar rapidamente una variable categorica
# frente a la variable resultado.

analisis_bivariado <- function(variable){

  tabla <- table(datos[[variable]], datos$complejo_respiratorio)
  print(tabla)

  # Si alguna celda esperada es menor a 5, usamos Fisher.
  prueba_chi <- suppressWarnings(chisq.test(tabla))

  if(any(prueba_chi$expected < 5)){
    prueba <- fisher.test(tabla)
    tipo_prueba <- "Fisher"
  } else {
    prueba <- prueba_chi
    tipo_prueba <- "Chi-cuadrado"
  }

  cat("\nPrueba usada:", tipo_prueba, "\n")
  print(prueba)

  # Odds Ratio solo es directo cuando la tabla es 2x2.
  if(nrow(tabla) == 2 & ncol(tabla) == 2){
    print(oddsratio(tabla))
  } else {
    cat("\nOdds Ratio no mostrado porque la variable tiene mas de 2 categorias.\n")
  }
}


# ============================================================
# 9. OE3 - VACUNACION BORDETELLA Y COMPLEJO RESPIRATORIO
# ============================================================

analisis_bivariado("vacunado_bordetella")

# Interpretacion esperada:
# Si p < 0.05, existe asociacion estadisticamente significativa
# entre vacunacion frente a Bordetella y complejo respiratorio.
# Si el OR es mayor a 1 para los no vacunados, se interpreta como
# mayor probabilidad de enfermedad en perros no vacunados.


# ============================================================
# 10. OE4 - GUARDERIA CANINA Y COMPLEJO RESPIRATORIO
# ============================================================

analisis_bivariado("guarderia_canina")

# Interpretacion esperada:
# Si p < 0.05, existe asociacion entre asistir a guarderia canina
# y la presencia de complejo respiratorio.


# ============================================================
# 11. OE5 - PROCEDENCIA, HACINAMIENTO Y COMPLEJO RESPIRATORIO
# ============================================================

analisis_bivariado("procedencia")

analisis_bivariado("hacinamiento")

analisis_bivariado("convivencia_otros_perros")

analisis_bivariado("visitas_parques")


# ============================================================
# 12. ANALISIS BIVARIADO COMPLEMENTARIO
# ============================================================

analisis_bivariado("vacunado_multiple")
analisis_bivariado("raza")
analisis_bivariado("sexo")
analisis_bivariado("tos")
analisis_bivariado("descarga_nasal")
analisis_bivariado("fiebre")


# ============================================================
# 13. COMPARACION DE EDAD SEGUN ENFERMEDAD
# ============================================================

# Primero revisamos normalidad por grupo.
by(datos$edad_meses, datos$complejo_respiratorio, shapiro.test)

# En bases medianas/grandes, si la edad no es normal,
# se puede usar Wilcoxon.
wilcox.test(edad_meses ~ complejo_respiratorio, data = datos)

# Interpretacion:
# Si p < 0.05, la edad difiere entre perros con y sin enfermedad.


# ============================================================
# 14. OE6 - REGRESION LOGISTICA MULTIVARIADA
# ============================================================

# Para regresion logistica, R necesita una variable resultado binaria.
# Vamos a crear una variable numerica:
# 1 = Si presenta complejo respiratorio
# 0 = No presenta complejo respiratorio

datos <- datos %>%
  mutate(
    complejo_binario = ifelse(complejo_respiratorio == "Si", 1, 0)
  )

# Modelo multivariado:
# Incluimos variables epidemiologicamente importantes y faciles de interpretar.

modelo <- glm(
  complejo_binario ~
    edad_meses +
    vacunado_bordetella +
    guarderia_canina +
    convivencia_otros_perros +
    visitas_parques +
    procedencia +
    hacinamiento +
    vacunado_multiple,
  data = datos,
  family = binomial
)

summary(modelo)


# ============================================================
# 15. ODDS RATIO AJUSTADOS
# ============================================================

or_ajustados <- exp(cbind(
  OR = coef(modelo),
  confint(modelo)
))

or_ajustados

# Interpretacion:
# OR > 1: mayor probabilidad de enfermedad.
# OR < 1: menor probabilidad de enfermedad o efecto protector.
# Si el intervalo de confianza no incluye 1, el resultado suele considerarse significativo.


# Tabla ordenada y limpia de la regresion
tabla_modelo <- tbl_regression(
  modelo,
  exponentiate = TRUE
)

tabla_modelo


# ============================================================
# 16. FOREST PLOT DE ODDS RATIO AJUSTADOS
# ============================================================

modelo_tidy <- tidy(modelo, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = fct_reorder(term, estimate)
  )

g8 <- ggplot(modelo_tidy,
             aes(x = estimate,
                 y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high),
                 height = 0.2) +
  geom_vline(xintercept = 1,
             linetype = "dashed") +
  scale_x_log10() +
  labs(
    title = "Odds Ratio ajustados para complejo respiratorio canino",
    x = "Odds Ratio ajustado, escala logaritmica",
    y = "Variable"
  ) +
  theme_minimal()

g8
ggsave("resultados_CIRDC/grafico_08_forest_plot_or_ajustados.png", g8,
       width = 9, height = 6, dpi = 300)


# ============================================================
# 17. PROBABILIDADES PREDICHAS
# ============================================================
# Este grafico ayuda a entender como cambia la probabilidad estimada
# de enfermedad segun guarderia y vacunacion.

nuevo <- expand.grid(
  edad_meses = median(datos$edad_meses),
  vacunado_bordetella = factor(c("Si", "No"), levels = levels(datos$vacunado_bordetella)),
  guarderia_canina = factor(c("No", "Si"), levels = levels(datos$guarderia_canina)),
  convivencia_otros_perros = factor("No", levels = levels(datos$convivencia_otros_perros)),
  visitas_parques = factor("Ocasional", levels = levels(datos$visitas_parques)),
  procedencia = factor("Hogar", levels = levels(datos$procedencia)),
  hacinamiento = factor("No", levels = levels(datos$hacinamiento)),
  vacunado_multiple = factor("Si", levels = levels(datos$vacunado_multiple))
)

nuevo$probabilidad_predicha <- predict(modelo, newdata = nuevo, type = "response")

nuevo

g9 <- ggplot(nuevo,
             aes(x = vacunado_bordetella,
                 y = probabilidad_predicha,
                 fill = guarderia_canina)) +
  geom_col(position = "dodge") +
  labs(
    title = "Probabilidad predicha de complejo respiratorio",
    subtitle = "Segun vacunacion frente a Bordetella y asistencia a guarderia",
    x = "Vacunacion frente a Bordetella",
    y = "Probabilidad predicha",
    fill = "Guarderia canina"
  ) +
  theme_minimal()

g9
ggsave("resultados_CIRDC/grafico_09_probabilidades_predichas.png", g9,
       width = 8, height = 5, dpi = 300)


# ============================================================
# 18. EXPORTAR RESULTADOS A EXCEL
# ============================================================

# Guardamos tablas principales para que puedan usarlas en el informe.

resultados_exportar <- list(
  resumen_numerico = resumen_numerico,
  prevalencia = tabla_prevalencia,
  frecuencia_sexo = tabla_sexo,
  frecuencia_raza = tabla_raza,
  frecuencia_vacunacion_bordetella = tabla_vacuna_bordetella,
  frecuencia_guarderia = tabla_guarderia,
  frecuencia_procedencia = tabla_procedencia,
  frecuencia_hacinamiento = tabla_hacinamiento,
  or_ajustados = as.data.frame(or_ajustados),
  probabilidades_predichas = nuevo
)

write_xlsx(resultados_exportar,
           "resultados_CIRDC/tablas_resultados_CIRDC.xlsx")


# ============================================================
# 19. RESPUESTAS A LOS OBJETIVOS
# ============================================================
# Esta parte crea un archivo de texto con una guia de interpretacion.
# Los estudiantes deben completar los espacios XX con los valores obtenidos.

sink("resultados_CIRDC/respuestas_objetivos_CIRDC.txt")

cat("RESPUESTAS A LOS OBJETIVOS - COMPLEJO RESPIRATORIO INFECCIOSO CANINO\n")
cat("=====================================================================\n\n")

cat("Objetivo general:\n")
cat("Determinar los factores asociados a la presentacion del complejo respiratorio infeccioso canino.\n\n")

cat("OE1. Describir las caracteristicas de los perros evaluados.\n")
cat("Respuesta sugerida:\n")
cat("Se evaluaron", nrow(datos), "perros. La edad promedio fue de",
    round(mean(datos$edad_meses), 2), "meses, con una desviacion estandar de",
    round(sd(datos$edad_meses), 2), "meses.\n\n")

cat("OE2. Determinar la frecuencia del complejo respiratorio.\n")
prev <- prop.table(table(datos$complejo_respiratorio))["Si"]*100
cat("La frecuencia de complejo respiratorio infeccioso canino fue de",
    round(prev, 2), "%.\n\n")

cat("OE3. Evaluar la asociacion entre vacunacion frente a Bordetella y enfermedad.\n")
cat("Interpretar el p-valor de la prueba chi-cuadrado/Fisher y el OR crudo.\n")
cat("Si p < 0.05, se concluye que existe asociacion estadisticamente significativa.\n\n")

cat("OE4. Evaluar la asociacion entre guarderia canina y enfermedad.\n")
cat("Interpretar si la asistencia a guarderia incrementa la probabilidad de enfermedad.\n\n")

cat("OE5. Evaluar procedencia, hacinamiento y exposicion a otros perros.\n")
cat("Interpretar las tablas cruzadas, p-valores y OR cuando corresponda.\n\n")

cat("OE6. Identificar factores asociados mediante regresion logistica.\n")
cat("Revisar los OR ajustados. Las variables con p < 0.05 e intervalo de confianza que no incluya 1 se consideran asociadas.\n\n")

cat("Conclusion general sugerida:\n")
cat("Los factores relacionados con mayor contacto entre perros, como guarderia, convivencia con otros perros, procedencia de albergue o criadero y hacinamiento, pueden aumentar la probabilidad de complejo respiratorio. La vacunacion frente a Bordetella puede actuar como factor protector si el OR ajustado es menor que 1.\n\n")

sink()


# ============================================================
# 20. FIN DEL SCRIPT
# ============================================================

cat("Analisis finalizado. Revisa la carpeta 'resultados_CIRDC'.\n")
cat("Alli encontraras graficos, tablas y respuestas a objetivos.\n")
