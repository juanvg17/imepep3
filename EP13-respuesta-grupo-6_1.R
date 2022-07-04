# Actividad 13 IME
# Grupo 6: Angel Avendaño, Jorge González y Juan Vásquez

# Importar paquetes.
if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE)
  require(tidyverse)
}

if(!require(ggpubr)){
  install.packages("ggpubr",dependencies = TRUE)
  require(ggpubr)
}

if(!require(ez)){
  install.packages("ez",dependencies = TRUE)
  require(ez)
}
if(!require(nlme)){
  install.packages("nlme",dependencies = TRUE)
  require(nlme)
}
if(!require(emmeans)){
  install.packages("emmeans",dependencies = TRUE)
  require(emmeans)
}

if(!require(ggplot2)){
  install.packages("ggplot2",dependencies = TRUE)
  require(ggplot2)
}

if(!require(WRS2)){
  install.packages("WRS2",dependencies = TRUE)
  require(WRS2)
}

if(!require(leaps)){
  install.packages("leaps",dependencies = TRUE)
  require(leaps)
}

if(!require(car)){
  install.packages("car",dependencies = TRUE)
  require(car)
}

# Se pide construir un modelo de regresión lineal múltiple para predecir la variable Peso, de acuerdo con las 
# siguientes instrucciones:

# 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos del RUN (sin considerar el dígito 
# verificador) del integrante de menor edad del equipo.

# 2. Seleccionar una muestra de 50 mujeres (si la semilla es un número par) o 50 hombres (si la semilla es impar).

# 3. Seleccionar de forma aleatoria ocho posibles variables predictoras.

# 4. Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la variable 
# Peso, justificando bien esta selección.

# 5. Usando el entorno R, construir un modelo de regresión lineal simple con el predictor seleccionado en el 
# paso anterior.

# 6. Usando herramientas para la exploración de modelos del entorno R, buscar entre dos y cinco predictores de 
# entre las variables seleccionadas al azar en el punto 3, para agregar al modelo de regresión lineal simple 
# obtenido en el paso 5.

# 7. Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema con las condiciones que deben 
# cumplir.

# 8. Evaluar el poder predictivo del modelo en datos no utilizados para construirlo (o utilizando validación 
# cruzada).


# Archivo de entrada 
poblacion <- read.csv2(file.choose(new = FALSE), encoding="utf8")

# Definir una semilla (1)
set.seed(8470)

# Seleccionar la muestra (2)
muestra <- poblacion %>% filter(Gender == '0')
muestra <- sample_n(muestra, size = 50, replace = FALSE)

# Seleccionar 8 variables de la tabla aleatoriamente (3)
# De las que no seleccionamos elegir una que sea coherente para predecir el peso.
muestra1 <- muestra[,sample(ncol(muestra), size = 8)]
muestra1 <- cbind(muestra1, muestra$Weight)
names(muestra1)[names(muestra1) == "muestra$Weight"] <- "Weight"

# Variables seleccionadas:
# Profundidad.del.pecho, Circunferencia.de.la.rodilla, Circunferencia.del.muslo,
# Circunferencia.de.la.cintura, Circunferencia.del.bíceps, Diámetro.del.pecho


# 4. Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la variable 
# Peso, justificando bien esta selección.

# La circunferencia de la cadera es una medida útil al momento de predecir el peso de las personas, según estudios,
# en los niños y adolescentes se puede usar este indicador indiferentemente del sexo de la persona para diagnosticar
# obesidad, sin embargo, para adultos se presentan diferencias en lo que a sexo corresponde, para fines de este estudio
# sigue siendo útil este indicador.

# https://docs.bvsalud.org/biblioref/2020/04/1094977/17889-144814488126-1-pb.pdf


# 5. Usando el entorno R, construir un modelo de regresión lineal simple con el predictor seleccionado en el 
# paso anterior.

# Ajustar modelo con R.
modelo <- lm(Weight ~ Navel.Girth, data = muestra)
print(summary(modelo))

# Graficar el modelo.
# p <- ggscatter(muestra, x = "Navel.Girth", y = "Weight", color = "blue", fill = "blue",
#                xlab = "Circunferencia a la altura del ombligo [cm]", ylab = "Peso [kg]")
# 
# p <- p + geom_smooth(method = lm, se = FALSE, colour = "red")
# print(p)

# Crear gráficos para evaluar el modelo.
plot(modelo)






# Se crea el modelo nulo para agregar predictores útiles.
nulo <- lm(Weight ~ 1, data = muestra1)

# Se evalúan todos los predictores con el método exhaustivo (subconjuntos).
modelos <- regsubsets(Weight ~ ., data = muestra1, method = "exhaustive",
                      nbest = 1, nvmax = 3)

print(summary(modelos))

# Se agregan los predictores que arroja el método anterior.
final <- update(nulo, . ~ . + Knee.Girth + Hip.Girth + Waist.Girth)

plot(final)






# Identificación de valores atipicos.
predictores <- names(coef(final))[-1]
muestra1 <- muestra1[, c(predictores, "Weight")]

resultados <- data.frame(respuesta_predicha = fitted(modelo))
resultados[["residuos_estandarizados"]] <- rstandard(modelo)
resultados[["residuos_estudiantizados"]] <-rstudent(modelo)
resultados[["distancia_Cook"]] <- cooks.distance(modelo)
resultados[["dfbeta"]] <- dfbeta(modelo)
resultados[["dffit"]] <- dffits(modelo)
resultados[["apalancamiento"]] <- hatvalues(modelo)
resultados[["covratio"]] <- covratio(modelo)

cat("Identificación de valores atípicos:\n")
# Observaciones con residuos estandarizados fuera del 95% esperado.
sospechosos1 <- which(abs(resultados[["residuos_estandarizados"]]) > 1.96)

cat("- Residuos estandarizados fuera del 95% esperado:", sospechosos1, "\n")

# Observaciones con distancia de Cook mayor a uno.
sospechosos2 <- which(resultados[["cooks.distance"]] > 1)

cat("- Residuos con una distancia de Cook alta:", sospechosos2, "\n")

# Observaciones con apalancamiento mayor igual al doble del
# apalancamiento promedio.

apal_medio <- (ncol(muestra1) + 1) / nrow(muestra1)
sospechosos3 <- which(resultados[["apalancamiento"]] > 2 * apal_medio)

cat("- Residuos con apalancamiento fuera de rango:",sospechosos3, "\n")

# Observaciones con DFBeta mayor o igual a 1.
sospechosos4 <- which(apply(resultados[["dfbeta"]] >= 1, 1, any))
names(sospechosos4) <- NULL

cat("- Residuos con DFBeta >= 1:",sospechosos4, "\n")

# Observaciones con razón de covarianza fuera de rango.
inferior <- 1 - 3 * apal_medio
superior <- 1 + 3 * apal_medio
sospechosos5 <- which(resultados[["covratio"]] < inferior | resultados[["covratio"]] > superior)

cat("- Residuos con razón de covarianza fuera de rango:", sospechosos5, "\n")

# Resumen de valores sospechosos.
sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4, sospechosos5)

sospechosos <- sort(unique(sospechosos))

cat("\nResumen de valores sospechosos:\n")
cat("Apalancamiento promedio:", apal_medio, "\n")

cat("Intervalo razón de covarianza: [", inferior, "; ",
    superior, "]\n\n", sep = "")

print(round(resultados[sospechosos, c("distancia_Cook", "apalancamiento", "covratio")], 3))
  
# Revision de condiciones.

# Primero ninguna de las variables corresponde a una constante y las observaciones son independientes entre si.
# Luego se verifican las otras condiciones:

# Independencia de residuos.
cat("Durbin-Watson\n")
print(durbinWatsonTest(final))
# El p-value que se obtiene de la independencia de residuos es 0.748

# Normalidad de los residuos.
cat("Normalidad de residuos\n")
print(shapiro.test(modelo$residuals))
# La normalidad de los residuos da un p-value de 0.138, es decir, el supueso no se cumple.

# Homocedasticidad de los residuos.
cat("Homocedasticidad de residuos\n")
print(ncvTest(final))
# La homocedasticidad de los residuos da un p-value de 0.861, habiendo bastantes valores que se alejan del rango,
# por lo que no se cumple.

# Comprobar la multicolinealidad.
vifs <- vif(final)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs:\n")
print(vifs)
cat("- Tolerancias:\n")
print(1 / vifs)
cat("- VIF medio:", mean(vifs), "\n")
#  En el caso de la inflación de la varianza no parece ser preocupante, sin embargo, el estadistico de la tolerancia,
# parece que debe ser estudiado.

# Validacion cruzada.

set.seed(121)
n <- nrow(muestra1)
n_entrenamiento <- floor(0.7 * n)
muestra_e <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- muestra1[muestra_e, ]
prueba  <- muestra1[-muestra_e, ]

# Ajustar modelo con el conjunto de entrenamiento.
modelo_e <- lm(Weight ~ Knee.Girth + Hip.Girth + Waist.Girth, data = entrenamiento)
print(summary(modelo_e))

# Calcular error cuadrado promedio para el conjunto de entrenamiento.
mse_entrenamiento <- mean(modelo_e$residuals ** 2)
cat("MSE para el conjunto de entrenamiento:", mse_entrenamiento, "\n")

# Hacer predicciones para el conjunto de prueba.
predicciones <- predict(modelo_e, prueba)

# Calcular error cuadrado promedio para el conjunto de prueba.
error <- prueba[["Weight"]] - predicciones
mse_prueba <- mean(error ** 2)
cat("MSE para el conjunto de prueba:", mse_prueba)

# Al terminar la validación cruzada se puede ver que el error cuadrado promedio del conjunto de entrenamiento
# es muy similar al del conjunto de prueba, es decir, se puede concluir que es generalizable una vez arregladas las
# condiciones que no cumplían anteriormente.

