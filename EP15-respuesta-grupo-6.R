# Actividad 14 IME
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

if(!require(pROC)){
  install.packages("pROC",dependencies = TRUE)
  require(pROC)
}
# Ahora podemos construir un modelo de regresión logística para predecir la variable EN, de acuerdo con las 
# siguientes instrucciones:
#   1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos del RUN (sin considerar el dígito 
#                                                                                           verificador) del integrante de mayor edad del equipo.
# 2. Seleccionar una muestra de 120 mujeres (si la semilla es un número par) o 120 hombres (si la semilla es impar), 
# asegurando que la mitad tenga estado nutricional “sobrepeso” y la otra mitad “no sobrepeso”. Dividir 
# esta muestra en dos conjuntos: los datos de 80 personas (40 con EN “sobrepeso”) para utilizar en la 
# construcción de los modelos y 40 personas (20 con EN “sobrepeso”) para poder evaluarlos.
# 3. Recordar las ocho posibles variables predictoras seleccionadas de forma aleatoria en el ejercicio anterior.
# 4. Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la clase EN, 
# justificando bien esta selección.
# 5. Usando el entorno R y paquetes estándares1
# , construir un modelo de regresión logística con el predictor 
# seleccionado en el paso anterior y utilizando de la muestra obtenida.
# 6. Usando herramientas estándares1 para la exploración de modelos del entorno R, buscar entre dos y cinco 
# predictores de entre las variables seleccionadas al azar, recordadas en el punto 3, para agregar al modelo 
# obtenido en el paso 5.
# 7. Evaluar la confiabilidad de los modelos (i.e. que tengan un buen nivel de ajuste y son generalizables) y 
# “arreglarlos” en caso de que tengan algún problema.
# 8. Usando código estándar1
# , evaluar el poder predictivo de los modelos con los datos de las 40 personas que no 
# se incluyeron en su construcción en términos de sensibilidad y especificidad


# Archivo de entrada.
poblacion <- read.csv2(file.choose(new = FALSE), encoding="utf8")

poblacion$EN <- factor(poblacion$EN)

# Crear la variable IMC (índice de masa corporal) como el peso de una persona (en kilogramos) dividida
# por el cuadrado de su estatura (en metros).
poblacion$IMC <- poblacion$Weight / ((poblacion$Height / 100) * (poblacion$Height / 100))

# Crear la variable dicotómica EN (estado nutricional) de acuerdo al valor de IMC de cada persona.
poblacion$EN <- ifelse(poblacion$IMC >= 23, "sobrepeso", "nosobrepeso")

# Definir una semilla (1)
set.seed(3734)

# Obtener las muestras al sexo corespodiente y estado nutricional
muestraSP <- poblacion %>% filter(EN == "sobrepeso")
muestraSP <- sample_n(muestraSP, size = 50, replace = FALSE)

muestraNSP <- poblacion %>% filter(EN == "nosobrepeso")
muestraNSP <- sample_n(muestraNSP, size = 50, replace = FALSE)

# Union de las muestras en un frame
muestraEvaluacion <- rbind(muestraSP, muestraNSP)
muestraEvaluacion$EN<-ifelse(muestraEvaluacion$EN=="sobrepeso",1,0)
muestraEvaluacion$EN <- factor(muestraEvaluacion$EN)


# Variables seleccionadas:
# Profundidad.del.pecho, Circunferencia.de.la.rodilla, Circunferencia.del.muslo,
# Circunferencia.de.la.cintura, Circunferencia.del.bíceps, Diámetro.del.pecho

# Ahora, una posible variable predictora sería: 

# La circunferencia de la cadera es una medida útil al momento de predecir el peso de las personas, según estudios,
# en los niños y adolescentes se puede usar este indicador indiferentemente del sexo de la persona para diagnosticar
# obesidad, sin embargo, para adultos se presentan diferencias en lo que a sexo corresponde, para fines de este estudio
# sigue siendo útil este indicador.

# https://docs.bvsalud.org/biblioref/2020/04/1094977/17889-144814488126-1-pb.pdf

# regresion logistica con la variable seleccionada.

# Ajustar modelo.
modelo <- glm(EN ~ Navel.Girth, family = binomial(link = "logit"), data = muestraModelos)
print(summary(modelo))

# Se comprueba que la variable es significativa, ya que, tiene un p-value extremadamente bajo.

# Evaluar el modelo con el conjunto de entrenamiento.
cat("Evaluación del modelo a partir del conjunto de entrenamiento:\n")
probs_e <- predict(modelo, muestraModelos, type = "response")

umbral <- 0.5
preds_e <- sapply(probs_e, function(p) ifelse(p >= umbral, "1", "0"))
preds_e <- factor(preds_e, levels = levels(muestraModelos$EN))

ROC_e <- roc(muestraModelos[["EN"]], probs_e)
plot(ROC_e)

matriz_e <- confusionMatrix(preds_e, muestraModelos[["EN"]])
print(matriz_e)

# El Accuracy resulta clasificar un 83,75% efectivamente, por lo que, resulta ser bastante alto.

# Evaluar el modelo con el conjunto de prueba.
cat("Evaluación del modelo a partir del conjunto de prueba:\n")
probs_p <- predict(modelo, muestraEvaluacion, type = "response")

preds_p <- sapply(probs_p, function(p) ifelse(p >= umbral, "1", "0"))
preds_p <- factor(preds_p, levels = levels(muestraEvaluacion[["EN"]]))

ROC_p <- roc(muestraEvaluacion[["EN"]], probs_p)
plot(ROC_p)
print(ROC_p)

matriz_p <- confusionMatrix(preds_p, muestraEvaluacion[["EN"]])
print(matriz_p)

#  Anlizando, se tiene
#  Sensitivity : 0.6500  Clasifica en un 65% bien a los positivos        
#  Specificity : 0.9500  Clasifica en un 95% de que qué tan exacta es asignar observaciones a la clase negativa.
#  En la prueba se aprecia que Accuracy : 0.8 disminuyó en un 3%, sin embargo, ambos son bastante similares
#  por lo que, se puede decir que el modelo está generalizado.

# Respuesta 6
modelo2 <- glm(EN ~  Bicep.Girth +Chest.depth+Thigh.Girth, family = binomial(link = "logit"), data = muestraModelos)
print(summary(modelo2))

#+ Se aprecia que las variables en el modelo son altamente significativas, en el caso de Chest.depth
#+ esta la mantenemos dado que se encuentra muy cercana de estar en un nivel de significancia de 0.1
#+ y dado que hay pocas variables a utilizar,

# Respuesta 7
# Evaluar el modelo con el conjunto de entrenamiento.
cat("Evaluación del modelo a partir del conjunto de entrenamiento:\n")
probs_e <- predict(modelo2, muestraModelos, type = "response")

umbral <- 0.5
preds_e <- sapply(probs_e, function(p) ifelse(p >= umbral, "1", "0"))
preds_e <- factor(preds_e, levels = levels(muestraModelos$EN))

ROC_e <- roc(muestraModelos[["EN"]], probs_e)
plot(ROC_e)
print(ROC_e)

matriz_e <- confusionMatrix(preds_e, muestraModelos[["EN"]])
print(matriz_e)

# Evaluar el modelo2 con el conjunto de prueba.
cat("Evaluación del modelo a partir del conjunto de prueba:\n")
probs_p <- predict(modelo2, muestraEvaluacion, type = "response")

preds_p <- sapply(probs_p, function(p) ifelse(p >= umbral, "1", "0"))
preds_p <- factor(preds_p, levels = levels(muestraEvaluacion[["EN"]]))

ROC_p <- roc(muestraEvaluacion[["EN"]], probs_p)
plot(ROC_p)
print(ROC_p)

matriz_p <- confusionMatrix(preds_p, muestraEvaluacion[["EN"]])
print(matriz_p)

#+ Se aprecia que el accuracy en la muestra de entrenamiento, mejora con respecto
#+ al modelo de solamente una variable de 0,83 a  0.91, en el caso de la muestra de prueba, 
#+ podemos apreciar que el accuracy practicamente no disminuye, siendo este de 0.9 lo que indica que
#+ el modelo resulta estar generalizado, obteniendo cercanos resultados en ambas muestras.
