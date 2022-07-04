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

# Archivo de entrada.
poblacion <- read.csv2(file.choose(new = FALSE), encoding="utf8")

# Crear la variable IMC (índice de masa corporal) como el peso de una persona (en kilogramos) dividida
# por el cuadrado de su estatura (en metros).
poblacion$IMC <- poblacion$Weight / ((poblacion$Height / 100) * (poblacion$Height / 100))

# Crear la variable dicotómica EN (estado nutricional) de acuerdo al valor de IMC de cada persona.
poblacion$EN <- ifelse(poblacion$IMC >= 23, "sobrepeso", "nosobrepeso")

# Definir una semilla (1)
set.seed(3734)

# Obtener las muestras al sexo corespodiente y estado nutricional
muestraSP <- poblacion %>% filter(Gender == '0', EN == "sobrepeso")
muestraSP <- sample_n(muestraSP, size = 60, replace = FALSE)

muestraNSP <- poblacion %>% filter(Gender == '0', EN == "nosobrepeso")
muestraNSP <- sample_n(muestraNSP, size = 60, replace = FALSE)

# Índices de de las muestras con sobrepeso
pos <- sample.int(nrow(muestraSP), size = 40, replace = FALSE)

muestraModelos <- muestraSP[pos, ]
muestraEvaluacion <- muestraSP[-pos, ]

# Índices de de las muestras con No sobrepeso
pos1 <- sample.int(nrow(muestraNSP), size = 40, replace = FALSE)

muestraModelos1 <- muestraNSP[pos1, ]
muestraEvaluacion1 <- muestraNSP[-pos1, ]

# Union de las muestras en un frame
muestraEvaluacion <- rbind(muestraEvaluacion, muestraEvaluacion1)
muestraModelos <- rbind(muestraModelos, muestraModelos1)

# Cambiar valores a binarios
muestraModelos$EN<-ifelse(muestraModelos$EN=="sobrepeso",1,0)

muestraNSP$EN<-ifelse(muestraNSP$EN=="sobrepeso",1,0)

# 3. recordar las 8 variables del ep anterior
# de las otras seleccionar una que prediga bien el estado nutricional

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

# Evaluar el modelo con el conjunto de entrenamiento.
cat("Evaluación del modelo a partir del conjunto de entrenamiento:\n")
probs_e <- predict(modelo, muestraModelos, type = "response")


#################### ACCAAAAAA VER EL ERROR, NO SE QUE DATA PUSO JUANITO COMO LA DATA ORIGINAL
# HAY QUE CAMBIAR A DICO
umbral <- 0.5
preds_e <- sapply(probs_e, function(p) ifelse(p >= umbral, "1", "0"))
preds_e <- factor(preds_e, levels = levels(muestraNSP[["EN"]]))

ROC_e <- roc(muestraModelos[["EN"]], probs_e)
plot(ROC_e)

matriz_e <- confusionMatrix(preds_e, entrenamiento[["EN"]])
print(matriz_e)

# Evaluar el modelo con el conjunto de prueba.
cat("Evaluación del modelo a partir del conjunto de prueba:\n")
probs_p <- predict(modelo, prueba, type = "response")

preds_p <- sapply(probs_p, function(p) ifelse(p >= umbral, "1", "0"))
preds_p <- factor(preds_p, levels = levels(muestraNSP[["EN"]]))

ROC_p <- roc(prueba[["EN"]], probs_p)
plot(ROC_p)

matriz_p <- confusionMatrix(preds_p, prueba[["EN"]])
print(matriz_p)




# de las 8 escogidas seleccionar de 2 a 5 para agregar al modelo.

# repetir lo mismo que el ep anterior.
