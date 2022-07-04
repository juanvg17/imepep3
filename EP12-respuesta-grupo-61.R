# Actividad 12 IME
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


# Generar la tabla de datos
instanciaA <- c(167, 8, 65, 91, 125, 64, 196, 117, 41, 56)
tiempoA <- c(1510394, 251843, 834565, 37449, 48705, 402929, 885722, 8576989, 62764, 783108)

instanciaB <- c(197, 7, 21, 195, 191, 149, 39, 139, 154, 137)
tiempoB <- c(48408, 35974, 5743260, 6684497, 1252837, 6701654, 6568968, 120276, 1174562, 2830464)

tiempo <- c(tiempoA, tiempoB)
algoritmo <- c(rep("A", length(tiempoA)), rep("B", length(tiempoB)))
datos <- data.frame(tiempo, algoritmo)

# Grafica para visualizar distribuición
g <- ggqqplot(datos, x = "tiempo", facet.by = "algoritmo",
              palette = c("blue", "red"), color = "algoritmo")

print(g)

# Prueba de shapiro - wilk para verificar si es cercana a la normal
shapiro.test(tiempoA)
shapiro.test(tiempoB)

# Debido a que no se cumple la normalidad para ambas muestras se aplica una transformación
# Se ordenan los datos
tiempoA <- sort(tiempoA)
tiempoB <- sort(tiempoB)

# Se realiza una transformación logarítmica
tiempoA_log <- log(tiempoA)
tiempoB_log <- log(tiempoB)
tiempo <- c(tiempoA_log, tiempoB_log)
datos <- data.frame(tiempo, algoritmo)
g <- ggqqplot(datos, x = "tiempo", facet.by = "algoritmo",
              palette = c("blue", "red"), color = "algoritmo")

print(g)

#Prueba de shapiro - wilk para verificar si es cercana a la normal post transformación
shapiro.test(tiempoA_log)
shapiro.test(tiempoB_log)

# Se aplica la prueba de t de student (prueba paramétrica)
t.test(tiempoA_log, y = tiempoB_log, alternative = "two.sided", conf.level = 0.95)

# Respuesta
################ DATOS PARA LA PREGUNTA 2 Y 3 ####################
poblacion <- read.csv2(file.choose(new = FALSE), encoding="utf8")#
##################################################################

# PREGUNTA 2
# YUEN CON BOOSTRAP CON MEDIANA, SI ES QUE NO CUMPLE LA NORMALIDAD

# ESTUDIO: En base a los datos recopilados de las personas encuentadas en la Casen 2017, se requiere obtener un 
# estudio de cómo se ha ido estructurando la educación en base a la orientación sexual de las personas. Es por esto,
# que resulta de suma importancia llevar a cabo un análisis de los años de escolaridad de cada grupo (muestra) y,
# así lograr responder a la interrogante: "¿La mediana de años de escolaridad de la comunidad lgbt es igual a la mediana
# de años de escolaridad de la comunidad heterosexual?

# Denotando MeL como la mediana de años de escolaridad de la comunidad lgbt, y MeH como la mediana de años de escolaridad 
# de la comunidad heterosexual, entonces:

# H0 : MeH - MeL = 0; esto es  MeL  = MeH
# H1 : MeH - MeL!= 0; esto es  MeL != MeH

# Seleccionar varibles para llevar a cabo el estudio
# id viviendo, orientación y escolaridad
muestra <- select(poblacion, id.vivienda, r23, esc)

# Definir semilla 
set.seed(205508111)

# Filtrar datos
heterosexual <- muestra %>% filter(r23 == 'Heterosexual (Atracción hacia el sexo opuesto)')
lgbt <- muestra %>% filter(r23 == 'Bisexual (Atracción hacia ambos sexos)' | r23 == 'Gay/Lesbiana (Atracción hacia el mismo sexo)')

# Obtener la muestra 
muestraHtero <- sample_n(heterosexual, size = 300, replace = FALSE)
muestraLgbt <- sample_n(lgbt, size = 300, replace = FALSE)

# Eliminar valores nulos, es decir, que fueron omitidos por la persona encuestada
a <- muestraHtero$esc
a <- a[!is.na(a)]

b <- muestraLgbt$esc
b <- b[!is.na(b)]

# Conprobar independencia
print(sum(muestraHtero$id.vivienda %in% muestraLgbt$id.vivienda))

# Como no hay id de viviendas que se repita entre las muestras, podemos concluir de que ambos grupos
# resultan ser independientes

# Agrupar datos para efecruar prueba de Yuen
anios <- c(a, b)
grupo <- c(rep("Hetero", length(a)), rep("LGBT", length(b)))
datos2 <- data.frame(anios, grupo)

g <- ggqqplot(datos2, x = "anios", facet.by = "grupo",
              palette = c("blue", "red"), color = "grupo")

print(g)

# En base a la gráfica obtenida, se logra observar que las observaciones 
# no siguen una distribución normal, posee demasiadas observaciones  atípicas

# Prueba de shapiro - wilk para verificar si es cercana a la normal
# H
shapiro.test(a)
# L
shapiro.test(b)

# En base a los test realizados, se ha comprobado que la escolaridad (años) de
# ambas comunidades no siguen una distribución normal. Dicho esto, se ha decidido
# usar una prueba robusta

# Establecer nivel de significación y cantidad de muestras a generar
# con bootstrapping.
alfa <- 0.05
bootstrap <- 999

# Se decide aplicar la prueba de YUEN con boostrap con el estadistico correspondiente a la
# mediana, ya que, al truncar la data que se encuentra ordenada, aún así se logran observar 
# valores atipicos y evetualmente una distribución no cercana a la normal, por lo que la media no 
# sería la mejor opción.

# Aplicar prueba con la mediana
set.seed(135)

prueba_mediana <- pb2gen(anios ~ grupo,
                         data = datos2,
                         est = "median",
                         nboot = bootstrap)

cat("Resultado al usar la mediana como estimador\n\n")
print(prueba_mediana)

median(a) - median(b)

# Como la diferencia entre las medianas resulta ser "-1, esto esto está sugiriendo que la mediana
# de años de escolaridad de la comunidad lgbt es mejor a la  mediana de años de escolaridad de la 
# comunidad heterosexual.

# Respuesta: Luego de aplicar la prueba de Yuen con bootstrapping, el P-value obtenido es 0.25225 lo cual resulta
# ser mayor al nivel de significacion alfa = 0.05, por lo que se falla al rechazar H0, es por esto que con un 
# 95% de confianza se concluye que la mediana de años de escolaridad entre la comunidad LGBT y Heterosexual
# resultan ser iguales.


#################### PREGUNTA 3 ####################
# Analice la segunda pregunta abordada en el ejercicio práctico 11, con los mismos datos, utilizando un
# método robusto adecuado.

# Estudiar las diferencias entre la media de las edades de las personas de distintos estados civiles
# Casado(a), Conviviente o pareja sin acuerdo de unión civil y Conviviente civil (con acuerdo de unión civil)) 

# Se denota como µS al estado Soltero(a), µC Estado Conviviente y µCC Conviviente civil.
# H0: La media de las edades de las personas de los tres distintos estados civiles son iguales.
# H1: La media de las edades de las personas de los tres distintos estados civiles son distintas.

# H0: µS = µC = µCC
# H1: µS != µC o µS != µCC o µC != µCC

# Primero se define una semilla para obtener la muestra de cada grupo que se desea analizar.
set.seed(223)
muestra2 <- select(poblacion, id.vivienda, edad, ecivil)

# Se obtiene la muestra de cada grupo a analiar y se verifica que no esten repetidos (sean independientes).
casado <- muestra2 %>% filter(ecivil == 'Casado(a)')
casado_m <- sample_n(casado, size = 500, replace = FALSE)
casado_m <- casado_m$edad
casado_m <- casado_m[!is.na(casado_m)]

separado <- muestra2 %>% filter(ecivil == 'Separado(a)')
separado_m <- sample_n(separado, size = 500, replace = FALSE)
separado_m <- separado_m$edad
separado_m <- separado_m[!is.na(separado_m)]

soltero <- muestra2 %>% filter(ecivil == 'Soltero(a)')
soltero_m <- sample_n(soltero, size = 500, replace = FALSE)
soltero_m <- soltero_m$edad
soltero_m <- soltero_m[!is.na(soltero_m)]

# Se juntan las edades de cada muestra en un data frame para el analisis.
edad <- c(casado_m, separado_m, soltero_m)
ecivil <- c(rep("casado_m", length(casado_m)), rep("separado_m", length(separado_m)), rep("soltero_m", length(soltero_m)))

datos <- data.frame(edad, ecivil)

# Como se puede ver en este gráfico los datos de las personas casadas y las separadas se podría llegar a considerar que
# cumplen con la condición de normalidad, sin embargo, el gráfico de las personas solteras se escapa bastante de la normalidad.
# debido a este último también, es necesario encontrar un método robusto que sea pertinente, en este caso es med1way,
# ya que como se ve en el gráfico se debe ocupar la mediana porque aunque se truncuqen los datos no llegará a la normalidad.
g <- ggqqplot(datos, x = "edad", facet.by = "ecivil")
print(g)

gamma <- 0.2

comp_mediana <- med1way(formula = edad ~ ecivil, data = datos, iter = 1000)
print(comp_mediana)

# Al hacer la estimación sobre la mediana no tenemos ningúna función en el paquete que facilite algún método post-hoc.
# En conclusión con un p-value = 0 se puede decir con un 95% de confianza que se rechaza h0, es decir, la media de las edades
# de los estados civiles antes mencionados es distinta.

# Por otro lado, no sabemos si la implementación del método robusto, la definición de los datos, algún error de cálculo
# o la cantidad de muestras es lo que hace que aplicando cualquier método robusto el p-value siempre sea exactamente cero.
