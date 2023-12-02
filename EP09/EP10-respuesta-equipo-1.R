# Importar librerias
library(dplyr)
library(pROC)

# 1. El equipo crea la variable IMC (índice de masa corporal) como el peso de una persona (en kilogramos) dividida por el cuadrado de su estatura (en metros).
# 2. Si bien esta variable se usa para clasificar a las personas en varias clases de estado nutricional (bajo peso, normal, sobrepeso, obesidad, obesidad mórbida), para efectos de este ejercicio, usaremos dos clases: sobrepeso (IMC ≥ 25,0) y no sobrepeso (IMC < 25,0).
# 3. El equipo crea la variable dicotómica EN (estado nutricional) de acuerdo al valor de IMC de cada persona.

# Se leen los datos
datos <- read.csv2("Desktop/EI-EP-09-10-11/EP10/EP09 Datos.csv")

# Se verifica que los datos se leen correctamente
head(datos)

# Se crea una nueva columna para calcular el IMC
datos[["IMC"]] <- datos[["Weight"]]/((datos[["Height"]])/100)^2

# Se crea una nueva columna para determinar si está con sobrepeso o no
# según el IMC.
datos[["EN"]] <- ifelse(datos[["IMC"]] >= 25, 1, 0)

# Se verifica que se hayan agregado correctamente las columnas
head(datos)

datos <- datos %>% mutate(index = row_number())

# Ahora podemos construir un modelo de regresión logística para predecir la 
# variable EN, de acuerdo con las siguientes instrucciones:
# 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro 
# dígitos del RUN (sin considerar el dígito verificador) del integrante de mayor 
# edad del equipo.
set.seed(5547)

# 2. Seleccionar una muestra de 90 mujeres (si la semilla es un número par) o 
# 90 hombres (si la semilla es impar), asegurando que la mitad tenga estado 
# nutricional “sobrepeso” y la otra mitad “no sobrepeso” en cada caso. Dividir 
# esta muestra en dos conjuntos: los datos de 60 personas (30 con EN “sobrepeso”) 
# para utilizar en la construcción de los modelos y 30 personas (15 con EN 
# “sobrepeso”) para poder evaluarlos.

# Se filtran los datos para tener solo hombres
hombres <- datos[datos$Gender == 1, ]

# Se saca una muestra de 45 hombres que tengan sobrepeso
sobrepeso <- hombres %>% filter(EN == 1) %>% sample_n(45)
# Se saca una muestra de 45 hombres que no tengan sobrepeso
nosobrepeso <- hombres %>% filter(EN == 0) %>% sample_n(45)

# Se unen las dos muestras
muestra <- rbind(sobrepeso, nosobrepeso)

# Se unen las muestras donde se extraen 30 hombres con sobrepeso
# y 30 hombres sin sobrepeso
set_construccion <- rbind(
  sample_n(muestra %>% filter(EN == 1), 30),
  sample_n(muestra %>% filter(EN == 0), 30)
)

# Se saca la muestra restante tomando a la "muestra global" y a la 
# submuestra, restándose
set_evaluacion <- setdiff(muestra, set_construccion)

# 3. Recordar las ocho posibles variables predictoras seleccionadas de forma 
# aleatoria en el ejercicio anterior.

# VARIABLES
# Bitrochanteric.diameter
# Elbows.diameter
# Wrists.diameter
# Knees.diameter
# Waist.Girth
# Hip.Girth
# Knee.Girth
# Ankle.Minimum.Girth

# 4. Seleccionar, de las otras variables, una que el equipo considere que 
# podría ser útil para predecir la clase EN, justificando bien esta selección.
# Variable escogida: -> Navel.Girth
# Creemos que es determinante, ya que la mayoría de la grasa se almacena en 
# esta zona del cuerpo.

# 5. Usando el entorno R y paquetes estándares, construir un modelo de 
# regresión logística con el predictor seleccionado en el paso anterior y 
# utilizando de la muestra obtenida.

# Construir el modelo de regresión logística empleando la variable "Navel.Girth"
# con la muestra de construcción

# Primer modelo
modelo_logistico <- glm(EN ~ Navel.Girth,
                        family = binomial(link = "logit"), data = set_construccion)

modeloTest <- add1(modelo_logistico, scope = .~. + Bitrochanteric.diameter + Elbows.diameter +
                   Wrists.diameter + Knees.diameter + Waist.Girth + Hip.Girth + Knee.Girth + Ankle.Minimum.Girth)

modeloTest

desviacion <- -2*logLik(modelo_logistico)
cat("La desviación es de: ", desviacion)

# k = número de predictores
# n = tamaño de muestra
k <- length(coef(modelo_logistico))
n <- nrow(set_construccion)

cat("La cantidad de predictores es de: ", k)
cat("El tamaño de la muestra es de: ", n)

AIC <- desviacion+2*k
BIC <- desviacion+2*k*log(n)

cat("Valor de AIC: ", AIC)
cat("Valor de BIC: ", BIC)

# Resumen del modelo
summary(modelo_logistico)

# 6. Usando herramientas estándares1 para la exploración de modelos del entorno 
# R, buscar entre dos y cinco predictores de entre las variables seleccionadas 
# al azar, recordadas en el punto 3, para agregar al modelo obtenido en el paso 5.
modelo_logistico_2 <- update(modelo_logistico, .~. + Waist.Girth)

desviacion <- -2*logLik(modelo_logistico_2)
cat("La desviación es de: ", desviacion)

# k = número de predictores
# n = tamaño de muestra
k <- length(coef(modelo_logistico_2))
n <- nrow(set_construccion)

cat("La cantidad de predictores es de: ", k)
cat("El tamaño de la muestra es de: ", n)

AIC <- desviacion+2*k
BIC <- desviacion+2*k*log(n)

cat("Valor de AIC: ", AIC)
cat("Valor de BIC: ", BIC)

modeloTest <- add1(modelo_logistico_2, scope = .~. + Bitrochanteric.diameter + Elbows.diameter +
                     Wrists.diameter + Knees.diameter + Hip.Girth + Knee.Girth + Ankle.Minimum.Girth)

modelo_logistico_3 <- update(modelo_logistico_2, .~. + Bitrochanteric.diameter)

desviacion <- -2*logLik(modelo_logistico_3)
n <- nrow(set_construccion)

cat("La cantidad de predictores es de: ", k)
cat("El tamaño de la muestra es de: ", n)

AIC <- desviacion+2*k
BIC <- desviacion+2*k*log(n)

cat("Valor de AIC: ", AIC)
cat("Valor de BIC: ", BIC)

modeloTest <- add1(modelo_logistico_3, scope = .~. + Elbows.diameter +
                     Wrists.diameter + Knees.diameter + Hip.Girth + Knee.Girth + Ankle.Minimum.Girth)

 # 7. Evaluar la confiabilidad de los modelos (i.e. que tengan un buen nivel de 
# ajuste y son generalizables) y “arreglarlos” en caso de que tengan algún problema.
desviacion <- -2*logLik(modelo_logistico_3)
cat("La desviación es de: ", desviacion)

# k = número de predictores
# n = tamaño de muestra
k <- length(coef(modelo_logistico_3))
n <- nrow(set_construccion)

cat("La cantidad de predictores es de: ", k)
cat("El tamaño de la muestra es de: ", n)

AIC <- desviacion+2*k
BIC <- desviacion+2*k*log(n)

cat("Valor de AIC: ", AIC)
cat("Valor de BIC: ", BIC)

cat("Evaluación del modelo a partir del conjunto de entrenamiento:\n")
probs_e <- predict(modelo_logistico_3, set_evaluacion, type = "response")
umbral <- 0.5
preds_e <- ifelse(probs_e > umbral, 1, 0)
ROC_e <- roc(set_evaluacion[["EN"]], probs_e)
plot(ROC_e)
matriz_confusion <- table(Predicciones = preds_e, Real = set_evaluacion$EN)
sensibilidad <- matriz_confusion[2, 2] / sum(matriz_confusion[2, ])
especificidad <- matriz_confusion[1, 1] / sum(matriz_confusion[1, ])
print(paste("Sensibilidad:", sensibilidad))
print(paste("Especificidad:", especificidad))

cat("Evaluación del modelo con el conjunto de prueba:\n")
probs_p <- predict(modelo_logistico_3, set_construccion, type = "response")
umbral <- 0.5
preds_p <- ifelse(probs_p > umbral, 1, 0)
ROC_p <- roc(set_construccion[["EN"]], probs_p)
plot(ROC_p)
matriz_confusion <- table(Predicciones = preds_p, Real = set_construccion$EN)
sensibilidad <- matriz_confusion[2, 2] / sum(matriz_confusion[2, ])
especificidad <- matriz_confusion[1, 1] / sum(matriz_confusion[1, ])
print(paste("Sensibilidad:", sensibilidad))
print(paste("Especificidad:", especificidad))

# Ambos gráficos presentan un buen rendimiento al tener que la curva ROC
# está bastante por encima de la línea diagonal.

# 8. Usando código estándar1, evaluar el poder predictivo de los modelos con 
# los datos de las 40 personas que no se incluyeron en su construcción en 
# términos de sensibilidad y especificidad.

datos_disponibles <- datos %>%
  anti_join(set_construccion, by = "index") %>% 
  anti_join(set_evaluacion, by = "index")

set_nuevo <- sample_n(datos_disponibles, 40)
cat("Evaluación del modelo con el conjunto de 40 personas:\n")
probs_personas <- predict(modelo_logistico_3, set_nuevo, type = "response")
umbral <- 0.5
preds_personas <- ifelse(probs_personas > umbral, 1, 0)
ROC_personas <- roc(set_nuevo[["EN"]], probs_personas)
plot(ROC_personas)
matriz_confusion <- table(Predicciones = preds_personas, Real = set_nuevo$EN)
sensibilidad <- matriz_confusion[2, 2] / sum(matriz_confusion[2, ])
especificidad <- matriz_confusion[1, 1] / sum(matriz_confusion[1, ])
print(paste("Sensibilidad:", sensibilidad))
print(paste("Especificidad:", especificidad))

# La curva ROC se acerca más a la esquina superior izquierda, lo que quiere
# decir que tiene un mehor rendimiento. Tiene alta sensibilidad (pocos falsos
# negativos) y alta especificidad (pocos falsos positivos)
# Este modelo es útil para predecir la variable "peso".
