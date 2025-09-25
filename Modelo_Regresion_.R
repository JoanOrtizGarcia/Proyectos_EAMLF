#Limpiar escritorio
ls(all=TRUE)
rm(list = ls(all=TRUE))
ls()
ls(all=TRUE)
#Determinar el escritorio (si descargan el dataset y lo guardan en sus computadoras, sino no lo usen)
setwd("/Users/PaolaZambrano/Desktop/MachineLearning-UNAL2023-I/Python-R-ML")
getwd()
#datos (Cargar datos, recuerden que pueden cargar el dataset desde la carpeta en sus computadores o usando la URL)
# En este caso es usando la URL
casas <- read.csv("https://raw.githubusercontent.com/jeffheaton/data/master/housing.csv")
head(casas)

#Explicación de las variables del dataset:
# Dataset del precio medio de las casas en Boston: 
# Este data set cuanta con 506 filas de obsrevaciones y 14 columnas, 13 variables descriptivas y 1 variable dependiente o de salida. 
# A continuación la descripción de las 14 observaciones, comenzando por la de salida:
# 1.medv
# La variable "medv" es la variabe objetivo o variable dependiente.
# Valor medio de las viviendas ocupadas por sus propietarios en \$1000s.
# 2. crim
# Tasa de crimen por ciudad. 
# 3. zb
# Proporción de suelo residencial para lotes de más de 25,000 pies cuadrados.
# 4. indus
# Proporción de acres comerciales no minoristas por ciudad.
# 5. chas
# Variable dummy del río Charles (= 1 si el tramo limita con el río; 0 en caso contrario). 
# 6. nox
# Concentración de óxidos de nitrógeno (partes por 10 millones).
# 7. rm
# Número promedio de cuartos por vivienda.
# 8. age
# Proporción de unidades ocupadas por sus propietarios construidas antes de 1940.
# 9. dis
# Media ponderada de las distancias a cinco centros de empleo de Boston.
# 10. rad
# índice de accesibilidad a las carreteras radiales.
# 11. tax
# Tasa de impuesto a la propiedad de valor total por \$10,000.
# 12. ptratio
# Tasa alumno-maestro por ciudad.
# 13. b
# 1000(Bk - 0.63)^2 donde Bk es la proporción de negros por ciudad.
# 14. lstat
# Menor estatus de la población (porcentaje).
# 

# Teniendo en cuenta esa explicación, ahora algunos comandas ya usados en las otras dos dataset para analizar algunas estadísticas y comportamiento:
summary(casas)
str(casas)
sum(is.na(casas))
casas <- na.omit(casas)

# Gráfica de la densidad, pueden hacerlo uno a uno o todas al mismo tiempo y comparar:
# La función de densidad de probabilidad de un vector x, que se suele denotar como f ( x ) f (x) f(x) describe la probabilidad de que la variable tome un determinado valor.
# Pueden usar cualquir característica estadística para analizar sus variables, curtosis por ejemplo, entre otras.
# Uno a uno:
plot(density(casas$crim))
plot(density(casas$zb))
# Y así para cada variable

# A continuación todas las densidades en una gráfica:

x11() # Pueden usar esta función para abrir una ventana externa y ver la gráfica en mejor tamaño
par(mfrow=c(2,7),mar=c(2, 4, 3, 0.5)) # Características de cuántas gráficas quieren ver y las márgenes que las separan 
# El siguiente ciclo for recorre el dataset desde 1 hasta las 14 variables y grafica la densidad
for (variables in 1:(dim(casas)[2]))
{
        var = casas[,variables]
        d <- density(var)
        plot(d, main=names(casas[variables]),xlab = "")
        polygon(d, col="chocolate2", border = "chocolate4")
        title("Densidad para las 14 variables", cex.main = 1, outer = TRUE)
        
}

#Ahora pueden eliminar columnas o hacer transformaciones de sus valores, con log o elevando a la potencia los valores de la columna que deseen.
# Por ejemplo:
# casas$b <- log(casas$b+5)
# plot(density(casas$b))
# O eliminar las columnas:
# casas <- casas[,-which(colnames(casas)=="b")] #eliminar columnas
### otra forma de eliminar columnas
# install.packages("dplyr") # Instalar paquete de manipulación de dataframes
# library(dplyr)
#datos2 <- select(datos, -valor, -categoria) # Forma simple 1
#datos2 <- select(datos, id, color) # Forma simple 2

# Luego vamos a visualizar la correlación de las variables, existen varias formas de hacerlo, depende de las características que ustedes usen para graficar, como mejor les guste:
install.packages("corrplot")
library(corrplot)
MCorr <- cor(casas)
# El más simple:
corrplot(cor(casas)) # Pueden reemplazar cor(casas) por Mcorr
# Con números...
corrplot(MCorr, method = "number", tl.cex = 0.5)
# Más elaborado:
corrplot.mixed(MCorr, lower.col = "black", number.cex = 0.7)
# Aún más elaborado:
corrplot(MCorr, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag=FALSE)
# Otro más:
corrplot(MCorr, method="color", outline = TRUE,type="lower",order = "hclust",
         tl.col="black", tl.srt=45, diag=FALSE,tl.cex = 1,mar=c(0,0,3,0),
         title="Matriz de correlación entre las variables predictoras y la variable de salida")

#Finalmente el modelo!!!! Este es el modelo más sencillo y utilizamos el paquete Caret, en R pueden usar este paquete para otros modelos de ML que veremos más adelante en el curso.
# Más info en: https://rpubs.com/joser/caret
# Caret: El paquete caret (classification and regression training, Kuhn (2016)) incluye una serie de funciones que facilitan el uso de decenas de métodos complejos de clasificación y regresión. Utilizar este paquete en lugar de las funciones originales de los métodos presenta dos ventajas:
#Permite utilizar un código unificado para aplicar reglas de clasificación muy distintas, implementadas en diferentes paquetes.
#Es más fácil poner en práctica algunos procedimientos usuales en problemas de clasificación. Por ejemplo, hay funciones específicas para dividir la muestra en datos de entrenamiento y datos de test o para ajustar parámetros mediante validación cruzada.

# Primero instalamos algunos paquetes útiles: "caret","tidyverse","ggplot2","dplyr", pueden isntalar uno a uno o todos al tiempo, eso depende, a veces un paquete necesita de otros, así que comencemos uno a uno en orden:
# Lo instalan una vez, luego solo verifican que esté instalado y cargan siempre las librerías de cada paquete.
install.packages("dplyr") # El paquete dplyr proporciona una “gramática” para trabajar con data frames.
install.packages("ggplot2") # El paquete ggplot2 permite personalizar los gráficos con temas. Es posible personalizar cada detalle de un gráfico, como los colores, tipos de líneas, fuentes o alineación, entre otros, usando los componentes de la función theme. Además, la librería proporciona otras funciones que permiten añadir títulos, subtítulos, líneas, flechas o textos.

# Podrian solo instalar esta librería "tidyverse" y no las dos anteriores: Tidyverse es un conjunto de librerías de R diseñadas para la “Ciencia de datos (Data Science)”. Todas las librerías de este conjunto comparten la misma filosofía de trabajo, la misma gramática y las mismas estructuras de datos.
# El universo tidyverse en R está formado por los paquetes ggplot2, dplyr tidyr, forecats, stringr, readr y tibbleFacebook.
install.packages("tidyverse")
install.packages("lattice") # Es un potente y elegante sistema de visualización de datos de alto nivel con énfasis en datos multivariables y es flexible de manipular.
install.packages("caret") # Finalmente "caret"

library(dplyr)
library(ggplot2)
library(tidyverse)
library(lattice)
library(caret)

# Si ya tienen instalados los paquetes no tienen que volver a instalarlos, a menos que requieran una actualización
# Para saber si ya están instalados pueden usar los siguientes comandos:
# packageDescription('ggplot2') 
# sessionInfo("ggplot2")

# A continuación solo para este ejrcicio y si ustedes consideran necesario en su dataset personal, se va a organizar de menor a mayor teniendo en cuenta la variable dependiente "medv", esto no influye en la partición, es solo para ver el comportamiento de los resutados del modelo de una mejor manera. 
casas <- casas[order(casas$medv),]
plot(casas$medv) # Solo para visualizar, no es necesario

#Esta semilla garantiza que se van a generar los mismos valores aleatorios en cada corrida, esto es útil para notar los cambios en los hiperparámetros que definen el modelo, de lo contrario sería más difícil notar los cambios. Luego pueden eliminar esta línea.
set.seed(12345) # Oueden usar cualquier número en un rango definido por R

#Particiones, 80% para entrenar el modelo y el 20% restante para testear el modelo, son valores aleatorios, no en orden

particioncasas <- createDataPartition(casas$medv, p = 0.8, list = FALSE)

casastraining <- casas[particioncasas,] #80% para entrenamiento
plot(casastraining$medv, col="blue") # Esto solo para visualizar, no es necesario
casastesting <- casas[-particioncasas,] #20% para testear/evaluar
points(casastesting$medv) # Visualizar, no es necesario

# Definición del modelo de regresión lineal

modelolineal <- lm (medv~., data = casastraining)

# Aquí pueden ver los parámetros del modelo, ustedes ya conocen estos resultados de sus clases previas.
summary(modelolineal)

# Ya con el modelo ahora se debe evaluar si lo hizo bien o no, para esto usamos ese 20% de los datos que no se usaron para entrenar y se evalúa en el modelo:
predccionmodelo <- predict(modelolineal, newdata = casastesting)

# Graficamos los resultados, de los datos reales (Color azul) Vs los datos que el modelo predijo (color rojo)
plot(casastesting$medv, col="blue",pch=20)
points(predccionmodelo, col="red", pch=20)
# Pueden ver que tan lejos o cerca está cada punto de su modelo, aparentemente se ve decente, qué opinan ustedes?

#Para evaluar su modelo pueden usar varias métricas, donde comparan punto a punto qué tan lejos están los datos reales de los datos que su modelo predijo:
# Mas info: https://github.com/mfrasco/Metrics
install.packages("Metrics")
library(Metrics)

mse(casastesting$medv, predccionmodelo) #Mean Squared Error
rmse(casastesting$medv, predccionmodelo) #Root Mean Squared Error
mae(casastesting$medv, predccionmodelo) #Mean Absolute Error
sse(casastesting$medv, predccionmodelo) #Sum of Squared Errors
rae(casastesting$medv, predccionmodelo) #Relative Absolute Error
rrse(casastesting$medv, predccionmodelo) #Root Relative Squared Error
# Con cuál métrica se quedan ustedes?

#Finalmente pueden hacer una tabla con los resultados para comparar y discutir:

#Tabla, método 1
Tablademetricas <- data.frame(Tipo_de_métrica=rep(c('Mean Squared Error', 'Root Mean Squared Error', 'Mean Absolute Error', 'Sum of Squared Errors',"Relative Absolute Error","Root Relative Squared Error")),
                 Valor_de_la_métrica=rep(c(mse(casastesting$medv, predccionmodelo), rmse(casastesting$medv, predccionmodelo),mae(casastesting$medv, predccionmodelo),sse(casastesting$medv, predccionmodelo),rae(casastesting$medv, predccionmodelo),rrse(casastesting$medv, predccionmodelo))))
Tablademetricas

#Tabla, método 2
tabmetricas <- matrix(c (mse(casastesting$medv, predccionmodelo), rmse(casastesting$medv, predccionmodelo),mae(casastesting$medv, predccionmodelo),sse(casastesting$medv, predccionmodelo),rae(casastesting$medv, predccionmodelo),rrse(casastesting$medv, predccionmodelo)), ncol = 1 , byrow = TRUE ) 
colnames (tabmetricas) <- c ('Valor de la métrica') 
rownames (tabmetricas) <- c ('Mean Squared Error', 'Root Mean Squared Error', 'Mean Absolute Error', 'Sum of Squared Errors',"Relative Absolute Error","Root Relative Squared Error") 
tabmetricas <- as.table (tabmetricas)
tabmetricas
