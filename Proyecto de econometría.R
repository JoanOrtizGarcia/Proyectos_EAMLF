# Limpiar entorno de trabajo
rm(list = ls())

# Librerias ---------------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(modeest)
library(moments)

# Exportando la base de datos
# Conociendo la ubicación de los datos
file.choose()

# Crear la variable que contenga la ruta de la base de datos

ubicacionbase <- "C:\\Users\\Joan Ortiz\\Desktop\\ECONOMETRIA\\Proyecto econo\\Base de datos final (2).xlsx"  

# Creando la variable que contenga los datos

basedatos <- read_excel(ubicacionbase, 
                        sheet = 'Base de datos', range = 'A1:G29')

# Creando nuevas variables que contengan la columna requerida.
CPIB  <- (basedatos$TPIB)
TDES  <- (basedatos$TDES)
FSTK <- (basedatos$TFK)
XN    <- (basedatos$TXN)
PAGR  <- (basedatos$PAGR)

# Estadística descriptiva de las variables del modelo --------------------- 
 
# Resultados arrojados de: Crecimiento PIB, Tasa de desempleo, formación del 
# stock de capital exportaciones netas, población agricola, RESPECTIVAMENTE
# --------------------------------------------------------------------------
# Medidas de tendencia central

summary(basedatos)

# Media  
mean(CPIB) 
mean(TDES) 
mean(FSTK) 
mean(XN) 
mean(PAGR)  

# Mediana
median(CPIB) 
median(TDES) 
median(FSTK) 
median(XN) 
median(PAGR)

# Moda
mfv(CPIB) 
mfv(TDES)
mfv(FSTK) 
mfv(XN) 
mfv(PAGR)      
# Concluimos que la moda no es un dato estadístico interesante para este modelo
# debido a que ningún dato se repite más de una vez con excepción a la población
# agricola


# Medidas de dispersión
# Varianza, desviación estándar, rango estadístico, coeficiente de variación

# Rango 

max(CPIB) - min(CPIB) 
max(TDES) - min(TDES) 
max(FSTK) - min(FSTK) 
max(XN) - min(XN)     
max(PAGR) - min(PAGR) 

# Varianza

var(CPIB)  
var(TDES) 
var(FSTK)
var(XN)
var(PAGR)

# Desviación estándar

sd(CPIB)
sd(TDES)
sd(FSTK)
sd(XN)
sd(PAGR)

# Coeficiente de variación

(sd(CPIB)/mean(CPIB))*100
(sd(TDES)/mean(TDES))*100
(sd(FSTK)/mean(FSTK))*100
(sd(XN)/mean(XN))*100
(sd(PAGR)/mean(PAGR))*100


# Medidas de forma
# Karl Pearson. El único valor útil será el de población agricola
# que tiene una moda de un solo valor

(mean(PAGR)-mfv(PAGR))/sd(PAGR)

# Los valores de la población agricola se encuentran en la parte izquierda 
# de la distribución


# Fisher, si es positivo habrá asimetría hacia derecha, caso contrario si 
# nuestro valor da negativo

skewness(CPIB)
skewness(TDES)
skewness(FSTK)
skewness(XN)
skewness(PAGR)

# Kurtosis, si el valor es < 3 significa que la distribución es platicúrtica o
# "achatada", caso contrario significa distribución leptocúrtica o puntuda

kurtosis(CPIB)
kurtosis(TDES)
kurtosis(FSTK)
kurtosis(XN)
kurtosis(PAGR)

#-----------------------------------------------------------------------------
# Realizando la regresión del modelo

regresion1 <- lm((TPIB)~ TDES + TFK + TXN + PAGR , data = basedatos )  
summary(regresion1) 

# Intervarlos de confianza

conf.level <- 0.95 # nivel de confianza del 95% 
alpha <- 1-conf.level # nivel de significancia del 5%

# Desviaciones estándar de cada beta obtenidas de la regresión

sigma2 <- 0.08220
sigma3 <- 0.02120
sigma4 <- 0.05692
sigma5 <- 0.08585

# Utilizaando el valor universal para Talpha2 del 5% con
# n-k (28-5) grados de libertad

Talpha2 <- 2.06865761

# El valor inferior es = Bj estimado - (Talpha2 * desviación estandar del Bj)
# El valor superior es = Bj estimado + (Talpha2 * desviación estandar del Bj)
# B(Número del beta) (Inferior o superior)


B2I <- -0.10045 - (Talpha2*sigma2);B2I
B2S <- -0.10045 + (Talpha2*sigma2);B2S
B3I <- 0.15976 - (Talpha2*sigma3);B3I  
B3S <- 0.15976 + (Talpha2*sigma3);B3S
B4I <- 0.13669 - (Talpha2*sigma4);B4I
B4S <- 0.13669 + (Talpha2*sigma4);B4S
B5I <- -0.06083 - (Talpha2*sigma5);B5I
B5S <- -0.06083 + (Talpha2*sigma5);B5S

# Pruebas de hipótesis
# Significancia individual

#H0: B2 = 0  (Beta 2 es igual a 0)
#H1: B2 != 0 (Beta 2 es diferente de 0)

#H0: B3 = 0  
#H1: B3 != 0

#H0: B4 = 0
#H1: B4 != 0

#H0: B5 = 0
#H1: B5 != 0

pvalueB2 <- 0.23409
alpha > pvalueB2 
# No se rechaza H0 a un alfa del 5%, la variable no es significativa para el modelo

pvalueB3 <- 1.18e-07
alpha > pvalueB3
# Se rechaza H0 a un alfa del 5%, la variable es significativa para el modelo

pvalueB4 <- 0.02482
alpha > pvalueB4
# Se rechaza H0 a un alfa del 5%, la variable es significativa para el modelo

pvalueB5 <- 0.48575
alpha > pvalueB5
# No se rechaza H0 a un alfa del 5%, la variable no es significativa para el modelo

# Pruebas de significancia global

#H0: B2=B3=B4=B5 = 0      
#H1: al menos un Bk es diferente de cero 

r2=summary(regresion1)$r.squared;r2

Fcalc = (r2/4)/((1-r2)/23); Fcalc
Ftab = qf(0.05, 4, 23, lower.tail=F) ; Ftab

p.valueFcal= pf(q=Fcalc, df1=4, df2=23, lower.tail = F) ; p.valueFcal

Fcalc > Ftab  #True o False será la respuesta

# Como el Fcalc es mayor al Ftab se rechaza Ho con un alfa del 5 % 
# El modelo es globalmente significativo


# ----------------------------------------------------------------------------
# Gráficas para cada variable


ggplot(basedatos, aes( x = YEAR, y = TPIB,)) +
  ggtitle('Tasa de cambio del PIB') +
 geom_line(colour="#6600CC") + xlab('Año') + ylab ('PIB') + theme_classic()

ggplot(basedatos, aes( x = YEAR, y = TDES,)) +
  ggtitle('Tasa de desempleo') +
  geom_line(colour="#6600CC") + xlab('Año') + ylab ('T. Desempleo') + theme_classic()

ggplot(basedatos, aes( x = YEAR, y = TFK,)) +
  ggtitle('Tasa formación de capital') +
  geom_line(colour="#6600CC") + xlab('Año') + ylab ('T. Formación del Capital') + theme_classic()

ggplot(basedatos, aes( x = YEAR, y = TXN,)) +
  ggtitle('Tasa de cambio de las exportaciones netas') +
  geom_line(colour="#6600CC") + xlab('Año') + ylab ('T. Exportaciones Netas') + theme_classic()

ggplot(basedatos, aes( x = YEAR, y = PAGR,)) +
  ggtitle('% de la población dediaca a act. agricolas') +
  geom_line(colour="#6600CC") + xlab('Año') + ylab ('Pob. en Act. Agricolas') + theme_classic()


