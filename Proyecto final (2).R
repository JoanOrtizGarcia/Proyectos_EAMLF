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

ubicacionbase <-  "C:\\Users\\Joan Ortiz\\Desktop\\ECONOMETRIA\\Proyecto econo\\Base de datos final (2).xlsx"

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
  ggtitle('% de la población dedicada a Act. Agricolas') +
  geom_line(colour="#6600CC") + xlab('Año') + ylab ('Pob. en Act. Agricolas') + theme_classic()

#------------------------------------------------------------------------------#
  # Normalidad

#------------------------------------------------------------------------------#

hist(regresion1$residuals)

qqPlot(regresion1$residuals)

#Jarque-Bera (Ho: Normalidad)

library(tseries) 
jarque.bera.test(regresion1$residuals)

0.05 > 0.7629

# Alpha menor a p value, aceptamos la hipotesis, hay normalidad
#Shapiro-wilk (Ho: Normalidad)

shapiro.test(regresion1$residuals)

0.05 > 0.8701




#------------------------------------------------------------------------------
   #Correcta especificacion 
#------------------------------------------------------------------------------


library(lmtest)

resettest(regresion1)

# Ho: Y1=Y2=0 (correcta especificacion)
# Ha: al menos un gamma es diferente de 0 
# Regla de decisiÃ³n: si alfa (0.05) > p.value se rechaza Ho

0.05 < 0.5982
# En este caso alfa: 0.05 < p-value, se acepta Ho, y se concluye que 
# no hay error en la especificaciÃ³n del modelo.

# Prueba RESET de Ramsey con calculo de regresiÃ³n auxiliar
#_____________________________________________________________

u.est   = fitted(regresion1)
u.2.est = u.est^2
u.3.est = u.est^3

reg_ramsey <- lm(formula = TPIB ~ TDES + TFK + TXN + PAGR  + u.2.est + u.3.est , data = basedatos)
summary(reg_ramsey)


install.packages("car")
library(car)

# Ho: Y1=Y2=0 (correcta especificaciÃ³n)
# Ha: al menos un gamma es diferente de 0 
# Regla de decisiÃ³n: si alfa (0.05) > p.value se rechaza Ho

# Funcion de R para F-cal directa
linearHypothesis(reg_ramsey, c("u.2.est","u.3.est"),c(0,0))

# no se cumple que alfa (0.05) > p.value, por tanto, se acepta Ho.



#------------------------------------------------------------------------------
# HOMOCEDASTICIDAD
#------------------------------------------------------------------------------

library(car)     
library(lmtest)

#Grafico de dispersionn de residuos studentizados

spreadLevelPlot(regresion1)

#Prueba Breush-Pagan(Ho:Homocedasticidad)

bptest(regresion1)

0.05 > 0.003584

# Al seguir teniendo homocedasticidad luego de la estimacion robusta,
# consideramos que para este caso particular es un problema de la propia muestra 


#------------------------------------------------------------------------------#
#Correccion heterocedasticidad

#------------------------------------------------------------------------------#

# Aceptando que los errores tengan heterocedasticidad con Estimacion Robusta

library(MASS)
regresion2<-rlm(TPIB ~ TDES + TFK + TXN + PAGR, data = basedatos)
summary(regresion2)
summary(regresion1)

bptest(regresion2)

0.05 > 0.003584


#------------------------------------------------------------------------------
 # Cambio estructural 
#------------------------------------------------------------------------------

library (strucchange)

#test de Chow 
sctest(regresion2)
f.tab = qf(0.05, 28, 24)
f.tab

# Hay cambio estructural

# Como Fcal > Ftab se rechaza Ho, y se concluye que hay cambio estructural en el 

plot(basedatos$TDES, basedatos$TPIB)
plot(basedatos$TFK, basedatos$TPIB)
plot(basedatos$TXN, basedatos$TPIB)
plot(basedatos$PAGR, basedatos$TPIB)
plot(basedatos$TDES)  # Variable con el cambio estructural
plot(basedatos$TFK)
plot(basedatos$TXN)
plot(basedatos$PAGR)
plot(basedatos$TPIB)

CUSUM<- efp(formula =TPIB ~ TDES + TFK + TXN + PAGR, data = basedatos)

plot(CUSUM, alpha=0.05, boundary = TRUE)
sctest(CUSUM)


#------------------------------------------------------------------------------#
# Correccion de cambio estructural

#------------------------------------------------------------------------------#
# Tabla numero 2, "correcion"

file.choose()


# Crear la variable que contenga la ruta de la base de datos

datosco <- "C:\\Users\\Joan Ortiz\\Desktop\\ECONOMETRIA\\Proyecto econo\\correcion.xlsx"

# Creando la variable que contenga los datos

datoscorr <- read_excel(datosco, 
                        sheet = 'Base de datos', range = 'A1:H29')

regresion3 <- lm((TPIB) ~ D1 + TDES + TFK + TXN + PAGR + D1TDES, data = datoscorr)  
summary(regresion3)      
summary(regresion2)

# Los valores de la dummy van a ser = a 0 para el periodo < o igual al año 2000
# Los valores de la dummy van a ser = a 1 para el periodo > al año 2000

#------------------------------------------------------------------------------
  # NO MULTICOLINEALIDAD

#------------------------------------------------------------------------------

#Matriz de correlaciones 
#Multicolinealidad si algÃºn coeficiente es mayor a 0,8.

attach(datoscorr)
cor(cbind(D1+TDES,TFK,TXN,PAGR+D1TDES))

#Regresiones auxiliares 
#Multicolinealidad si algÃºn R2 es mayor al del modelo original.

summary(regresion3)$r.squared #R2 de reg

R2=function(call){return(summary(lm(call))$r.squared)}
regresion3$call #muestra la forrmula de la regresionn
R2(TDES~TFK+TXN+PAGR)
R2(TFK~TXN+PAGR+TDES)
R2(TXN~TDES+TFK+PAGR)
R2(PAGR~TDES+TFK+TXN)

#------------------------------------------------------------------------------#
 # NO AUTOCORRELACION   

#------------------------------------------------------------------------------#

#Durbin Watson test (Ho:No autocorrelacion de 1er orden)

dwtest(regresion3)

0.05 > 0.2453

#Prueba Breush-Godfrey (Ho:No autocorrelacionn de orden p)

bgtest(regresion3)

0.05 > 0.7344 

#Grafico de autocorrelacionn

acf(regresion3$residuals)

#Si se sale de las bandas indica autocorrelacion.



#------------------------------------------------------------------------------#
# Ajuste

#------------------------------------------------------------------------------#

fitted<-regresion3$fitted.values
matrix<-cbind(basedatos$TPIB, fitted)
ajustados<-ts(data=matrix) 
#start, frequency (si tienen serie de tiempo)
ts.plot(ajustados, col=6:2)

#------------------------------------------------------------------------------#
  # Prediccion

#------------------------------------------------------------------------------#

regresion3 <- lm((TPIB) ~ D1 + TDES + TFK + TXN + PAGR + D1TDES, data = datoscorr)  
summary(regresion3)

# Creando el vector de pronostico

x.0 = data.frame(D1= 1, TDES= 10.44, TFK= 5.63, TXN=4.753, PAGR=15.37, D1TDES=10.44)

# Prediccion puntual y por intervalo

predict(regresion3, x.0, level = 0.95, interval = "prediction")

# fift = el valor puntual de la t. de var. pib para la prediccion será de 4.98%

#lwr Intervalo de la prediccion, la t. de v del pib a estos valores (...) 
# estara entre -2.8% y 12.8%
#upr



r2=summary(regresion3)$r.squared;r2

Fcalc = (r2/6)/((1-r2)/22); Fcalc
Ftab = qf(0.05, 6, 22, lower.tail=F) ; Ftab

Fcalc > Ftab

library(stargazer)
stargazer::stargazer(regresion3)
stargazer(regresion3, type = "text", title = "Resumen de la regresión")




        
        