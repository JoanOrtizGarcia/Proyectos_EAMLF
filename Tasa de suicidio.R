#RELACIÓN ENTRE LA TASA DE SUICIDIOS CON LA TASA DE VIOLENCIA INTRAFAMILIAR, DELITO SEXUAL
# Y POBREZA MONETARIA POR EN 24 DEPARTAMENTOS (2015-2018)

rm(list = ls()) #limpiar el enviroment 

#Paquetes 
 install.packages("plm")         #Panel linear models 
 install.packages("gplots")      #Tools for plotting data 
 install.packages("stargazer")   #Tablas más estéticas 
 install.packages("haven")       #Importar datos 
 install.packages("sandwich")    #Estimador de erores robustos 
 install.packages("lmtest")      #Provee herramientas para hacer inferencia estadística para modelos paramétricos lineales
 install.packages("tseries")     
 install.packages("wooldridge")  # Contiene data sets con los que se pueden realizar ejercicios econométricos
 install.packages("tidyverse")
install.packages("readxl")



#Paquetes 

 install.packages("plm")         #Panel linear models 
 install.packages("gplots")      #Tools for plotting data 
 install.packages("stargazer")   #Tablas más estéticas 
 install.packages("haven")       #Importar datos 
 install.packages("sandwich")    #Estimador de erores robustos 
 install.packages("lmtest")      #Provee herramientas para hacer inferencia estadística para modelos paramétricos lineales
 install.packages("tseries")     
 install.packages("wooldridge")  # Contiene data sets con los que se pueden realizar ejercicios econométricos
 install.packages("tidyverse")
 
 library(plm);library(gplots);library(stargazer)
 library(haven);library(sandwich);library(lmtest)
 library(tseries);library(wooldridge);library(tidyverse)
 library("readxl")


# Nota: Foreign es una biblioteca que es muy freuentemente usada para importar bases
#       de datos de stata (.dta es el archivo de base de datos de stata).
#       No obstante, dicho paquete genera inconvenientes a la hora de importar cierto
#       bases de datos provenientes de ciertas versiones de stata.
#       El paquete haven es mucho más general que el paquete foreign y no genera 
#       inconvenientes a la hora de importar bases de datos tipo .dta. Por lo tanto, 
#       al igual que en el presente script, se recomienda usar la función read_dta a la
#       hora de trabajar bases de datos de stata. 

  
#Vamos a cargar las series

Data <- read_excel(file.choose())

attach(Data)
# Manera de visualizar toda la base de datos desde R
View(Data)
# Manera compacta de ver una base de datos
glimpse(Data)

#Tratar la base de datos como un panel de datos:
#Recordemos que los datos panel son una combinación de corte transversal 
#(parte espacial) y series de tiempo (parte dinámica).
#Nuestro objetivo es observar varios agentes en un periodo de tiempo determinado. 

help("pdata.frame") #conocer un poco más sobre la función 
panel.Data = pdata.frame(Data, index = c("Departamento","Años")) #indexamos por individuo y año

#Estructura del codigo: ---------------------------------------------------#
#Nombre <- pdata.frame(Base_datos, index=c("Var_indivuo","Var_tiempo"))
#-------------------------------------------------------------------------#

#Para conocer las dimensiones del panel
pdim(panel.Data)

#Para determinar si las variables cambian a lo largo del tiempo (por variación temporal e individual).
pvar(panel.Data)

#Distribución de la población.
plotmeans(lwage ~ married, main="Distribución de la población", data=panel.Data)


#Estimación MCO combinados -E.F.- E.A.----------------------------------------------------

#Con datos panel, el error se descompone en a_i, que es el componente del error 
#propio de cada individuo y que no cambia a lo largo del tiempo, más u_i, el cual
#sí varia dentro de las personas y a lo largo del tiempo.

#Con el Modelo de Efectos Aleatorios vamos a excluir culquier tipo de correlación de 
#a_i con alguna o varias de las variables. 

#### Mínimos Cuadrados Combinados ---> pooling ------------

Pooled = plm(`Tasa de Suicidio`~`ln TV`+`ln TD`+`Pobreza %`,
             data=panel.Data, model="pooling")
summary(Pooled) #Incluye variables que cambian y no cambian a lo largo del tiempo.

#### Modelo de Efectos Aleatorios ---> random ------------
Random = plm(`Tasa de Suicidio`~`ln TV`+`ln TD`+`Pobreza %`,
             data=panel.Data, model="random")
summary(Random) #Sucede lo mismo con MCOC: permanecen las variables que cambian y no cambian a lo largo del tiempo.

#Con Primeras Diferencias y Efectos Fijos se intenta eliminar el componente a_i del error!
#Esto, debido a que suponemos que existe una correlación entre dicho componente y alguna o varias de las variables. 

#### Modelo de Efectos Fijos ---> within ------------
Fixed = plm(`Tasa de Suicidio`~`ln TV`+`ln TD`+`Pobreza %`,
            data=panel.Data, model="within")
summary(Fixed) #Solo tenemos las variables que cambian en el tiempo.

#### Modelo de Primeras Diferencias ---> fd -------------
FD = plm(`Tasa de Suicidio`~`ln TV`+`ln TD`+`Pobreza %`,
         data=panel.Data, model="fd")
summary(FD) #Diferenciamos y perdemos un periodo de tiempo. 
#También elimina variables que son constantes en el tiempo. 

#Presentación de resultados:
stargazer(Pooled,Random,Fixed,FD, type="text",           
          column.labels=c("OLS","RE","FE", "PD", "BT"),keep.stat=c("n","rsq"))

 ## NOTA ACLARATORIA SOBRE BETWEEN ------------------------------------------------## 
## El modelo "between" en plm realiza una transformación diferente a la que vieron  
## en clase, pues tiene en cuenta las variaciones que puedan existir entre grupos 
## de individuos. Una anotación importante es que con este tipo de modelos se
## pierden obervaciones. 

## Mientras que el modelo de efectos fijos o "within" tiene en cuenta 
## las variaciones de los sujetos y no entre sujetos. 

## En otras palabras, "within" usa la media de cada individuo, mientras que "between" 
## usa la media de todos los datos para un año especifico.

## Para evaluar efectos especificos en los modelos que estimen con PLM, puden usar 
## el argumento " effect" y este permite "individual" , "time", "twoways"
## (efectos individuales y de tiempo) y "nested" (anidados). Por defectos PLM usa "twoways"
##---------------------------------------------------------------------------------------##

#Selección del modelo ---------------------------------------------------------------------

#¿Es mejor Efectos fijos o Efectos Aleatorios?:

#### Test de Hausman para comparar EF vs EA ------------
phtest(Fixed, Random) #Ho: Los modelos son equivalentes estadisticamente, por eficiencia elijo EA. 
#Si p-value es menor a 0.05, se rechaza Ho, por consiguiente el modelo de efectos aleatorios es consistente
#y eficiente.
#Si p-value es mayor a 0.05, no se rechaza la hipotesis nula y por lo tanto no hay diferencia sistematica
#entre EF Y EA. Pero, mejor usaremos efectos aleatorios que no muestra correlación.

#### Test de Primeras diferencias de Wooldridge para comparar EF vs PD ------------
pwfdtest(`Tasa de Suicidio`~`ln TV`+`ln TD`+`Pobreza %`, data=panel.Data,h0= "fe") #H0 = corr(Uit,Uit-1) = 0
pwfdtest(`Tasa de Suicidio`~`ln TV`+`ln TD`+`Pobreza %`, data=panel.Data,h0= "fd") #H0 = errores diferenciados no correlacionados
#Errores no diferenciados sin correlación serial. 0.77>0.05
#Errores diferenciados correlacionados. 0.01<0.05

#### Pooled VS Efectos Aleatorios ------------
#Prueba de Multiplicadores de Lagrange de Breusch-Pagan para E.A
plmtest(Pooled,type = "bp")  #Ho = Mejor Pooled porque var(ai)=0 / H1 = Se prefiere EA.

#### Prueba Breush-Pagan (Ho: Homocedasticidaad) ------------
bptest(Pooled) #Si p-value>5% posiblemente es Pooled. 
#p-value: 0.603, POR LO TANTO ESCOGEMOS POOLED.

#### ¿El efecto es individual, temporal, o ambos? ------------

plmtest(Pooled,"time","bp")         #Ho:Efectos temporales no significativos 
plmtest(Pooled,"individual","bp")   #Ho:Efectos indivuduales no significativos
#p- value <2.2e-16, se rechaza Ho, por ende EI significativos.
plmtest(Pooled,"twoways","bp")      #Ho:Efectos temporales e individuales no significativos
#2.2e-16#p-value <2.2e-16, se rechaza Ho, por ende ET, EI significativos.


#Validación de supuestos------------------------------------------------------------------

#### Prueba de heterocedasticidad ------------ NO HAY HETEROCEDASTICIDAD
bptest(Pooled);bptest(Random);bptest(Fixed); bptest(FD)

#### Test Breusch-Godfrey para autocorrelaci?n de orden p ------------
bgtest(Pooled);bgtest(Random);bgtest(Fixed);bgtest(FD)

#### Breusch-Pagan LM test for cross-sectional dependence in panels ------------
pcdtest(Pooled,test = "lm");pcdtest(Random,test = "lm");pcdtest(Fixed,test = "lm");#pcdtest(FD,test = "lm")      

#### Correción de correlación serial para EF ------------ CREO QUE NO LO USAMOS
# MCOV=vcovHC.plm(Fixed, method=c("arellano"))          
MCOV1=vcovHC(Fixed, method="arellano")

# coeftest(Fixed,MCOV)
coeftest(Fixed,MCOV1)
help("vcovHC.plm")
# Arellano : Soluciona heterocedasticidad y autocorrelacion serial 
# White1 - Whit 2 : Soluciona heteroscedaticidad. 

#### Análisis de Normalidad ------------
hist(residuals(Fixed))

qqnorm(residuals(Fixed))
qqline(residuals(Fixed))

library(car)
Residuales=residuals(Fixed)
qqPlot(Residuales)

#Es deseable que se ajusten a la linea de tendencia para cumplir normalidad 

jarque.bera.test(residuals(Fixed))  

lin_reg <- lm((TS) ~ lnTV, data= Data2)
lin_reg
Data1 %>% 
  ggplot(aes(x=Data2$lnTV , y=Data2$TS)) +
  geom_point() +
  geom_abline(slope = lin_reg$coefficients[[2]],
              intercept = lin_reg$coefficients[[1]], 
              color="Blue") +
  labs(x="Tasa de violencia intrafamiliar en logaritmo", y="Tasa de suicidio",
       title="Regresion de tasa de suicidio en función de la violencia intrafamiliar") +
  theme_classic()
lin_reg$coefficients[[2]] 
lin_reg$coefficients[[1]]