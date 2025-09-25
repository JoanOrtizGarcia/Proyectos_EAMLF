library(ggplot2)
library(tidyverse)
library(tidyquant)
library(tseries)
library(urca)
library(stargazer)
library(psych)
library(quantmod)
library(readxl) 
library(tidyquant)
library(tidyverse) 
library(timetk)
library(tibbletime)
library(broom)
library(quantmod )
library(PerformanceAnalytics)
library(xts)
library(rugarch)
library(highcharter)

#Trabajo presentado por Augusto Bossio y Joan Ortiz

microsoft=getSymbols("MSFT", periodicity="daily", from="2016-01-01", to= "2023-10-30")
disney=getSymbols("DIS", periodicity="daily", from="2016-01-01", to= "2023-10-30")
amazon=getSymbols("AMZN", periodicity="daily", from="2016-01-01", to= "2023-10-30")

###########################################################Primer punto
#Calculando los retornos


ret.microsoft=dailyReturn(MSFT)
ret.disney=dailyReturn(DIS)
ret.amazon=dailyReturn(AMZN)

plot(cbind(ret.microsoft, ret.disney, ret.amazon),col=c("black","aquamarine1", "red"), main="Retornos")
chartSeries(ret.microsoft)

##############################
#Los modelos ARCH y GARCH se estiman sobre modelos ARMA
#Los modelos ARCH tienen la varianza condicional depdendiendo de lor errores al cuadrado rezagados
#Los modelos GARCH, ademas de la parte ARCH, tiene la misma varianza rezagada
#Usualmente se usa unicamente el GARCH(1,1) O arch(1)

#######################################Estimación modelos ARMA
MSFT_arima=arima(ret.microsoft, order = c(2,0,1))
dis_arima=arima(ret.disney, order = c(1,0,1))
amzn_arima=arima(ret.amazon, order = c(1,0,1))

plot((MSFT_arima$residuals)^2)

#########################################Declarar modelo GARCH
garchmsft=ugarchspec(mean.model = list(armaOrder=c(2,1)),
                     variance.model =list(garchOrder=c(1,1)))
garchmsft


garchDIS=ugarchspec(mean.model = list(armaOrder=c(2,2)),
                     variance.model =list(garchOrder=c(1,1)))


garchAMZN=ugarchspec(mean.model = list(armaOrder=c(1,1)),
                     variance.model =list(garchOrder=c(1,1)))


#########################################Estimación de lo modelos garch

garch_fit_msft=ugarchfit(spec =garchmsft, data = ret.microsoft )
garch_fit_msft

garch_fit_dis=ugarchfit(spec =garchDIS, data = ret.disney )
garch_fit_dis


garch_fit_amzn=ugarchfit(spec =garchAMZN, data = ret.amazon )
garch_fit_amzn



###########################################Segundo punto

### Comprobando que no hayan datos nulos en cada activo ###

sum(ret.microsoft$daily.returns == "NULL")
sum(ret.amazon$daily.returns == "NULL")
sum(ret.disney$daily.returns == "NULL")

### Efectivamente indica que hay 0 datos nulos ###

### Añadiendo la volatilidad estocastica por cada activo ###

## Microsoft ##

micro_svtest = svsample(ret.microsoft, designmatrix = "ar1")
plot(micro_svtest)

df_micro = as.data.frame(micro_svtest$summary$sd)
test_micro=df_micro$sd^2 
plot(test_micro, type= "l")

## Amazon ##

amzn_svtest = svsample(ret.amazon, designmatrix = "ar1")
plot(amzn_svtest)

df_amzn = as.data.frame(amzn_svtest$summary$sd)
test_amzn=df_amzn$sd^2 
plot(test_amzn, type= "l")

## Disney ##

dis_svtest = svsample(ret.disney, designmatrix = "ar1")
plot(dis_svtest)

df_dis = as.data.frame(dis_svtest$summary$sd)
test_dis=df_dis$sd^2 
plot(test_dis, type= "l")

### Cuadrando las series de tiempo ###

res_mic = MSFT_arima$residuals^2
res_amzn = amzn_arima$residuals^2
res_dis = dis_arima$residuals^2

## Metiendo en una misma base los residuales estandarizados

basec = cbind(res_mic, res_amzn, res_dis)

## Paso omitible si se tiene ya la base ## 

write.xlsx(basec, "eliminar3.xlsx")

## Seleccionando el archivo ##

file.choose()  

## Creando variable con la base ajustada (la direccion va a cambiar)##

direccion = ("C:\\Users\\Jhoan\\Documents\\eliminar3.xlsx") 

## Leyendo la base ajustada ##

baseco = read_xlsx(direccion)

colnames(baseco) = c("VE.Microsoft","VE.Amazon","VE.Disney")

basecom = cbind(baseco, test_micro, test_amzn, test_dis)

## Convirtiendo los valores simulados a una escala mejor para comparar ##

basecom$test_micro = basecom$test_micro*70 
basecom$test_amzn = basecom$test_amzn*70
basecom$test_dis = basecom$test_dis*70

## Metiendo en una sola variable ambos estimadores por activo (punto 1 y 2) ##

Comparacion_micro = cbind(basecom$VE.Microsoft, basecom$test_micro)
Comparacion_amzn = cbind(basecom$VE.Amazon, basecom$test_amzn)
Comparacion_dis = cbind(basecom$VE.Disney, basecom$test_dis)

## Creando gráficas entre ambos estimadores para comparar ##

matplot(Comparacion_micro, type = "l", col = 8:9, xlab = "Muestra", 
        ylab = "Valor de Volatilidad" , 
        ylim = c(0, 0.005),
        main = "Comparación Volatilidad estocástica Microsoft")
matplot(Comparacion_amzn, type = "l", col = 8:9, xlab = "Muestra", 
        ylab = "Valor de Volatilidad", 
        ylim = c(0, 0.010),
        main = "Comparación Volatilidad estocástica Amazon")
matplot(Comparacion_dis, type = "l", col = 8:9, xlab = "Muestra", 
        ylab = "Valor de Volatilidad", 
        ylim = c(0, 0.006),
        main = "Comparación Volatilidad estocástica Disney")

################################Tercer punto

#Para este tercer punto se descargan los datos para los bonos de US usando el iShares 
#a 20 años con 0.70, y utilizamos acciones de microsof y Disney,
#cada una un .15 del portafolio


bonos=getSymbols("TLT", periodicity="daily", from="2010-01-01", to= "2023-10-30")
microsoft=getSymbols("MSFT", periodicity="daily", from="2010-01-01", to= "2023-10-30")
disney=getSymbols("DIS", periodicity="daily", from="2010-01-01", to= "2023-10-30")

#calculando sus retornos

ret.microsoft=dailyReturn(MSFT)
ret.disney=dailyReturn(DIS)
ret.bonos=dailyReturn(TLT)

#Se crea el portafolio

portafolio=cbind(ret.bonos,ret.disney,ret.microsoft)
colnames(portafolio)=c("ret_bonos","ret_disney","ret_msft")
plot(portafolio)

#Se definen los pesos
pesos=c(0.70, 0.15, 0.15)

#El retorno total del portafolio

retorno.portafolio=Return.portfolio(portafolio, weights = pesos);head(retorno.portafolio)

#Calculando el VaR y el cVaR para los activos


VaR=VaR(retorno.portafolio, p=0.95, method = "historical");VaR
cVaR=CVaR(retorno.portafolio, p=0.95, method = "historical");cVaR
plot(cbind(retorno.portafolio, VaR, cVaR))

chartSeries(retorno.portafolio, up.col = "black", theme = "white", main="Retornos del portafolio")

chartSeries(retorno.portafolio, up.col = "black", theme = "white")
abline(h=VaR, col="red", lwd=3);abline(h=cVaR, col="blue", lwd=3)

plot(cbind(retorno.portafolio, VaR, cVaR)*10000000)

varUSD=VaR*10000000;varUSD
CvarUSD=cVaR*10000000;CvarUSD

