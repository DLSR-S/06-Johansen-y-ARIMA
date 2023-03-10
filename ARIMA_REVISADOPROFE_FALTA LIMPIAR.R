# Limpieza del area de trabajo:
rm(list=ls())
######################### Cointegration Test Function #########################
###------------------------------ Load Data -------------------------------###
library(quantmod)
library(dplyr)
library(ggplot2)
library(readxl)
dat <- read_excel("04_Mbase arma.xlsx")
attach(dat)
head(dat)

spx.p <- Pib
spx.r <- diff(log(Pib))

###--------------------------- Normality Test-------------------------------###
hist(spx.r, freq=F,ylim=c(0,0.5), breaks=20)
curve(dnorm(x), from=-5,to=5,col='red',add=T)
qqnorm(spx.r)
qqline(spx.r,col=2)
  # shapiro test also finds spx.r far from normal
shapiro.test(coredata(spx.r))
#Nota* usar para rendimientos_
  
###-------------------------- Stationarity Test ----------------------------###
library(tseries)
  # price has unit root, not stationary
adf.test(spx.p)
#H0: Raíz unitaria, es un proc explosico basicamente no se puede usar, buscas rechazar H0
#3 FORMAS QUE DEFINIMOS QUE CAE DEBE SER MENOR A .05, para este caso se afirma que tiene raíz
  # ADF test rejects null hypothesis of unit root, suggesting stationarity
adf.test(spx.r)    		# Augmented Dickey-Fuller unit root
  # PP test also rejects null hypothesis of unit root and suggests stationarity
pp.test(spx.p)
pp.test(spx.r)				# Phillips-Perron unit root
#.05>p-value recahza 
#Nota: checar pueba KPSS de robustez estadistica. No rechazo KPSS buscar que P-VALUE sea mayor a 0.05
  library(pracma)
  # Hurst test shows trending
hurstexp(spx.p)
  
###-------------------------------- ARIMA Test -------------------------------###
# at 5%, acf has negative lag 1, lag 5, lag 18, and positive lag 16
acf(spx.r) #MA's, siempre te va a dar uno, porque se corre contra si misma.
pacf(spx.r) #AR's
library(forecast)
# It automatically picks ARIMA(3,0,3)
auto.arima(spx.r)		# automatically find the p,d,q, no necesariamente es el mejor modelo
spx.r.arima <- arima((spx.p), order=c(1,1,1))
#Log para corregir el problema de heterocedasticidad
# Suaviza los picos (variaciones) log.
#Para llevar a los datos usando log aplica exponencial.

## five period forecast
spx.r.arima.forecast <- forecast(spx.r.arima, h=30)

autoplot(spx.r.arima.forecast)
# Si se ven así, componlo y llevalo a los niveles que estas haciendo


#Autocorrelación
# residual is AR(0)
acf(spx.r.arima.forecast$residuals)
#Checa que otros MAs puedes ocupar

# test autocorrelation of forecast errors
Box.test(spx.r.arima.forecast$residuals, lag = 30, type = "Ljung-Box") 	
#Los lags que caen dentro de las bandas.
h <- 30
LB <- data.frame(X=1:h,LB=rep(NA,h))
for(i in 1:h){
  LB$LB[i] <- Box.test(spx.r.arima.forecast$residuals,lag=i,fitdf = 0,
                       type = 'Lj')$p.value
}

ggplot(LB,aes(x=X,y=LB))+
  geom_point()+
  geom_hline(yintercept = 0.05,col='blue',lty=2)
#
#sI SE VE LO DE LOS PUNTOS 12-15 DEBERIAS PROBAR OTROS MODELOS HASTA QUE TODOS QUEDEN FUERA.

# H0=Que los datos no esten autocorrelacionados, CALCULA P-VALUE y me dice si cae por arriba
# tecnicamwente todos deben caer por arriba esta correlacionada con 12 trimestres en el pasado
# Modelos SARIMA seasonal ARIMA.
#ASTSA paqueteria: De un libro, se llama TIME SERIES ALGO,

# REPORTES DINAMICOS QUE SE GENERAN DESDE UN MARKDOWN
# DOCUMENTOS PARAMETRIZADOS
# Constuye tu maquina financidra en r. CHECAR EL LIBRI >>>>>.<<<<<<<< 



# test if the forecast errors are normally distributed
plot.ts(spx.r.arima.forecast$residuals) 			# make a time plot of forecast errors


###-------------------------------- GARCH Test -------------------------------###
# r^2 is autocorrelated
acf((spx.r - mean(spx.r))^2)  				# conditional heteroscedasticity
spx.r.garch <- garch(spx.r,trace = F)
spx.r.res <- spx.r.garch$res[-1]
acf(spx.r.res)
acf(spx.r.res^2)


return(result)
