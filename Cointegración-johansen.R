############################################ Prueba 1 --------------------- ####
rm(list=ls())
library(readxl)
dat <- read_excel("04_Mbase arma.xlsx")
attach(dat)
head(dat)
#-------------------------------------------------------------------------#
#Se supone hasta ahora que esta es lago plazo.
m1 <- lm(GCP~ID,data=dat)

summary(m1)

library(lmtest)
dwtest(m1, alternative = "two.sided")

library(tseries)
y0=GCP
adf.test(y0)
pp.test(y0)

x0=ID
adf.test(x0)
pp.test(x0)
#se busca correr la puerba del eror
r0=m1$effects
adf.test(r0)
pp.test(r0)
# Variable GCP no es estacionaria

# 1. Prueba de media cero:
# H0 : E[u] = 0
# H1 : E[u] != 0
t.test(m1$residuals,mu=0)
# p-value > 0.05, no se rechaza HO: en este caso los errores tienen media cero.
#-------------------------------------------------------------------------#
#Se supone hasta ahora que esta es corto plazo.
m2 <- lm(diff(GCP)~diff(ID),data=dat)
summary(m2)

library(lmtest)
dwtest(m2, alternative = "two.sided")

library(tseries)
y1=diff(GCP)
adf.test(y1)
pp.test(y1)

x1=diff(ID)
adf.test(x1)
pp.test(x1)
#se busca correr la puerba del eror
r1=m2$effects
adf.test(r1)
pp.test(r1)

# 1. Prueba de media cero:
# H0 : E[u] = 0
# H1 : E[u] != 0
t.test(m2$residuals,mu=0)
# p-value > 0.05, no se rechaza HO: en este caso los errores tienen media cero.
#-------------------------------------------------------------------------#
##### Como se ve que estan en equilibrio o no
# a la de diff agregale 1 
coef(summary(m2))[1, "Std. Error"] 
m3 <- lm(diff(GCP)~diff(ID),data=dat)
summary(m2)
#### falta ajustar lo del error

#Prueba johansen
library(urca)
jotest=ca.jo(data.frame(y0,x0), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)



############################################ Prueba 2 --------------------- ####
rm(list=ls())
library(readxl)
dat <- read_excel("04_Mbase arma1.xlsx")
attach(dat)
head(dat)
#-------------------------------------------------------------------------#
#Se supone hasta ahora que esta es lago plazo.
m1 <- lm(log(Consumo)~log(PIB),data=dat)
summary(m1)

library(lmtest)
dwtest(m1, alternative = "two.sided")
#DW>r^2

library(tseries)
y0=log(PIB)
adf.test(y0)
pp.test(y0)

x0=log(Consumo)
adf.test(x0)
pp.test(x0)
#se busca correr la puerba del eror
r0=m1$effects
adf.test(r0)
pp.test(r0)
# Variable GCP no es estacionaria

# 1. Prueba de media cero:
# H0 : E[u] = 0
# H1 : E[u] != 0
t.test(m1$residuals,mu=0)
# p-value > 0.05, no se rechaza HO: en este caso los errores tienen media cero.

#-------------------------------------------------------------------------#
#Se supone hasta ahora que esta es corto plazo.
m2 <- lm(diff(log(Consumo))~diff(log(PIB)),data=dat)
summary(m2)

library(lmtest)
dwtest(m2, alternative = "two.sided")

library(tseries)
y1=diff(log(PIB))
adf.test(y1)
pp.test(y1)

x1=diff(log(Consumo))
adf.test(x1)
pp.test(x1)
#se busca correr la puerba del eror
r1=m2$effects
adf.test(r1)
pp.test(r1)

# 1. Prueba de media cero:
# H0 : E[u] = 0
# H1 : E[u] != 0
t.test(m2$residuals,mu=0)
# p-value > 0.05, no se rechaza HO: en este caso los errores tienen media cero.
#-------------------------------------------------------------------------#
##### Como se ve que estan en equilibrio o no
# a la de diff agregale 1 
coef(summary(m2))[1, "Std. Error"] 
m3 <- lm(diff(PIB)~diff(Consumo),data=dat)
summary(m2)
#### falta ajustar lo del error

#Prueba johansen
library(urca)
jotest=ca.jo(data.frame(y0,x0), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

