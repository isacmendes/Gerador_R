# Title     : TODO
# Objective : TODO
# Created by: Isac
# Created on: 15/01/2021

options(scipen=999)
#
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
if (!require(rstatix)) install.packages("rstatix")
library(rstatix)
if (!require(nortest)) install.packages("nortest")
library(nortest)
if (!require(fpp)) install.packages("fpp")
library(fpp)
if (!require(ARTool)) install.packages("ARTool")
library(ARTool)
if (!require(car)) install.packages("car")
library(car)

# 1) Carga e definição de fatores
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-14/Result2021-14-01.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-17/Result2021-17-01.csv', sep=',',dec=',' )
amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-27/Result2021-27-01_30.csv', sep=',',dec=',' )
attach(amostra)

amostra$algo <- factor(amostra$algo)
amostra$vertices <- factor(amostra$vertices)
amostra$percNeg <- factor(amostra$percNeg)
amostra$disRate <- factor(amostra$disRate)
amostra$layer <- factor(amostra$layer)
amostra$fan <- factor(amostra$fan)

#amostra$effort <- 1/(sqrt(1/(1/(sqrt(1/sqrt(sqrt(1/sqrt(amostra$effort))))))))
#amostra$effort <- log(amostra$effort)

hist(amostra$effort)

# 2) Aplicação ANOVA

m <- art(effort ~ algo*percNeg, amostra)
anova(m, response='aligned')
print('summary m:')
summary(m)
anova(m)
hist(m$residuals)

#qqnorm(residuals(m));qqline(residuals(m))

#fitAov<-aov(effort ~ algo*vertices*percNeg*cpMult*disRate*fan*layer, amostra)
fitAov<-aov(effort ~ algo*percNeg, amostra)
fitAov

# 3) Tranformação e reaplicação ANOVA
lambda <- BoxCox.lambda(amostra$effort, method=c("guerrero"), lower=-1, upper=1)
#lambda <- BoxCox.lambda(fitAov$residuals, method=c("guerrero", "loglik"), lower=-5, upper=5)
amostra$new_residuals <- BoxCox(amostra$effort, lambda)
#fitAov_new <- aov(new_residuals~algo*vertices*percNeg*cpMult*disRate*fan*layer)
fitAov_new <- aov(new_residuals ~ algo*percNeg, amostra)

# lambda <- BoxCox.lambda(amostra$effort)
# lynx.fit <- ar(BoxCox(amostra$effort,lambda))
# plot(forecast(lynx.fit,h=20,lambda=lambda))
# hist(lynx.fit$residuals)

# 4) Post-hoc
print("Teste Normalidade:")
#lillie.test(fitAov_new$residuals)
#lillie.test(m$residuals)
lillie.test(residuals(m))

print("Teste Normalidade Shapiro:")
#shapiro_test(fitAov_new$residuals)
#shapiro_test(m$residuals)
#shapiro.test(residuals(m))

print("Teste Normalidade Anderson-Darling:")
ad.test(fitAov_new$residuals)
ad.test(m$residuals)

print("Teste Homgeneidade:")
leveneTest(effort ~ algo*percNeg, data=amostra)
leveneTest(effort ~ algo*percNeg, data=amostra)

#fligner.test(amostra$effort ~ amostra$algo*amostra$percNeg)

#hist(m$residuals)