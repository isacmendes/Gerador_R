# Title     : TODO
# Objective : TODO
# Created by: Isac
# Created on: 18/01/2021

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
if (!require(moments)) install.packages("moments")
library(moments)
if (!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubr)

#1) Carga
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-14/Result2021-14-01.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-17/Result2021-17-01.csv', sep=',',dec=',' )
# amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random_G1/Result2021-02-02.csv', sep=',',dec=',' )
# amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random_G1/Result2021-02-02.csv', sep=',',dec=',' )
# amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random_G1/Result2021-03-02_5000.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random/Result2021-03-02_10000.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random/Result2021-03-02.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random/Result2021-03-02_1000.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random/Result2021-05-02_1000_90_100.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random/Result2021-05-02_1000_80atv_90_100.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random_G2/Result2021-08-02_1000.csv', sep=',',dec=',' )
amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random_G1/Result_Lote_1.csv', sep=',',dec=',' )
attach(amostra)

# amostra$algo <- factor(amostra$algo)
# amostra$vertices <- factor(amostra$vertices)
# amostra$percNeg <- factor(amostra$percNeg)
# amostra$disRate <- factor(amostra$disRate)
# amostra$layer <- factor(amostra$layer)
# amostra$fan <- factor(amostra$fan)
# amostra$cpMult <- factor(amostra$cpMult)

amostra$algo <- factor(amostra$algo)
amostra$vertices <- factor(amostra$vertices)
amostra$percNeg <- factor(as.integer(amostra$percNeg))
# amostra$disRate <- factor(amostra$disRate)
# amostra$layer <- factor(amostra$layer)
# amostra$fan <- factor(amostra$fan)
#amostra$percNeg <- as.integer(amostra$percNeg)

View(amostra)
glimpse(amostra)
lillie.test(log(amostra$all_cost[amostra$percNeg==70]))
hist(log(amostra$all_cost[amostra$percNeg==70]))
df$effort_perc <- amostra$all_cost[amostra$percNeg==1]
df$percNeg_perc <- factor(amostra$percNeg[amostra$percNeg==1])
df$algo_perc <- factor(amostra$algo[amostra$percNeg==1])


#amostra$effort <- 1/(sqrt(1/(1/(sqrt(1/sqrt(sqrt(1/sqrt(amostra$effort))))))))
#amostra$effort <- log(sqrt(amostra$effort))

# effort_RS <- amostra$effort[amostra$algo == 'RS']
# percNeg_RS <-amostra$percNeg[amostra$algo == 'RS']
# effort_SAA_HS <- amostra$effort[amostra$algo != 'RS']
# percNeg_SAA_HS <-amostra$percNeg[amostra$algo != 'RS']
#
# time_RS <- amostra$time[amostra$algo == 'RS']
# time_SAA_HS <- amostra$time[amostra$algo != 'RS']


#d <- data.frame(effort_RS, time_RS, percNeg_RS)
#d2 <- data.frame(effort_SAA_HS, time_SAA_HS, percNeg_SAA_HS)
#View(d2)


#2) Tranformação
#m <- art(effort ~ algo*percNeg, amostra)
# m <- art(effort ~ algo*vertices*percNeg*disRate*layer*fan, amostra)
# anova(m, response='aligned')
# print('summary m:')
# summary(m)
# anova(m)
# hist(m$residuals)
#shapiro.test(residuals(m))


# lambda <- BoxCox.lambda(amostra$effort, method=c("guerrero"), lower=-5, upper=5)
# lambda <- BoxCox.lambda(amostra$effort, method=c("loglik"), lower=-5, upper=5)
# first_values <- BoxCox(amostra$effort, lambda)

lambda <- BoxCox.lambda(df$effort_perc, method=c("guerrero"), lower=-5, upper=5)
lambda <- BoxCox.lambda(df$effort_perc, method=c("loglik"), lower=-5, upper=5)
first_values <- BoxCox(df$effort_perc, lambda)
#
#m1 <- art(effort ~ algo*vertices*percNeg*disRate*fan*layer*cpMult, amostra)
#m1 <- art(effort ~ algo*vertices*percNeg*disRate*fan*layer, amostra)
#m1 <- art(effort ~ vertices, amostra)
#m1 <- art(first_values ~ amostra$algo_perc * amostra$percNeg_perc)
m1 <- art(first_values ~ df$algo_perc * df$percNeg_perc)
anova(m1)

#lambda2 <- BoxCox.lambda(first_values, method=c("guerrero"), lower=-5, upper=5)
# lambda2 <- BoxCox.lambda(first_values, method=c("loglik"), lower=-5, upper=5)
# second_values <- BoxCox(first_values, lambda2)
#
# #lambda3 <- BoxCox.lambda(second_values, method=c("guerrero"), lower=-5, upper=5)
# lambda3 <- BoxCox.lambda(second_values, method=c("loglik"), lower=-5, upper=5)
# third_values <- BoxCox(second_values, lambda3)
# #as.matrix(third_values)
#
# #lambda4 <- BoxCox.lambda(third_values, method=c("guerrero"), lower=-5, upper=5)
# lambda4 <- BoxCox.lambda(third_values, method=c("loglik"), lower=-5, upper=5)
# forth_values <- BoxCox(third_values, lambda4)
#as.matrix(forth_values)

#lambda5 <- BoxCox.lambda(forth_values, method=c("guerrero"), lower=-5, upper=5)
# lambda5 <- BoxCox.lambda(forth_values, method=c("loglik"), lower=-5, upper=5)
# fifth_values <- BoxCox(forth_values, lambda5)
# amostra$fifth <- as.matrix(fifth_values)

#m5 <- art(fifth_values ~ algo*vertices*percNeg*disRate*fan*layer, amostra)
# m5 <- art(fifth_values ~ percNeg, amostra)
# anova(m5)

# lambda6 <- BoxCox.lambda(fifth_values, method=c("loglik"), lower=-5, upper=5)
# sixth_values <- BoxCox(fifth_values, lambda6)
# as.matrix(sixth_values)

par(mfrow = c(2,2))
par(mfrow = c(1,1))
#hist(m5$residuals)
#hist(amostra$effort)
#hist(log(amostra$effort))

# hist(second_values)
# hist(third_values)
# hist(forth_values)
#hist(first_values)
hist(m1$residuals)

#lambda <- BoxCox.lambda(effort_RS, method=c("guerrero"), lower=-5, upper=)
# lambda <- BoxCox.lambda(effort_RS, method=c("loglik"), lower=-1, upper=1)
# first_values <- BoxCox(effort_RS, lambda)
#
# m1 <- art(log(first_values) ~ percNeg_RS)
# anova(m1)

# lambda <- BoxCox.lambda(first_values, method=c("loglik"), lower=-5, upper=5)
# second_values <- BoxCox(first_values, lambda)
#
# m1 <- art(second_values ~ percNeg_RS)
# anova(m1)

# par(mfrow=c(1,2))
# hist(first_values)
# hist(m1$residuals)
# hist(log(effort_SAA_HS))

lillie.test(first_values)
# #jarque.test(first_values)
# ad.test(first_values)
lillie.test(m1$residuals)
ad.test(m1$residuals)

# lillie.test(amostra$effort)
# lillie.test(log(amostra$effort))
lillie.test(df$effort_perc)
lillie.test(log(df$effort_perc))


# print('Homocedasticidade Levene resíduos first_values:')
# leveneTest(m1$residuals ~ d$percNeg_RS, center=median)

#
# lillie.test(log(effort_SAA_HS))
#jarque.test(m1$residuals)
# ad.test(log(effort_SAA_HS))

# par(mfrow=c(1,2))
# plot(d$time_RS, d$effort_RS,
#      xlab='Cost', ylab='Time', main='RS', cex.main=2, cex.lab=1.3, cex.axis=2)
# abline(lm(d$time_RS ~ d$effort_RS), data=d, col="black")
#
# plot(d2$time_SAA_HS, d2$effort_SAA_HS,
#      xlab='Cost', ylab='Time', main='SAA e HS', cex.main=2, cex.lab=1.3, cex.axis=2)
# abline(lm(d2$time_SAA_HS ~ d2$effort_SAA_HS), data=d, col="black")


#3) Aplicação ANOVA

#4) Teste de pressupostos
# jarque.test(as.matrix(first_values))
# print('Normalidade Jarque m1:')
# jarque.test(m1$residuals)
# jarque.teste(as.matrix(second_values))
# jarque.teste(as.matrix(third_values))

# print('Normalidade AD first_values:')
# ad.test(first_values)
#print('Normalidade AD resíduos m1:')
#ad.test(m5$residuals)
# print('Normalidade AD dados brutos:')
# ad.test(amostra$effort)
#
# ad.test(first_values)
# ad.test(second_values)
# ad.test(third_values)
# ad.test(forth_values)
# #ad.test(fifth_values)
# ad.test(rank(m1$residuals))
#
# print('Normalidade KS first_values:')
# lillie.test(first_values)
# lillie.test(second_values)
# lillie.test(third_values)
# lillie.test(forth_values)
# #lillie.test(fifth_values)
# lillie.test(rank(m1$residuals))
#
# # print('Normalidade AD first_values com rank:')
# # ad.test(rank(first_values))
# # ad.test(rank(second_values))
# # print('Normalidade AD m1 com rank:')
# # ad.test(rank(m1$residuals))
# # ad.test(rank(third_values))
# # ad.test(rank(forth_values))
# # ad.test(rank(fifth_values))
#
# # print('Normalidade KS first_values:')
# # lillie.test(first_values)
#
# # print('Normalidade KS m1:')
# # lillie.test(log(m5$residuals))
#
# print('Homocedasticidade Levene resíduos first_values:')
# leveneTest(first_values ~ amostra$algo*amostra$percNeg, center=median)
print('Homocedasticidade Levene resíduos first_values:')
#leveneTest(m1$residuals ~ amostra$algo * amostra$percNeg, center=median)
leveneTest(m1$residuals ~ df$algo_perc * df$percNeg_perc, center=median)
#
# print('Homocedasticidade Bartlett residuos first_values:')
# bartlett.test(first_values~amostra$algo)
# #
# print('Homocedasticidade Levene Resíduo m1:')
# #leveneTest(m1$residuals ~ amostra$algo*amostra$vertices*amostra$percNeg, center=median)
# #leveneTest(m1$residuals ~ amostra$vertices, center=median)
# leveneTest(m1$residuals ~ amostra$algo, center=median)
#
# print('Homocedasticidade Bartlett Resíduo m1:')
# #bartlett.test(m1$residuals~amostra$algo*amostra$vertices*amostra$percNeg)
# bartlett.test(m1$residuals~amostra$algo)

#5) Post-hoc
