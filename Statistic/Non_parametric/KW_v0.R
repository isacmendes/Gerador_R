# Title     : TODO
# Objective : TODO
# Created by: Isac
# Created on: 14/01/2021

options(scipen=999)

if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
if (!require(rstatix)) install.packages("rstatix")
library(rstatix)
if (!require (dgof)) install.packages("dgof")
library(dgof)
if (!require (car)) install.packages("car")
library(car)
if (!require (emmeans)) install.packages("emmeans")
library(emmeans)
if (!require (DescTools)) install.packages("DescTools")
library(DescTools)
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if (!require(MASS)) install.packages("MASS")
library(MASS)
if (!require(nortest)) install.packages("nortest")
library(nortest)

if (!require(fpp)) install.packages("fpp")
library(fpp)

# 1) Data load and factor definition
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-14/Result2021-14-01.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-14/Result2021-14-01_GRANDES - Página1.csv', sep=',',dec=',' )
#amostra<-read.csv("G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-14/Result2021-14-01_GRANDES_v2 - Plan 1.csv", sep=',',dec=',' )
#amostra<-read.csv("G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-17/Result2021-17-01.csv", sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-21/Result2021-21-01.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-21/Result2021-21-01_Last_batch_ss.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-21/Result2021-21-01_Last_batch_rec_itShift.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-21/Result2021-21-01_Last_batch_rec_function_calls.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-21/Result2021-21-01_Last_batch.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-21/Result2021-25-01.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-26/Result2021-26-01.csv', sep=',',dec=',' )
amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-27/Result2021-27-01_50.csv', sep=',',dec=',' )
amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-27/Result2021-27-01_G130.csv', sep=',',dec=',' )
amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-27/Result2021-27-01_G230.csv', sep=',',dec=',' )
amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random/Result2021-02-02.csv', sep=',',dec=',' )
amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random/Result2021-03-02_5000.csv', sep=',',dec=',' )
amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random/Result2021-03-02_10000.csv', sep=',',dec=',' )
amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random/Result2021-05-02_1000_90_100.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random/Result2021-03-02.csv', sep=',',dec=',' )
# amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-28/Result2021-28-01.csv', sep=',',dec=',' )
# amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-28/Result2021-28-01_pequena.csv', sep=',',dec=',' )


amostra$algo <- factor(amostra$algo)
amostra$batch <- factor(amostra$batch)
amostra$vertices <- factor(amostra$vertices)
amostra$percNeg <- factor(amostra$percNeg)
amostra$disRate <- factor(amostra$disRate)
amostra$layer <- factor(amostra$layer)
amostra$fan <- factor(amostra$fan)

amostra$cost <- amostra$effort
amostra$time <- as.double(amostra$time)# * 1000
glimpse(amostra)
View(amostra)

# 2) Apply Kruskal-Wallis
mod <- kruskal.test(effort ~ vertices, data=amostra)

mod

print('# 3) Post-hoc')
dunn_test(effort ~ vertices, data=amostra, p.adjust.method = "bonferroni")

print('Normalidade KS:')
lillie.test(amostra$cost)
#
print('Normalidade AD:')
lillie.test(amostra$cost)
#
print('Homocedasticidade Levene:')
#leveneTest(amostra$cost ~ amostra$algo*amostra$percNeg)
leveneTest(amostra$cost, amostra$percNeg)

print('Homocedasticidade Bartlett:')
bartlett.test(amostra$cost~amostra$percNeg)



