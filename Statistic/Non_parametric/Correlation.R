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
# amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-27/Result2021-27-01_50.csv', sep=',',dec=',' )
# amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-27/Result2021-27-01_G130.csv', sep=',',dec=',' )
# amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-27/Result2021-27-01_G230.csv', sep=',',dec=',' )
# amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random/Result2021-02-02.csv', sep=',',dec=',' )
amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random_G1/Result2021-03-02_5000.csv', sep=',',dec=',' )
# amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random/Result2021-03-02_10000.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random/Result2021-03-02.csv', sep=',',dec=',' )
# amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-28/Result2021-28-01.csv', sep=',',dec=',' )
# amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-28/Result2021-28-01_pequena.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random_G2/Result2021-06-02_1000.csv', sep=',',dec=',' )

# amostra$algo <- factor(amostra$algo)
# amostra$batch <- factor(amostra$batch)
# amostra$vertices <- factor(amostra$vertices)
# amostra$percNeg <- factor(amostra$percNeg)
# amostra$disRate <- factor(amostra$disRate)
# amostra$layer <- factor(amostra$layer)
# amostra$fan <- factor(amostra$fan)

amostra$cost <- amostra$effort
amostra$time <- as.double(amostra$time)# * 1000
glimpse(amostra)
View(amostra)
par(mfrow=c(2,2))
#plot(amostra$vertices, amostra$effort)
fit <- lm(amostra$effort ~ amostra$vertices)
plot(fit)

# 2) Apply Kruskal-Wallis
#mod <- kruskal.test(amostra$effort ~ cpMult, data=amostra)

# 3) Correlation coeffient

#corr_global <- cor.test(amostra$cost, amostra$time, method='pearson')

corr_global <- cor.test(amostra$cost, amostra$time, method='spearman')

# Todos os lotes
################
# corr_RS <- cor.test(amostra$cost[amostra$algo=='RS'], amostra$time[amostra$algo=='RS'], method='spearman')
# corr_SAA <- cor.test(amostra$cost[amostra$algo=='SAA'], amostra$time[amostra$algo=='SAA'], method='spearman')
# corr_HS <- cor.test(amostra$cost[amostra$algo=='HS'], amostra$time[amostra$algo=='HS'], method='spearman')

# Tira os lotes com percNeg de interesse
########################################
PER<-'X'
PER2<-'X'
PER3<-'X'
# corr_RS <- cor.test(amostra$cost[amostra$algo=='RS' & amostra$percNeg!=PER & amostra$percNeg!=PER2 & amostra$percNeg!=PER3],
#                     amostra$time[amostra$algo=='RS' & amostra$percNeg!=PER & amostra$percNeg!=PER2 & amostra$percNeg!=PER3], method='spearman')
# corr_SAA <- cor.test(amostra$cost[amostra$algo=='SAA' & amostra$percNeg!=PER & amostra$percNeg!=PER2 & amostra$percNeg!=PER3],
#                      amostra$time[amostra$algo=='SAA' & amostra$percNeg!=PER & amostra$percNeg!=PER2 & amostra$percNeg!=PER3], method='spearman')
# corr_HS <- cor.test(amostra$cost[amostra$algo=='HS' & amostra$percNeg!=PER & amostra$percNeg!=PER2 & amostra$percNeg!=PER3],
#                     amostra$time[amostra$algo=='HS' & amostra$percNeg!=PER & amostra$percNeg!=PER2 & amostra$percNeg!=PER3], method='spearman')

corr_RS <- cor.test(amostra$cost[amostra$algo=='RS' & amostra$percNeg!=PER],
                    amostra$time[amostra$algo=='RS' & amostra$percNeg!=PER], method='spearman')
corr_SAA <- cor.test(amostra$cost[amostra$algo=='SAA' & amostra$percNeg!=PER],
                     amostra$time[amostra$algo=='SAA' & amostra$percNeg!=PER], method='spearman')
corr_HS <- cor.test(amostra$cost[amostra$algo=='HS' & amostra$percNeg!=PER],
                    amostra$time[amostra$algo=='HS' & amostra$percNeg!=PER], method='spearman')

# # Filtro por percNeg
# ####################
# PER='100'
# #corr_RS <- cor.test(amostra$cost[amostra$algo=='RS'], amostra$time[amostra$algo=='RS'], method='spearman')
# corr_SAA <- cor.test(amostra$cost[amostra$algo=='SAA' & amostra$percNeg==PER], amostra$time[amostra$algo=='SAA' & amostra$percNeg==PER], method='spearman')
# corr_HS <- cor.test(amostra$cost[amostra$algo=='HS' & amostra$percNeg==PER], amostra$time[amostra$algo=='HS' & amostra$percNeg==PER], method='spearman')

# # Um lote
# #########
# lote <- "['320', '5', '2', '10', '100', '2']"
# # corr_RS <- cor.test(amostra$cost[amostra$algo=='RS' & amostra$batch==lote],
# #                     amostra$time[amostra$algo=='RS' & amostra$batch==lote], method='spearman')
# corr_SAA <- cor.test(amostra$cost[amostra$algo=='SAA' & amostra$batch==lote],
#                      amostra$time[amostra$algo=='SAA' & amostra$batch==lote], method='spearman')
# corr_HS <- cor.test(amostra$cost[amostra$algo=='HS' & amostra$batch==lote],
#                     amostra$time[amostra$algo=='HS' & amostra$batch==lote], method='spearman')

#corr <- cor.test(amostra$cust, amostra$time, method='kendall')

# Correlação global
# ##################################
#corr_s_global <- c("RS x SAA x HS - Spearman's Coeff.:", as.character(corr_global$estimate))
corr_s_global <- c("SAA x HS - Spearman's Coeff.:", as.character(corr_global$estimate))
#plot(amostra$cost, amostra$time, xlab='Cost', ylab='Time', main=corr_s_global, cex.main=1.3, cex.lab=1.3, cex.axis=2)
#plot(amostra$cost[amostra$cost>=50000 & amostra$cost <=120000], amostra$time[amostra$cost>=50000 & amostra$cost <=120000], xlab='Cost', ylab='Time', main=corr_s_global)
abline(lm(amostra$time ~ amostra$cost, data=amostra), col="black")

# Correlação separada por algoritmo
###################################
#par(mfrow=c(1,3))
#par(mfrow=c(1,2))
corr_s_RS <- c("RS - Spearman:\n", as.character(corr_RS$estimate))
corr_s_SAA <- c("SAA - Spearman:\n", as.character(corr_SAA$estimate))
corr_s_HS <- c("HS - Spearman:\n", as.character(corr_HS$estimate))

# # Todos os lotes
# ################
# plot(amostra$cost[amostra$algo=='RS' & amostra$percNeg!=PER & amostra$percNeg!=PER2 & amostra$percNeg!=PER3],
#      amostra$time[amostra$algo=='RS' & amostra$percNeg!=PER & amostra$percNeg!=PER2 & amostra$percNeg!=PER3],
#      xlab='Cost', ylab='Time', main=corr_s_RS, cex.main=2, cex.lab=1.3, cex.axis=2)
# abline(lm(amostra$time ~ amostra$cost, data=amostra), col="black")
# # #
# plot(amostra$cost[amostra$algo=='SAA' & amostra$percNeg!=PER & amostra$percNeg!=PER2 & amostra$percNeg!=PER3],
#      amostra$time[amostra$algo=='SAA' & amostra$percNeg!=PER & amostra$percNeg!=PER2 & amostra$percNeg!=PER3],
#      xlab='Cost', ylab='Time', main=corr_s_SAA, cex.main=2, cex.lab=1.3, cex.axis=2)
# abline(lm(amostra$time ~ amostra$cost, data=amostra), col="black")
#
# plot(amostra$cost[amostra$algo=='HS' & amostra$percNeg!=PER & amostra$percNeg!=PER2 & amostra$percNeg!=PER3],
#      amostra$time[amostra$algo=='HS' & amostra$percNeg!=PER & amostra$percNeg!=PER2 & amostra$percNeg!=PER3],
#      xlab='Cost', ylab='Time', main=corr_s_HS, cex.main=2, cex.lab=1.3, cex.axis=2)
# abline(lm(amostra$time ~ amostra$cost, data=amostra), col="black")

# Separação por percNeg
#######################
# plot(amostra$cost[amostra$algo=='SAA' & amostra$percNeg==PER], amostra$time[amostra$algo=='SAA' & amostra$percNeg==PER],
#      xlab='Cost', ylab='Time', main=corr_s_SAA, cex.main=2, cex.lab=1.3, cex.axis=2)
# abline(lm(amostra$time ~ amostra$cost, data=amostra), col="black")
# #abline(lm(amostra$time[amostra$algo=='HS'] ~ amostra$cost[amostra$algo=='HS'], data=amostra), col="black")
#
# plot(amostra$cost[amostra$algo=='HS' & amostra$percNeg==PER], amostra$time[amostra$algo=='HS' & amostra$percNeg==PER],
#      xlab='Cost', ylab='Time', main=corr_s_HS, cex.main=2, cex.lab=1.3, cex.axis=2)
# abline(lm(amostra$time ~ amostra$cost, data=amostra), col="black")

# # Um lote todos os algoritmos (SAA, HS)
# #######################################
# par(mfrow=c(1,3))
# lotes <- c("['40', '10', '3', '10', '0', '1']",
#            "['40', '10', '3', '10', '50', '1']",
#            "['40', '10', '3', '10', '100', '1']")
#
# # lotes <- c("['320', '10', '3', '10', '0', '1']",
# #            "['320', '10', '3', '10', '50', '1']",
# #            "['320', '10', '3', '10', '100', '1']")
# for (i in lotes){
#   corr_corrente <- cor.test(amostra$cost[amostra$batch==i],
#                      amostra$time[amostra$batch==i], method='spearman')
#   titulo <- c(i,'\n', as.character(corr_corrente$estimate))
#   plot(amostra$cost[amostra$batch==i], amostra$time[amostra$batch==i],
#        xlab='Cost', ylab='Time', main=titulo, cex.main=2, cex.lab=1.3, cex.axis=2)
#   abline(lm(amostra$time ~ amostra$cost, data=amostra), col="black")
# }


# # Um lote separando por algoritmo
# #################################
# plot(amostra$cost[amostra$algo=='RS' & amostra$batch==lote],
#      amostra$time[amostra$algo=='RS' & amostra$batch==lote], xlab='Cost', ylab='Time', main=corr_s_RS, cex.main=2, cex.lab=1.3, cex.axis=2)
# abline(lm(amostra$time ~ amostra$cost, data=amostra), col="black")
#
# plot(amostra$cost[amostra$algo=='SAA' & amostra$batch==lote],
#      amostra$time[amostra$algo=='SAA' & amostra$batch==lote], xlab='Cost', ylab='Time', main=corr_s_SAA, cex.main=2, cex.lab=1.3, cex.axis=2)
# abline(lm(amostra$time ~ amostra$cost, data=amostra), col="black")
# #abline(lm(amostra$time[amostra$algo=='HS'] ~ amostra$cost[amostra$algo=='HS'], data=amostra), col="black")
#
# plot(amostra$cost[amostra$algo=='HS' & amostra$batch==lote],
#      amostra$time[amostra$algo=='HS' & amostra$batch==lote], xlab='Cost', ylab='Time', main=corr_s_HS, cex.main=2, cex.lab=1.3, cex.axis=2)
# abline(lm(amostra$time ~ amostra$cost, data=amostra), col="black")

#mtx <- cor(data.frame(amostra$cust, amostra$time))
#mtx <- cor(c(amostra$cust, amostra$time, amostra$percNeg, amostra$vertices, amostra$g_edges))
#View(mtx)

# print('Normalidade shapiro:')
# shapiro_test(amostra$cost)
#
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
bartlett.test(amostra$cost~amostra$algo)
