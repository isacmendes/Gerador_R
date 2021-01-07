# Title     : ANOVA v3
# Objective : ANOVA for sparse graphs in 05.01.2021
# Created by: TODO
# Created on: 05/01/2021

# 0) Basic libraries

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

# 1) Data load
#amostra<-read.csv('c:/Users/Dell/PycharmProjects/Gerador_R/Versions/Anova/v_2_01012021/amostras2412.csv', sep=';',dec=',' )
amostra<-read.csv('c:/Users/Dell/PycharmProjects/Gerador_R/Versions/Anova/v_3_05012021/Result2020-12-30_ANOVA.csv', sep=',',dec=',' )
#amostra<-read.csv('c:/Users/Dell/PycharmProjects/Gerador_R/Versions/Anova/v_3_05012021/Result2020-12-30_5000_ANOVA.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-05/Result2021-05-01.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-06/Result2021-06-01.csv', sep=',',dec=',' )
attach(amostra)
View(amostra)
#summary(amostra)
#table(algo,vertices,fan,layer,disRate,percNeg,cpMult)
table(algo,vertices,disRate,percNeg,cpMult)
glimpse(amostra)

# dados <- amostra
# dados$algo <- factor(dados$algo)
# dados$vertices <- factor(dados$vertices)
# dados$percNeg <- factor(dados$percNeg)

# amostra$algo <- factor(amostra$algo)
# amostra$vertices <- factor(amostra$vertices)
# amostra$percNeg <- factor(amostra$percNeg)

#glimpse(dados)



#table(dados$algo,dados$vertices,dados$percNeg)
#table(batch ,algo, vertices)

# suEf<-aggregate(effort, by=list(algo),FUN=summary)
# cn<-c('Algo','Effort')
# colnames(suEf)<-cn
# suEf
#
# agEf<-aggregate(effort, by=list(algo),FUN=mean)
# cn<-c('Algo','Mean effort')
# colnames(agEf)<-cn
# agEf
#
# agEf<-aggregate(effort, by=list(algo),FUN=sd)
# cn<-c('Algo','Sd effort')
# colnames(agEf)<-cn
# agEf


#2) Check assumptions
# dados %>% group_by(dados$algo, dados$vertices, dados$percNeg) %>%
#   shapiro_test(effort)

options(scipen = 999)
# amostra %>% group_by(algo, vertices) %>%
#   shapiro_test(effort)
#
# amostra %>% group_by(algo, percNeg) %>%
#   shapiro_test(effort)
#
#boxplot(amostra$effort ~ amostra$vertices:amostra$percNeg:amostra$algo)
#boxplot(amostra$effort ~ amostra$percNeg:amostra$algo:amostra$vertices)
#boxplot(dados$effort ~ dados$percNeg:dados$algo:dados$vertices)
#boxplot(effort ~ percNeg)

# 2.1) Teste de Homogeneidade
#leveneTest(amostra$effort ~ amostra$algo, amostra$vertices, amostra$percNeg, center=mean)
#leveneTest(amostra$effort ~ amostra$vertices * amostra$percNeg, amostra, center=mean)

# 3) Apply ANOVA
options(contrasts=c("contr.sum", "contr.poly"))
fitAov<-aov(effort ~ algo*vertices*percNeg*cpMult*disRate, amostra)
Anova(fitAov, type='III')
#fitAov<-aov(effort ~ algo * vertices, amostra)
#fitAov<-aov(effort~algo*vertices, amostra)
#fitAov<-aov(effort ~ algo:vertices:percNeg, amostra)
summary(fitAov)
#summary(anova_ajusted)
# ks.test(fitAov$residuals,"rnorm", mean(fitAov$residuals), sd(fitAov$residuals))

# shapiro.test(amostra$effort)
# ks.test(amostra$effort,"rnorm", mean(amostra$effort), sd(amostra$effort))

# d <- rnorm(100)
# shapiro.test(d)
# ks.test(d,"pnorm", mean(d), sd(d))

#

#boxplot(fitAov$residuals)
#identify_outliers(fitAov$residuals)

#leveneTest(fitAov$residuals ~ amostra$vertices * amostra$percNeg, amostra, center=mean)
#hist(amostra$effort)
#hist(pnorm(10,mean=0,sd=1))
#hist(d)



#boxplot(fitAov$residuals)

# dados %>% group_by(algo, vertices) %>%
#   identify_outliers(dados$effort)

# 4) Post-hoc
#PostHocTest(fitAov, method="hsd")
#PostHocTest(fitAov, method="bonf")

# 5) Interaction plots
#interaction.plot(vertices,algo,effort,main='Algo x Vertices', xlab='Vertices',ylab='Mean effort',type = 'l')
#hist(fitAov$residuals)

# # disRate x vertices
ggplot(amostra, aes(x=disRate, y=effort, group=vertices, color=vertices)) +
  geom_line(stat="summary", fun.data="mean_se", size=0.6) +
  geom_point(stat="summary", fun.y="mean") +
  geom_errorbar(stat="summary", fun.data="mean_se", width=0.2)

# # algo x vertices
# ggplot(amostra, aes(x=algo, y=effort, group=vertices, color=vertices)) +
#   geom_line(stat="summary", fun.data="mean_se", size=0.6) +
#   geom_point(stat="summary", fun.y="mean") +
#   geom_errorbar(stat="summary", fun.data="mean_se", width=0.2)

# # algo x percNeg
# ggplot(amostra, aes(x=algo, y=effort, group=percNeg, color=percNeg)) +
#   geom_line(stat="summary", fun.data="mean_se", size=0.6) +
#   geom_point(stat="summary", fun.y="mean") +
#   geom_errorbar(stat="summary", fun.data="mean_se", width=0.2)
#
# # vertices x percNeg
# ggplot(amostra, aes(x=vertices, y=effort, group=percNeg, color=percNeg)) +
#   geom_line(stat="summary", fun.data="mean_se", size=0.6) +
#   geom_point(stat="summary", fun.y="mean") +
#   geom_errorbar(stat="summary", fun.data="mean_se", width=0.2)


# 6) Results description

