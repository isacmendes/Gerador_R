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
if (!require(MASS)) install.packages("MASS")
library(MASS)
if (!require(nortest)) install.packages("nortest")
library(nortest)

if (!require(fpp)) install.packages("fpp")
library(fpp)


options(scipen = 999)

# 1) Data load and factor definition
#amostra<-read.csv('c:/Users/Dell/PycharmProjects/Gerador_R/Versions/Anova/v_2_01012021/amostras2412.csv', sep=';',dec=',' )
#amostra<-read.csv('c:/Users/Dell/PycharmProjects/Gerador_R/Versions/Anova/v_3_05012021/Result2020-12-30_ANOVA.csv', sep=',',dec=',' )
#amostra<-read.csv('c:/Users/Dell/PycharmProjects/Gerador_R/Versions/Anova/v_3_05012021/Result2020-12-30_5000_ANOVA.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-05/Result2021-05-01.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-06/Result2021-06-01.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-06/Cópia de Result2021-06-01.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-13/Result2021-13-01.csv', sep=',',dec=',' )
amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-14/Result2021-14-01.csv', sep=',',dec=',' )
attach(amostra)
#View(amostra)
#summary(amostra)
# Check datatypes
glimpse(amostra)

amostra$algo <- factor(amostra$algo)
amostra$vertices <- factor(amostra$vertices)
amostra$percNeg <- factor(amostra$percNeg)
amostra$disRate <- factor(amostra$disRate)
amostra$layer <- factor(amostra$layer)
amostra$fan <- factor(amostra$fan)

amostra$effort_adj <- sqrt(amostra$effort)^-5

#2) Check assumptions
# dados %>% group_by(dados$algo, dados$vertices, dados$percNeg) %>%
#   shapiro_test(effort)

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

# print("Normality test:")
# lillie.test(amostra$effort_adj)

#print("Homocedastificade:")


#2.1) Teste de Homogeneidade
#leveneTest(amostra$effort ~ amostra$algo, amostra$vertices, amostra$percNeg, center=mean)
#leveneTest(amostra$effort ~ amostra$vertices * amostra$percNeg, amostra, center=mean)

# 3) Apply ANOVA
#amostra$effort <- log(amostra$effort)
options(contrasts=c("contr.sum", "contr.poly"))
#fitAov<-aov(effort ~ algo + vertices + percNeg + cpMult + disRate, amostra)
#fitAov<-aov(effort ~ algo*#vertices*percNeg*cpMult*disRate*fan*layer*g_edges, amostra)
#fitAov<-aov(effort ~ algo*vertices*percNeg*cpMult*disRate*g_edges, amostra)
fitAov<-aov(rank(effort_adj) ~ algo*vertices*percNeg*cpMult*disRate*fan*layer, amostra)
# fitAov_ajusted <- Anova(fitAov, type='III')
#fitAov<-aov(effort ~ algo * vertices, amostra)
#fitAov<-aov(effort~algo*vertices, amostra)
#fitAov<-aov(effort ~ algo:vertices:percNeg, amostra)
print("Primeira Anova:")
summary(fitAov)
print("Normalidade:")
lillie.test(rank(fitAov$residuals))

print('Homgeneirdade:')
leveneTest(rank(fitAov$residuals) ~ algo*vertices*percNeg*disRate*fan*layer, amostra, center=mean)


#fitAov
# print("Segunda Anova")
# summary(fitAov_ajusted)
# fitAov_ajusted

print("Shapiro com transformação sqrt e log:")
#shapiro.test(sqrt(fitAov_ajusted$residuals))
#shapiro.test(log(fitAov_ajusted$residuals))
#shapiro.test(fitAov$residuals)

#shapiro.test(sqrt(fitAov$residuals))
#shapiro.test(log(fitAov$residuals))

print("Levene com tranformação sqrt e log:")
#leveneTest(sqrt(amostra$effort) ~ amostra$algo, amostra$vertices, amostra$percNeg, center=mean)
#leveneTest(log(amostra$effort) ~ amostra$algo, amostra$vertices, amostra$percNeg, center=mean)

#ks.test(fitAov$residuals,"rnorm", mean(fitAov$residuals), sd(fitAov$residuals))

# shapiro.test(amostra$effort)
# ks.test(amostra$effort,"rnorm", mean(amostra$effort), sd(amostra$effort))

# d <- rnorm(100)
# shapiro.test(d)
# ks.test(d,"pnorm", mean(d), sd(d))

#

#boxplot(fitAov$residuals)
#identify_outliers(fitAov$residuals)

# X) Apply Box-Cox transformation


###########
#leveneTest(fitAov$residuals ~ amostra$vertices * amostra$percNeg, amostra, center=mean)

#hist(amostra$effort)
#hist(pnorm(10,mean=0,sd=1))
#hist(d)

print("Teste Tukey:")
#TukeyHSD(fitAov)



#boxplot(fitAov$residuals)

# dados %>% group_by(algo, vertices) %>%
#   identify_outliers(dados$effort)

print("4) Post-hoc:")
# tukey.fitAov <- PostHocTest(fitAov, method="hsd")
# tukey.fitAov
#bonferroni.fitAov <- PostHocTest(fitAov, method="bonf")
#bonferroni.fitAov
#tukey.fitAov

# 5) plots
#interaction.plot(vertices,algo,effort,main='Algo x Vertices', xlab='Vertices',ylab='Mean effort',type = 'l')
#hist(fitAov$residuals)
#hist(amostra$effort)

# 5.1) Interactions plots
# # disRate x vertices
# ggplot(amostra, aes(x=disRate, y=effort, group=vertices, color=vertices)) +
#   geom_line(stat="summary", fun.data="mean_se", size=0.6) +
#   geom_point(stat="summary", fun.y="mean") +
#   geom_errorbar(stat="summary", fun.data="mean_se", width=0.2)

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


# X) Box-Cox
#result_boxcox <- boxcox(amostra$effort ~ vertices*percNeg*fan*layer*g_edges, data=amostra, lambda=seq(-1,1, 1/10))
# result_boxcox <- boxcox(amostra$effort ~ vertices*percNeg, data=amostra, lambda=seq(-1,1, 1/10))
# print('lambda is: ')
#
# amostra$effort_tranformed <- (amostra$effort^(-0.2) - 1/0.1)

# result_boxcox2 <- boxcox((amostra$effort_tranformed)*-1 ~ vertices*percNeg*fan*layer*g_edges, data=amostra, lambda=seq(-1,1, 1/10))
# amostra$effort_tranformed2 <- (amostra$effort_tranformed^(-0.2) - 1/0.1)


#d <- sqrt(sqrt(sqrt(sqrt(sqrt(sqrt(log(log(log(amostra$effort)))))))))
###########
lambda <- BoxCox.lambda(amostra$effort_adj, method=c("loglik"), lower=-5, upper=5)

#lambda <- BoxCox.lambda(d, method=c("loglik"), lower=-3, upper=3)
#print('lambda é:')
#lambda

###########
new_residuals <- BoxCox(fitAov$residuals, lambda)
fitAov_new <- aov(new_residuals~algo*vertices*percNeg*cpMult*disRate*fan*layer*g_edges)
qqp(rstandard(fitAov_new),"norm")

###########
# print("Normalidade transformada:")
# lillie.test(fitAov_new$residuals)
#hist(rank(amostra$effort))

# ggplot(amostra, aes(x=fitAov_new$residuals)) +
#   geom_histogram(aes(y=..density..), bins=50, colour="black", fill="white") +
#   geom_density(alpha=.6, fill="#FF6666")
# View(amostra)

###########
#print("Homocedasticidade:")
#leveneTest(amostra$effort_tranformed ~ vertices*percNeg*g_edges)
#levene_test(fitAov$residuals)
#leveneTest(fitAov_new$residuals ~ amostra$vertices * amostra$percNeg, amostra, center=median)
#leveneTest(fitAov_new$residuals ~ algo*vertices*percNeg*disRate*fan*layer, amostra, center=mean)

# 6) Results description