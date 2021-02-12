#processando amostra
options(digits = 4)
options(scipen = 999)
#dica para anova: https://www.scribbr.com/statistics/anova-in-r/

#library(ggplot2)
#library(ggpubr)
#library(tidyverse)
#library(broom)
#library(AICcmodavg)

###
#1-carga
###
#amostra<-read.csv('result20201230.csv',sep=';',dec=',' )
amostra<-read.csv('amostras2412.csv', sep=';', dec=',' )
#amostra<-read.csv('amostras2412.csv',sep=';',dec=',' )
attach(amostra)
#options(scipen=999)
###
#2-verificando os dados
###
summary(amostra)
table(algo,vertices,fan,layer,disRate,percNeg,cpMult)
print("###############")


###
#3-análise exloratória do esforço por algoritmo
###
suEf<-aggregate(effort, by=list(algo),FUN=summary)
cn<-c('Algo','Effort')
colnames(suEf)<-cn
suEf

agEf<-aggregate(effort, by=list(algo),FUN=mean)
cn<-c('Algo','Mean effort')
colnames(agEf)<-cn
agEf

agEf<-aggregate(effort, by=list(algo),FUN=sd)
cn<-c('Algo','Sd effort')
colnames(agEf)<-cn
agEf
#
# par(par=rep(2,4))
#plot(algo,effort,xlab='Algoritmo',ylab='Esforço')
#
# #################
# #graphics.off()
# #par("mar")
# #par(mar=c(1,1,1,1))
# #################
# #plot(algo,effort,xlab='algo',ylab='effort')

fitaov<-aov(effort~algo)
summary(fitaov)
shapiro.test(residuals(fitaov))
shapiro.test(fitaov$residuals)
boxplot(fitaov$residuals)

#Comentário: algoritmos requerem esforços diferentes - as médias são significativas
#
###
#3-Analisando dois fatores: algo e vertices
###
print("3-Analisando dois fatores: algo e vertices")
agEf<-aggregate(effort, by=list(algo,vertices),FUN=mean)
cn<-c('Algo','Vertices','Mean effort')
colnames(agEf)<-cn
agEf
#
fitAov<-aov(effort~algo*vertices)
summary(fitAov)
shapiro.test(residuals(fitAov))
shapiro.test(fitAov$residuals)
#boxplot(fitAov$residuals)
# interaction.plot(vertices,algo,effort,main='Algo x Vertices',
#                  xlab='Vertices',ylab='Mean effort',type = 'l')
#Comentário: (1)tanto algoritmo como vertices influenciam o esforço e (2)algoritmos
#tem comportamente diferente dependendo do número de vertices
#Isoladamente 'algo' explica 17%; Isoladamente 'vertices' explica 20%; e a interação entre eles 15%.
#
###
#4-Analisando tres fatores: algo, vertices,cpMult
###
print("4-Analisando tres fatores: algo, vertices,cpMult")
agEf<-aggregate(effort, by=list(algo,vertices,cpMult),FUN=mean)
cn<-c('Algo','Vertices','cpMult','Mean effort')
colnames(agEf)<-cn
agEf

fitAov<-aov(effort~algo+vertices+cpMult)
summary(fitAov)
# #Comentário: cpMult não influencia o esforço!!!
#
# ###
# #5-Analisando tres fatores: algo, vertices,percNeg
# ###
# agEf<-aggregate(effort, by=list(algo,vertices,percNeg),FUN=mean)
# cn<-c('Algo','Vertices','percNeg','Mean effort')
# colnames(agEf)<-cn
# agEf
#
# fitAov<-aov(effort~algo+vertices+percNeg)
# summary(fitAov)
# #Comentário: percNeg influencia o esforço!!!
#
# fitAov<-aov(effort~algo*vertices*percNeg)
# summary(fitAov)
# #Comentário: algo, vertices e percNeg influenciam e a média de cada combinação é significativa

