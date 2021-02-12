# Title     : TODO
# Objective : Compares pairs samples (batchs xx)
# Created by: Isac
# Created on: 19/01/2021

#1) Library and data source
options(scipen=999)
#
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
if (!require(rstatix)) install.packages("rstatix")
library(rstatix)
if (!require(stats)) install.packages("stats")
library(stats)
if (!require(PMCMRplus)) install.packages("PMCMRplus")
library(PMCMRplus)
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
# if (!require(nortest)) install.packages("nortest")
# library(nortest)
# if (!require(fpp)) install.packages("fpp")
# library(fpp)
# if (!require(ARTool)) install.packages("ARTool")
# library(ARTool)
# if (!require(car)) install.packages("car")
# library(car)
# if (!require(moments)) install.packages("moments")
# library(moments)

#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-14/Result2021-14-01.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-17/Result2021-17-01.csv', sep=',',dec=',' )
amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2021-01-21/Result2021-21-01_Last_batch.csv', sep=',',dec=',' )

amostra$algo <- factor(amostra$algo)
amostra$vertices <- factor(amostra$vertices)
amostra$percNeg <- factor(amostra$percNeg)
amostra$disRate <- factor(amostra$disRate)
amostra$layer <- factor(amostra$layer)
amostra$fan <- factor(amostra$fan)

id <- c()
count_e <- 1
len <- (length(amostra$effort))/3
for (i in 1:len){
  for (count_i in 1:3) {
    id <- c(id, as.integer(count_e))
  }
  count_e <- count_e + 1
}

algo <- amostra$algo
effort <- amostra$effort
time <- amostra$time
amostra2 <- tibble(id, algo, effort, time)

amostra2$id <- factor(amostra2$id)

View(amostra2)
glimpse(amostra2)

#2) Aplly the Wilcoxon test
# amostra2 %>% group_by(algo) %>%
#   get_summary_stats(amostra2$effort, type="median_iqr")

friedman.test(effort ~ algo | id, data=amostra2)

#3) Post-hoc
frdAllPairsSiegelTest(amostra2$effort, amostra2$algo,
                     amostra2$id, p.adjust.method = "bonferroni")

print('#4) Descriptve analysis')
amostra2 %>% group_by(algo) %>%
  get_summary_stats(effort, type="median_iqr")

# #4) Plots to the explanied
# par(mfrow=c(1,3))
# boxplot(amostra2$effort[amostra2$algo=='RS'], main='RS', cex.main=3, cex.lab=1.3, cex.axis=2)
# boxplot(amostra2$effort[amostra2$algo=='SAA'], main='SAA', cex.main=3, cex.lab=1.3, cex.axis=2)
# boxplot(amostra2$effort[amostra2$algo=='HS'], main='HS', cex.main=3, cex.lab=1.3, cex.axis=2)
# par(mfrow=c(1,1))
# boxplot(amostra2$effort ~amostra2$algo)
# #
# par(mfrow=c(1,3))
# hist(amostra2$effort[amostra2$algo=='RS'],
#      ylab='Frequency', xlab='Cost', main='RS', cex.main=3, cex.lab=1.3, cex.axis=2)
# hist(amostra2$effort[amostra2$algo=='SAA'],
#      ylab='Frequency', xlab='Cost', main='SAA', cex.main=3, cex.lab=1.3, cex.axis=2)
# hist(amostra2$effort[amostra2$algo=='HS'],
#      ylab='Frequency', xlab='Cost', main='HS', cex.main=3, cex.lab=1.3, cex.axis=2)

# #Global histogram
# hist(amostra2$effort,
#      ylab='Frequency', xlab='Cost', main='RS x SAA x HS', cex.main=3, cex.lab=1.3, cex.axis=2)

# Global stacked histogram
# ggplot(amostra2, aes(x=effort)) +
#   geom_histogram(aes(color=algo, fill=algo,
#                      alpha=0.2, position='stack', binwidth=30))

# O teste de Friedman mostrou que há efeito significativo do algoritmo
# sobre a ordem do custo computacional para o escalonamento (chi-quadrado = 3209.3;
# p-value < 2.2e-16 em grau de significação de 5%). O post-hoc de Dunn-Bonferroni mostrou mostrou que o custo computacional
# de HS < SAA < RS.