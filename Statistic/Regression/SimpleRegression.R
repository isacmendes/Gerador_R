# Title     : TODO
# Objective : TODO
# Created by: Dell
# Created on: 04/02/2021

options(scipen=999)
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

pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubr, QuantPsyc, psych, scatterplot3d, gridExtra)

# Carrega dados
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random_G1/Result2021-03-02_10000.csv', sep=',',dec=',' )
amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random_G1/Result2021-03-02_5000.csv', sep=',',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/Random_G2/Result2021-08-02_1000.csv', sep=',',dec=',' )
glimpse(amostra)
#View(amostra)
#amostra$vertices <- factor(amostra$percNeg)
amostra$percNeg <- as.integer(amostra$percNeg)

# Modelo
#mod <- lm(effort ~ percNeg + vertices, amostra)

amostra$effort <- log(sqrt(amostra$effort))
#amostra$effort <- log(amostra$effort)
#lambda <- BoxCox.lambda(amostra$effort, method=c("guerrero"), lower=-5, upper=5)
lambda <- BoxCox.lambda(amostra$effort, method=c("loglik"), lower=-5, upper=5)
valor_transformado <- BoxCox(amostra$effort, lambda)
#valor_transformado <- amostra$effort

#mod <- lm(valor_transformado ~ amostra$percNeg + amostra$vertices)
mod <- lm(valor_transformado[amostra$algo=='RS'] ~ amostra$vertices[amostra$algo=='RS'])
#mod <- lm(amostra$effort ~ amostra$percNeg+amostra$vertices)

# m1 <- art(log(first_values) ~ percNeg_RS)
# anova(m1)

# Análise gráfica
# par(mfrow=c(2,2))
# plot(mod)

# Testes pressupostos
print('Normalidade Lillie resíduos:')
lillie.test(mod$residual)
#lillie.test(valor_transformado)

print('Normalidade Aderson Darlin esiduos:')
ad.test(mod$residuals)

#shapiro.test(mod$residuals)

#jarque.test(c(valor_transformado))
jarque.test(c(mod$residuals))


#View(data.frame(c(valor_transformado)))

#print('Homocedasticidade Levene:')
#leveneTest(mod$residuals ~ amostra$vertices, center=median)
#leveneTest(mod$residuals ~ amostra$percNeg, center=median)

#print('Homocedasticidade Bartlett:')
#bartlett.test(mod$residuals ~ amostra$vertices)

bptest(mod)

#vif(mod)

# Outliers
summary(rstandard(mod))


amostraRS <- subset(amostra, algo=='RS')
#amostraRS$percNeg <- as.integer(amostraRS$percNeg)

#View(amostra_100)

fit_corrente <- lm(amostra_corrente$effort ~ amostra_corrente$vertices)
plot(fit_corrente)

#shapiro.test(amostraRS100$effort)
#bptest(amostraRS100$effort ~ amostraRS100$vertices)

#Gráfico de dispersão
alg <- 'RS'

alg <- 'SAA'
alg <- 'HS'
per <- 0
per <- 25
per <- 50
per <- 75
per <- 1000

amostra_corrente <- subset(amostra, algo==alg & percNeg!=per)
ggplot(data=amostra_corrente, mapping=aes(x=vertices, y=effort)) +
  geom_point() +
  geom_smooth(method="lm", col = "red") +
  stat_regline_equation(aes(label=paste(..eq.label..,
                                        sep="*plain(\",\")~~")),
                                        label.x=15, label.y=5.5, size=10) +
    #theme_classic()
    theme(text=element_text(size=20))

# for (per in c(0,25,50,75,100)){
# amostra_corrente <- subset(amostra, algo==alg & percNeg==per)
# graficos <- ggplot(data=amostra_corrente, mapping=aes(x=vertices, y=effort), main='main', cex.main=2, cex.lab=3, cex.axis=4) +
#   geom_point() +
#   geom_smooth(method="lm", col = "red") +
#   stat_regline_equation(aes(label=paste(..eq.label..,
#                                         sep="*plain(\",\")~~")),
#                                         label.x=15, label.y=2) +
#     theme_classic()
# }
