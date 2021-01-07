#processando amostra
#1-carga
amostra<-read.csv('c:/Users/Dell/PycharmProjects/Gerador_R/Versions/Anova/v_1/teste.csv', sep=';',dec=',' )
#amostra<-read.csv('G:/Meu Drive/Doutorado/Orientação/Escalonamento/Implementações/Max_NPV/Samples/2020-12-26/Result2020-12-27_v2.csv', sep=',',dec=',' )

attach(amostra)
#2-verificando os dados
summary(amostra)
table(algo,vertices,fan)
aggregate(effort, by=list(algo,vertices,fan),FUN=mean)
aggregate(effort, by=list(algo,vertices,fan),FUN=sd)

# aggregate(time, by=list(algo,vertices,fan),FUN=mean)
# aggregate(time, by=list(algo,vertices,fan),FUN=sd)
#boxplot(effort~algo+vertices+fan)
#3-anova com 3 fatores:algo,vertices e fan
fit<-aov(effort~algo*vertices*fan)
summary(fit)

# fit_time<-aov(time~algo*vertices*fan)
# summary(fit_time)