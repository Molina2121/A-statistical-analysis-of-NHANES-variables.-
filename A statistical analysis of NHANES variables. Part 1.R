knitr::opts_chunk$set(echo = TRUE)

library(NHANES)
library(RcmdrMisc)

#group H(j=8)
set.seed(800)
n=800


which(!is.na(NHANES$RegularMarij))

X1.<-NULL
X2.<-NULL
F1.<-NULL
F2.<-NULL

X1<-NULL
X2<-NULL
F1<-NULL
F2<-NULL
for (i in 1:length(which(!is.na(NHANES$RegularMarij))))
{
  X1.[i]<-NHANES$HHIncomeMid[which(!is.na(NHANES$RegularMarij))[i]]
  X2.[i]<-NHANES$AlcoholYear[which(!is.na(NHANES$RegularMarij))[i]]
  F1.[i]<-NHANES$RegularMarij[which(!is.na(NHANES$RegularMarij))[i]]
  F2.[i]<-NHANES$Education[which(!is.na(NHANES$RegularMarij))[i]] 
}

random<-sample(c(1:length(which(!is.na(NHANES$RegularMarij)))), size=800)

for (i in 1:800)
{
  X1[i]<-X1.[random[i]]
  X2[i]<-X2.[random[i]]
  F1[i]<-F1.[random[i]]
  F2[i]<-F2.[random[i]]
}
#Con el warning no hay problema si se ejecuta desde el principio. Comprobado y se pq.
F1<-cut(F1,breaks=2,labels=c("No", "Yes"))
F2<-cut(F2,breaks=5,labels=c("8th Grade", "9-11th Grade", "High School", "Some College","College Grad"))


#EXERCISE 1
#descriptive analysis of X1
summary(X1)
sigma<-sd(X1,na.rm=TRUE)
mu<-mean(X1,na.rm = TRUE)
mu
sigma

qqnorm(X1)
qqline(X1, col='red') 
hist(X1, probability=TRUE, breaks=10, xlim=c(2000,100000))

dxE<-density(X1,na.rm=TRUE)
lines(dxE, col="black")

xn<-qnorm(seq(0,1,length.out=100),mu,sigma)
lines(xn,dnorm(xn,mu,sigma), col='red')

xg<-qgamma(seq(0,1,length.out=100),shape=(mu^2)/(sigma^2),scale=(sigma^2)/mu)

lines(xg,dgamma(xg,shape=(mu^2)/(sigma^2),scale=(sigma^2)/mu), col='green')
legend(40000, 0.000025, inset = 0.1, c("Density","Normal","Gamma"), lty = 1, col =1:3)

#descriptive analysis of X2
summary(X2)
sigma2<-sd(X2,na.rm=TRUE)
mu2<-mean(X2,na.rm=TRUE)
mu2
qqnorm(X2)
qqline(X2, col='red') 

hist(X2, probability=TRUE, breaks=11, xlim=c(0,400))


dxE2<-density(X2,na.rm=TRUE)
lines(dxE2, col="black")

xn2<-qnorm(seq(0,1,length.out=100),mu2,sigma2)
lines(xn2,dnorm(xn2,mu2,sigma2), col='red')

xg2<-qgamma(seq(0,1,length.out=100),shape=(mu2^2)/(sigma2^2),scale=(sigma2^2)/mu2)
lines(xg2,dgamma(xg2,shape=(mu2^2)/(sigma2^2),scale=(sigma2^2)/mu2), col='green')
legend(200, 0.01, inset = 0.1, c("Density","Normal","Gamma"), lty = 1, col =1:3)

#descriptive analysis F1
table(as.factor(F1))#valores absolutos
table(as.factor(F1))/length(F1)*100#porcentajes

par(mfrow = c(1, 2))
barplot(table(F1), col = c("red", "blue"))
pie(table(F1), col = c("red", "blue"))

#descriptive analysis F2
table(as.factor(F2))#valores absolutos
table(as.factor(F2))/length(F1)*100#porcentajes

par(mfrow = c(1, 1))
barplot(table(F2), col = c("green", "blue","purple","orange","yellow"))
pie(table(F2),col = c("green", "blue","purple","orange","yellow"))



#PREGUNTA 2

numSummary(X2, group=F1)
par(mfrow=c(1,1))
Hist(X2, groups=F1, breaks= c(-1,50, 100,150,200,250, 300, 365), probability=FALSE)
Boxplot(X2~F1)

groupsX2<- cut(X2, breaks = c(-1,50, 100,150,200,250, 300, 365), labels = c(1, 2, 3, 4, 5,6,7))


groupsX2
X2
summary(groupsX2)
barplot(table(groupsX2))


Tabla<-xtabs(~F1 + groupsX2)
Tabla
T1<-totPercents((Tabla))
T1
T2<-colPercents((Tabla))
T2
T3<-colPercents(t(Tabla))
T3
Barplot(groupsX2, by=F1, style="parallel", col = c("red", "blue"),scale="percent")
#Si se cambia la semilla se ve mejor la correlacion.
Barplot(F1, by=groupsX2, style="parallel", scale="percent", conditional="TRUE")

resultado <- chisq.test(table(groupsX2, F1))
resultado

#PREGUNTA 3

F11 <- factor(F1, labels = c("No consume","Consume"))
F22 <- factor(F2, labels=c("8th grade","9-11th grade", "Highschool", "SomeCollege", "CollegeGrad"))

#Table+Plot F1 and F2
tabla <- xtabs(~F11 + F22, data = NHANES)
print(tabla)
barplot(tabla, beside = TRUE, legend.text = rownames(tabla), args.legend = list(title = "Regular Marijuana Use", cex = 0.7),
        xlab = "Education", ylab = "Marijuana", col = c("red", "blue"), main = "Regular Marijuana Use by Education")

#Percents
tabla_prop <- prop.table(tabla) * 100
tabla_prop_round <- round(tabla_prop, digits = 1)
tabla_prop_total_round <- addmargins(tabla_prop_round)
print(tabla_prop_total_round)

colpercents <- prop.table(tabla, margin = 2) * 100
colpercents_round <- round(colpercents, digits = 1)
barplot(colpercents, beside = TRUE, legend.text = rownames(tabla), args.legend = list(title = "Regular Marijuana Use", cex = 0.7), xlab = "Education", ylab = "Marijuana", col = c("red", "blue"), main = "Regular Marijuana Use by Education")



#Exercise 4


#Dependence Between X1 and X2 in the whole sample
#Numerical
lm1<-lm(X2 ~ X1)
summary(lm1)

df <- data.frame(X1, X2)

# Eliminar las filas que contienen valores faltantes en ambos vectores
df_complete <- df[complete.cases(df), ]

# Calcular la matriz de correlación entre x e y
correlacion <- cor(df_complete$X1, df_complete$X2)

# Imprimir la matriz de correlación
print(correlacion)
#Plots
par(mfrow = c(1, 1))
plot(X1, X2, xlab="HHIncomeMid", ylab="AlcoholYear",
     main = "Dispersion plot  HHIncomeMid-AlcoholYear and smoother",  pch=16, cex=1.4)
abline(a=lm1$coefficients[1], b=lm1$coefficients[2], col="2")
lines(lowess(X1, X2, f = 0.2, delta=10), col = "3")
legend(62, 290, inset = 0.1, c("Regression line.","smoother"), lty = 1, col = 2:3)

#Dependence Between X1 and X2 as function of F1
#Numerical
library(dplyr)
df <- data.frame(F11, X1, X2)
# Eliminar las filas que contienen valores faltantes en ambos vectores
df_complete <- df[complete.cases(df), ]

lmG<-df %>% group_by(F11) %>% do(model = lm(X2 ~ X1, data = .))
for (i in 1:2)
{
  print(F11[i])
  print(summary(lmG[[2]][[i]]))
  i <- i+1
} 
CorrG<-df_complete %>% group_by(F11) %>% summarize(cor=cor(X1,X2))
print(CorrG)

#Plots
colorF11 = c(2,3)[F11]
plot(X1, X2, xlab="HHIncomeMid", ylab="AlcoholYear", 
     main = "Dispersion plot  HHIncomeMid-AlcoholYear and smoother",  pch=16, cex=1.4, col=colorF11)
for (i in 1:2)
{
  abline(a=lmG[[2]][[i]]$coefficients[1], b=lmG[[2]][[i]]$coefficients[2], col=i+1)
  i <- i+1
}
legend(62, 290, inset = 0.1, c("Consume","No consume"), lty = 1, col =2:3)


