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

#exercise 1# X=X2
X=X2
#normality
sigma<-sd(X,na.rm=TRUE)
mu<-mean(X,na.rm=TRUE)

qqPlot(X) 

hist(X, probability=TRUE, breaks=11, xlim=c(0,400))
dxE <- density(X, na.rm=TRUE)
lines(dxE, col="black")
xn <- qnorm(seq(0,1,length.out=100), mu, sigma)
lines(xn, dnorm(xn, mu, sigma), col='red')

# Añadir leyenda
legend("topright", legend=c("Densidad", "Distribución normal"),
       col=c("black", "red"), lty=c(1,1), lwd=c(1,1))



ks.test(X,"pnorm",mean=mu,sd=sigma) #Kolmogorov-Smirnov test
shapiro.test(X)#shapiro.Wilk test

#The variable X is not Normal, but the sample size is large, so that we can use the asymptotic CI for μ
#(X¯−z1−α/2Sn−−√,X¯+z1−α/2Sn−−√)
#This CI is not implemented in any specific R function but we can use again t.test since,
#it the sample size is large, the distribution tn−1 can be approximated by a Normal distirbution.
#However, we do not know any CI for the variance of a non-Normal distribution.

t.test(X, conf.level=0.9)

library(BSDA)
median(X,na=TRUE)
SIGN.test(X,md=24,alternative = "two.sided",conf.level = 0.9)
wilcox.test(X, alternative='two.sided', mu=24, conf.int=TRUE)


#exercise 2
F=F1

t.test(X[F=='Yes'], X[F=='No'], conf.level=0.95, var.equal=FALSE)#for the difference of the mean

wilcox.test(X[F=='Yes'],X[F=='No'], alternative="two.sided", conf.int=TRUE)# test for the difference of the medians
median(X[F=='No'],na.rm=TRUE)
median(X[F=='Yes'],na.rm=TRUE)
91.17051-66.91120

#exercise 3
F11 <- factor(F1, labels = c("No consume","Consume"))
F22 <- factor(F2, labels=c("8th grade","9-11th grade", "Highschool", "SomeCollege", "CollegeGrad"))
tabla <- xtabs(~F11 + F22, data = NHANES,)
addmargins(tabla)

tabla.test<-chisq.test(tabla, correct=TRUE)
tabla.test
tabla.test[3]>0.05 #They are dependent

round(tabla.test$expected,2)
round(tabla,2)
round(tabla.test$residuals,2)
