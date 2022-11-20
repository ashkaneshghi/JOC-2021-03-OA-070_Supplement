library(stargazer)
library(data.table)
library(pastecs)
library(corrplot)
library(MASS)
library(DescTools)
library(rsq)
library(performance)

data=fread("FinalData.csv",header = TRUE)

# Descriptive Statistics and Correlation Matrix

DS <- stat.desc(data[, c("Category", "Rank","Advertising","Login","UnsafePrimary","AbuseDummy","Abuse","ObfuscateDummy","ObfuscateLevel")])
round(DS, 3)

CM <- cor(data[, c("Category", "Rank","Advertising","Login","UnsafePrimary","AbuseDummy","Abuse","ObfuscateDummy","ObfuscateLevel")])
round(CM, 3)

stargazer(data[, c("Category", "Rank","Advertising","Login","UnsafePrimary","AbuseDummy","ObfuscateDummy","Abuse","ObfuscateLevel")])
stargazer(CM, title="Correlation Matrix")

# One-stage Model

OneStageAbuseDummy = glm(AbuseDummy~Category+Rank+Advertising+Login ,data=data,family = "binomial")
summary(OneStageAbuseDummy)
c(PseudoR2(OneStageAbuseDummy, c("AIC", "McFadden", "CoxSnell")),rsq.kl(OneStageAbuseDummy))
performance_hosmer(OneStageAbuseDummy)

OneStageObfuscateDummy = glm(ObfuscateDummy~Category+Rank+Advertising+Login ,data=subset(data, AbuseDummy=="1"),family = "binomial")
summary(OneStageObfuscateDummy)
c(PseudoR2(OneStageObfuscateDummy, c("AIC", "McFadden", "CoxSnell")),rsq.kl(OneStageObfuscateDummy))
performance_hosmer(OneStageObfuscateDummy)

OneStageAbuse = glm.nb(Abuse~Category+Rank+Advertising+Login ,data=data)
summary(OneStageAbuse)
c(PseudoR2(OneStageAbuse, c("AIC", "McFadden", "CoxSnell")),rsq.kl(OneStageAbuse))
performance_hosmer(OneStageAbuse)

OneStageObfuscate = glm.nb(Obfuscate~Category+Rank+Advertising+Login ,data=subset(data, AbuseDummy=="1"))
summary(OneStageObfuscate)
c(PseudoR2(OneStageObfuscate, c("AIC", "McFadden", "CoxSnell")),rsq.kl(OneStageObfuscate))
performance_hosmer(OneStageObfuscate)


stargazer(OneStageAbuseDummy,OneStageAbuse,OneStageObfuscateDummy,OneStageObfuscate, title="Results", align=TRUE)


#2-stages models

FirstStage = glm(Advertising~Category+Rank+Login,data=data,family = "binomial")
summary(FirstStage)
Advertisingres = residuals(FirstStage, type = "pearson")
data=cbind(data,Advertisingres)

#2-stages model (Primary Only)

SecondStageUnsafePrimary = glm.nb(UnsafePrimary~Category+Rank+Advertisingres+Login,data=data,offset(Primary))
summary(SecondStageUnsafePrimary)
UnsafePrimaryres = SecondStageUnsafePrimary$residuals
data=cbind(data,UnsafePrimaryres)

SecondStagePrimaryAbuseDummy = glm(AbuseDummy~Category+Rank+Advertisingres+Login+UnsafePrimaryres ,data=data,family = "binomial")
summary(SecondStagePrimaryAbuseDummy)
c(PseudoR2(SecondStagePrimaryAbuseDummy, c("AIC", "McFadden", "CoxSnell")),rsq.kl(SecondStagePrimaryAbuseDummy))
performance_hosmer(SecondStagePrimaryAbuseDummy)

SecondStagePrimaryObfuscateDummy = glm(ObfuscateDummy~Category+Rank+Advertisingres+Login+UnsafePrimaryres ,data=subset(data, AbuseDummy=="1"),family = "binomial")
summary(SecondStagePrimaryObfuscateDummy)
c(PseudoR2(SecondStagePrimaryObfuscateDummy, c("AIC", "McFadden", "CoxSnell")),rsq.kl(SecondStagePrimaryObfuscateDummy))
performance_hosmer(SecondStagePrimaryObfuscateDummy)

SecondStagePrimaryAbuse = glm.nb(Abuse~Category+Rank+Advertisingres+Login+UnsafePrimaryres ,data=data)
summary(SecondStagePrimaryAbuse)
c(PseudoR2(SecondStagePrimaryAbuse, c("AIC", "McFadden", "CoxSnell")),rsq.kl(SecondStagePrimaryAbuse))
performance_hosmer(SecondStagePrimaryAbuse)

SecondStagePrimaryObfuscate = glm.nb(Obfuscate~Category+Rank+Advertisingres+Login+UnsafePrimaryres ,data=subset(data, AbuseDummy=="1"))
summary(SecondStagePrimaryObfuscate)
c(PseudoR2(SecondStagePrimaryObfuscate, c("AIC", "McFadden", "CoxSnell")),rsq.kl(SecondStagePrimaryObfuscate))
performance_hosmer(SecondStagePrimaryObfuscate)

stargazer(SecondStagePrimaryAbuseDummy,SecondStagePrimaryAbuse,SecondStagePrimaryObfuscateDummy,SecondStagePrimaryObfuscate, title="Results", align=TRUE)


#2-stages model (Primary Only + log(Size))

SecondStageUnsafePrimary = glm.nb(UnsafePrimary~Category+log(Size)+Advertisingres+Login,data=data,offset(Primary))
summary(SecondStageUnsafePrimary)
UnsafePrimaryresl = SecondStageUnsafePrimary$residuals
data=cbind(data,UnsafePrimaryresl)

SecondStagePrimaryAbuseDummy = glm(AbuseDummy~Category+log(Size)+Advertisingres+Login+UnsafePrimaryresl ,data=data,family = "binomial")
summary(SecondStagePrimaryAbuseDummy)
c(PseudoR2(SecondStagePrimaryAbuseDummy, c("AIC", "McFadden", "CoxSnell")),rsq.kl(SecondStagePrimaryAbuseDummy))
performance_hosmer(SecondStagePrimaryAbuseDummy)

SecondStagePrimaryObfuscateDummy = glm(ObfuscateDummy~Category+log(Size)+Advertisingres+Login+UnsafePrimaryresl ,data=subset(data, AbuseDummy=="1"),family = "binomial")
summary(SecondStagePrimaryObfuscateDummy)
c(PseudoR2(SecondStagePrimaryObfuscateDummy, c("AIC", "McFadden", "CoxSnell")),rsq.kl(SecondStagePrimaryObfuscateDummy))
performance_hosmer(SecondStagePrimaryObfuscateDummy)

SecondStagePrimaryAbuse = glm.nb(Abuse~Category+log(Size)+Advertisingres+Login+UnsafePrimaryresl ,data=data)
summary(SecondStagePrimaryAbuse)
c(PseudoR2(SecondStagePrimaryAbuse, c("AIC", "McFadden", "CoxSnell")),rsq.kl(SecondStagePrimaryAbuse))
performance_hosmer(SecondStagePrimaryAbuse)

SecondStagePrimaryObfuscate = glm.nb(Obfuscate~Category+log(Size)+Advertisingres+Login+UnsafePrimaryresl ,data=subset(data, AbuseDummy=="1"))
summary(SecondStagePrimaryObfuscate)
c(PseudoR2(SecondStagePrimaryObfuscate, c("AIC", "McFadden", "CoxSnell")),rsq.kl(SecondStagePrimaryObfuscate))
performance_hosmer(SecondStagePrimaryObfuscate)

stargazer(SecondStagePrimaryAbuseDummy,SecondStagePrimaryAbuse,SecondStagePrimaryObfuscateDummy,SecondStagePrimaryObfuscate, title="Results", align=TRUE)


#2-stages model (Total)

SecondStageUnsafeTotal = glm.nb(UnsafeTotal~Category+Rank+Advertisingres+Login,data=data,offset(Total))
summary(SecondStageUnsafeTotal)
UnsafeTotalres = SecondStageUnsafeTotal$residuals
data=cbind(data,UnsafeTotalres)

SecondStageTotalAbuseDummy = glm(TotalAbuseDummy~Category+Rank+Advertisingres+Login+UnsafeTotalres ,data=data,family = "binomial")
summary(SecondStageTotalAbuseDummy)
c(PseudoR2(SecondStageTotalAbuseDummy, c("AIC", "McFadden", "CoxSnell")),rsq.kl(SecondStageTotalAbuseDummy))
performance_hosmer(SecondStageTotalAbuseDummy)

SecondStageTotalObfuscateDummy = glm(TotalObfuscateDummy~Category+Rank+Advertisingres+Login+UnsafeTotalres ,data=subset(data, TotalAbuseDummy=="1"),family = "binomial")
summary(SecondStageTotalObfuscateDummy)
c(PseudoR2(SecondStageTotalObfuscateDummy, c("AIC", "McFadden", "CoxSnell")),rsq.kl(SecondStageTotalObfuscateDummy))
performance_hosmer(SecondStageTotalObfuscateDummy)

SecondStageTotalAbuse = glm.nb(TotalAbuse~Category+Rank+Advertisingres+Login+UnsafeTotalres ,data=data)
summary(SecondStageTotalAbuse)
c(PseudoR2(SecondStageTotalAbuse, c("AIC", "McFadden", "CoxSnell")),rsq.kl(SecondStageTotalAbuse))
performance_hosmer(SecondStageTotalAbuse)

SecondStageTotalObfuscate = glm.nb(TotalObfuscate~Category+Rank+Advertisingres+Login+UnsafeTotalres ,data=subset(data, TotalAbuseDummy=="1"))
summary(SecondStageTotalObfuscate)
c(PseudoR2(SecondStageTotalObfuscate, c("AIC", "McFadden", "CoxSnell")),rsq.kl(SecondStageTotalObfuscate))
performance_hosmer(SecondStageTotalObfuscate)

stargazer(SecondStageTotalAbuseDummy,SecondStageTotalAbuse,SecondStageTotalObfuscateDummy,SecondStageTotalObfuscate, title="Results", align=TRUE)


#2-stages model (Total + log(Size))

SecondStageUnsafeTotal = glm.nb(UnsafeTotal~Category+log(Size)+Advertisingres+Login,data=data,offset(Total))
summary(SecondStageUnsafeTotal)
UnsafeTotalresl = SecondStageUnsafeTotal$residuals
data=cbind(data,UnsafeTotalresl)

SecondStageTotalAbuseDummy = glm(TotalAbuseDummy~Category+log(Size)+Advertisingres+Login+UnsafeTotalresl ,data=data,family = "binomial")
summary(SecondStageTotalAbuseDummy)
c(PseudoR2(SecondStageTotalAbuseDummy, c("AIC", "McFadden", "CoxSnell")),rsq.kl(SecondStageTotalAbuseDummy))
performance_hosmer(SecondStageTotalAbuseDummy)

SecondStageTotalObfuscateDummy = glm(TotalObfuscateDummy~Category+log(Size)+Advertisingres+Login+UnsafeTotalresl ,data=subset(data, TotalAbuseDummy=="1"),family = "binomial")
summary(SecondStageTotalObfuscateDummy)
c(PseudoR2(SecondStageTotalObfuscateDummy, c("AIC", "McFadden", "CoxSnell")),rsq.kl(SecondStageTotalObfuscateDummy))
performance_hosmer(SecondStageTotalObfuscateDummy)

SecondStageTotalAbuse = glm.nb(TotalAbuse~Category+log(Size)+Advertisingres+Login+UnsafeTotalresl ,data=data)
summary(SecondStageTotalAbuse)
c(PseudoR2(SecondStageTotalAbuse, c("AIC", "McFadden", "CoxSnell")),rsq.kl(SecondStageTotalAbuse))
performance_hosmer(SecondStageTotalAbuse)

SecondStageTotalObfuscate = glm.nb(TotalObfuscate~Category+log(Size)+Advertisingres+Login+UnsafeTotalresl ,data=subset(data, TotalAbuseDummy=="1"))
summary(SecondStageTotalObfuscate)
c(PseudoR2(SecondStageTotalObfuscate, c("AIC", "McFadden", "CoxSnell")),rsq.kl(SecondStageTotalObfuscate))
performance_hosmer(SecondStageTotalObfuscate)

stargazer(SecondStageTotalAbuseDummy,SecondStageTotalAbuse,SecondStageTotalObfuscateDummy,SecondStageTotalObfuscate, title="Results", align=TRUE)

