setwd("C:/Users/jarkovskyj/Desktop/patek seminar")

install.packages('epiDisplay')
install.packages('MASS')
install.packages('ROCit')
library(epiDisplay)
library(MASS)
library(ROCit)

# nacteni dat

data <- read.csv("C:/Users/jarkovskyj/Desktop/patek seminar/data_COVID19_jaro2020.csv")
View(data)

# prekodovani 65+

data$age65 <- as.numeric(cut(data$vek, breaks = c(-1,64, Inf)))
data$comorb_kat <- as.numeric(cut(data$pocet_komorbidit, breaks = c(-1,0,1,2, Inf)))

# sumarizace prediktoru

tab1(data$vek, cum.percent = TRUE)
tab1(data$vek_kat, cum.percent = TRUE)
tab1(data$age65, cum.percent = TRUE)
tab1(data$pohlavi_muz, cum.percent = TRUE)
tab1(data$hypertenze, cum.percent = TRUE)
tab1(data$srdecni_selhani, cum.percent = TRUE)
tab1(data$selhani_ledvin, cum.percent = TRUE)
tab1(data$diabetes_mellitus, cum.percent = TRUE)
tab1(data$astma_CHOPN, cum.percent = TRUE)
tab1(data$porucha_acidity, cum.percent = TRUE)
tab1(data$nadorova_lecba, cum.percent = TRUE)
tab1(data$pocet_komorbidit, cum.percent = TRUE)
tab1(data$comorb_kat, cum.percent = TRUE)

# vyskyt endpointu

tab1(data$endpoint, cum.percent = TRUE)

# endpoint vs. prediktory

mytable <- table(data$vek_kat,data$endpoint)
mytable # print table
plot(prop.table(mytable, 1))
mytable <- table(data$age65,data$endpoint)
mytable # print table
plot(prop.table(mytable, 1))
mytable <- table(data$pohlavi_muz,data$endpoint)
mytable # print table
plot(prop.table(mytable, 1))
mytable <- table(data$hypertenze,data$endpoint)
mytable # print table
plot(prop.table(mytable, 1))
mytable <- table(data$srdecni_selhani,data$endpoint)
mytable # print table
plot(prop.table(mytable, 1))
mytable <- table(data$selhani_ledvin,data$endpoint)
mytable # print table
plot(prop.table(mytable, 1))
mytable <- table(data$diabetes_mellitus,data$endpoint)
mytable # print table
plot(prop.table(mytable, 1))
mytable <- table(data$astma_CHOPN,data$endpoint)
mytable # print table
plot(prop.table(mytable, 1))
mytable <- table(data$porucha_acidity,data$endpoint)
mytable # print table
plot(prop.table(mytable, 1))
mytable <- table(data$nadorova_lecba,data$endpoint)
mytable # print table
plot(prop.table(mytable, 1))
mytable <- table(data$pocet_komorbidit,data$endpoint)
mytable # print table
plot(prop.table(mytable, 1))
mytable <- table(data$comorb_kat,data$endpoint)
mytable # print table
plot(prop.table(mytable, 1))

# pozor na redundanci

mytable <- table(data$vek_kat,data$pocet_komorbidit)
mytable # print table
plot(prop.table(mytable, 1))

# jednorozmerne modely

modelUNI <- glm(endpoint~factor(data$vek_kat),family=binomial(link='logit'),data=data)
data.frame(a=round(exp(coef(modelUNI)),3),round(exp(confint(modelUNI)),3))
data$predikce <-predict(modelUNI, data=data, type = 'response')
ROC <- rocit(score=data$predikce,class=data$endpoint)
ROC
plot(ROC)

modelUNI <- glm(endpoint~data$age65,family=binomial(link='logit'),data=data)
data.frame(a=round(exp(coef(modelUNI)),3),round(exp(confint(modelUNI)),3))
data$predikce <-predict(modelUNI, data=data, type = 'response')
ROC <- rocit(score=data$predikce,class=data$endpoint)
ROC
plot(ROC)

modelUNI <- glm(endpoint~data$pohlavi_muz,family=binomial(link='logit'),data=data)
data.frame(a=round(exp(coef(modelUNI)),3),round(exp(confint(modelUNI)),3))
data$predikce <-predict(modelUNI, data=data, type = 'response')
ROC <- rocit(score=data$predikce,class=data$endpoint)
ROC
plot(ROC)

modelUNI <- glm(endpoint~data$hypertenze,family=binomial(link='logit'),data=data)
data.frame(a=round(exp(coef(modelUNI)),3),round(exp(confint(modelUNI)),3))
data$predikce <-predict(modelUNI, data=data, type = 'response')
ROC <- rocit(score=data$predikce,class=data$endpoint)
ROC
plot(ROC)

modelUNI <- glm(endpoint~data$srdecni_selhani,family=binomial(link='logit'),data=data)
data.frame(a=round(exp(coef(modelUNI)),3),round(exp(confint(modelUNI)),3))
data$predikce <-predict(modelUNI, data=data, type = 'response')
ROC <- rocit(score=data$predikce,class=data$endpoint)
ROC
plot(ROC)

modelUNI <- glm(endpoint~data$selhani_ledvin,family=binomial(link='logit'),data=data)
data.frame(a=round(exp(coef(modelUNI)),3),round(exp(confint(modelUNI)),3))
data$predikce <-predict(modelUNI, data=data, type = 'response')
ROC <- rocit(score=data$predikce,class=data$endpoint)
ROC
plot(ROC)

modelUNI <- glm(endpoint~data$diabetes_mellitus,family=binomial(link='logit'),data=data)
data.frame(a=round(exp(coef(modelUNI)),3),round(exp(confint(modelUNI)),3))
data$predikce <-predict(modelUNI, data=data, type = 'response')
ROC <- rocit(score=data$predikce,class=data$endpoint)
ROC
plot(ROC)

modelUNI <- glm(endpoint~data$astma_CHOPN,family=binomial(link='logit'),data=data)
data.frame(a=round(exp(coef(modelUNI)),3),round(exp(confint(modelUNI)),3))
data$predikce <-predict(modelUNI, data=data, type = 'response')
ROC <- rocit(score=data$predikce,class=data$endpoint)
ROC
plot(ROC)

modelUNI <- glm(endpoint~data$porucha_acidity,family=binomial(link='logit'),data=data)
data.frame(a=round(exp(coef(modelUNI)),3),round(exp(confint(modelUNI)),3))
data$predikce <-predict(modelUNI, data=data, type = 'response')
ROC <- rocit(score=data$predikce,class=data$endpoint)
ROC
plot(ROC)

modelUNI <- glm(endpoint~data$nadorova_lecba,family=binomial(link='logit'),data=data)
data.frame(a=round(exp(coef(modelUNI)),3),round(exp(confint(modelUNI)),3))
data$predikce <-predict(modelUNI, data=data, type = 'response')
ROC <- rocit(score=data$predikce,class=data$endpoint)
ROC
plot(ROC)

modelUNI <- glm(endpoint~factor(data$pocet_komorbidit),family=binomial(link='logit'),data=data)
data.frame(a=round(exp(coef(modelUNI)),3),round(exp(confint(modelUNI)),3))
data$predikce <-predict(modelUNI, data=data, type = 'response')
ROC <- rocit(score=data$predikce,class=data$endpoint)
ROC
plot(ROC)

modelUNI <- glm(endpoint~factor(data$comorb_kat),family=binomial(link='logit'),data=data)
data.frame(a=round(exp(coef(modelUNI)),3),round(exp(confint(modelUNI)),3))
data$predikce <-predict(modelUNI, data=data, type = 'response')
ROC <- rocit(score=data$predikce,class=data$endpoint)
ROC
plot(ROC)

# vicerozmerne modely

modelFULL <- glm(endpoint~factor(data$vek_kat)+data$age65+data$pohlavi_muz+data$hypertenze+data$srdecni_selhani+data$selhani_ledvin+data$diabetes_mellitus+data$astma_CHOPN+data$porucha_acidity+data$nadorova_lecba+factor(data$pocet_komorbidit)+factor(data$comorb_kat),family=binomial(link='logit'),data=data)
modelMIN <- glm(endpoint~1,family=binomial(link='logit'),data=data)
summary(modelFULL)
summary(modelMIN)

data.frame(a=round(exp(coef(modelFULL)),3),round(exp(confint(modelFULL)),3))

modelSTEP <- step(modelMIN,scope = list(upper=modelFULL), direction="both",test="Chisq", trace = F)
summary(modelSTEP)

data.frame(a=round(exp(coef(modelSTEP)),3),round(exp(confint(modelSTEP)),3))
data$predikce <-predict(modelSTEP, data=data, type = 'response')
ROC <- rocit(score=data$predikce,class=data$endpoint)
ROC
plot(ROC)

# alternativni model


modelFULL <- glm(endpoint~factor(data$vek_kat)+data$pohlavi_muz+data$selhani_ledvin+factor(data$pocet_komorbidit),family=binomial(link='logit'),data=data)
summary(modelFULL)
data.frame(a=round(exp(coef(modelFULL)),3),round(exp(confint(modelFULL)),3))
data$predikce <-predict(modelFULL, data=data, type = 'response')
ROC <- rocit(score=data$predikce,class=data$endpoint)
ROC
plot(ROC)

