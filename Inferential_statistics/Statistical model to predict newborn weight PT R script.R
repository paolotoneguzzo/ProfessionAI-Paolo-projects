#1)
newborn = read.csv("neonati.csv", sep = ",",stringsAsFactors = TRUE)
head(newborn)

#2)
summary(newborn)

#3)
attach(newborn)

install.packages("moments")
library(moments)

install.packages("ggplot2")
library(ggplot2)

install.packages("lmtest")
library(lmtest)

install.packages("car")
library(car)

install.packages("carData")
library(carData)

coeff.vaiation = function(x){
  return((sd(x)/mean(x))*100)
}

install.packages("ggpubr")
library(ggpubr)

install.packages("scatterplot3d")
library("scatterplot3d")

# Anni.madre
summary(Anni.madre)
IQR(Anni.madre)
var(Anni.madre)
sd(Anni.madre)
coeff.vaiation(Anni.madre)
skewness(Anni.madre)
kurtosis(Anni.madre)-3

       
plot(density(Anni.madre),xlab = "mother's age" ,ylab = "frequency",cex.lab=1.4, cex.axis =1.4)
legend("topright", legend = c("mean","median"), pch = "|", col = c("red","black"),cex=1.4)
abline(v = mean(Anni.madre), col = "red")
abline(v = median(Anni.madre))

length(newborn[Anni.madre<10,])


# N.gravidanze
ni=table(N.gravidanze)
fi=table(N.gravidanze)/(dim(newborn)[1])
Ni = cumsum(ni)
Fi = cumsum(fi)
relftableNG=cbind(ni,fi,Ni,Fi)

# Fumatrici
ni=table(Fumatrici)
fi=table(Fumatrici)/(dim(newborn)[1])
relftableFum_=cbind(ni,fi)

# Gestazione
summary(Gestazione)
IQR(Gestazione)
var(Gestazione)
sd(Gestazione)
coeff.vaiation(Gestazione)
skewness(Gestazione)
kurtosis(Gestazione)-3

hist((Gestazione), freq = FALSE, xlim = c(25,45),
     xlab = "weeks of gestation",ylab = "frequency")
legend("topright", legend = c("mean","median"), pch = "|", col = c("red","black"))
abline(v = mean(Gestazione), col = "red")
abline(v = median(Gestazione))


# Peso
summary(Peso)
IQR(Peso)
var(Peso)
sd(Peso)
coeff.vaiation(Peso)
skewness(Peso)
kurtosis(Peso)-3

plot(density(Peso),xlab = "weight of the newborn (grams)" ,ylab = "frequency")
legend("topright", legend = c("mean","median"), pch = "|", col = c("red","black"))
abline(v = mean(Peso), col = "red")
abline(v = median(Peso))

# Lunghezza
summary(Lunghezza)
IQR(Lunghezza)
var(Lunghezza)
sd(Lunghezza)
coeff.vaiation(Lunghezza)
skewness(Lunghezza)
kurtosis(Lunghezza)-3

plot(density(Lunghezza),xlab = "height of the newborn (millimiters)" ,ylab = "frequency")
legend("topright", legend = c("mean","median"), pch = "|", col = c("red","black"))
abline(v = mean(Lunghezza), col = "red")
abline(v = median(Lunghezza))

# Cranio
summary(Cranio)
IQR(Cranio)
var(Cranio)
sd(Cranio)
coeff.vaiation(Cranio)
skewness(Cranio)
kurtosis(Cranio)-3

plot(density(Cranio),xlab = "diameter of the newborn skull (millimiters)" ,ylab = "frequency")
legend("topright", legend = c("mean","median"), pch = "|", col = c("red","black"))
abline(v = mean(Cranio), col = "red")
abline(v = median(Cranio))

# Tipo.parto
ni=table(Tipo.parto)
fi=table(Tipo.parto)/(dim(newborn)[1])
relftablTP=cbind(ni,fi)

# Ospedale
ni=table(Ospedale)
fi=table(Ospedale)/(dim(newborn)[1])
relftableOs=cbind(ni,fi)

# Sesso
ni=table(Sesso)
fi=table(Sesso)/(dim(newborn)[1])
relftableSex=cbind(ni,fi)

#4) 
summary(newborn)

# 3200 g as mean weight at  birth
t.test(Peso,
       mu = 3200,
       conf.level = 0.95,
       alternative ="two.sided")

#500 cm as mean lenght at birth
t.test(Lunghezza,
       mu = 500,
       conf.level = 0.95,
       alternative ="two.sided")


#5) 
ggplot(newborn)+
  geom_boxplot(aes(Sesso,Peso, fill = Sesso))+
  labs(x = "Sex", y = "Weight (grams)",title = "Difference in weight between sexes")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill= "none")
ggsave("Difference in weight between sexes.jpeg")

t.test(data=newborn,
       Peso~Sesso,
       conf.level = 0.99,
       alternative ="less")

mean(Peso[Sesso=="M"])
mean(Peso[Sesso=="F"])


ggplot(newborn)+
  geom_boxplot(aes(Sesso,Lunghezza, fill = Sesso))+
  labs(x = "Sex", y = "Height (millimiters)",title = "Difference in height between sexes")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill= "none")
ggsave("Difference in height between sexes.jpeg")

t.test(data=newborn,
       Lunghezza~Sesso,
       conf.level = 0.99,
       alternative ="less")


ggplot(newborn)+
  geom_boxplot(aes(Sesso,Cranio, fill = Sesso))+
  labs(x = "Sex", y = "Skull diameter (millimiters)",title = "Difference in skull diameter between sexes")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill= "none")
ggsave("Difference in skull diameter between sexes.jpeg")

t.test(data=newborn,
       Cranio~Sesso,
       conf.level = 0.99,
       alternative ="less")


ggplot(newborn)+
  geom_boxplot(aes(Sesso,Gestazione, fill = Sesso))+
  labs(x = "Sex", y = "Gestation time (weeks)",title = "Difference in gestation time between sexes")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill= "none")
ggsave("Difference in gestation time between sexes.jpeg")

t.test(data=newborn,
       Gestazione~Sesso,
       conf.level = 0.99,
       alternative ="less")


#6)
Observed = as.matrix(table(Tipo.parto, Ospedale))
Observed.1 = cbind(Observed,TotaleR = margin.table(Observed,1))
Observed.2  = rbind(Observed.1,totaleC = margin.table(Observed.1,2))
Observed.2

install.packages("ggpubr")
ggpubr::ggballoonplot(data = as.data.frame(Observed),
                     fill = "blue", 
                     main = "Frequency of Natural & Caesarean delivery in each Hospital", cex.main  = 1.5, 
                     cex.label = 1.5)

chisq.test(Observed)


# MULTIDIMENSIONAL REGRESSION
moments::skewness(Peso)
moments::kurtosis(Peso)-3

shapiro.test(Peso)

#check for multicollinearity
round(cor(newborn),2)
#1
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(newborn, upper.panel = panel.smooth,lower.panel = panel.cor)

#2
plot(Lunghezza,Peso, pch = 20, main = "Height vs Weight")
plot(Cranio,Peso, pch = 20,  main = "Skull diameter vs Weight")
plot(Gestazione,Peso,pch = 20,  main = "Gestation weeks vs Weight")

plot(Anni.madre, Peso,pch = 20,  main = "Mother's age vs Weight")
cor(Anni.madre,Peso)

plot(N.gravidanze, Peso, pch = 20,main = "Number of gestations vs Weight")
cor(N.gravidanze,Peso)


newborn$FumatriciC = ifelse(Fumatrici==0,"no","s?")
boxplot(Peso~FumatriciC, main = "Smoking mother vs Weight",xlab = "Fumatrici")
t.test(Peso~Fumatrici)
mean(Peso[Fumatrici==0])
mean(Peso[Fumatrici==1])

boxplot(Peso~Tipo.parto, main = "Delivery type vs Weight")
t.test(Peso~Tipo.parto)
mean(Peso[Tipo.parto=="Nat"])
mean(Peso[Tipo.parto=="Ces"])

boxplot(Peso~Ospedale, main = "Hospital vs Weight")
pairwise.t.test(Peso,Ospedale,
                     paired = FALSE,
                     pool.sd = TRUE,
                     p.adjust.method = "bonferroni")
mean(Peso[Ospedale=="osp1"])
mean(Peso[Ospedale=="osp2"])
mean(Peso[Ospedale=="osp3"])


#3 
model1 = lm(Peso~., data = newborn)
summary(model1)

model2 = update(model1, ~.-Fumatrici - Anni.madre - Ospedale)
summary(model2)

model3 = update(model2, ~.-Tipo.parto)
summary(model3)

model4 = update(model3, ~.-N.gravidanze)
summary(model4)

AIC(model1,model2,model3,model4)
BIC(model1,model2,model3,model4)

#4
gest = ggplot(newborn)+ geom_point(aes(Gestazione,Peso))+ 
  geom_smooth(aes(Gestazione,Peso), col = "red")

skull = ggplot(newborn)+ geom_point(aes(Cranio,Peso))+ 
  geom_smooth(aes(Cranio,Peso), col = "red")

infleng =ggplot(newborn)+ geom_point(aes(Lunghezza,Peso))+ 
  geom_smooth(aes(Lunghezza,Peso), col = "red")

ggarrange(gest,skull,infleng,
          ncol = 2, nrow = 2)

model5 = update(model3,~. +I(Lunghezza^2))
summary(model5) 

model9 = update(model3,~. +I(Cranio^2))
summary(model9)

model10 = update(model3,~. +I(Gestazione^2))
summary(model10)

model6 = update(model5,~. +I(Lunghezza*Cranio))
summary(model6)

model7 = update(model5,~. +I(Gestazione*Cranio))
summary(model7)

model8 = update(model5,~. +I(Gestazione*Lunghezza))
summary(model8)

model11 = update(model5,~. +I(N.gravidanze*Lunghezza))
summary(model11)

model12 = update(model5,~. +I(N.gravidanze*Cranio))
summary(model12)

AIC(model3,model6,model5,model8)
BIC(model3,model6,model5,model8)


#5 
shapiro.test(model8$residuals)
skewness(model8$residuals)
kurtosis(model8$residuals)-3
plot(density(model8$residuals))
lmtest::bptest(model8) 
lmtest::dwtest(model8) 

par(mfrow = c(2,2))
plot(model8)

#Leverage values
n = nrow(newborn)
lev = hatvalues(model8)
plot(lev)
p = sum(lev)
soglia = 2*p/n
abline(h = soglia, col = "red")
length(lev[lev>soglia])

#outliers
plot(rstudent(model8))
abline(h = c(-2,2), col = "red")
outlierTest(model8) 

#Cook's distance
cook =cooks.distance(model8)
plot(cook)
max(cook) 

#6
newborn[1551,1:10]
newborn1 = newborn[-1551,]
model8.1 = lm(Peso ~ N.gravidanze + Gestazione + Lunghezza + Cranio + Sesso 
              + I(Lunghezza^2) + I(Gestazione*Lunghezza), 
              data = newborn1)
summary(model8.1)

model8.2 = update(model8.1,~. -I(Gestazione*Lunghezza))
summary(model8.2)

par(mfrow = c(2,2))
plot(model8.2)
shapiro.test(model8.2$residuals)
lmtest::bptest(model8.2) 
lmtest::dwtest(model8.2) 

AIC(model8,model8.2)
BIC(model8,model8.2)

#7
NB = data.frame(N.gravidanze = 2, Gestazione=39,
                Lunghezza = mean(Lunghezza), Cranio = mean(Cranio),
                Sesso = "F")
predict(model8.2,newdata = NB)      


#8
colors = c("#FFC0CB", "#56B4E9")
colors = colors[as.numeric(Sesso)]
scatterplot3d(Peso~Cranio +Lunghezza,  color = colors, pch = 16)
legend("right", legend = levels(newborn$Sesso),
       col =  c("#FFC0CB", "#56B4E9"), pch = 16)

scatterplot3d(Peso~N.gravidanze + Gestazione,  color = colors, pch = 16)
legend("right", legend = levels(newborn$Sesso),
       col =  c("#FFC0CB", "#56B4E9"), pch = 16)

scatterplot3d(Peso~Lunghezza + Gestazione,  color = colors, pch = 16)
legend("right", legend = levels(newborn$Sesso),
       col =  c("#FFC0CB", "#56B4E9"), pch = 16)

scatterplot3d(Peso~Cranio + Gestazione,  color = colors, pch = 16)
legend("right", legend = levels(newborn$Sesso),
       col =  c("#FFC0CB", "#56B4E9"), pch = 16)


       
