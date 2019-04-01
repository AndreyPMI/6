

library('GGally')       # графики совместного разброса переменных
data <- read.csv("Polish_Comp_Bnkrp_3year_for_models.csv", sep = ';', dec = ",", as.is = T)
str(data)
##################################################################################################data$Atter2 <- NULL
data$class <- as.factor(data$class)
library('GGally')       # графики совместного разброса переменных
library('boot')
library('ISLR')
library('MASS')
data <- read.csv("Polish_Comp_Bnkrp_3year_for_models.csv", sep = ';', dec = ",", as.is = T)
str(data)
data$class <- as.factor(data$class)

my.seed <- 14

ggp <- ggpairs(data[-6])
print(ggp, progress = F)
attach(data)
model.logit <- glm(class ~ Attr1+ Attr2 + Attr3+Attr9+Attr12, data = data, family = 'binomial')
summary(model.logit)
cor(data[-6])
summary(data)
data$Atter2 <- NULL
model.logit <- glm(class ~ Attr1+  Attr3+Attr9+Attr12, data = data, family = 'binomial')
summary(model.logit)
model.logit <- glm(class ~ Attr1+  Attr3+Attr12, data = data, family = 'binomial')
summary(model.logit)

model.logit <- glm(class ~ Attr1+  Attr3, data = data, family = 'binomial')
summary(model.logit)
df.train <- data
cv.err.k.fold <- rep(0, 5)
cv.err.k.fold0 <- cv.glm(df.train, model.logit, K = 10)$delta[1]
data$class <- as.integer(data$class)
data$class <- as.factor(data$class)
str(data)
y <- data$class

for (i in 1:5) {
  fit.glm <- glm(y ~ poly(Attr1, i)+poly(Attr3, i))
  cv.err.k.fold[i] <- cv.glm(data=df.train, fit.glm, K = 10)$delta[1]
}
cv.err.k.fold0
