library(MASS)
library(ggplot2)
library(magrittr)
library(caret)
library(ROSE)
library(randomForest)


x <- cbind.data.frame(dados$change_mou, dados$churn)
x <- na.omit(x)
aggregate(x$`dados$change_mou`, list(x$`dados$churn`), mean)


rate <- dados$complete_Mean/dados$attempt_Mean

dados <- churn_challenge_data

summary(rate)

x <- cbind.data.frame(rate, dados$churn)
x <- na.omit(x)

aggregate(x$rate, list(x$`dados$churn`), median)


getPalette = colorRampPalette(brewer.pal(9, "Set1"))

X = cbind.data.frame(dados$complete_Mean/dados$attempt_Mean, dados$churn)

library(caret)

ggplot(dados) + geom_bar(aes(x = as.factor(churn)))


ggplot(dados) + geom_bar(aes(as.factor(dados$churn),  as.factor(dados$area))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(dados) + geom_histogram(aes(complete_Mean/attempt_Mean), bins = 50)


hist(dados$complete_Mean/dados$attempt_Mean)
mean(dados$churn)

X <- cbind.data.frame(dados$churn, dados$change_mou, dados$change_rev, dados$attempt_Mean, dados$complete_Mean, dados$custcare_Mean, 
                      dados$months, dados$kid11_15)
colnames(X) <- c('churn', 'change_mou', 'change_rev', 'attempt_Mean', 'complete_Mean', 'custcare_Mean', 'months', 'kid11_15')
X <- na.omit(X)
X <- X[X$change_mou>=-100 & X$change_rev>=-100 & X$change_mou<=250 & X$change_rev<=250,]

X$rate_call <- X$complete_Mean/X$attempt_Mean
X[is.na(X)] <- 0


n_train <- sample(1:nrow(X), 0.8*nrow(X), replace = FALSE)
X_train <- X[n_train,-c(4,5)]
X_test <- X[-n_train,-c(4,5)]


pred <- glm(churn ~ change_mou + change_rev + rate_call + custcare_Mean + months + kid11_15, data = X_train, family = binomial())
y_pred <- as.numeric(predict(pred, newdata = X_test, type = 'response') > 0.5)




roc.curve(X_test$churn, y_ld$class)


p2 <- confusionMatrix(table(as.factor(X_test$churn), y_ld$class), reference = as.factor('1'))


ld <- lda(churn ~ change_mou + change_rev + rate_call + custcare_Mean + months + kid11_15, data = X_train)
y_ld<- predict(ld, newdata = X_test[-c(1)]) 
 

rf <- randomForest(as.factor(churn) ~ change_mou + change_rev + rate_call + custcare_Mean + months + kid11_15, data = X_train, ntree = 50)

y_rf <- predict(rf, X_test[,-1])

cor(X_train[,-c(1,6)])
