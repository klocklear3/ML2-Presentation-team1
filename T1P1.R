##PREDICT NUMBER OF APPLICATIONS RECEIVED

#a.) Split data into train and test
library(ISLR2)
?College
str(College)
set.seed(3)
subset<-sample(nrow(College),nrow(College)*0.7)
train <- College[subset,]
test <- College[-subset,]

#b.) Fit a linear model using least squares on the training set, and report the test error obtained.
lsmodel <- lm(Apps~., data = train)
summary(lsmodel)
predicted.apps<-predict(lsmodel,test)
testerror.ls<-mean((test$Apps-predicted.apps)^2)
testerror.ls

#c.) Fit a ridge regression model on the training set, with λ chosen by cross-validation. Report the test error obtained.
train.mat<-model.matrix(Apps~.,data=train)[,-1]
test.mat<-model.matrix(Apps~.,data=test)[,-1]
grid<-10^seq(4,-2,length=100)
library(glmnet)

ridge<-glmnet(train.mat,train$Apps,alpha=0,lambda=grid)
cv.ridge<-cv.glmnet(train.mat,train$Apps,alpha=0,lambda=grid)
bestlam.ridge<-cv.ridge$lambda.min
bestlam.ridge
pred.newridge<-predict(ridge,s=bestlam.ridge,newx =test.mat)
testerror.r <- mean((test$Apps-pred.newridge)^2)
testerror.r

#d.) Fit a lasso model on the training set, with λ chosen by crossvalidation. Report the test error obtained, along with the number of non-zero coefficient estimates.
lasso<-glmnet(train.mat,train$Apps,alpha=1,lambda=grid)
cv.lasso<-cv.glmnet(train.mat,train$Apps,alpha=1,lambda=grid)
bestlam.lasso<-cv.lasso$lambda.min
bestlam.lasso
pred.newlasso<-predict(lasso,s=bestlam.lasso,newx =test.mat)
testerror.l <- mean((test$Apps-pred.newlasso)^2)
testerror.l
predict(lasso,s=bestlam.lasso,type="coefficients")

#e.) Comment on the results obtained. How accurately can we predict the number of college applications received? Is there much difference among the test errors resulting from these different approaches?
library(ggplot2)
mses <- c(testerror.ls, testerror.r, testerror.l)
name <- c('LM', 'Ridge', 'Lasso')
mse.df <- data.frame(mses, name)
ggplot(data = mse.df, aes(name, mses)) + geom_bar(stat = 'identity') + geom_text(aes(label=round(mses)), vjust=1.6, color="white", size=6)



test.avg <- mean(test$Apps)

lm.r2 <- 1 - mean((predicted.apps - test$Apps)^2) / mean((test.avg - test$Apps)^2)
ridge.r2 <- 1 - mean((pred.newridge - test$Apps)^2) / mean((test.avg - test$Apps)^2)
lasso.r2 <- 1 - mean((pred.newlasso - test$Apps)^2) / mean((test.avg - test$Apps)^2)


r2 <- c(lm.r2, ridge.r2, lasso.r2)
r2.df <- data.frame(r2, name)
ggplot(data = r2.df, aes(name, r2)) + geom_bar(stat = 'identity') + geom_text(aes(label=round(r2,6)), vjust=1.6, color="white", size=6)
