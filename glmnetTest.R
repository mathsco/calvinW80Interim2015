Credit = read.csv("http://www-bcf.usc.edu/~gareth/ISL/Credit.csv",row.names=1)
x = model.matrix(Balance ~., data=Credit)[,-1]
y = Credit$Balance
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
regParams = 10^seq(10, -2, length=100)
ridge.mod = glmnet(x[train,], y[train], alpha=0, lambda=regParams, thresh=1e-12)

estTestMSE = rep(0,500)
for(lam in 1:500) {
  ridge.pred=predict(ridge.mod, s=lam/10, newx=x[test,], exact=TRUE)
  estTestMSE[lam] = mean((ridge.pred - y[test])^2)
}
plot((1:500)/10,estTestMSE,pch=20,cex=.6,xlab="lambda",ylab="est. test MSE")
print(which.min(estTestMSE)/10)

# Now to use the glmnet routines
cv.out = cv.glmnet(x[train,], y[train], alpha=0,lambda=regParams)
print(cv.out$lambda.min)