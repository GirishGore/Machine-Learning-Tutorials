X = c(2,4,15,20)
Y = c(1,2,3,4)
Z = c(0,0,1,1)
mm1 = lm(X~Z)
res1 = mm1$residuals
mm2 = lm(Y~Z)
res2 = mm2$residuals
cor(res1,res2)
##0.919145
cor(X,Y)
##0.9695016
