library(ISLR)
library(tibble)
library(caret)
library(rpart)
library(rpart.plot)
as_tibble(OJ)


uin = 123456789
set.seed(uin)
oj_idx = createDataPartition(OJ$Purchase, p = 0.50, list = FALSE)
oj_trn = OJ[ oj_idx,]
oj_tst = OJ[-oj_idx,]


cv_5 = trainControl(method = "cv", number = 5)
oob  = trainControl(method = "oob")




set.seed(420)
system.time({oj_tree = train(Purchase ~ ., data = oj_trn, trControl = cv_5, method = "rpart")})
set.seed(420)
system.time({oj_rf_oob = train(Purchase ~ ., data = oj_trn, trControl = oob)})
set.seed(420)
system.time({oj_rf_cv  = train(Purchase ~ ., data = oj_trn, trControl = cv_5)})


prp(oj_tree$finalModel)

oj_tree = rpart(Purchase ~ ., data = oj_trn)
prp(oj_tree)

mean(predict(oj_tree, oj_tst) == oj_tst$Purchase)
mean(predict(oj_tree, oj_tst, type = "class") == oj_tst$Purchase)
mean(predict(oj_rf_oob, oj_tst) == oj_tst$Purchase)
mean(predict(oj_rf_cv, oj_tst) == oj_tst$Purchase)







aa = train(Salary ~ ., data = na.omit(Hitters), trControl = oob)
bb = train(log(Salary) ~ ., data = na.omit(Hitters), trControl = oob)


sqrt(mean((predict(aa) - na.omit(Hitters)$Salary) ^ 2))
sqrt(mean((exp(predict(bb)) - na.omit(Hitters)$Salary) ^ 2))
