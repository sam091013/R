#載入套件 & data
if(!require(randomForest)) install.packages("randomForest")
if(!require(nortest)) install.packages("nortest")
if(!require(data.table)) install.packages("data.table")
if(!require(ggplot2)) install.packages("ggplot2")
carseat_train = fread("carseat_train.csv",data.table = F)
carseat_test  = fread("carseat_test.csv" ,data.table = F)

# 1.畫出density plot
qplot(Sales , geom = "density" , data = carseat_train)

# density plot看起來近似常態，為了驗證我們進行檢定
Map(function(x) x(carseat_train$Sales) ,
    list(lillie.test,ad.test,shapiro.test)) 
#lapply(list(lillie.test,ad.test,shapiro.test),function(x) x(carseat_train$Sales))
#Lilliefors p-value = 0.2468
#Anderson-Darling p-value = 0.1543
#Shapiro-Wilk p-value = 0.3256
carseat_train$Sales %>% scale() %>% ks.test("pnorm")
#Kolmogorov-Smirnov p-value = 0.6886
# 2.找到最佳模型、並說明哪個變數跟sales最有關？

# 一般線性 找出所有變數 
lm_carseat = lm(Sales ~ .,data = carseat_train)
summary(lm_carseat) #p<0.05有CompPrice、Income、Advertising、Price、ShelveLocGoo、ShelveLocMedium、Age

# 只列出有關係的變數
lm_carseat_final = lm(Sales ~ CompPrice+Income+Advertising+Price+ShelveLoc+Age ,data = carseat_train)
summary(lm_carseat_final)#移除不相關之變數，調整後Ｒ平方上升0.09
#forward selection
nullModel = lm(Sales~1,data=carseat_train)
fullModel = lm(Sales~(.)^2,data=carseat_train)
lm_carseat_forward = step(nullModel,scope = list(lower = nullModel , upper = fullModel),direction = "forward",trace = 1)
#我們認為使用forward selection來選取變數是不錯的模型，從lm可以找出所有p值<0.05的有CompPrice、Income、Advertising、Price、ShelveLoc、age這些變數，
#移除相對不相關的變數後，調整後的R平方也提升了。

# 3.隨機森林
# 載入套件
library(randomForest)
#找出變數重要性排名
set.seed(1)
rank_CS = randomForest(Sales ~ . ,data = carseat_train)
importance(rank_CS)
#前三名是shelveLoc、Price、Age

# 4. MAE
MAE <- function(actual ,predicet) 
  return(mean(abs(actual - predicet)))

# forward selection 預測值
train_pred <- lm_carseat_forward$fitted.values
test_pred <- predict(lm_carseat_forward , newdata = carseat_test)
# forward selection 跑MAE
MAE(carseat_train$Sales , train_pred) # 0.7740193
MAE(carseat_test$Sales  , test_pred ) # 0.8188417

# 手動選取final 預測值
train_final <- lm_carseat_final$fitted.values
test_final  <- predict(lm_carseat_final , newdata = carseat_test)
# 手動選取final 跑MAE
MAE(carseat_train$Sales , train_final) # 0.8087632
MAE(carseat_test$Sales  , test_final ) # 0.8182953
# 兩種跑MAE會比較傾向使用下面這個手動選取變數的方式，
# 在testing差不多的情況下，針對較重要之變數下去預測不是全部變數也較為精簡

# 5. 10-fold cross-validation MAE
# 載入套件
library(randomForest)

#找出變數重要性排
set.seed(1)
rank_CS = randomForest(Sales ~ . ,data = carseat_train) #rank_CS = rpart::rpart(Sales ~ . ,data = carseat_train)
# 跑MAE
MAE(carseat_train$Sales , predict(rank_CS , newdata = carseat_train)) # 0.62201
MAE(carseat_test$Sales  , predict(rank_CS , newdata = carseat_test )) # 1.48714
# 使用隨機森林做testing太高,沒有符合在0.9以下

# Forward Selection up to 2-way with 10-fold cross-validation MAE
library(caret)
library(lattice)
set.seed(1)
carseat_twoway <- train(Sales ~(.)^2 , data = carseat_train,method = 'leapForward',
                        tuneLength = 60 , trControl = trainControl(method="CV",number = 10))
carseat_twoway 
# 最好的變數為第14個
# nvmax  RMSE      Rsquared   MAE  
# 14     1.071859  0.8631831  0.8474596
finalVar = summary(carseat_twoway$finalModel)
names(finalVar$which[14,])[finalVar$which[14,]]
# 他選擇的變數有
# [1] "(Intercept)"               "CompPrice"                 "Price"                    
# [4] "ShelveLocGood"             "Age"                       "V1:ShelveLocGood"         
# [7] "CompPrice:ShelveLocGood"   "CompPrice:ShelveLocMedium" "Income:Advertising"       
# [10] "Income:ShelveLocGood"      "Income:Age"                "ShelveLocMedium:Age"      
# [13] "ShelveLocMedium:Education" "ShelveLocGood:USYes"       "Education:UrbanYes" 

lm_carseat_forward_cv = lm(Sales ~(CompPrice+Price+ShelveLoc+Age+Income+Advertising+US)^2,
                           data = carseat_train)
# training、testing
MAE(carseat_train$Sales ,  lm_carseat_forward_cv $fitted.values)  # 0.7379105
MAE(carseat_test$Sales , predict(lm_carseat_forward_cv , newdata = carseat_test)) #0.8848878