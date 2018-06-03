DataPath = "~/Dropbox/projects/ADA/Data2016/model/"
load(paste(DataPath,"citation_year_info.RData",sep = ''))
load(paste(DataPath,'papernew.RData', sep = ''))


# Consider the papers published between 1981-2006, a twenty-five year period
ind.year = which(papernew$year < 2006 &papernew$year>=1981)
# the average citations among these papers
mean(rowSums(paper_year[ind.year,])) # 7.8
paper.ind = ind.year[which(rowSums( paper_year[ind.year,] )>= 8)]
(n.papers = length(paper.ind)) # 8738
zero.year = 1975

########################################################################
# labeling
highcit.rate = 0.1
y.data = numeric(length=n.papers)
for(y in c(1981, 1986, 1991, 1996, 2001)){
    pub.year = papernew$year[paper.ind]
    ind = which(pub.year >= y & pub.year < y + 5)
    total.ind = paper.ind[ind]
    ordered.ind = ind[order(rowSums(paper_year[total.ind,]),decreasing = T)] 
    y.data[ordered.ind[1:round(length(ind) * highcit.rate)]] = 1
    cat(length(ind),'\n')
}

########################################################################
# features
source('Dropbox/research/citation/code/loadFeatures.R')
load.out = load_features(paper.ind)
x.data = load.out$df
data = data.frame(x.data, class = as.factor(y.data))

# # plot the features
# par(mfrow = c(3,5), mai = c(0.6,0.3,0.1,0.1))
# for(i in 1:15){
#     plot(x.data[,i], y.data, xlab =names(x.data)[i], ylab = 'lambda',
#          col = scales::alpha('black',0.5), cex = 0.5, pch = 16)
#     fit = lm( y.data ~ x.data[,i])
#     abline(fit, col='red', lwd=1)
# }
# par(mfrow = c(1,1), mar = rep(4,4))
targetFeaturePlot(data, feature.ind = 1:15, target.ind = 16)

###############################################################
# random forest
# source('/Users/PengMinshi/Dropbox/research/citation/code/functions.R')
require(randomForest)

n.exp = 20
ab = numeric(length = n.exp)
for( e in 1:n.exp){
    train <- caret::createDataPartition(y = data$class, p = 0.8, list = FALSE)
    data.train = data[train,]
    data.test = data[-train,]
    
    train.train <- caret::createDataPartition(y = data.train$class, p = 0.8, list = FALSE)
    data.train.train = data.train[train.train,]
    data.train.label = data.train[-train.train,]
    
    # # ROC curve
    # ntree.list = c(100,150,200,250)
    # mtry.list = 3:10
    # rf = randomForest(class ~ . , data = data.train.train, importance=TRUE, ntree = 200, mtry = 6)
    # pred.test = predict(rf, data.test, type = 'prob')[,2]
    # roccurve_rf <- pROC::roc(data.test$class ~ pred.test)
    # plot(1-roccurve_rf$specificities, roccurve_rf$sensitivities,
    #      type  ='l', pch = '.', lty = 1, lwd = 2.6, main = 'ROC curve',
    #      xlab = 'FPR (1- specificity)', ylab = 'TPR (sensitivity)',
    #      cex.lab = 1.2, cex.main = 1.4,  col = 'blue')
    # abline(a = 0,b=1, lty = 'dashed',lwd = 2)
    # legend(0.47,0.26,legend = '',lty = 1, col = 'blue', bty = 'n',lwd = 2.6)
    # temp <- locator(1)
    # text(temp,'ROC', cex = 1.5)
    # temp <- locator(1)
    # text(temp,'Random forest', cex = 1)
    # # classification tree
    # for( i in 2:length(mtry.list)){
    #     rf = randomForest(class ~ . , data = data.train, importance=TRUE, ntree = 200, mtry = mtry.list[i])
    #     pred.test = predict(rf, data.test, type = 'prob')[,2]
    # 
    #     roccurve_rf <- pROC::roc(data.test$class ~ pred.test)
    #     points(1-roccurve_rf$specificities, roccurve_rf$sensitivities,
    #          type  ='l', pch = '.', lty =i , col = i,lwd = 2)
    #     # plot(roccurve_rf)
    # }
    # legend.text = sapply(1:length(mtry.list), function(i) paste('mtry=', mtry.list[i]))
    # legend('bottomright', legend = legend.text, lty = 1:length(mtry.list),
    #        col = 1:length(mtry.list), lwd = 2)

    # select the threshold on test data by cross validation
    ntree = 200
    mtry = 6
    rf = randomForest(class ~ . , data = data.train.train, importance=TRUE, ntree = ntree, mtry = mtry)
    
    pred.train = predict(rf, data.train.label, type = 'prob')[,2]
    roc = pROC::roc(data.train.label$class ~ pred.train)
    threshold = roc$thresholds[which.max(roc$sensitivities + roc$specificities)]
    
    pred.test = predict(rf, data.test, type = 'prob')[,2]
    pred.label = as.factor(as.numeric(pred.test>threshold))
    conf = caret::confusionMatrix(pred.label, data.test$class)
    ab[e] = conf$byClass[length(conf$byClass)]

}

###############################################################
# SVM
library("e1071")

n.exp = 20
result = numeric(length = n.exp)
param = numeric(length = n.exp)

for(i in 1:n.exp){
    train <- caret::createDataPartition(y = data$class, p = 0.8, list = FALSE)
    # train = createPartition(data$class, 0.8)
    data.train = data[train,]
    data.test = data[-train,]
    
    cat('i=',i,'\n')
    # tune.out.linear = tune(svm, class~ ., data = data.train, kernel="linear", # linear
    #                        ranges = list(c(0.0001, 0.001,0.01,0.1,1,10,100)), scale = T,
    #                        class.weights= c("0" = 1, "1" = 9))
    # tune.out.radial = tune(svm, class~ ., data = data.train, kernel="radial", # linear
    #                        ranges = list(gamma=c(5,10,50,100), cost =c(0.001,0.1,0.5)),
    #                        class.weights= c("0" = 1, "1" = 9),scale = T)
    tune.out.sigmoid = tune(svm, class~ ., data = data.train, kernel="sigmoid", # linear
                            ranges = list(gamma=c(10), cost =c(1,10),coef0 = c(0)),
                            class.weights= c("0" = 1, "1" = 9))
    # tune.out.poly = tune(svm, class~ ., data = data.train, kernel="sigmoid", # linear
    #                      ranges = list(gamma=c(4,6,8), cost =c(0.5,1,2,5,10), coef0 = c(0,5,10), degree = 3:5),
    #                      class.weights= c("0" = 1, "1" = 9))
    #cat('param = ',tune.out.linear$best.parameters[[1]],'\n' )
    svm_model <- svm(class ~ ., data=data.train, kernel="sigmoid",
                     scale = T,cost= 1,gamma = 10,coef0 = 0,
                     class.weights= c("0" = 1, "1" = 9))
    pred <- predict(svm_model,data.test)
    (conf = caret::confusionMatrix(pred, data.test$class))
    result[i] =  conf$byClass[length(conf$byClass)]
    param[i] = tune.out.linear$best.parameters[[1]]
    # conf = table(pred, data.test$class)
    # result[i] = 1/2*(conf[1,1]/(conf[1,1]+conf[2,1]) + conf[2,2]/(conf[1,2] + conf[2,2]))
}


###############################################################
# eigen analysis
eigen.out = eigen(t(as.matrix(x.data)) %*% as.matrix(x.data))
plot(log(eigen.out$values))

# PCA analysis
pc = prcomp(x.data, scale = T)
summary(pc)

pc.data = as.data.frame(pc$x[,1:10])
pc.data = data.frame(pc.data, class = as.factor(data$class))



result = numeric(length = 10)
for(i in 1:length(result)){
    train <- caret::createDataPartition(y = data$class, p = 0.8, list = FALSE)
    data.train = pc.data[train,]
    data.test = pc.data[-train,]
    
    tune.out.linear = tune(svm, class~ ., data = data.train, kernel="linear", # linear radial
                           ranges = list(cost =c(0.001, 0.001,0.01,0.1,1,10)), scale = T,
                           class.weights= c("0" = 1, "1" = 9))
    
    tune.out.linear$best.parameters
    svm_model <- svm(class ~ ., data=data.train, kernel="radial", scale = T, 
                     class.weights= c("0" = 1, "1" = 9))

    pred <- predict(svm_model,data.test)
    (conf = caret::confusionMatrix(pred, data.test$class))
    result[i] =  conf$byClass[length(conf$byClass)]
}












