DataPath = "~/Dropbox/projects/ADA/Data2016/model/"
load(paste(DataPath,"citation_year_info.RData",sep = ''))
load(paste(DataPath,'papernew.RData', sep = ''))
load(paste(DataPath,'paperRefnewTrim.RData',sep = ''))

source.path = '/Users/PengMinshi/Dropbox/research/citation/code/'
source(paste(source.path, 'rpp package/rpp.R', sep = ''))

# select papers that have 15 years of citations and have more than 20 citations
papers.ind = which(rowSums(paper_year) >= 20 & papernew$year < 2015-15 & papernew$year > 1977)

(n.papers = length(papers.ind))
year.zero = 1975
papers.list  = lapply(1:n.papers,
                      function(i){
                          papers.selected = paper_year[papers.ind,]
                          papers.year = papernew[papers.ind,'year']
                          paper.title = papernew[papers.ind,'title']

                          years = min(papers.year[i],2014):2014
                          citperyear = papers.selected[i,years-year.zero]
                          return(list(years = years,
                                      citperyear = citperyear,
                                      paper.title = as.character(paper.title[i])))
                      } )

# use the average reference counts as m
m = mean(sapply(paperRefnewTrim[papers.ind], function(x) length(x)))

# ########
# # awakening time
# source(paste(source.path, 'SBfunctions.R', sep = ''))
# SB.list = lapply(1:n.papers, function(i){
#     paper = papers.list[[i]]
#     # sb.out = SBCoefficient3(paper$citperyear, paper$years,
#     #                         k.smoothing = 1, eta = 1/2)
#     sb.out = SBCoefficient2(paper$citperyear, paper$years,
#                             k.smoothing = 0)
# })
# sb.cut = 20

#####
fit.result =  lapply(1:n.papers, function(i) {
    cat(i,'\n')
    # # starting from awakening time if sb coef is greater than mean
    # if( SB.list[[i]]$sleeping.coef > sb.cut){
    #     at = SB.list[[i]]$awaken.time
    #     sleeping.time = at - min(papers.list[[i]]$years +1)
    # } else{
    #     at = NULL
    #     sleeping.time = 0
    # }
    out = rppAnalysis(paper = papers.list[[2586]], m.list = m,
                      fit.nyears = NULL, nyear.extend = 100,
                      real.only = F, k.smoothing = 0, decay.type = 'lognormal',
                      verbose = F, max.iter = 1000, plot = T,
                      mu.init = 8, sigma.init = 1,eps = 10^(-8),
                      n.day = 365, main = NULL, awake.time = 2004,
                      alpha = 3, beta = 1)
    if(out == -1){
        return(-1)
    }else{
        return(list(mu = out$fit.parameters[[1]]$mu,
                    sigma = out$fit.parameters[[1]]$sigma,
                    lambda = out$fit.parameters[[1]]$lambda,
                    converge = out$fit.parameters[[1]]$converge))
                    #sleeping.years = sleeping.time))
    }
})


##############################
# fit NLS model
source.path = '/Users/PengMinshi/Dropbox/research/citation/code/'
source(paste(source.path, 'functions.R', sep = ''))
fit.result = lapply(1:n.papers, function(i){
    cat(i,'\n')
    fitNLS(papers.list[[i]], m)
})

# save(fit.result, papers.ind, n.papers,papers.list,m, file = '/Users/PengMinshi/Dropbox/research/citation/code/results/fit_3_parameter.RData')
# save(fit.result, papers.ind, n.papers,papers.list,m, file = '/Users/PengMinshi/Dropbox/research/citation/code/results/fit_3_parameter_alpha4_beta1.RData')
# save(fit.result, papers.ind, n.papers,papers.list,m, file = '/Users/PengMinshi/Dropbox/research/citation/code/results/fit_3_parameter_alpha3_beta1.RData')
# save(fit.result, papers.ind, n.papers,papers.list,m, file = '/Users/PengMinshi/Dropbox/research/citation/code/results/fit_3_parameter_alpha3_beta1_sleeping20.RData')
#####################################################################
# fillter out the unconverged results
load('/Users/PengMinshi/Dropbox/research/citation/code/results/fit_3_parameter.RData')
load('/Users/PengMinshi/Dropbox/research/citation/code/results/fit_3_parameter_alpha4_beta1.RData')
load('/Users/PengMinshi/Dropbox/research/citation/code/results/fit_3_parameter_alpha3_beta1.RData')
tmp.ind = which(sapply(1:n.papers, function(i) length(fit.result[[i]])!=1))
flag.conv = sapply(tmp.ind, function(i) fit.result[[i]]$converge)
tmp.ind = tmp.ind[flag.conv]
# cut the lambda, so there is not extremly large values
lambda = sapply(tmp.ind, function(i) fit.result[[i]]$lambda)
mu = sapply(tmp.ind, function(i) fit.result[[i]]$mu)
sigma = sapply(tmp.ind, function(i) fit.result[[i]]$sigma)
#sleep.years = sapply(tmp.ind, function(i) fit.result[[i]]$sleeping.years)

papers.list = papers.list[tmp.ind]
papers.ind = papers.ind[tmp.ind]
n.papers = length(papers.ind)
#####################################################################
# comparison of the true accumulative citation counts and the citations counts from model 
source.path = '/Users/PengMinshi/Dropbox/research/citation/code/'
source(paste(source.path, 'functions.R', sep = ''))
source(paste(source.path, 'rpp package/rpp.R', sep = ''))
pred.year = 15
pred.cits = sapply(1:length(papers.list), function(i){
    out = citationFit(to.date = pred.year * 365, m = m,
                      lambda = lambda[i], mu = mu[i], sigma = sigma[i])
    sum(out$yearcounts[1:pred.year])
})
true.cits = sapply(1:length(papers.list), function(i) {
    n.years = length(papers.list[[i]]$citperyear)
    if (n.years < pred.year){
        cat('Warning!\n')
        return(F)
    }
    sum(papers.list[[i]]$citperyear[1:pred.year])
} )
plot_y_yhat(y = true.cits, yhat = pred.cits, log = F,
            ylim = NULL, cut = NULL, main = NULL, xlab = NULL, ylab = NULL)

######################################################################
# load the features
source(paste(source.path, 'loadFeatures.R', sep = ''))
load.out = load_features(paper.ind = papers.ind)
data = cbind(load.out$df,lambda, mu, sigma)
#data = cbind(load.out$df,lambda = lambda.nls, mu, sigma)
# log tranform
data$lambda = log(data$lambda + 1)


# 
# test <- sample(1:n.papers, round(n.papers) * 0.2)
# data.train = data[-test,]
# data.test = data[test,]
# x.train = data.train[,1:15] %>% data.matrix()
# x.test = data.test[,1:15] %>% data.matrix()
# 
# lasso.cvfit.lambda = cv.glmnet(x.train, data.train$sleep.years , alpha = 1)
# pred.sy = predict(lasso.cvfit.lambda, s = lasso.cvfit.lambda$lambda.min, newx = x.test) 
# 
# 
targetFeaturePlot(data, feature.ind = 1:15, target.ind = 16)
#####################################################################
#####################################################################
# the simple multivariate linear regression
# partition
test <- sample(1:n.papers, round(n.papers) * 0.2)
data.train = data[-test,]
data.test = data[test,]

lm.fit = lm(cbind(lambda, mu, sigma) ~., data = data.train)
pred = predict(lm.fit, data.test)

pred.year = 15
pred.cits = sapply(1:nrow(pred), function(i){
    out = citationFit(to.date = pred.year * 365, m = m,
                      lambda = exp(pred[i,'lambda'])-1, mu = pred[i,'mu'], sigma = pred[i,'sigma'])
    sum(out$yearcounts[1:pred.year])
})
true.cits = sapply(test, function(ind) {
    n.years = length(papers.list[[ind]]$citperyear)
    if (n.years < pred.year){
        cat('Warning!\n')
        return(F)
    }
    sum(papers.list[[ind]]$citperyear[1:pred.year])
} )
plot_y_yhat(y = true.cits, yhat = pred.cits, log = F,
            ylim = NULL, cut = NULL, main = NULL, xlab = NULL, ylab = NULL)
#####################################################################
#####################################################################
# simple multivariate linear regression with stepwise selection
# (the performance is slightly worse than the previous one)
test <- sample(1:n.papers, round(n.papers) * 0.2)
data.train = data[-test,]
data.test = data[test,]

# fit lambda
lm.fit.lambda = lm(lambda ~., data = data.train[,1:16])
lm.fit.lambda.step = step(lm.fit.lambda, direction = 'both')
pred.lambda = exp(predict(lm.fit.lambda.step, newdata = data.test[,1:16])) -1

# fit mu
lm.fit.mu = lm(mu ~., data = data.train[,c(1:15,17)])
lm.fit.mu.step = step(lm.fit.mu, direction = 'both')
pred.mu = predict(lm.fit.mu.step, newdata = data.test[,c(1:15,17)])

# fit sigma
lm.fit.sigma = lm(sigma ~., data = data.train[,c(1:15,18)])
lm.fit.sigma.step = step(lm.fit.sigma, direction = 'both')
pred.sigma = predict(lm.fit.sigma.step, newdata = data.test[,c(1:15,18)])

pred.year = 5
pred.cits = sapply(1:length(test), function(i){
    out = citationFit(to.date = pred.year * 365, m = m,
                      lambda = pred.lambda[i], mu = pred.mu[i], sigma = pred.sigma[i])
    sum(out$yearcounts[1:pred.year])
})
true.cits = sapply(test, function(ind) {
    n.years = length(papers.list[[ind]]$citperyear)
    if (n.years < pred.year){
        cat('Warning!\n')
        return(F)
    }
    sum(papers.list[[ind]]$citperyear[1:pred.year])
} )
(p = plot_y_yhat(y = true.cits, yhat = pred.cits, log = F,
                 ylim = NULL, cut = NULL, main = NULL, xlab = NULL, ylab = NULL))

#####################################################################
#####################################################################
# ridge regression (better than lasso?)
library(glmnet)

test <- sample(1:n.papers, round(n.papers) * 0.2)
data.train = data[-test,]
data.test = data[test,]

# lambdas = 10^seq(3, -2, by = -.1)
x.train = data.train[,1:15] %>% data.matrix()
x.test = data.test[,1:15] %>% data.matrix()

ridge.cvfit.lambda = cv.glmnet(x.train, data.train$lambda , alpha = 0)
pred.lambda = exp(predict(ridge.cvfit.lambda, s = ridge.cvfit.lambda$lambda.min, newx = x.test)) - 1

ridge.cvfit.mu = cv.glmnet(x.train, data.train$mu , alpha = 0)
pred.mu = predict(ridge.cvfit.mu, s = ridge.cvfit.mu$lambda.min, newx = x.test)

ridge.cvfit.sigma = cv.glmnet(x.train, data.train$sigma, alpha = 0)
pred.sigma = predict(ridge.cvfit.sigma, s = ridge.cvfit.sigma$lambda.min, newx = x.test)

pred.year = 15
pred.cits = sapply(1:length(test), function(i){
    out = citationFit(to.date = pred.year * 365, m = m,
                      lambda = pred.lambda[i], mu = pred.mu[i], sigma = pred.sigma[i])
    sum(out$yearcounts[1:pred.year])
})
true.cits = sapply(test, function(ind) {
    n.years = length(papers.list[[ind]]$citperyear)
    if (n.years < pred.year){
        cat('Warning!\n')
        return(F)
    }
    sum(papers.list[[ind]]$citperyear[1:pred.year])
} )
(p = plot_y_yhat(y = true.cits, yhat = pred.cits, log = F,
                 ylim = NULL, cut = NULL, main = NULL, xlab = NULL, ylab = NULL))


#####################################################################
#####################################################################
#lasso
library(glmnet)

test <- sample(1:n.papers, round(n.papers) * 0.2)
data.train = data[-test,]
data.test = data[test,]

# lambdas = 10^seq(3, -2, by = -.1)
x.train = data.train[,1:15] %>% data.matrix()
x.test = data.test[,1:15] %>% data.matrix()

lasso.cvfit.lambda = cv.glmnet(x.train, data.train$lambda , alpha = 1)
pred.lambda = exp(predict(lasso.cvfit.lambda, s = lasso.cvfit.lambda$lambda.min, newx = x.test)) - 1

lasso.cvfit.mu = cv.glmnet(x.train, data.train$mu , alpha = 1)
pred.mu = predict(lasso.cvfit.mu, s = lasso.cvfit.mu$lambda.min, newx = x.test)

lasso.cvfit.sigma = cv.glmnet(x.train, data.train$sigma, alpha = 1)
pred.sigma = predict(lasso.cvfit.sigma, s = lasso.cvfit.sigma$lambda.min, newx = x.test)

pred.year = 15
pred.cits = sapply(1:length(test), function(i){
    out = citationFit(to.date = pred.year * 365, m = m,
                      lambda = pred.lambda[i], mu = pred.mu[i], sigma = pred.sigma[i])
    sum(out$yearcounts[1:pred.year])
})
true.cits = sapply(test, function(ind) {
    n.years = length(papers.list[[ind]]$citperyear)
    if (n.years < pred.year){
        cat('Warning!\n')
        return(F)
    }
    sum(papers.list[[ind]]$citperyear[1:pred.year])
} )
(p = plot_y_yhat(y = true.cits, yhat = pred.cits, log = F,
                 ylim = NULL, cut = NULL, main = NULL, xlab = NULL, ylab = NULL))

#####################################################################
#####################################################################
# SVR
library(e1071)

test <- sample(1:n.papers, round(n.papers) * 0.2)
data.train = data[-test,]
data.test = data[test,]

svr.fit.lambda = svm(lambda ~., data = data.train[,1:16],kernel ='radial')
pred.lambda = exp(predict(svr.fit.lambda, newdata = data.test[,1:16])) -1

svr.fit.mu = svm(mu ~., data = data.train[,c(1:15,17)])
pred.mu = predict(svr.fit.mu, newdata = data.test[,c(1:15,17)])

svr.fit.sigma = svm(sigma ~., data = data.train[,c(1:15,18)])
pred.sigma = predict(svr.fit.sigma, newdata = data.test[,c(1:15,18)])

pred.year = 15
pred.cits = sapply(1:length(test), function(i){
    out = citationFit(to.date = pred.year * 365, m = m,
                      lambda = pred.lambda[i], mu = pred.mu[i], sigma = pred.sigma[i])
    sum(out$yearcounts[1:pred.year])
})
true.cits = sapply(test, function(ind) {
    n.years = length(papers.list[[ind]]$citperyear)
    if (n.years < pred.year){
        cat('Warning!\n')
        return(F)
    }
    sum(papers.list[[ind]]$citperyear[1:pred.year])
} )
(p = plot_y_yhat(y = true.cits, yhat = pred.cits, log = F,
                 ylim = NULL, cut = NULL, main = NULL, xlab = NULL, ylab = NULL))

#####################################################################
#####################################################################
# random forest
library(randomForest)

test <- sample(1:n.papers, round(n.papers) * 0.2)
data.train = data[-test,]
data.test = data[test,]

rf.fit.lambda = randomForest(lambda ~ . , data = data.train[,1:16])#, importance=TRUE, ntree = ntree, mtry = mtry)
pred.lambda = exp(predict(rf.fit.lambda, data.test[,1:16]))-1

rf.fit.mu = randomForest(mu ~. , data = data.train[,c(1:15,17)])
pred.mu = predict(rf.fit.mu, data.test[,c(1:15,17)])

rf.fit.sigma = randomForest(sigma ~. , data = data.train[,c(1:15,18)])
pred.sigma = predict(rf.fit.sigma, data.test[,c(1:15,18)])

pred.year = 15
pred.cits = sapply(1:length(test), function(i){
    out = citationFit(to.date = pred.year * 365, m = m,
                      lambda = pred.lambda[i], mu = pred.mu[i], sigma = pred.sigma[i])
    sum(out$yearcounts[1:pred.year])
})
true.cits = sapply(test, function(ind) {
    n.years = length(papers.list[[ind]]$citperyear)
    if (n.years < pred.year){
        cat('Warning!\n')
        return(F)
    }
    sum(papers.list[[ind]]$citperyear[1:pred.year])
} )
(p = plot_y_yhat(y = true.cits, yhat = pred.cits, log = T,
                 ylim = NULL, cut = NULL, main = NULL, xlab = NULL, ylab = NULL))


#####################################################################
#####################################################################
# neural network
library(neuralnet)

maxs = apply(data,2, max)
mins = apply(data,2, min)
data.scale = data.frame(scale(data, center = mins, scale = maxs - mins))

test <- sample(1:n.papers, round(n.papers) * 0.2)
data.train = data.scale[-test,]
data.test = data.scale[test,]

f = formula(paste('lambda~', paste(colnames(data.train[,1:15]), collapse = '+')))
fit.nn.lambda = neuralnet(f , data = data.train[,1:16], hidden = 5, linear.output = T )
pred.lambda = compute(fit.nn.lambda, data.test[,c(1:15)])$net.result 
plot_y_yhat(data.test$lambda, pred.lambda)
#####################################################################
#####################################################################
#####################################################################
#####################################################################
# results over many experiments
library(glmnet)
library(e1071)
library(randomForest)


n.exp = 5
n.method = 7
# initialize
dn = list(exp = 1:n.exp, method = c('lm','lm step','lm ridge','lm lasso','svr.linear','svr.radial','rf'))
cor.lambda.hat = matrix(0, nrow = n.exp, ncol = n.method)
cor.mu.hat =  matrix(0, nrow = n.exp, ncol = n.method)
cor.sigma.hat = matrix(0, nrow = n.exp, ncol = n.method)
cor.pearson.5 =  matrix(0, nrow = n.exp, ncol = n.method)
cor.pearson.10 =  matrix(0, nrow = n.exp, ncol = n.method)
cor.pearson.15 =  matrix(0, nrow = n.exp, ncol = n.method)
cor.log.pearson.5 =  matrix(0, nrow = n.exp, ncol = n.method)
cor.log.pearson.10 =  matrix(0, nrow = n.exp, ncol = n.method)
cor.log.pearson.15 =  matrix(0, nrow = n.exp, ncol = n.method)
cor.spearman.5 =  matrix(0, nrow = n.exp, ncol = n.method)
cor.spearman.10 =  matrix(0, nrow = n.exp, ncol = n.method)
cor.spearman.15 =  matrix(0, nrow = n.exp, ncol = n.method)
cor.log.spearman.5 =  matrix(0, nrow = n.exp, ncol = n.method)
cor.log.spearman.10 =  matrix(0, nrow = n.exp, ncol = n.method)
cor.log.spearman.15 =  matrix(0, nrow = n.exp, ncol = n.method)


for(e in 1: n.exp){
    cat('experiment ', e,'\n')
    for(m in c(1:4,7)){
        test <- sample(1:n.papers, round(n.papers) * 0.2)
        data.train = data[-test,]
        data.test = data[test,]
        
        # 1. simple linear regression
        if(m == 1){
            lm.fit = lm(cbind(lambda, mu, sigma) ~., data = data.train)
            pred = predict(lm.fit, data.test)
            pred.lambda = exp(pred[,'lambda'])-1
            pred.mu = pred[,'mu']
            pred.sigma = pred[,'sigma']
        } else if(m == 2){ # 2. simple linear regression with stepwise selection
            # fit lambda
            lm.fit.lambda = lm(lambda ~., data = data.train[,1:16])
            lm.fit.lambda.step = step(lm.fit.lambda, direction = 'both', trace=0)
            pred.lambda = exp(predict(lm.fit.lambda.step, newdata = data.test[,1:16])) -1
            
            # fit mu
            lm.fit.mu = lm(mu ~., data = data.train[,c(1:15,17)])
            lm.fit.mu.step = step(lm.fit.mu, direction = 'both', trace=0)
            pred.mu = predict(lm.fit.mu.step, newdata = data.test[,c(1:15,17)])
            
            # fit sigma
            lm.fit.sigma = lm(sigma ~., data = data.train[,c(1:15,18)])
            lm.fit.sigma.step = step(lm.fit.sigma, direction = 'both', trace=0)
            pred.sigma = predict(lm.fit.sigma.step, newdata = data.test[,c(1:15,18)])
        } else if(m == 3){# 3. ridge regreesion
            x.train = data.train[,1:15] %>% data.matrix()
            x.test = data.test[,1:15] %>% data.matrix()
            
            ridge.cvfit.lambda = cv.glmnet(x.train, data.train$lambda , alpha = 0)
            pred.lambda = exp(predict(ridge.cvfit.lambda, s = ridge.cvfit.lambda$lambda.min, newx = x.test)) - 1
            
            ridge.cvfit.mu = cv.glmnet(x.train, data.train$mu , alpha = 0)
            pred.mu = predict(ridge.cvfit.mu, s = ridge.cvfit.mu$lambda.min, newx = x.test)
            
            ridge.cvfit.sigma = cv.glmnet(x.train, data.train$sigma, alpha = 0)
            pred.sigma = predict(ridge.cvfit.sigma, s = ridge.cvfit.sigma$lambda.min, newx = x.test)
        } else if(m == 4){ # lasso
            x.train = data.train[,1:15] %>% data.matrix()
            x.test = data.test[,1:15] %>% data.matrix()
            
            lasso.cvfit.lambda = cv.glmnet(x.train, data.train$lambda , alpha = 1)
            pred.lambda = exp(predict(lasso.cvfit.lambda, s = lasso.cvfit.lambda$lambda.min, newx = x.test)) - 1
            
            lasso.cvfit.mu = cv.glmnet(x.train, data.train$mu , alpha = 1)
            pred.mu = predict(lasso.cvfit.mu, s = lasso.cvfit.mu$lambda.min, newx = x.test)
            
            lasso.cvfit.sigma = cv.glmnet(x.train, data.train$sigma, alpha = 1)
            pred.sigma = predict(lasso.cvfit.sigma, s = lasso.cvfit.sigma$lambda.min, newx = x.test)
        } else if(m == 5) { # SVR linear kernel
            tune.out.linear = tune(svm, lambda ~ ., data = data.train[,1:16], kernel="linear", # linear radial
                                   ranges = list(cost =c(0.0001, 0.001,0.01,0.1,1,10)), scale = T)
            svr.fit.lambda = svm(lambda ~., data = data.train[,1:16],kernel ='linear',
                                 cost = tune.out.linear$best.parameters$cost)
            pred.lambda = exp(predict(svr.fit.lambda, newdata = data.test[,1:16])) -1
            
            tune.out.linear = tune(svm, mu ~ ., data = data.train[,c(1:15,17)], kernel="linear", # linear radial
                                   ranges = list(cost =c(0.0001, 0.001,0.01,0.1,1,10)), scale = T)
            svr.fit.mu = svm(mu ~., data = data.train[,c(1:15,17)], kernel = 'linear',
                             cost =  tune.out.linear$best.parameters$cost)
            pred.mu = predict(svr.fit.mu, newdata = data.test[,c(1:15,17)])
            
            tune.out.linear = tune(svm, sigma ~ ., data = data.train[,c(1:15,18)], kernel="linear", # linear radial
                                   ranges = list(cost =c(0.0001, 0.001,0.01,0.1,1,10)), scale = T)
            svr.fit.sigma = svm(sigma ~., data = data.train[,c(1:15,18)], kernel = 'linear',
                                cost =  tune.out.linear$best.parameters$cost)
            pred.sigma = predict(svr.fit.sigma, newdata = data.test[,c(1:15,18)])
        } else if(m == 6){ #svr radial kernel
            tune.out.radial = tune(svm, lambda ~ ., data = data.train[,1:16], kernel="radial", scale = T,
                                   ranges = list(cost = c( 0.001,0.01,0.1,1,10),gamma = c(0,0.5,1,5)))
            svr.fit.lambda = svm(lambda ~., data = data.train[,1:16],kernel ='radial',
                                 cost = tune.out.radial$best.parameters$cost, gamma = tune.out.radial$best.parameters$gamma)
            pred.lambda = exp(predict(svr.fit.lambda, newdata = data.test[,1:16])) -1
            
            tune.out.radial = tune(svm, mu ~ ., data = data.train[,c(1:15,17)], kernel="radial", scale = T,
                                   ranges = list(cost = c( 0.001,0.01,0.1,1,10),gamma = c(0,0.5,1,5)))
            svr.fit.mu = svm(mu ~., data = data.train[,c(1:15,17)], kernel = 'radial',
                             cost = tune.out.radial$best.parameters$cost, gamma = tune.out.radial$best.parameters$gamma )
            pred.mu = predict(svr.fit.mu, newdata = data.test[,c(1:15,17)])
            
            tune.out.radial = tune(svm, sigma ~ ., data = data.train[,c(1:15,18)], kernel="radial", scale = T,
                                   ranges = list(cost = c( 0.001,0.01,0.1,1,10),gamma = c(0,0.5,1,5)))
            svr.fit.sigma = svm(sigma ~., data = data.train[,c(1:15,18)], kernel = 'radial',
                                cost = tune.out.radial$best.parameters$cost, gamma = tune.out.radial$best.parameters$gamma)
            pred.sigma = predict(svr.fit.sigma, newdata = data.test[,c(1:15,18)])
        } else if(m==7){ # rf
            rf.fit.lambda = randomForest(lambda ~ . , data = data.train[,1:16])#, importance=TRUE, ntree = ntree, mtry = mtry)
            pred.lambda = exp(predict(rf.fit.lambda, data.test[,1:16]))-1
            
            rf.fit.mu = randomForest(mu ~. , data = data.train[,c(1:15,17)])
            pred.mu = predict(rf.fit.mu, data.test[,c(1:15,17)])
            
            rf.fit.sigma = randomForest(sigma ~. , data = data.train[,c(1:15,18)])
            pred.sigma = predict(rf.fit.sigma, data.test[,c(1:15,18)])
        }
        
        cor.lambda.hat[e,m] = cor(data.test$lambda, log(pred.lambda+1))
        cor.mu.hat[e,m] = cor(data.test$mu, pred.mu)
        cor.sigma.hat[e,m] = cor(data.test$sigma, pred.sigma)
        
        
        # 5 years prediction
        pred.year = 5
        pred.cits = sapply(1:length(test), function(i){
            out = citationFit(to.date = pred.year * 365, m = m,
                              lambda = pred.lambda[i], mu = pred.mu[i], sigma = pred.sigma[i])
            sum(out$yearcounts[1:pred.year])
        })
        true.cits = sapply(test, function(ind) {
            n.years = length(papers.list[[ind]]$citperyear)
            sum(papers.list[[ind]]$citperyear[1:pred.year])
        })
        cor.pearson.5[e,m] = cor(pred.cits, true.cits, method = 'pearson')
        cor.spearman.5[e,m] = cor(pred.cits, true.cits, method = 'spearman')
        cor.log.pearson.5[e,m] = cor(log(pred.cits+1), log(true.cits+1), method = 'pearson')
        cor.log.spearman.5[e,m] = cor(log(pred.cits+1),log(true.cits+1), method = 'spearman')
        
        # ten years prediction
        pred.year = 10
        pred.cits = sapply(1:length(test), function(i){
            out = citationFit(to.date = pred.year * 365, m = m,
                              lambda = pred.lambda[i], mu = pred.mu[i], sigma = pred.sigma[i])
            sum(out$yearcounts[1:pred.year])
        })
        true.cits = sapply(test, function(ind) {
            n.years = length(papers.list[[ind]]$citperyear)
            sum(papers.list[[ind]]$citperyear[1:pred.year])
        })
        cor.pearson.10[e,m] = cor(pred.cits, true.cits, method = 'pearson')
        cor.spearman.10[e,m] = cor(pred.cits, true.cits, method = 'spearman')
        cor.log.pearson.10[e,m] =  cor(log(pred.cits+1), log(true.cits+1), method = 'pearson')
        cor.log.spearman.10[e,m] = cor(log(pred.cits+1),log(true.cits+1), method = 'spearman')
        
        # ten years prediction
        pred.year = 15
        pred.cits = sapply(1:length(test), function(i){
            out = citationFit(to.date = pred.year * 365, m = m,
                              lambda = pred.lambda[i], mu = pred.mu[i], sigma = pred.sigma[i])
            sum(out$yearcounts[1:pred.year])
        })
        true.cits = sapply(test, function(ind) {
            n.years = length(papers.list[[ind]]$citperyear)
            sum(papers.list[[ind]]$citperyear[1:pred.year])
        })
        cor.pearson.15[e,m] = cor(pred.cits, true.cits, method = 'pearson')
        cor.spearman.15[e,m] = cor(pred.cits, true.cits, method = 'spearman')
        cor.log.pearson.15[e,m] =  cor(log(pred.cits+1), log(true.cits+1), method = 'pearson')
        cor.log.spearman.15[e,m] = cor(log(pred.cits+1),log(true.cits+1), method = 'spearman')
        
    }
}
pr <- function(x, d = 2){
    x = round(x,2)
    paste(x, collapse = ' & ')
}

pr(apply(cor.lambda.hat, 2, mean))
pr(apply(cor.mu.hat, 2, mean))
pr(apply(cor.sigma.hat, 2, mean))

pr(apply(cor.pearson.5, 2,mean))
pr(apply(cor.pearson.10, 2,mean))
pr(apply(cor.pearson.15, 2,mean))
pr(apply(cor.log.pearson.5, 2,mean))
pr(apply(cor.log.pearson.10, 2,mean))
pr(apply(cor.log.pearson.15, 2,mean))
pr(apply(cor.spearman.5, 2,mean))
pr(apply(cor.spearman.10, 2,mean))
pr(apply(cor.spearman.15, 2,mean))
pr(apply(cor.log.spearman.5, 2,mean))
pr(apply(cor.log.spearman.10, 2,mean))
pr(apply(cor.log.spearman.15, 2,mean))




############################################################################################
# analyze why fit for lambda is worse than fitting only one parameter
load('/Users/PengMinshi/Dropbox/research/citation/code/results/fit_3_parameter_alpha3_beta1.RData')
load('/Users/PengMinshi/Dropbox/research/citation/code/results/lambda.nls.2726.Rdata')
source.path = '/Users/PengMinshi/Dropbox/research/citation/code/'
source(paste(source.path, 'functions.R', sep = ''))

flag.conv = sapply(1:n.papers, function(i) fit.result[[i]]$converge)
# cut the lambda, so there is not extremly large values
lambda = sapply(1:n.papers, function(i) fit.result[[i]]$lambda)
flag = flag.conv & lambda <= max(lambda)

lambda = lambda[flag]
mu = sapply(1:n.papers, function(i) fit.result[[i]]$mu)[flag]
sigma = sapply(1:n.papers, function(i) fit.result[[i]]$sigma)[flag]

plot_y_yhat(lambda.nls, lambda, cut=9, legend = T,
            xlab = expression(lambda*' when '*mu*','*sigma*' fixed'),
            ylab = expression(lambda*' when fitting 3 params'))

total.cit = sapply(1:n.papers, function(i) sum(papers.list[[i]]$citperyear))
plot(log(lambda+1),log(total.cit+1), pch = 16, col = scales::alpha('black',0.5),cex = 0.7, 
     xlab =  expression(lambda*' when fitting 3 params'), ylab ='total citatio counts(log)')
plot( log(lambda.nls+1),log(total.cit+1), pch = 16, col = scales::alpha('black',0.5),cex = 0.7, 
     xlab = expression(lambda*'(log) when '*mu*','*sigma*' fixed'), ylab ='total citatio counts(log)')
plot(mu, sapply(1:n.papers, function(i) length(papers.list[[i]]$years)))

c = mu
gr = .bincode(c, seq(min(c), max(c), len=length(c)), include.lowest = T) # heat color
col = colorRampPalette(c("yellow", "blue"))(length(c))[gr]
plot(lambda.nls, lambda,col = col,
     cex = 0.3, pch = 16,
     xlab = expression(lambda*' when '*mu*','*sigma*' fixed'),
     ylab = expression(lambda*' when fitting 3 params'))
legend(5.6,12.5, legend= expression(' color =  '*mu*' ') )
abline(b=1,a = 0, lty = 'dashed')

# color.bar( colorRampPalette(c("yellow", "blue"))(100),min = 0.2, max =1)
# example 2586

#####################################################
# check whether those weird points have high sleeping coefficent
source(paste(source.path, 'SBfunctions.R', sep = ''))
B.list = sapply(1:n.papers, function(i){
    paper = papers.list[[i]]
    # sb.out = SBCoefficient3(paper$citperyear, paper$years,
    #                         k.smoothing = 1, eta = 1/2)
    # 345 387 400 500 606 874
    sb.out = SBCoefficient2(paper$citperyear, paper$years,
                            k.smoothing = 0)
    return(sb.out$sleeping.coef)
})

b = sqrt(B.list -min(B.list)+1)
gr = .bincode(b, seq(min(b), max(b), len=length(b)), include.lowest = T) # heat color
col = colorRampPalette(c("yellow", "blue"))(length(b))[gr]

plot(lambda.nls, lambda, col = col, pch = 16, cex = (b-min(b)+0)/(max(b)-min(b)+0),
     xlab = expression(lambda*' when '*mu*','*sigma*' fixed'),
     ylab = expression(lambda*' when fitting 3 params'), ylim = c(1,14.4))
legend(3,12.5, legend= expression(' color =  Sleeping Coef ') )
abline(b=1,a = 0, lty = 'dashed')
