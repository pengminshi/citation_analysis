DataPath = "~/Dropbox/projects/ADA/Data2016/model/"
# save_data_path = "/Users/PengMinshi/Dropbox/research/citation/data/"
# FigPath = '/Users/PengMinshi/Dropbox/research/citation/notes/notes0410/fig/'

load(paste(DataPath,"citation_year_info.RData",sep = ''))
load(paste(DataPath,'paperRefnewTrim.RData',sep = ''))
load(paste(DataPath,'papernew.RData', sep = ''))




# select papers that have 15 years of citations and have more than 20 citations
paper.ind = which(rowSums(paper_year) >= 20 & papernew$year < 2015-15 & papernew$year > 1977)
# paper.ind = which(rowSums(paper_year) >= 20 & papernew$year < 2010 & papernew$year >= 2000)
(n.papers = length(paper.ind))
zero.year = 1975
m = mean(sapply(1:n.papers, function(i) length(paperRefnewTrim[[paper.ind[i]]])))

##########################################################################################
# # estimate the lambda for each paper
source('Dropbox/research/citation/code/rpp package/rpp.R')
mu0 = 8.5
sigma0 = 1
# # par(mfrow = c(2,3), mai = c(.2,0.5,0.3,0.3))
y.data = sapply(paper.ind, function(i){
    cit.accum = cumsum(paper_year[i,papernew$year[i]:2014 -zero.year])
    n.years = 2014 -papernew$year[i]+1
    Ft = plnorm((1:n.years)*365, meanlog = mu0, sdlog = sigma0)

    citedata = data.frame(Ft = Ft, cit.accum = cit.accum)
    fit = minpack.lm::nlsLM(cit.accum  ~ m*(exp(lambda * Ft)-1), start=list(lambda = 3),
                            data = citedata,control = list(maxiter = 100))
    # if (which(paper.ind==i) %% 200){
    #     plot(papernew$year[i]:2014, cit.accum, type = 'b',pch = 1, ylab = 'accumulative citations', xlab = 'year')
    #     points(papernew$year[i]:2014, m*(exp(summary(fit)$coefficients[1]*Ft)-1),col= 'blue',type = 'b',pch = 3)
    #     lambda = as.character(round(summary(fit)$coefficients[1],2))
    #     legend('topleft', legend = c('real data', paste('fitted with \nlambda =',lambda)),
    #            col = c('black','blue'), pch = c(1,3),bty = 'n')
    # }

    return(summary(fit)$coefficients[1])
})
lambda.nls = y.data
save(lambda.nls, file = '/Users/PengMinshi/Dropbox/research/citation/code/results/lambda.nls.2726.RData')
# par(mfrow = c(1,1), mar = rep(4,4))
################################################################################
## features
source('Dropbox/research/citation/code/loadFeatures.R')
load.out = load_features(paper.ind)
x.data = load.out$df
data = data.frame(x.data, target = y.data)


# plot the features
par(mfrow = c(3,5), mai = c(0.6,0.3,0.1,0.1))
for(i in 1:15){
    plot(x.data[,i], y.data, xlab =names(x.data)[i], ylab = 'lambda',
         col = scales::alpha('black',0.5), cex = 0.5, pch = 16)
    fit = lm( y.data ~ data[,i])
    abline(fit, col='red', lwd=1)
}

ev = eigen(t(as.matrix(x.data)) %*% (as.matrix(x.data)))
pairs(~.,data = x.data[,-c(7,8)], cex = 0.3, pch = 16, col = scales::alpha('black',0.5))

##################################
# construct the dataframe of (t_i, y^d_i), i = 1,..., nd; d = 1, .., N
code.path = '/Users/PengMinshi/Dropbox/research/citation/code/'
source(paste(code.path,'functions.R',sep = ''))

train.prop = 0.8
train.ind = sample(1:n.papers, floor(n.papers * train.prop), replace = F)

df.train = make.nls.data(x.data[train.ind,], papernew, paper_year, paper.ind[train.ind])
# df.test = make.nls.data(x.data[-train.ind,], papernew, paper_year, paper.ind[-train.ind])
################################################################################
# Fit NLS without penalty
################################################################################
var.ind = (1:15)
f.X = paste(sapply(var.ind, function(i) paste(colnames(df.train)[i], '*beta.',
                                              colnames(df.train)[i],sep = '')), collapse = '+')
f = as.formula(paste('yt ~ m *(exp( (',f.X,')* plnorm(t, mu, sigma))-1)'))
paste(sapply(var.ind, function(i) paste('beta.',colnames(df.train)[i],'=0',sep = '')), collapse = ', ')
fit = minpack.lm::nlsLM(formula = f, 
                        start=list( mu = 8, sigma = 1, 
                                    beta.pubyear=0, beta.refsize.log=0, beta.selfcit.prop=0, beta.authorcit.log=0, beta.authorprod.log=0, beta.authorratio.loglog=0, beta.teamsize=0, beta.topjournal=0, beta.jif=0, beta.refcit.log=0, beta.refage.log=0, beta.cocit.log=0, beta.coprod.log=0, beta.npco.log=0, beta.titlelen=0),
                        data = df.train,
                        control = list(maxiter = 100))
summary(fit)

percent.list = c(0.1,0.2,0.3)
year.list = c(5,10,15)
acc.result = matrix(0, nrow = length(percent.list), ncol = length(year.list))
cor.spearman = numeric(length=length(year.list))
cor.pearson = numeric(length=length(year.list))
for(j in 1:length(year.list)){
    t = year.list[j]
    for(i in 1:length(percent.list)){
        p = percent.list[i]
        predcit = predictCit(fit = fit, xdata = x.data[-train.ind,], 
                             var.ind = var.ind, m = m, t.years = t)$citations
        
        test.paper.ind = paper.ind[-train.ind]
        truecit = trueCit(papernew$year[test.paper.ind], paper_year[test.paper.ind,], t.years = t)$citations
        acc.result[i,j] = topcitAccuracy(predcit,truecit, percent = p)
    }
    cor.spearman[j] = cor(predcit,truecit, method = 'spearman')
    cor.pearson[j] = cor(predcit,truecit, method = 'pearson')
}
plotAccResult(result = acc.result, percent.list = percent.list, year.list = year.list)
cat('iter', iter, 'Cor:', cor.spearman,cor.pearson,'\n')



# 
test.paper.ind = paper.ind[-train.ind]
year.pred = matrix(0, nrow = 15, ncol = length(paper.ind[-train.ind]))
year.true = matrix(0, nrow = 15, ncol = length(paper.ind[-train.ind]))
for(t in 1: 15){
    year.pred[t,] = predictCit(fit = fit, xdata = x.data[-train.ind,], 
                         var.ind = var.ind, m = m, t.years = t)$citations
    year.true[t,] = trueCit(papernew$year[test.paper.ind], paper_year[test.paper.ind,], t.years = t)$citations
}

plot_y_yhat(log(c(year.true)+1), log(c(year.pred)+1), xlab = 'true log(y+1)', ylab = 'predicted log(y+1)')
plot_y_yhat(c(year.true), c(year.pred), ylim = 200)
###########################################################################
# fit ols
###########################################################################


train.prop = 0.8
train.ind = sample(1:n.papers, floor(n.papers * train.prop), replace = F)
data.train = data[train.ind,]
data.test = data[-train.ind,]


fit.ols = lm( target ~., data = data.train)
pred = predict(fit.ols, data.test)
cor.ols = plot_y_yhat(data.test$target, pred, log = F)

fit = minpack.lm::nlsLM(cit.accum  ~ m*(exp(lambda * Ft)-1), start=list(lambda = 3),
                        data = citedata,control = list(maxiter = 100))
###########################################################################
# fit lasso
###########################################################################
x.data.scale = scale(x.data)
train.prop = 0.8
train.ind = sample(1:n.papers, floor(n.papers * train.prop), replace = F)
x.train = x.data.scale[train.ind,]
x.test = x.data.scale[-train.ind,]
y.train = y.data[train.ind]
y.test = y.data[-train.ind]


library(glmnet)
cvfit = cv.glmnet(scale(x.train), y.train)

par(mfrow = c(1,3), mai = c(0.6,0.6,0.2,0.1))
plot(cvfit)
coef(cvfit, s = "lambda.1se")



# plot(y.test, y.test.pred, ylim = c(min(y.test),max(y.test)),pch = 16, col=alpha('black',0.5), cex = 0.7)

cut = 8

y.test.pred.min = predict(cvfit, newx = x.test, s = "lambda.min")
plot(y.test, y.test.pred.min,xlim = c(min(y.test), cut),ylim = c(min(y.test),cut),
     pch = 16, col=alpha('black',0.5), cex = 0.7,
     main = 'lambda = lambda.min')
fit = lm(y.test.pred.min[y.test<cut]~y.test[y.test<cut])
abline(a = fit$coefficients[1], b = fit$coefficients[2], lty = 'dashed',lwd = 1.5, col = 'red')

y.test.pred.1se = predict(cvfit, newx = x.test, s = "lambda.1se")
plot(y.test, y.test.pred.1se,xlim = c(min(y.test), cut),ylim = c(min(y.test),cut),
     pch = 16, col=alpha('black',0.5), cex = 0.7,
     main = 'lambda = lambda.1se')
fit = lm(y.test.pred.1se[y.test<cut]~y.test[y.test<cut])
abline(a = fit$coefficients[1], b = fit$coefficients[2], lty = 'dashed',lwd = 1.5, col = 'red')


################################################################
# prediction on test data
test.ind = paper.ind[-train.ind]
n.test = length(test.ind)

percent.list = c(0.1,0.2,0.3)
evalue.year = c(5,10,15)

par(mfrow = c(1,3), mai = c(0.6,0.6,0.2,0.1))
for(percent in percent.list){
    ratio.list = numeric(length = length(evalue.year))
    
    pred.top.ind = test.ind[order(y.test.pred.rf,decreasing = T)[1:ceiling(percent*n.test)]]
    for(i in 1:length(evalue.year)){
        n.year = evalue.year[i]
        cit.after.evalueyear = sapply(test.ind, function(j) 
            cumsum(paper_year[j, (papernew$year[j]:2014)-zero.year])[n.year])
        true.top.ind = test.ind[order(cit.after.evalueyear,decreasing = T)[1:ceiling(percent*n.test)]]
        ratio.list[i] = length(intersect(true.top.ind, pred.top.ind))/length(true.top.ind)
    }
    plot(evalue.year, ratio.list, 
         ylim = c(0, max(ratio.list)*4/3),
         xlab = 'year after publish', ylab = paste("prediction accuracy"),
         main = paste("predict top ", percent*100,"% papers", sep = ''),
         type = 'b', pch = 3, col = 'blue', lty =1)
    points(evalue.year, rep(percent, length(evalue.year)),
           type = 'b', pch = 1, col = 'black', lty = 2)
    legend('top', legend=c('prediction','ramdon guess'),
           pch = c(3,1), lty = c(1,2), col = c('blue','black'))
}
par(mfrow = c(1,1),mar = rep(4,4))

# spearman correlation

evalue.year = c(5,10,15)
spearman.cor.list = numeric(length = length(evalue.year))
pearson.cor.list = numeric(length = length(evalue.year))
for(i in 1:length(evalue.year)){
    n.year = evalue.year[i]
    cit.after.evalueyear = sapply(test.ind, function(j) 
        cumsum(paper_year[j, (papernew$year[j]:2014)-zero.year])[n.year])
    spearman.cor.list[i] = cor(y.test.pred.min,cit.after.evalueyear,method ='spearman')
    pearson.cor.list[i] = cor(y.test.pred.min,cit.after.evalueyear,method ='pearson') 
}
spearman.cor.list
pearson.cor.list



##############################################################################
# random forest
##############################################################################
train.prop = 0.8
data = data.frame(x.data, target = y.data)
train.ind = sample(1:n.papers, floor(n.papers * train.prop), replace = F)
train.data = data[train.ind,]
test.data = data[-train.ind,]

library(randomForest)
library(miscTools)
rf <- randomForest(target ~ ., data=train.data, ntree=20)
y.test.pred.rf =  predict(rf, test.data)
(r2 <- rSquared(test.data$target,test.data$target - y.test.pred.rf))
(cor.spearman <- cor(test.data$target, y.test.pred.rf, method = 'spearman'))
cut = 8
plot(test.data$target,y.test.pred.rf,
     xlim = c(min(test.data$target), cut),ylim = c(min(test.data$target),cut),
     pch = 16, col=alpha('black',0.5), cex = 0.7,
     main = 'random forest ')


#######################################
# prediction on test data
test.ind = paper.ind[-train.ind]
(n.test = length(test.ind))

percent.list = c(0.1,0.2,0.3)
evalue.year = c(5,10,15)

par(mfrow = c(1,3), mai = c(0.6,0.6,0.2,0.1))
for(percent in percent.list){
    ratio.list = numeric(length = length(evalue.year))
    
    pred.top.ind = test.ind[order(y.test.pred.rf,decreasing = T)[1:ceiling(percent*n.test)]]
    for(i in 1:length(evalue.year)){
        n.year = evalue.year[i]
        cit.after.evalueyear = sapply(test.ind, function(j) 
            cumsum(paper_year[j, (papernew$year[j]:2014)-zero.year])[n.year])
        true.top.ind = test.ind[order(cit.after.evalueyear,decreasing = T)[1:ceiling(percent*n.test)]]
        ratio.list[i] = length(intersect(true.top.ind, pred.top.ind))/length(true.top.ind)
    }
    plot(evalue.year, ratio.list, 
         ylim = c(0, max(ratio.list)*4/3),
         xlab = 'year after publish', ylab = paste("prediction accuracy"),
         main = paste("predict top ", percent*100,"% papers", sep = ''),
         type = 'b', pch = 3, col = 'blue', lty =1)
    points(evalue.year, rep(percent, length(evalue.year)),
           type = 'b', pch = 1, col = 'black', lty = 2)
    legend('top', legend=c('prediction','ramdon guess'),
           pch = c(3,1), lty = c(1,2), col = c('blue','black'))
}
par(mfrow = c(1,1),mar = rep(4,4))

# spearman correlation

evalue.year = c(5,10,15)
spearman.cor.list = numeric(length = length(evalue.year))
for(i in 1:length(evalue.year)){
    n.year = evalue.year[i]
    cit.after.evalueyear = sapply(test.ind, function(j) 
        cumsum(paper_year[j, (papernew$year[j]:2014)-zero.year])[n.year])
    spearman.cor.list[i] = cor(y.test.pred.rf,cit.after.evalueyear,method ='spearman')
}
spearman.cor.list

