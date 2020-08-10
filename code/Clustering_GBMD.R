DataPath = "~/Dropbox/projects/ADA/Data2016/model/"


## LOAD DATA
load(paste(DataPath,"papernew.RData",sep = ''))
load(paste(DataPath,"citation_year_info.RData",sep = ''))
load(paste(DataPath,'journal_info.RData',sep = ''))

source('~/Desktop/ADA/code/myfunc.R')
#
# ############################################################################
# # Construct the data set at currate of 0.025
# ############################################################################
# PARAMETERS
citecutrate = 0.10
# paperyear = papernew$year
zero = 1975
years = c(1976,1981,1986,1991,1996)

# consider only the papers having ciations greater or equal to MINIMUM
MINIMUM = 5
ind = which(rowSums(paper_year) > MINIMUM)
paper_ind = papernew[ind,]
npaper_ind = nrow(paper_ind)
paperyear_ind = paper_ind$year
paper_year_ind = paper_year[ind,]


# Find the top citecutrate percent cited paper published between years[i] and years[i]+5
data = matrix(0 ,nrow = 0,ncol = 39)
data_norm = matrix(0 ,nrow = 0,ncol = 39)
indicator = rep(0,length(ind))

for(i in 1:length(years)){
    y = years[i]
    ind_between = which(paperyear_ind >= y & paperyear_ind < y+5)
    ind_between_sort = sort(rowSums(paper_year_ind[ind_between,]),decreasing = T,index.return = T)
    ind_between_sort = ind_between_sort$ix
    ind_topcite = ind_between[ind_between_sort[1:floor(length(ind_between)*citecutrate)]]
    l = length(ind_topcite)
    indicator[ind_topcite] = 1
    print(l)
    # smoothing
    citesmooth = smoothing_nyear(paper_year_ind[ind_topcite,(y-zero):39], 5)
    citesmoothnorm = citesmooth/rowSums(citesmooth)
    if( y - zero > 1){
        na_matrix = matrix(NA, nrow = l, ncol = y-zero-1)
        data = rbind(data, cbind(citesmooth,na_matrix))
        data_norm = rbind(data_norm, cbind(citesmoothnorm,na_matrix))
    }else{
        data = rbind(data, citesmooth)
        data_norm = rbind(data_norm, citesmoothnorm)
    }
}

n = nrow(data)
yeardf = matrix(rep(1:39,n),nrow = n, ncol = 39, byrow = T)
data = as.data.frame(cbind(yeardf, data))
data_norm = as.data.frame(cbind(yeardf, data_norm))

# df_name = character()
# for(i in 1:39) df_name = c(df_name, paste('p',as.character(i),sep = ''))
# for(i in 1:39) df_name = c(df_name, paste('obs',as.character(i),sep = ''))

# colnames(data) = df_name
# colnames(data_norm) = df_name

paper_ind = ind[indicator > 0]

DataPath2 = "~/Desktop/ADA/code/clustering_GBMD_data"
save(data, paper_ind, file = paste(DataPath2,'/data.RData',sep = ""))
save(data_norm, paper_ind, file = paste(DataPath2,'/data_norm.RData',sep = ""))


##################################################################################################################
# 
DataPath = "~/Dropbox/projects/ADA/Data2016/model/"
DataPath2 = "~/Dropbox/projects/ADA/code/clustering_GBMD_data"
load(paste(DataPath2,'/data_norm.RData',sep = ""))
# write.dbf(data_norm, paste(DataPath2,'/data_norm_sas.dbf',sep = "")) # transfer to sas data file

# output the label and probability from GBMD model
library(readr)
csvdata <- read_csv(paste(DataPath2, '/data_output.csv',sep = ""))
group_prob = csvdata[,80:83]
paper_label = csvdata[,84]



source('~/Dropbox/projects/ADA/code/myfunc.R')
load(paste(DataPath,"papernew.RData",sep = ''))
load(paste(DataPath,'paperRefnewTrim.RData',sep = ''))
load(paste(DataPath,"author_year_info.RData",sep = ''))
load(paste(DataPath,'authorinfo.RData',sep=''))
load(paste(DataPath,'journal_info.RData',sep = ''))
load(paste(DataPath,'second_order_statistics.RData',sep = ''))
load(paste(DataPath,'selfcit_info.RData',sep = ''))

ind = paper_ind
label = paper_label$GROUP
label[label == 4] = 1



paperdf = papernew[ind,]
paperref = paperRefnewTrim[ind]
authorlist = authorlistnew[ind]
journalid = journal_info$id[ind]


npaper = length(ind)
zero =1975

# publish year
Pubyear = paperdf$year #>

# length of title
space_title = str_replace_all(paperdf$title,"-"," ")
titleLength = unlist(lapply(1:length(space_title),function(i) length(strsplit(space_title[i], " ")[[1]])))

# number of reference
RefCount = sapply(paperref, length) #>
logFixRefCount = rep(0,npaper)
for(i in 1:length(RefCount)){
    logFixRefCount[i] = log(RefCount[i]/ (Pubyear[i]-zero) * 40+1)
}

# self citation proportion 
selfcitProp = selfcitprop[ind]



# total number of citations of the authors of the paper
TotalCit = numeric(npaper)
for(i in 1:npaper){
    tempid = authorlist[[i]]$newID
    id = tempid[tempid>0]
    if(length(id) > 1){
        if( Pubyear[i]-zero == 1)
            TotalCit[i] = sum(authorcited_year[id,1])
        else
            TotalCit[i] = sum(rowSums(authorcited_year[id,1:(Pubyear[i]-zero)]))
    }else if(length(id) == 1){
        TotalCit[i] = sum(authorcited_year[id,1:(Pubyear[i]-zero)])
    }
}


# Average number of citations of the authors of the paper
AveCit = numeric(npaper)
for(i in 1:npaper){
    tempid = authorlist[[i]]$newID
    id = tempid[tempid>0]
    if(length(id) > 1){
        if( Pubyear[i]-zero == 1)
            AveCit[i] = sum(authorcited_year[id,1])/length(id)
        else
            AveCit[i] = sum(rowSums(authorcited_year[id,1:(Pubyear[i]-zero)]))/length(id)
    }else{
        AveCit[i] = sum(authorcited_year[id,1:(Pubyear[i]-zero)])
    }
}


# total number of publications of the author
TotalPub = numeric(npaper)
for(i in 1:npaper){
    tempid = authorlist[[i]]$newID
    id = tempid[tempid>0]
    if(length(id) > 1){
        if( Pubyear[i]-zero == 1)
            TotalPub[i] = sum(authorpub_year[id,1])
        else
            TotalPub[i] = sum(rowSums(authorpub_year[id,1:(Pubyear[i]-zero)]))
    }else{
        TotalPub[i] = sum(authorpub_year[id,1:(Pubyear[i]-zero)])
    }
}



# Average number of publications of the author
AvePub = numeric(npaper)
for(i in 1:npaper){
    tempid = authorlist[[i]]$newID
    id = tempid[tempid>0]
    if(length(id) > 1){
        if( Pubyear[i]-zero == 1)
            AvePub[i] = sum(authorpub_year[id,1])/length(id)
        else
            AvePub[i] = sum(rowSums(authorpub_year[id,1:(Pubyear[i]-zero)]))/length(id)
    }else{
        AvePub[i] = sum(authorpub_year[id,1:(Pubyear[i]-zero)])
    }
}


#  average citation/publication ratio
AveCitPubRatio = numeric(npaper)
for(i in 1:npaper){
    tempid = authorlist[[i]]$newID
    id = tempid[tempid>0]
    if(length(id) > 1){
        if( Pubyear[i]-zero == 1)
            AveCitPubRatio[i] = sum(authorcited_year[id,1]/max(authorpub_year[id,1],1))/length(id)
        else
            AveCitPubRatio[i] = sum(rowSums(authorcited_year[id,1:(Pubyear[i]-zero)])/
                                        max(rowSums(authorpub_year[id,1:(Pubyear[i]-zero)]),1))/length(id)
    }else{
        AveCitPubRatio[i] = sum(authorcited_year[id,1:(Pubyear[i]-zero)]/
                                    max(authorpub_year[id,1:(Pubyear[i]-zero)],1))
    }
}

# number of authors
AuthorN = numeric(npaper)
for(i in 1:npaper){
    temp = nrow(authorlist[[i]])
    if(length(temp) > 0){
        AuthorN[i] = temp
    }
}



# journal Aos Bmtrk JASA JRSS-B 
# Journal4 = journalid
# Journal4[journalid != 4 & journalid != 10 & journalid != 20 & journalid != 21] = 0 # 4 journals and the other
# Journal4 = as.factor(Journal4)
# Journal4 = sapply(levels(Journal4), function(x) as.integer(x == Journal4))

# keywords
# title = tolower(as.character(paperdf$title))
# title <- gsub("[^a-zA-Z]+", " ", title)
# biKeyword = trim_text_bigram(title,20)
# Keyword = trim_text(title,40)


# maximum citations of papers of the authors & age
Maxcitpaper = numeric(npaper)
Maxcitpubyear = numeric(npaper)
for(i in 1:npaper){
    tempid = authorlist[[i]]$newID
    id = tempid[tempid>0]
    if(length(id) > 0){
        tempmax = papermaxcitebefore_author_year[id,(Pubyear[i]-zero)]
        maxid = which.max(tempmax)
        Maxcitpaper[i] = tempmax[maxid]
        Maxcitpubyear[i] = papermaxcitebefore_author_year_pubyear[id[maxid],(Pubyear[i]-zero)]
    }
}
# MaxPaperCit = Maxcitpaper
# MaxPaperCit_PaperAge = Maxcitpubyear


# citation of collaborators
Collcit = numeric(npaper) #total citations of all the collaborators
Collavecit = numeric(npaper)
Collpub = rep(0,npaper) # total publications of all the collaborators
Collavepub = rep(0,npaper)
Ncoll = numeric(npaper)
for(i in 1:npaper){
    tempid = authorlist[[i]]$newID
    id = tempid[tempid>0]
    if(length(id) > 0){
        Collcit[i] = sum(totalcitofcollaborators[id,(Pubyear[i]-zero)])
        Collpub[i] = sum(totalpubofcollaborators[id,(Pubyear[i]-zero)])
        Ncoll[i] = sum(Ncollaborators[id,(Pubyear[i]-zero)])
        if( Ncoll[i] > 0 ){
            Collavecit[i] = Collcit[i]/Ncoll[i]
            Collavepub[i] = Collpub[i]/Ncoll[i]
        } 
    }
}
# CollaboratorTotalCit = Collcit
# CollaboratorTotalPub = Collpub
# Ncollaborator = Ncoll


# average cites of the reference papers
Refavecit = refaveciteperyear[ind]
# average age of the reference papers
Refavepubyear = refavepubyear[ind]


Journal4 = journalid
Journal4[journalid != 4 & journalid != 10 & journalid != 20 & journalid != 21] = 0 # 4 journals and the other
#Journal4[journalid == 4 | journalid == 10 | journalid == 20 | journalid == 21] = 1
#Journal4 = as.factor(Journal4)
#Journal4 = sapply(levels(Journal4), function(x) as.integer(x == Journal4))

df = data.frame(titleLength = titleLength, logNRef = logFixRefCount,
                selfcitProp = selfcitProp,Journal = as.factor(Journal4), #logTotalCitofAuthor = log(TotalCit+1),
                logAveCitPubRatio = log(AveCitPubRatio+1), 
                logAveCitofAuthor = log(AveCit+1), #logTotalPubofAuthor = log(TotalPub+1),
                NAuthor = AuthorN,  logAvePubofAuthor = log(AvePub+1),
                # Kw = Keyword,  Kw2 = biKeyword, 
                MaxPaperCit_PaperAge = Maxcitpubyear, logMaxPaperCit = log(Maxcitpaper+1), 
                #logCollaboratorTotalCit = log(Collcit+1),
                logCollaboratorTotalPub = log(Collpub+1),#logCollaboratorAveCit = log(Collavecit+1),  
                logNcollaborator  = log(Ncoll+1), #logCollaboratorAvePub = log(Collavepub+1), 
                logRefAveCit = log(Refavecit+1), logRefAveAge = log(Refavepubyear+1),
                Pubyear = (Pubyear-1976),
                Label = as.factor(label))

#########################################
# random forest
library(randomForest)
# Regression model
f = paste(names(df[,-length(names(df))]),collapse =  '+')
f = paste('Label~',f,sep = '')
# Keywordinteration = paste('(',paste(names(df[,8:67]),collapse =  '+'),')*Pubyear',sep = '')
# f = paste(f,'+MaxPaperCit_PaperAge*MaxPaperCit+RefAveCit*NRef+ RefAveAge*NRef+',Keywordinteration, sep = '')
rf <- randomForest(formula(f) ,data = df, importance=TRUE, ntree = 100)
varImpPlot(rf)
1 - mean(rf$err.rate[,1]) # out of bag error?

##########################################
# Logistic regression 
# the weight of dftrain for logistic regression is for the unbalanced class
# equals inverse of number of obs in that class
logweights = rep(0, nrow(dftrain))
for(i in 1:nlevels(dftrain$Label)){ 
    logweights[dftrain$Label == levels(dftrain$Label)[i]] = 1/sum(dftrain$Label == levels(dftrain$Label)[i])
}

# library(nnet)

# f = paste(names(df[,-length(names(df))]),collapse =  '+')
# f = paste('Label~',f,sep = '')
# # Keywordinteration = paste('(',paste(names(df[,8:67]),collapse =  '+'),')*Pubyear',sep = '')
# # f = paste(f,'+MaxPaperCit_PaperAge*MaxPaperCit+RefAveCit*NRef+ RefAveAge*NRef+',Keywordinteration, sep = '')
# fit = multinom(f,data=dftrain,family = binomial(),weights = logweights)
# predtest = as.factor(apply(predict(fit,newdata=dftest,type="probs"),1,which.max))
# confusionMatrix(predtest, dftest$Label)

# ##########################################
# inference
# logistic regression 
library(nnet)
# f = paste(names(df[,-length(names(df))]),collapse =  '+')
# f = paste('Label~',f,sep = '')
logweights = rep(0, nrow(df))
for(j in 1:nlevels(df$Label)){
    logweights[df$Label == levels(df$Label)[j]] = 1/sum(df$Label == levels(df$Label)[j])
}
f = as.formula(Label~titleLength+logNRef+selfcitProp+Journal+logAveCitPubRatio+
                   logNcollaborator+NAuthor+
                   (logCollaboratorTotalPub+logRefAveCit+logMaxPaperCit+
                        logAveCitofAuthor+logAvePubofAuthor+logRefAveAge+MaxPaperCit_PaperAge)*Pubyear)

reg = multinom(f,data=df,family = binomial(),weights = logweights)
summary(reg)

z <- summary(reg)$coefficients/summary(reg)$standard.errors
# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
exp(coef(reg))


# ###########################################
# prediction 
train_test = 0.8
split <- createDataPartition(y = df$Label, p = train_test, list = FALSE)
dftrain = df[split,]
dftest = df[-split,]


# lasso
library(glmnet)
library(caret)

X <- model.matrix(f, dftrain)[, -1]
#X = as.matrix(dftrain[,-ncol(df)])
y = dftrain[,ncol(df)]
a = 1 # lasso
logweights = rep(0, nrow(dftrain))
for(j in 1:nlevels(df$Label)){
    logweights[dftrain$Label == levels(dftrain$Label)[j]] = 1/sum(dftrain$Label == levels(dftrain$Label)[j])
}
# fit_all = glmnet(X,y,family = "multinomial", type.multinomial = "grouped",alpha = a,weights = logweights)
# plot(fit_all, xvar = "dev", label = TRUE, type.coef = "2norm")
fit_cv = cv.glmnet(X, y, family="multinomial", 
                    type.measure = "class", nfolds = 10, weights = logweights, alpha = a)
plot(fit_cv)


c = coef(fit_cv, s = "lambda.min")
# importance of the variables
sds = apply(X, 2, sd)
coefs = cbind(c$`1`,c$`2`,c$`3`)
coefs = as.matrix(coefs[-1,])
std_coefs <-  coefs * matrix(rep(sds,each=ncol(coefs)), ncol=ncol(coefs), byrow=T)
s = std_coefs[sort(abs(std_coefs[,3]),decreasing = T,index.return = T)$ix,] 
s = s[abs(s[,1])>0 | abs(s[,2])>0 | abs(s[,3])>0,]

# prediction performance
Xtest <- model.matrix(f, dftest)[, -1]
predtest = predict(fit_cv, newx = Xtest, s = "lambda.min", type = "class")
confusionMatrix(predtest, dftest$Label)


# barplot for the beta
names = rownames(s)
names[c(1,7,8,9)] = c('AOS','Bmtrk',"JASA","JRSS-B" )
rownames(s) = names
#name_plot = names[sort(abs(std_coefs[,1]),decreasing = T,index.return = T)$ix[1:nrow(s)]]
data = cbind(s, names)
colnames(data) = c("class 1",'class 2','class 3','variable')
data_plot = melt(data, id.vars = "variable")[1:(nlevels(df$Label)*nrow(data)),]
colnames(data_plot) = c('variable','class','beta')
data_plot = data_plot[nrow(data_plot):1,]
data_plot$variable = factor(data_plot$variable,levels = data_plot$variable[1:(nrow(data_plot)/3)])

#levels(data_plot$variable) <- gsub(".", "\n",levels(data_plot$variable)) 
data_plot$beta = round(as.numeric(as.character(data_plot$beta)),4)
library(scales)
S_sqrt <- function(x){sign(x)*sqrt(abs(x))}
IS_sqrt <- function(x){x^2*sign(x)}
S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt,IS_sqrt)
ggplot(data=data_plot, aes(x=variable, y=beta, fill=class)) +
    geom_bar(stat="identity",width=0.75, position=position_dodge(width=0.75))+
    theme(axis.text.x = element_text(angle = 270, hjust = 0))+
    scale_y_continuous(trans="S_sqrt",breaks = c(0,0.1,0.3,-0.1,-0.3,0.7,-0.7))+
    coord_flip()

plotPath = "/Users/PengMinshi/Dropbox/projects/ADA/NOTES/final/fig/"
ggsave(paste(plotPath,'/patternlasso.png',sep = ""), width = 15, height = 7, units = "cm")


# ###########################################


X = as.matrix(dftrain[,-ncol(df)]) # last column is y
y = dftrain[,ncol(df)]
a = 1 # a = 1 is lasso, a\in (0,1) is elastic net


logweights = rep(0, nrow(dftrain)) # if unbalanced add weights
for(j in 1:nlevels(dftrain$Label)){
    logweights[dftrain$Label == levels(dftrain$Label)[j]] = 1/sum(df$Label == levels(dftrain$Label)[j])
}

fit_lasso = glmnet(X,y,family = "multinomial", type.multinomial = "grouped",alpha = a,weights = logweights)
plot(fit_lasso, xvar = "dev", label = TRUE, type.coef = "2norm")

# choose the parameter by cross validation
cvfit = cv.glmnet(X, y, family="multinomial", type.multinomial = "grouped",
                  type.measure = "class", nfolds = 10, weights = logweights,alpha = a)
plot(cvfit)
log(cvfit$lambda.min)
coef(cvfit, s = "lambda.min")

# prediction performance
Xtest = as.matrix(dftest[,-ncol(df)])
predtest = predict(cvfit, newx = Xtest, s = "lambda.min", type = "class")
confusionMatrix(predtest, dftest$Label)

# importance of the variables
sds = apply(X, 2, sd)
coefs = cbind(coef(cvfit, s = "lambda.min")$`1`,coef(cvfit, s = "lambda.min")$`2`,coef(cvfit, s = "lambda.min")$`3`)
coefs = as.matrix(coefs[-1,])
std_coefs <-  coefs * matrix(rep(sds,each=ncol(coefs)), ncol=ncol(coefs), byrow=T)
std_coefs[sort(abs(std_coefs[,1]),decreasing = T,index.return = T)$ix,] 


##########################################
# get a robust result for logistic regression, get the variable ranking by voting for each class and
# the error by mean
library(caret)
library(glmnet)

# parameter to set
niter = 20
train_test = 0.6
a = 1 # the elastic net parameter a=1 lasso, a=0 ridge


# initialize for ranking
nvar = ncol(df) - 1
nlevel = nlevels(df$Label)
var_rank = matrix(0, nrow = niter, ncol = nvar*nlevel)
var_beta = matrix(0, nrow = niter, ncol = nvar*nlevel)
colnames(var_rank) = rep(colnames(df[,-ncol(df)]),nlevel)
lambdalist = rep(0,niter)
pred_accuracy_list = rep(0,niter)
pred_baccuracy_bylcass = matrix(0,nrow = niter, ncol = nlevel) # balanced accuracy for each class

for(i in 1:niter){
    print(i)
    # randomly split the training and testing
    split <- createDataPartition(y = df$Label, p = train_test, list = FALSE)
    dftrain = df[split,]
    dftest = df[-split,]

    # asign weights for training
    logweights = rep(0, nrow(dftrain))
    for(j in 1:nlevels(dftrain$Label)){
        logweights[dftrain$Label == levels(dftrain$Label)[j]] = 1/sum(dftrain$Label == levels(dftrain$Label)[j])
    }

    # fit the model, use cv to choose the lambda
    X = as.matrix(dftrain[,-ncol(df)])
    y = dftrain[,ncol(df)]
    cvfit = cv.glmnet(X, y, family="multinomial", type.multinomial = "grouped",
                      type.measure = "class", nfolds = 10, weights = logweights,alpha = a)

    # prediction performance
    Xtest = as.matrix(dftest[,-ncol(df)])
    predtest = predict(cvfit, newx = Xtest, s = "lambda.min", type = "class")
    cm = confusionMatrix(predtest, dftest$Label)


    # variable ranking
    sds = apply(X, 2, sd)
    coefs = cbind(coef(cvfit, s = "lambda.min")$`1`,coef(cvfit, s = "lambda.min")$`2`,coef(cvfit, s = "lambda.min")$`3`)
    coefs = as.matrix(coefs[-1,])
    std_coefs <-  coefs * matrix(rep(sds,each=ncol(coefs)), ncol=ncol(coefs), byrow=T)



    for(l in 1:nlevel){
        var_rank[i, ((l-1)*nvar + 1):(l*nvar)] = nvar + 1 - rank(abs(std_coefs[,l]))
        var_beta[i, ((l-1)*nvar + 1):(l*nvar)] = std_coefs[,l]
    }
    pred_accuracy_list[i] = cm$overall[1]
    pred_baccuracy_bylcass[i,] = cm$byClass[,ncol(cm$byClass)]
    lambdalist[i] = cvfit$lambda.min
}


# print out the first nshow result
nshow = 20
for( i in 1:nlevel){
    r = sort( apply(var_rank,2,mean)[((i-1)*nvar+1):(i*nvar)] , decreasing = F)
    beta =  apply(var_beta,2,mean)[((i-1)*nvar+1):(i*nvar)]
    ix_ind = sort(abs(beta) , decreasing = T,index.return = T)$ix
    beta = beta[ix_ind]
    names(beta) = names(df[,-ncol(df)])[ix_ind]


    print(t(t(r[1:nshow])))
    print(t(t(beta[1:nshow])))
}
mean(pred_accuracy_list)
colMeans(pred_baccuracy_bylcass)



