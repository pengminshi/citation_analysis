DataPath = "~/Desktop/ADA/Data2016/model/"
# FigPath = '/Users/PengMinshi/Desktop/ADA/NOTES/notes3/png'


## LOAD DATA
load(paste(DataPath,"papernew.RData",sep = ''))
load(paste(DataPath,"citation_year_info.RData",sep = ''))
load(paste(DataPath,'journal_info.RData',sep = ''))

source('~/Desktop/ADA/code/myfunc.R')


############################################################################
# Construct the data set at currate of 0.025
############################################################################
# PARAMETERS
citecutrate = 0.025
paperyear = papernew$year
zero = 1975
# paper_citation = t(apply(paper_year,1,cumsum))
startyear = 1981
endyear = 1986

# INITIALIZE


# Find the top 50 cited paper published before 1980
tempind = which(paperyear>=startyear & paperyear <endyear)
temp = sort(rowSums(paper_year[tempind,]),decreasing = T,index.return = T)
temp_ind = temp$ix
temp = tempind[temp_ind[1:floor(length(tempind)*citecutrate)]]
(length(temp))



citesmooth = smoothing_nyear(paper_year[temp,1:39], 5)
citesmoothnorm = citesmooth/rowSums(citesmooth)

data_matrix = citesmoothnorm
d <- dist(data_matrix)
hc <- hclust(d,method = 'complete')  # apply hirarchical clustering
plot(hc)
dend <- as.dendrogram((hc))
classification <- cutree(hc,k=5)



par(mfrow = c(2,2))
plotend = 2014-zero
for(c in 1:max(classification)){
    temp_plot = data_matrix[classification==c,]
    if(sum(classification==c)==1){
        plot(startyear:(plotend+zero),temp_plot[(startyear-zero):plotend],type = 'b',
             pch=20,ylim=c(0,max(data_matrix)),ylab = 'Citations per year',xlab = 'year')
    }else{
        plot(startyear:(plotend+zero),temp_plot[1,(startyear-zero):plotend],type = 'b',
             pch=20,ylim=c(0,max(data_matrix)),ylab = 'Citations per year',xlab = 'year')
        for(i in 2:nrow(temp_plot)){
            lines(startyear:(plotend+zero),temp_plot[i,(startyear-zero):plotend],type = 'b',pch=20)
        }
    }
}
par(mfrow = c(1,1))

################################################################################
# LABEL THE TRAINING DATA
# FOR PAPERS PUBLISHED BETWEEN 1976-2005,
# select out papers HAVING THE NUMBER OF CITATIONS as THE TOP citecutrate cited
# Label DATA according to the clustering result

ind = numeric()
label = numeric()

# 1976
templabel = numeric(length(classification))
templabel[classification == 2] = 1
templabel[classification == 1 | classification == 4] = 2
templabel[classification == 3] = 3
templabel[classification == 5 | classification == 6] = 4
label = c(label, templabel)
ind = c(ind, temp)

# 1981
# remove class 5
templabel = numeric(length(classification))
templabel[classification == 3] = 1
templabel[classification == 2] = 2
templabel[classification == 1] = 3
templabel[classification == 4 | classification == 6] = 4
label = c(label, templabel[classification!=5])
ind = c(ind, temp[classification!=5])

# 1986
templabel = numeric(length(classification))
templabel[classification == 1 | classification == 3] = 1
templabel[classification == 4] = 2
templabel[classification == 2] = 3
templabel[classification == 5 | classification == 6 | classification == 7] = 4
label = c(label, templabel)
ind = c(ind, temp)

# 1991
templabel = numeric(length(classification))
templabel[classification == 1] = 1
templabel[classification == 2] = 2
templabel[classification == 4 | classification == 5] = 3
templabel[classification == 3 | classification == 6] = 4
label = c(label, templabel[classification!=7])
ind = c(ind, temp[classification!=7])


# 1996
templabel = numeric(length(classification))
templabel[classification == 1] = 1
templabel[classification == 4] = 2
templabel[classification == 3] = 3
templabel[classification == 2 | classification == 5] = 4
label = c(label, templabel)
ind = c(ind, temp)
save(ind,label,file = '~/Desktop/ADA/Data2016/model/label686.RData')
############################################################################
#
############################################################################
# PARAMETERS
citecutratelow = 0.025
citecutrateup = 0.05
paperyear = papernew$year
zero = 1975
# paper_citation = t(apply(paper_year,1,cumsum))
startyear = 1976
endyear = 1981

# INITIALIZE


# Find the top 50 cited paper published before 1980
tempind = which(paperyear>=startyear & paperyear <endyear)
temp = sort(rowSums(paper_year[tempind,]),decreasing = T,index.return = T)
temp_ind = temp$ix
temp = tempind[temp_ind[(floor(length(tempind)*citecutratelow)+1):floor(length(tempind)*citecutrateup)]]
(length(temp))



citesmooth = smoothing_nyear(paper_year[temp,1:39], 5)
# incrate = increasing_rate(citesmooth)
# incratesmooth = smoothing_nyear(incrate,5)
citesmoothnorm = citesmooth/rowSums(citesmooth)

data_matrix = citesmoothnorm
d <- dist(data_matrix)
hc <- hclust(d,method = 'complete')  # apply hirarchical clustering
plot(hc)
dend <- as.dendrogram((hc))
classification <- cutree(hc,k=8)



par(mfrow = c(2,3))
plotend = 2014-zero
plotstart = startyear
ymin = min(data_matrix)
ymax = max(data_matrix)
for(c in 1:max(classification)){
    temp_plot = data_matrix[classification==c,]
    if(sum(classification==c)==1){
        plot(plotstart:(plotend+zero),temp_plot[(startyear-zero):plotend],type = 'b',
             pch=20,ylim=c(ymin,ymax),ylab = 'Normalized citation',xlab = 'year')
    }else{
        plot(plotstart:(plotend+zero),temp_plot[1,(startyear-zero):plotend],type = 'b',
             pch=20,ylim=c(ymin,ymax),ylab = 'Normalized citation',xlab = 'year')
        for(i in 2:nrow(temp_plot)){
            lines(plotstart:(plotend+zero),temp_plot[i,(startyear-zero):plotend],type = 'b',pch=20)
        }
    }
}
par(mfrow = c(1,1))



################################################################################
# LABEL THE TRAINING DATA
# FOR PAPERS PUBLISHED BETWEEN 1976-2005,
# select out papers HAVING THE NUMBER OF CITATIONS as THE TOP citecutrate cited
# Label DATA according to the clustering result

ind = numeric()
label = numeric()

# 1976
templabel = numeric(length(classification))
templabel[classification == 7 | classification == 8] = 1
templabel[classification == 3] = 2
templabel[classification == 2 | classification == 4] = 3
templabel[classification == 5 | classification == 6] = 4
label = c(label, templabel)
ind = c(ind, temp)

# 1981
# remove class 5
templabel = numeric(length(classification))
templabel[classification == 8| classification == 2] = 1
templabel[classification == 6] = 2
templabel[classification == 3 | classification == 7] = 3
templabel[classification == 1 | classification == 4 | classification == 5] = 4
label = c(label, templabel)
ind = c(ind, temp)

# 1986
templabel = numeric(length(classification))
templabel[classification == 5] = 1
templabel[classification == 1 | classification ==3] = 2
templabel[classification == 2 | classification == 4] = 3
templabel[classification == 6 | classification == 7 | classification == 8 ] = 4
label = c(label, templabel)
ind = c(ind, temp)

# 1991
templabel = numeric(length(classification))
templabel[classification == 9] = 1
templabel[classification == 3] = 2
templabel[classification == 6 | classification == 4] = 3
templabel[classification == 1 | classification == 5|classification == 7 | classification == 8] = 4
label = c(label, templabel[classification!=2])
ind = c(ind, temp[classification!=2])


# 1996
templabel = numeric(length(classification))
templabel[classification == 5] = 1
# templabel[classification == 4] = 2
templabel[classification == 3 | classification == 6] = 3
templabel[classification == 7 | classification == 8| classification ==1 |classification == 4] = 4
label = c(label, templabel[classification!=2])
ind = c(ind, temp[classification!=2])

ind2 = ind
label2 = label
save(ind2,label2,file = '~/Desktop/ADA/Data2016/model/label_2_664.RData')
####################################################################################
library(tm)
library(SnowballC)
library(RWeka)
DataPath = "~/Desktop/ADA/Data2016/model/"
setwd(DataPath)
source('~/Desktop/ADA/code/myfunc.R')

load(paste(DataPath,"papernew.RData",sep = ''))
load(paste(DataPath,'paperRefnewTrim.RData',sep = ''))
load(paste(DataPath,"author_year_info.RData",sep = ''))
load(paste(DataPath,'authorinfo.RData',sep=''))
load(paste(DataPath,'journal_info.RData',sep = ''))
load(paste(DataPath,'second_order_statistics.RData',sep = ''))
load(paste(DataPath,'label686.RData',sep=''))
 load(paste(DataPath,'label_2_664.RData',sep=''))



 
ind = c(ind, ind2[label2>0])
label = c(label, label2[label2>0])

label[label == 1] = 2
label[label == 4] = 3
label = label - 1


paperdf = papernew[ind,]
paperref = paperRefnewTrim[ind]
authorlist = authorlistnew[ind]
journalid = journal_info$id[ind]

npaper = length(ind)
zero =1975

# publish year
Pubyear = paperdf$year
# number of citation 
RefCount = sapply(paperref, length)
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
TotalCitofAuthor = TotalCit

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
AveCitofAuthor = AveCit

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

TotalPubofAuthor = TotalPub

# Average number of publications of the author
AvePub = numeric(npaper)
for(i in 1:npaper){
    tempid = authorlist[[i]]$newID
    id = tempid[tempid>0]
    if(length(id) > 1){
        if( Pubyear[i]-zero == 1)
            AvePub[i] = sum(authorpub_year[id,1])
        else
            AvePub[i] = sum(rowSums(authorpub_year[id,1:(Pubyear[i]-zero)]))
    }else{
        AvePub[i] = sum(authorpub_year[id,1:(Pubyear[i]-zero)])
    }
}
AvePubofAuthor = AvePub

# number of authors
AuthorN = numeric(npaper)
for(i in 1:npaper){
    temp = nrow(authorlist[[i]])
    if(length(temp) > 0){
        AuthorN[i] = temp
    }
}
NAuthor = AuthorN


# journal
Journal4 = journalid
Journal4[journalid != 4 & journalid != 10 & journalid != 20 & journalid != 21] = 0 # 4 journals and the other

# keywords
title = tolower(as.character(paperdf$title))
title <- gsub("[^a-zA-Z]+", " ", title)
biKeyword = trim_text_bigram(title,20)
Keyword = trim_text(title,40)
# title = trim_text(tolower(as.character(paperdf$title)))
# wordfreq = sort(rowSums(title),decreasing = T,index.return = T)
# # keywordselect = c(1:3,5,7,8,10,11,12,14,18,19,21,22,25,26,27,28,30,31,35,41,46,47,49,50) #,52,53,54,56,57,58)
# keywordselect = c(3,5,10,11,12,15,19,20,22,25,30)
# Keyword = t(title[wordfreq$ix[keywordselect],])


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
MaxPaperCit = Maxcitpaper
MaxPaperCit_PaperAge = Maxcitpubyear


# citation of collaborators
Collcit = numeric(npaper) #total citations of all the collaborators
Collpub = numeric(npaper) # total publications of all the collaborators
Ncoll = numeric(npaper)
for(i in 1:npaper){
    tempid = authorlist[[i]]$newID
    id = tempid[tempid>0]
    if(length(id) > 0){
        Collcit[i] = sum(totalcitofcollaborators[id,(Pubyear[i]-zero)])
        Collpub[i] = sum(totalpubofcollaborators[id,(Pubyear[i]-zero)])
        Ncoll[i] = sum(Ncollaborators[id,(Pubyear[i]-zero)])
    }
}
CollaboratorTotalCit = Collcit
CollaboratorTotalPub = Collpub
Ncollaborator = Ncoll


# average cites of the reference papers
Refavecit = refaveciteperyear[ind]
Refavepubyear = refavepubyear[ind]

RefAveCit = Refavecit[ind]
RefAveAge = Refavepubyear[ind]


# Total citation
TimesCit = as.numeric(as.character(paperdf$timesCited))

df = data.frame(NRef = RefCount,TotalCitofAuthor = TotalCit, AveCitofAuthor = AveCit, TotalPubofAuthor = TotalPub,
                AvePubofAuthor = AvePub, NAuthor = AuthorN, Journal = as.factor(Journal4), 
                Keyword = Keyword,  BigramKeyword = biKeyword,
                MaxPaperCit = Maxcitpaper, MaxPaperCit_PaperAge = Maxcitpubyear, CollaboratorTotalCit = Collcit,
                CollaboratorTotalPub = Collpub, Ncollaborator  = Ncoll, RefAveCit = Refavecit, RefAveAge = Refavepubyear,
                Label = as.factor(label))
# levels(df$Label) <- as.factor(c('sudden_incre','slowincre','stable','incre_decre'))
# levels(df$Label) <- as.factor(c('increase','stable','incre_decre'))
# levels(df$Label) <- as.factor(c('increase','stable','incre_decre'))
levels(df$Label) <- as.factor(c('increase','decrease'))
#PubAge = 2015-Pubyear,
#######################################################################################

train_test = 0.6

ntrain = floor(nrow(df)*train_test)
# ntest = nrow(df) - ntrain
trainsample = sample(1:nrow(df),ntrain)
dftrain = df[trainsample,]
dftest = df[-trainsample,]

# Logistic classification
library(nnet)
f = paste(names(df[,-length(names(df))]),collapse =  '+')
f = paste('Label~',f,sep = '')
reg = multinom(f,data=dftrain)
summary(reg)


cut = 0.01*(1:99)
auc = rep(0,length(cut))
fpr_tpr = matrix(0,ncol = 2,nrow = length(cut))
for(i in 1:length(cut)){
    predtest = predict(reg,newdata=dftest,type="probs")
    predtest[predtest > cut[i]] = 1
    predtest[predtest <= cut[i]] = 0
    r = roc.curve(dftest$Label,as.factor(predtest),plotit = T)
    auc[i] = r$auc
    fpr_tpr[i,1] = r$false.positive.rate[2]
    fpr_tpr[i,2] = r$true.positive.rate[2]
}
fpr_tpr  = rbind(fpr_tpr,c(1,1))
# plot roc 
plot(fpr_tpr[,1],fpr_tpr[,2],'b',xlim = c(0,1),ylim = c(0,1))
abline(0,1)
# the cut that maximize the AUC
cutopt = cut[which(auc == max(auc))]
predtest = predict(reg,newdata=dftest,type="probs")
predtest[predtest > cutopt] = 1
predtest[predtest <= cutopt] = 0
predtest = as.factor(predtest)
levels(predtest) <- as.factor(c('increase','decrease'))
confusionMatrix(as.factor(predtest), dftest$Label)
# predtrain = as.factor(apply(predict(reg,newdata=dftrain,type="probs"),1,which.max))
# levels(predtrain) <- as.factor(c('increase','decrease'))
# confusionMatrix(predtrain, dftrain$Label)
# 
# predtest = as.factor(apply(predict(reg,newdata=dftest,type="probs"),1,which.max))
# levels(predtest) <- as.factor(c('increase','decrease'))
# confusionMatrix(predtest, dftest$Label)

# model selection 
model1 = step(reg)
predtest = as.factor(apply(predict(model1,newdata=dftest,type="probs"),1,which.max))
levels(predtest) <- as.factor(c('increase','decrease'))
confusionMatrix(predtest, dftest$Label)
z <- summary(model1)$coefficients/summary(model1)$standard.errors
# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2

## random forest
library(randomForest)
# rf <- randomForest(Label ~ .,data = dftrain, importance=TRUE, ntree = 100)
# varImpPlot(rf)
# predtrain<- predict(rf, dftrain)
# confusionMatrix(predtrain, dftrain$Label)
# 
# predtest<- predict(rf, dftest)
# confusionMatrix(predtest, dftest$Label)
rf <- randomForest(Label~. ,data = df, importance=TRUE, ntree = 100)
varImpPlot(rf)
1 - mean(rf$err.rate[,1])

confusionMatrix(rf$predicted,df$Label)

