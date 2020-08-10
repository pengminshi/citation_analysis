DataPath = "~/Dropbox/projects/ADA/Data2016/model/"
load(paste(DataPath,"papernew.RData",sep = ''))
load(paste(DataPath,"citation_year_info.RData",sep = ''))

#######################################################################
ind = which(rowSums(paper_year) > 5 & papernew$year < 2006 &papernew$year>1981)
paper_ind = papernew[ind,] 
npaper_ind = nrow(paper_ind)
paperyear_ind = paper_ind$year
paper_year_ind = paper_year[ind,]

# initalize the label
label = rep(0, npaper_ind)

citecutrate = 0.1
for( y in c(1976,1981,1986,1991,1996,2001)){
    tempind = which(paperyear_ind>=y & paperyear_ind < y+5)
    temp = sort(rowSums(paper_year_ind[tempind,]),decreasing = T,index.return = T)
    temp_ind = temp$ix
    temp = tempind[temp_ind[1:floor(length(tempind)*citecutrate)]]
    label[temp] = 1
}
#######################################################################
# Feature construction library(tm)
library(SnowballC)
library(tm)
library(RWeka)
library(stringr)
DataPath = "~/Dropbox/projects/ADA/Data2016/model/"
source('~/Dropbox/projects/ADA/code/myfunc.R')

load(paste(DataPath,"papernew.RData",sep = ''))
load(paste(DataPath,'paperRefnewTrim.RData',sep = ''))
load(paste(DataPath,"author_year_info.RData",sep = ''))
load(paste(DataPath,'authorinfo.RData',sep=''))
load(paste(DataPath,'journal_info.RData',sep = ''))
load(paste(DataPath,'second_order_statistics.RData',sep = ''))
load(paste(DataPath,'selfcit_info.RData',sep = ''))


paperdf = papernew[ind,]
paperref = paperRefnewTrim[ind]
authorlist = authorlistnew[ind]
journalid = journal_info$id[ind]

npaper = nrow(paperdf)
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
# 

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
Journal4[journalid == 4 | journalid == 10 | journalid == 20 | journalid == 21] = 1
Journal4 = as.factor(Journal4)
#Journal4 = as.factor(Journal4)
#Journal4 = sapply(levels(Journal4), function(x) as.integer(x == Journal4))


df = data.frame(titleLength = titleLength, logNRef = logFixRefCount,
                selfcitProp = selfcitProp,Journal = Journal4, 
                logTotalCitofAuthor = log(TotalCit+1), #logAveCitofAuthor = log(AveCit+1),
                logTotalPubofAuthor = log(TotalPub+1),# logAvePubofAuthor = log(AvePub+1),
                logAveCitPubRatio = log(AveCitPubRatio+1), 
                NAuthor = AuthorN,  
                #Kw = Keyword,  Kw2 = biKeyword, 
                MaxPaperCit_PaperAge = Maxcitpubyear, logMaxPaperCit = log(Maxcitpaper+1), 
                logCollaboratorTotalCit = log(Collcit+1),#logCollaboratorAveCit = log(Collavecit+1),  
                logCollaboratorTotalPub = log(Collpub+1),#logCollaboratorAvePub = log(Collavepub+1), 
                logNcollaborator  = log(Ncoll+1), 
                logRefAveCit = log(Refavecit+1), logRefAveAge = log(Refavepubyear+1),
                Pubyear = (Pubyear-1976),
                Label = as.factor(label))


# df = data.frame(titleLength = titleLength, logNRef = logFixRefCount,
#                 selfcitProp = selfcitProp,Journal = Journal4, #logTotalCitofAuthor = log(TotalCit+1),
#                 logAveCitPubRatio = log(AveCitPubRatio+1), 
#                 logAveCitofAuthor = log(AveCit+1), #logTotalPubofAuthor = log(TotalPub+1),
#                 NAuthor = AuthorN,  logAvePubofAuthor = log(AvePub+1),
#                 # Kw = Keyword,  Kw2 = biKeyword, 
#                 MaxPaperCit_PaperAge = Maxcitpubyear, logMaxPaperCit = log(Maxcitpaper+1), 
#                 #logCollaboratorTotalCit = log(Collcit+1),
#                 logCollaboratorTotalPub = log(Collpub+1),#logCollaboratorAveCit = log(Collavecit+1),  
#                 logNcollaborator  = log(Ncoll+1), #logCollaboratorAvePub = log(Collavepub+1), 
#                 logRefAveCit = log(Refavecit+1), logRefAveAge = log(Refavepubyear+1),
#                 Pubyear = (Pubyear-1976),
#                 Label = as.factor(label))
train_test = 0.8
library(caret)
split <- createDataPartition(y = df$Label, p = train_test, list = FALSE)
dftrain = df[split,]
dftest = df[-split,]

################################################################################
# clean the variabels
plotPath = "/Users/PengMinshi/Dropbox/projects/ADA/NOTES/final/fig/"

# 'logAveCitofAuthor' and 'logAvePubofAuthor'
# png(paste(plotPath,'/collinearity1.png',sep = ""),width = 350, height = 350)
par(mar = rep(4, 4))
x = df$logAveCitofAuthor
y = df$logAvePubofAuthor
plot(x,y,pch = '.',xlab = 'logAveCitofAuthor',ylab ='logAvePubofAuthor' ,cex =2.5,axes=F)
c = round(cor(x,y),3)
axis(1)
axis(2)
legend(1,5,paste('Cor = ',as.character(c),sep = ''))
# dev.off()

# 'logAveCitofAuthor' and 'logMaxPaperCit'
# png(paste(plotPath,'/collinearity2.png',sep = ""),width = 350, height = 350)
par(mar = rep(4, 4))
x = df$logAveCitofAuthor
y = df$logMaxPaperCit
plot(x,y,pch = '.',xlab = 'logAveCitofAuthor',ylab ='logMaxPaperCit' ,cex = 2.5,axes=F)
c = round(cor(x,y),2)
axis(1)
axis(2)
legend(1,6,paste('Cor = ',as.character(c),sep = ''))
# dev.off()

# 'logAveCitofAuthor' and 'logMaxPaperCit'
# png(paste(plotPath,'/collinearity3.png',sep = ""),width = 350, height = 350)
par(mar = rep(4, 4))
x = df$logCollaboratorTotalCit
y = df$logCollaboratorTotalPub
plot(x,y,pch = '.',xlab = 'logCollaboratorTotalCit',ylab ='logCollaboratorTotalPub' ,cex = 2.5,axes=F)
c = round(cor(x,y),2)
axis(1)
axis(2)
legend(1,7,paste('Cor = ',as.character(c),sep = ''))
# dev.off()

#################################################################################

# inference
f_temp = paste(names(df[,-length(names(df))]),collapse =  '+')
f = paste('Label~',f_temp,sep = '')
f1 = paste('Label~','(',f_temp,')*Pubyear',sep='')

logweights = rep(0, nrow(df))
for(j in 1:nlevels(df$Label)){
    logweights[df$Label == levels(df$Label)[j]] = 1/sum(df$Label == levels(df$Label)[j])
}
# f = as.formula(Label~titleLength+logNRef+selfcitProp+Journal+logAveCitPubRatio+
#     logNcollaborator+NAuthor+
#         (logCollaboratorTotalPub+logRefAveCit+logMaxPaperCit+
#         logAveCitofAuthor+logAvePubofAuthor+logRefAveAge+MaxPaperCit_PaperAge)*Pubyear)

model_glm = glm(f, data = df,  family = binomial(logit))
summary(model_glm)

z = summary(model_glm)$coef[-1,3]
z = z[sort(abs(z),decreasing = F,index.return = T)$ix] 
summary(model_glm)$coefficients/summary(model_glm)$standard.errors

names = names(z)
#names[1] = 'topJournal'
data_plot = data.frame(variable = names, z = z)
data_plot$variable = factor(data_plot$variable,levels = data_plot$variable)
ggplot(data=data_plot, aes(x=variable, y=z)) +
    geom_bar(stat="identity",width=0.75, position=position_dodge(width=0.75))+
    theme(axis.text.x = element_text(angle = 270, hjust = 0))+
    geom_hline(yintercept = c(-2,2),linetype=3)+
    coord_flip() 

# plotPath = "/Users/PengMinshi/Dropbox/projects/ADA/NOTES/final/fig/"
# ggsave(paste(plotPath,'/highcite_infer.png',sep = ""), width = 14, height = 13, units = "cm")
##########################################################################################
# # stepwise selection 
max.model = glm(f, data = dftrain, family = binomial())
min.model = glm(label ~ 1, data = dftrain,family = binomial())
model_both = step(max.model,direction = 'backward',data = dftrain)

fstep = as.formula('
Label ~ logNRef + selfcitProp + Journal + logTotalCitofAuthor + 
    logTotalPubofAuthor + logAveCitPubRatio + NAuthor + MaxPaperCit_PaperAge + 
                   logCollaboratorTotalCit + logCollaboratorTotalPub + logNcollaborator + 
                   logRefAveCit + logRefAveAge + Pubyear
                   ')
# Label ~ titleLength + logNRef + selfcitProp + Journal + logTotalCitofAuthor + 
#     logTotalPubofAuthor + logAveCitPubRatio + NAuthor + MaxPaperCit_PaperAge + 
#                     logCollaboratorTotalCit + logCollaboratorTotalPub + logNcollaborator + 
#                     logRefAveCit + logRefAveAge + Pubyear


logweights = rep(0, nrow(dftrain))
for(j in 1:nlevels(df$Label)){
    logweights[dftrain$Label == levels(dftrain$Label)[j]] = 1/sum(dftrain$Label == levels(dftrain$Label)[j])
}
model_step = glm(fstep, data = dftrain, weights = logweights, family = binomial(logit))

predprob = predict(model_step,newdata = dftest, type = "response")
predlabel = rep(0,nrow(dftest))
predlabel[predprob>0.5] = 1
confusionMatrix(factor(predlabel), dftest$Label)
##########################################################################################
# LASSO prediction
library(glmnet)
X <- model.matrix(as.formula(f), dftrain)[, -1]

# X = as.matrix(dftrain[,-ncol(df)])
y = dftrain[,ncol(df)]
a = 1 # lasso
logweights = rep(0, nrow(dftrain))
for(j in 1:nlevels(df$Label)){
    logweights[dftrain$Label == levels(dftrain$Label)[j]] = 1/sum(dftrain$Label == levels(dftrain$Label)[j])
}
# fit_all = glmnet(X,y,family = "multinomial", type.multinomial = "grouped",alpha = a,weights = logweights)
# plot(fit_all, xvar = "dev", label = TRUE, type.coef = "2norm")
fit_cv = cv.glmnet(X, y, family="binomial", 
                   type.measure = "class",weights = logweights, nfolds = 10,  alpha = a)
plot(fit_cv)

c = coef(fit_cv, s = "lambda.min")
#lambdaselect = 11
#c = coef(fit_cv, lambda = lambdaselect)
# importance of the variables
sds = apply(X, 2, sd)
coefs = as.matrix(c[-1,])
std_coefs <-  coefs * sds
s = std_coefs[sort(abs(std_coefs[,1]),decreasing = T,index.return = T)$ix,] 
s = s[abs(s)>0]

# prediction performance
# Xtest = as.matrix(dftest[,-ncol(df)])
Xtest <- model.matrix(as.formula(f), dftest)[, -1]
# predtest = predict(fit_cv, newx = Xtest, lambda = lambdaselect, type = "class")
predtest = predict(fit_cv, newx = Xtest, s = "lambda.min", type = "class")
confusionMatrix(predtest, dftest$Label)

library(reshape)
# barplot for the beta
names = names(s)
# names[1] = "TopJournal"
data_plot = data.frame(variable = names, beta = s)
data_plot = data_plot[nrow(data_plot):1,]
data_plot$variable = factor(data_plot$variable,levels = data_plot$variable)
# S_sqrt <- function(x){sign(x)*sqrt(abs(x))}
# IS_sqrt <- function(x){x^2*sign(x)}
# S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt,IS_sqrt)
ggplot(data=data_plot, aes(x=variable, y=beta)) +
    geom_bar(stat="identity",width=0.75, position=position_dodge(width=0.75))+
    theme(axis.text.x = element_text(angle = 270, hjust = 0))+
    #scale_y_continuous(trans="S_sqrt")+
    coord_flip() 

plotPath = "/Users/PengMinshi/Dropbox/projects/ADA/NOTES/final/fig/"
# ggsave(paste(plotPath,'/highcite_pred.png',sep = ""), width = 12, height = 8, units = "cm")
# source('~/Desktop/ADA/code/myfunc.R')



# 15 variables
flasso = as.formula('Label ~ Journal + logNRef  + logRefAveAge + logMaxPaperCit + logRefAveCit +
                    MaxPaperCit_PaperAge +logAveCitPubRatio + NAuthor+  selfcitProp + 
                    titleLength  + logTotalCitofAuthor +  logTotalPubofAuthor + logCollaboratorTotalPub')

model_lasso = glm(flasso, data = dftrain, weights = logweights, family = binomial(logit))
summary(modelselect)

predprob = predict(model_lasso,newdata = dftest, type = "response")
predlabel = rep(0,nrow(dftest))
predlabel[predprob>0.5] = 1
confusionMatrix(factor(predlabel), dftest$Label)

# ##################################################################################################
# compare two model

# ##################################################################################################
# Variable ranking 
variables = c('Journal','logNRef','logRefAveAge','logMaxPaperCit','logRefAveCit',
              'MaxPaperCit_PaperAge','logAveCitPubRatio','NAuthor','selfcitProp','titleLength',
              'logTotalCitofAuthor','logTotalPubofAuthor','logCollaboratorTotalPub')

fselect = as.formula('Label ~ Journal + logNRef  + logRefAveAge + logMaxPaperCit+logRefAveCit +
                    MaxPaperCit_PaperAge +logAveCitPubRatio + NAuthor+  selfcitProp + 
                    titleLength  + logTotalCitofAuthor +  logTotalPubofAuthor + logCollaboratorTotalPub')

modelselect = glm(fselect, data = dftrain, family = binomial(logit))

predprob = predict(modelselect,newdata = dftest, type = "response")
predlabel = rep(0,nrow(dftest))
predlabel[predprob>0.5] = 1
confusionMatrix(factor(predlabel), dftest$Label)


library(ggplot2)
plotData <- lapply(names(dftest[,1:5]), function(x){
    out <- data.frame(
        var = x,
        type = c(rep('Actual',nrow(dftest)),rep('Predicted',nrow(dftest))),
        value = c(dftest[,x],dftest[,x]),
        label = c(as.numeric(dftest$Label)-1,as.numeric(predprob))
    )
    out$value <- out$value-min(out$value) #Normalize to [0,1]
    out$value <- out$value/max(out$value)
    out
})
plotData <- do.call(rbind,plotData)
qplot(value, label, data=plotData, facets = type ~ var, geom='smooth', span = 0.5)



# z-value
z = summary(modelselect)$coefficients[-1,3]
z = z[sort(abs(z),decreasing = F,index.return = T)$ix] 
names = names(z)
data_plot = data.frame(variable = names, z = z)
data_plot$variable = factor(data_plot$variable,levels = data_plot$variable)
# S_sqrt <- function(x){sign(x)*sqrt(abs(x))}
# IS_sqrt <- function(x){x^2*sign(x)}
# S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt,IS_sqrt)
ggplot(data=data_plot, aes(x=variable, y=z)) +
    geom_bar(stat="identity",width=0.75, position=position_dodge(width=0.75))+
    theme(axis.text.x = element_text(angle = 270, hjust = 0))+
    geom_hline(yintercept = c(-2,2),linetype=3)+
    coord_flip() 

plotPath = "/Users/PengMinshi/Dropbox/projects/ADA/NOTES/final/fig/"
# ggsave(paste(plotPath,'/rank_z.png',sep = ""), width = 12, height = 8, units = "cm")


library(fmsb)
NagelkerkeR2(modelselect)
logweights = rep(0, nrow(df))
for(j in 1:nlevels(df$Label)){
    logweights[df$Label == levels(df$Label)[j]] = 1/sum(df$Label == levels(df$Label)[j])
}

nv = length(variables)
R2 = rep(0,nv)
for(i in 1:nv){
    f = as.formula(paste("Label~",variables[i],''))
    mod = glm(f,data = df, weights = logweights, family = binomial(logit))
    R2[i] = NagelkerkeR2(mod)$R2
}
order = sort(abs(R2),decreasing = F,index.return = T)$ix
R2 = R2[order]
names = variables[order]
data_plot = data.frame(variable = names, R2 = R2)
data_plot$variable = factor(data_plot$variable,levels = data_plot$variable)
# S_sqrt <- function(x){sign(x)*sqrt(abs(x))}
# IS_sqrt <- function(x){x^2*sign(x)}
# S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt,IS_sqrt)
ggplot(data=data_plot, aes(x=variable, y=R2)) +
    geom_bar(stat="identity",width=0.75, position=position_dodge(width=0.75))+
    theme(axis.text.x = element_text(angle = 270, hjust = 0))+
    #scale_y_continuous(trans="S_sqrt")+
    coord_flip() 

plotPath = "/Users/PengMinshi/Dropbox/projects/ADA/NOTES/final/fig/"
# ggsave(paste(plotPath,'/rank_R2.png',sep = ""), width = 11, height = 8, units = "cm")

##########
# McFadden's r^2

# nv = length(variables)
# R2 = rep(0,nv)
# nullmod <- glm(Label~1,data = df, family=binomial())
# for(i in 1:nv){
#     f = as.formula(paste("Label~",variables[i],''))
#     mod = glm(f,data = df,  family = binomial())
#     R2[i] = 1-logLik(mod)/logLik(nullmod)
# }
# order = sort(abs(R2),decreasing = F,index.return = T)$ix
# R2 = R2[order]
# names = variables[order]
# data_plot = data.frame(variable = names, R2 = R2)
# data_plot$variable = factor(data_plot$variable,levels = data_plot$variable)
# # S_sqrt <- function(x){sign(x)*sqrt(abs(x))}
# # IS_sqrt <- function(x){x^2*sign(x)}
# # S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt,IS_sqrt)
# ggplot(data=data_plot, aes(x=variable, y=R2)) +
#     geom_bar(stat="identity",width=0.75, position=position_dodge(width=0.75))+
#     theme(axis.text.x = element_text(angle = 270, hjust = 0))+
#     #scale_y_continuous(trans="S_sqrt")+
#     coord_flip() 
# 
# plotPath = "/Users/PengMinshi/Dropbox/projects/ADA/NOTES/final/fig/"

####################################################################################
# prediction
logweights = rep(0, nrow(dftrain))
for(j in 1:nlevels(df$Label)){
    logweights[dftrain$Label == levels(dftrain$Label)[j]] = 1/sum(dftrain$Label == levels(dftrain$Label)[j])
}

fselect2 = as.formula('Label ~ Journal + logNRef + logRefAveCit +logMaxPaperCit+logTotalCitofAuthor + logAveCitPubRatio')
                     # MaxPaperCit_PaperAge + + NAuthor+  selfcitProp + 
                     # titleLength  +  logTotalPubofAuthor + logCollaboratorTotalPub')

modelselect2 = glm(fselect2, data = dftrain, weights = logweights, family = binomial(logit))
summary(modelselect)

# prediction performance
predtest = predict(modelselect2, newdata = dftest, type = "response")
predlabel = rep(0,length(predtest))
predlabel[predtest>0.5] = 1
confusionMatrix(as.factor(predlabel), dftest$Label)


# ################################
# heatmap
v = c('Journal','logNRef','titleLength','logMaxPaperCit','logRefAveCit','logTotalCitofAuthor','logTotalPubofAuthor',
              'MaxPaperCit_PaperAge','logAveCitPubRatio','NAuthor','selfcitProp',
              'logRefAveAge','logCollaboratorTotalPub','logNcollaborator','Pubyear' )
mydata <- df[, v]
# mydata <- df[,c(1,13,2,3,4,5,6,10,11,12,8,7,9)]
cormat <- round(cor(mydata),2)
library(reshape2)
melted_cormat <- melt(cormat)
library(ggplot2)
ggplot(data = melted_cormat, aes(x=X1, y=X2, fill=value)) + 
    theme(axis.text.x = element_text(angle = 270, hjust = 0))+
    geom_tile()


####################################################################################
# random forest

train_test = 0.8
library(caret)
split <- createDataPartition(y = df$Label, p = train_test, list = FALSE)
dftrain = df[split,]
dftest = df[-split,]


library(randomForest)
rf <- randomForest(Label~. ,data = dftrain, importance=TRUE, ntree = 300)
varImpPlot(rf)
predprod = predict(rf, type = "prob" )
library(pROC)
roccurve_rf <- roc(dftrain$Label ~ predprod[,2])
# plot(roccurverf,col = 'blue')
# auc(roccurve)


library(ggplot2)
predlabel <- predict(rf,dftest,'vote')[,2]
plotData <- lapply(names(dftest[,c(3,2,14,15)]), function(x){
    out <- data.frame(
        var = x,
        type = c(rep('Actual',nrow(dftest)),rep('Predicted',nrow(dftest))),
        value = c(dftest[,x],dftest[,x]),
        label = c(as.numeric(dftest$Label)-1,predlabel)
    )
    out$value <- out$value-min(out$value) #Normalize to [0,1]
    out$value <- out$value/max(out$value)
    out
})
plotData <- do.call(rbind,plotData)
qplot(value, label, data=plotData, facets = type ~ var, geom='smooth', span = 0.5)


###################################################################################
# logistic regression with stepwise selection
f_temp = paste(names(df[,-length(names(df))]),collapse =  '+')
f = paste('Label~',f_temp,sep = '')

max.model = glm(f, data = dftrain, family = binomial())
min.model = glm(label ~ 1, data = dftrain,family = binomial())
model_both = step(max.model,direction = 'backward',data = dftrain)

fstep = as.formula('
Label ~ logNRef + selfcitProp + Journal + logTotalCitofAuthor + 
    logTotalPubofAuthor + logAveCitPubRatio + NAuthor + MaxPaperCit_PaperAge + 
                   logCollaboratorTotalCit + logCollaboratorTotalPub + logNcollaborator + 
                   logRefAveCit + logRefAveAge + Pubyear
                   ')

logweights = rep(0, nrow(dftrain))
for(j in 1:nlevels(df$Label)){
    logweights[dftrain$Label == levels(dftrain$Label)[j]] = 1/sum(dftrain$Label == levels(dftrain$Label)[j])
}
model_step = glm(fstep, data = dftrain, weights = logweights, family = binomial(logit))

predprob = predict(model_step,newdata = dftest, type = "response")
predlabel = rep(0,nrow(dftest))
predlabel[predprob>0.5] = 1
confusionMatrix(factor(predlabel), dftest$Label)

library(pROC)
roccurve_logistic <- roc(dftest$Label ~ predprob)

model_step = glm(fstep, data = dftrain, family = binomial(logit))
z = summary(model_step)$coef[-1,3]
z = z[sort(abs(z),decreasing = F,index.return = T)$ix] 

names = names(z)
names[1] = 'topJournal'
data_plot = data.frame(variable = names, z = z)
data_plot$variable = factor(data_plot$variable,levels = data_plot$variable)
ggplot(data=data_plot, aes(x=variable, y=z)) +
    geom_bar(stat="identity",width=0.75, position=position_dodge(width=0.75))+
    theme(axis.text.x = element_text(angle = 270, hjust = 0))+
    geom_hline(yintercept = c(-2,2),linetype=3)+
    coord_flip() 

plot(roccurve_rf,col = 'blue',xlim = c(1,0),ylim = c(0,1), xaxs="i", yaxs="i")
plot(roccurve_logistic,add = T, col = 'red')
legend( .6,.4,legend=c("Logistic", "Random forest"),
       col=c("red", "blue"),lty=c(1,1), cex=1,box.lty=0)

# # prediction lasso with all keywords
# library(glmnet)
# library(caret)
# 
# 
# 
# X = as.matrix(dftrain[,-ncol(df)])
# y = dftrain[,ncol(df)]
# a = 0.5 # lasso
# logweights = rep(0, nrow(dftrain))
# for(j in 1:nlevels(df$Label)){
#     logweights[dftrain$Label == levels(dftrain$Label)[j]] = 1/sum(dftrain$Label == levels(dftrain$Label)[j])
# }
# # fit_all = glmnet(X,y,family = "multinomial", type.multinomial = "grouped",alpha = a,weights = logweights)
# # plot(fit_all, xvar = "dev", label = TRUE, type.coef = "2norm")
# fit_cv = cv.glmnet(X, y, family="binomial", 
#                    type.measure = "class", nfolds = 10, weights = logweights, alpha = a)
# plot(fit_cv)
# 
# 
# c = coef(fit_cv, s = "lambda.min")
# # importance of the variables
# sds = apply(X, 2, sd)
# coefs = as.matrix(c[-1,])
# std_coefs <-  coefs * sds
# s = std_coefs[sort(abs(std_coefs[,1]),decreasing = T,index.return = T)$ix,] 
# s = s[abs(s)>0]
# 
# # prediction performance
# Xtest = as.matrix(dftest[,-ncol(df)])
# predtest = predict(fit_cv, newx = Xtest, s = "lambda.min", type = "class")
# predprod = predict(fit_cv, newx = Xtest, s = "lambda.min", type = "response")
# confusionMatrix(predtest, dftest$Label)
# # roc curve
# library(pROC)
# roccurve_lasso <- roc(dftest$Label ~ predprod)
# plot(roccurve_train,col = 'blue',xlim = c(1,0),ylim = c(0,1), xaxs="i", yaxs="i")
# plot(roccurve_lasso,add = T, col = 'red')
# legend( .4,.26,legend=c("Lasso", "RF"),
#        col=c("red", "blue"),lty=c(1,1), cex=0.8,box.lty=0)
# auc(roccurve)
# auc(roccurve_lasso)
# 
# 
# library(reshape)
# # barplot for the beta
# names = colnames(df)
# names[8:12] = c('Other Journal','AOS','Bmtrk',"JASA","JRSS-B" )
# name_plot = names[sort(abs(std_coefs),decreasing = T,index.return = T)$ix[1:length(s)]]
# names(s) = name_plot
# data = cbind(s, name_plot)
# rownames(data) = name_plot
# colnames(data) = c("Highly_cited",'variable')
# data_plot = melt(data, id.vars = "variable")[1:nrow(data),]
# colnames(data_plot) = c('variable','class','beta')
# #levels(data_plot$variable) <- gsub(".", "\n",levels(data_plot$variable)) 
# data_plot$beta = round(as.numeric(as.character(data_plot$beta)),4)
# data_plot = data_plot[nrow(data_plot):1,]
# data_plot$variable = factor(data_plot$variable,levels = data_plot$variable)
# library(scales)
# S_sqrt <- function(x){sign(x)*sqrt(abs(x))}
# IS_sqrt <- function(x){x^2*sign(x)}
# S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt,IS_sqrt)
# ggplot(data=data_plot, aes(x=variable, y=beta)) +
#     geom_bar(stat="identity",width=0.75, position=position_dodge(width=0.75))+
#     theme(axis.text.x = element_text(angle = 270, hjust = 0))+
#     scale_y_continuous(trans="S_sqrt",breaks = c(0,0.05,0.15,0.3,-0.05,-0.15,-0.3))+
#     coord_flip() 
# 
# plotPath = "/Users/PengMinshi/Dropbox/projects/ADA/NOTES/final/fig/"
# ggsave(paste(plotPath,'/highcite.png',sep = ""), width = 15, height = 17, units = "cm")





# ########################################################################################
# # random forest
# library(randomForest)
# rf <- randomForest(Label~. ,data = df, importance=TRUE, ntree = 200)
# varImpPlot(rf)
# predprod = predict(rf, type = "prob" )
# library(pROC)
# roccurve <- roc(df$Label ~ predprod[,2])
# plot(roccurve,col = 'blue')
# auc(roccurve)
# 
# 
# 
# rftrain  = randomForest(Label~. ,data = dftrain, importance=TRUE, ntree = 200)
# varImpPlot(rftrain)
# predprod = predict(rftrain,dftest, type = "prob" )
# roccurve_train <- roc(dftest$Label ~ predprod[,2])
# plot(roccurve_train,col = 'yellow',add = T)
# 
# confusionMatrix(predprod[,2]>0.5, dftest$Label)

#############################################################################################################
# deal with imbalanced data

# prop.table(table(dftrain$Label))
# # try with the original data
# library(randomForest)
# rf <- randomForest(Label~. ,data = df, importance=TRUE, ntree = 200)
# # testpred = predict(rf, newdata = df)
# predprod = predict(rf, type = "prob" )
# roccurve <- roc(df$Label ~ predprod[,2])
# plot(roccurve)
# auc(roccurve)
# 
# library(ROSE)
# # over sampling
# data_balanced_over = ovun.sample(Label~., data = df, method = 'over', N = table(df$Label)[1]*2)$data
# data_balanced_under = ovun.sample(Label~., data = df, method = 'under', N = table(df$Label)[2]*2)$data
# data_balanced_both = ovun.sample(Label~., data = df, method = 'both', N = nrow(df))$data
# data_rose = ROSE(Label~., data = df)$data
# 
# # rf on four new dataset
# rf_rose = randomForest(Label~. ,data = data_rose, importance=TRUE, ntree = 200)
# rf_over = randomForest(Label~. ,data = data_balanced_over, importance=TRUE, ntree = 200)
# rf_under = randomForest(Label~. ,data = data_balanced_under, importance=TRUE, ntree = 200)
# rf_both = randomForest(Label~. ,data = data_balanced_both, importance=TRUE, ntree = 200)
# 
# # performance of four predictor on the testing data
# pred_rf_rose = predict(rf_rose, type = "prob" )
# pred_rf_over = predict(rf_over,  type = "prob" )
# pred_rf_under = predict(rf_under, type = "prob" )
# pred_rf_both = predict(rf_both,  type = "prob" )
# 
# predprod = pred_rf_rose[,2]
# roccurve <- roc(df$Label ~ predprod)
# plot(roccurve)
# auc(roccurve)
# 
# # confusionMatrix(pred_rf_rose,dftest$Label)
# # roc.curve(pred_rf_rose,dftest$Label,plotit = T)
# # 
# # confusionMatrix(pred_rf_over,dftest$Label)
# # roc.curve(pred_rf_over,dftest$Label, plotit = T)
# # 
# # confusionMatrix(pred_rf_under,dftest$Label)
# # roc.curve(pred_rf_under,dftest$Label, plotit = T)
# # 
# # confusionMatrix(pred_rf_both,dftest$Label)
# # roc.curve( pred_rf_both,dftest$Label,plotit = T)
# 
# rfbest = rf_under
# varImpPlot(rfbest)
