######################################################################################
# in this model we consider the least square regression instead of logistic regression
# try different variable selection method


DataPath = "~/Dropbox/projects/ADA/Data2016/model/"
load(paste(DataPath,"papernew.RData",sep = ''))
load(paste(DataPath,"citation_year_info.RData",sep = ''))
load(paste(DataPath,'paperRefnewTrim.RData',sep = ''))
load(paste(DataPath,'authorinfo.RData',sep=''))
load(paste(DataPath,'author_year_info.RData',sep = ''))
load(paste(DataPath,'journal_info.RData',sep = ''))
load(paste(DataPath,'second_order_statistics.RData',sep = ''))
load(paste(DataPath,'selfcit_info.RData',sep = ''))
source('~/Dropbox/projects/ADA/code/myfunc.R')

#######################################################################
# consider the paper with more than average citation
ind = which(rowSums(paper_year) > 5)
paperdf = papernew[ind,] 
paperref = paperRefnewTrim[ind]
authorlist = authorlistnew[ind]
journalid = journal_info$id[ind]

npaper = nrow(paperdf)
zero =1975


######################################################################################
# y
Citation = rowSums(paper_year[ind,])

######################################################################################
# features
library(stringr)

# publish year
Pubyear = paperdf$year #>
PaperAge = 2015 - Pubyear

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



# 
# # total number of citations of the authors of the paper
# TotalCit = numeric(npaper)
# for(i in 1:npaper){
#     tempid = authorlist[[i]]$newID
#     id = tempid[tempid>0]
#     if(length(id) > 1){
#         if( Pubyear[i]-zero == 1)
#             TotalCit[i] = sum(authorcited_year[id,1])
#         else
#             TotalCit[i] = sum(rowSums(authorcited_year[id,1:(Pubyear[i]-zero)]))
#     }else if(length(id) == 1){
#         TotalCit[i] = sum(authorcited_year[id,1:(Pubyear[i]-zero)])
#     }
# }


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

# 
# # total number of publications of the author
# TotalPub = numeric(npaper)
# for(i in 1:npaper){
#     tempid = authorlist[[i]]$newID
#     id = tempid[tempid>0]
#     if(length(id) > 1){
#         if( Pubyear[i]-zero == 1)
#             TotalPub[i] = sum(authorpub_year[id,1])
#         else
#             TotalPub[i] = sum(rowSums(authorpub_year[id,1:(Pubyear[i]-zero)]))
#     }else{
#         TotalPub[i] = sum(authorpub_year[id,1:(Pubyear[i]-zero)])
#     }
# }



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

# journal Aos Bmtrk JASA JRSS-B 
Journal4 = journalid
Journal4[journalid != 4 & journalid != 10 & journalid != 20 & journalid != 21] = 0 # 4 journals and the other
Journal4[journalid == 4 | journalid == 10 | journalid == 20 | journalid == 21] = 1
#Journal4 = as.factor(Journal4)

# average cites of the reference papers
Refavecit = refaveciteperyear[ind]
# average age of the reference papers
Refavepubyear = refavepubyear[ind]

df = data.frame(titleLength = titleLength, logNRef = logFixRefCount,#logTotalCitofAuthor = log(TotalCit+1),
                logAveCitPubRatio = log(AveCitPubRatio+1), 
                logAveCitofAuthor = log(AveCit+1), #logTotalPubofAuthor = log(TotalPub+1),
                logAvePubofAuthor = log(AvePub+1),NAuthor = AuthorN, Journal = Journal4, 
                selfcitProp = selfcitProp,# Kw = Keyword,  Kw2 = biKeyword, 
                logMaxPaperCit = log(Maxcitpaper+1), MaxPaperCit_PaperAge = Maxcitpubyear, 
                #logCollaboratorTotalCit = log(Collcit+1),
                logCollaboratorAveCit = log(Collavecit+1), #logCollaboratorTotalPub = log(Collpub+1), 
                logCollaboratorAvePub = log(Collavepub+1), logNcollaborator  = log(Ncoll+1), 
                logRefAveCit = log(Refavecit+1), logRefAveAge = log(Refavepubyear+1),
                PaperAge = PaperAge,PaperAge2 = PaperAge^2,
                logCitation = log(Citation+1))




########################################################################

f = paste(names(df[,1:(ncol(df)-3)]),collapse =  '+')

fit = lm(logCitation~(titleLength+logNRef+logAveCitPubRatio+logAveCitofAuthor+
             logAvePubofAuthor+NAuthor+Journal+selfcitProp+logMaxPaperCit+
             MaxPaperCit_PaperAge+logCollaboratorAveCit+logCollaboratorAvePub+
             logNcollaborator+logRefAveCit+logRefAveAge)*PaperAge+PaperAge2, data = df)
summary(fit)

##################################
# variable ranking

z = summary(fit)$coef[-1,3]
z = z[sort(abs(z),decreasing = F,index.return = T)$ix] 



names = names(z)
data_plot = data.frame(variable = names, z = z)
data_plot$variable = factor(data_plot$variable,levels = data_plot$variable)

ggplot(data=data_plot, aes(x=variable, y=z)) +
    geom_bar(stat="identity",width=0.75, position=position_dodge(width=0.75))+
    theme(axis.text.x = element_text(angle = 270, hjust = 0))+
    #scale_y_continuous(trans="S_sqrt")+
    geom_hline(yintercept = c(-2,2),linetype=3)+
    coord_flip() 

plotPath = "/Users/PengMinshi/Dropbox/projects/ADA/NOTES/final/fig/"
#  ggsave(paste(plotPath,'/highcite_infer.png',sep = ""), width = 14, height = 13, units = "cm")
# comment: subject to the time change, dosen't work
# rank regression?

# ######################################################################
# # model selection
# model_step = step(model_glm)
# dftest$prediction_step  <- predict( model_step, newdata = dftest , type = "response" )
# roccurve <- roc(dftest$Label ~ dftest$prediction_step)
# plot(roccurve)
# auc(roccurve)