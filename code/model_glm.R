## PATH
DataPath = "~/Desktop/ADA/Data2016/model/"
FigPath = '/Users/PengMinshi/Desktop/ADA/NOTES/notes3/png'


##
load(paste(DataPath,"papernew.RData",sep = ''))
load(paste(DataPath,"paperRefnew.RData",sep = ''))
load(paste(DataPath,'paperRefnewTrim.RData',sep = ''))
load(paste(DataPath,"citation_year_info.RData",sep = ''))
# including citation_year_info(list of 40); paper_year(71918*40); total_publishyear
load(paste(DataPath,"author_year_info.RData",sep = ''))
load(paste(DataPath,'authorinfo.RData',sep=''))
# authorlistnew,authorpub_year,authorcited_year
load(paste(DataPath,'journal_info.RData',sep = ''))
# fullName, shortname issn  count id(journal other than 36 is 0)



# PARAMETERS
# Consider the papers with citation >= citcut (maybe choose 5/10, exclude the self citations)
PredYear = 2014



npaper = length(paperRefnew)
nyear = 40
njournal = length(journal_info$issn)
paperyear = papernew$year
zero = 1975


# ==========================
# Dependent variables
# ==========================
## 1. The total number of citation in Web of Science
CitationWOS = as.numeric(as.character(papernew[,6]))
# > sum(is.na(CitationWOS))
# [1] 12983
CitationWOS[is.na(CitationWOS)] = 0

## 2. The total number of citation with the paper in our dataset
CitationDATA = rowSums(paper_year)
# > sum(CitationDATA==0)
# [1] 25746

# > sum(CitationDATA)
# [1] 390785
# > sum(CitationWOS)
# [1] 1280433


# ==========================
# Independent variables
# ==========================
#### Age of publication
# HISTOGRAM
# df = data.frame(year = year)
# ggplot(data = df, aes(df$year))+
#     geom_histogram(breaks=seq(1975, 2015, by = 1), col="#ffffff",aes(fill = ..count..),  alpha = .8) +
#     labs(title="Histogram for year of publication") +
#     labs(x="Year", y="Count")+
#     theme(plot.title = element_text(hjust = 0.5))
# ggsave(filename = 'count_pubyear.png', path = FigPath)
AgeofPub = PredYear - papernew$year

################################
#### Number of cited reference
RefCount = sapply(paperRefnew,length)
#HISTOGRAM
# df = data.frame(ref = log(RefCount))
# ggplot(data = df, aes(df$ref))+
#     geom_histogram(col="#ffffff",aes(fill = ..count..),  alpha = .8) +
#     labs(title="Histogram for number of cited reference") +
#     labs(x="log(# reference)", y="Count")+
#     theme(plot.title = element_text(hjust = 0.5))
# ggsave(filename = 'count_reference.png', path = FigPath)

## Number of cited reference within the data
RefCountin = sapply(paperRefnewTrim, length)
# df = data.frame(ref = log(RefCountin))
# ggplot(data = df, aes(df$ref))+
#     geom_histogram(col="#ffffff",aes(fill = ..count..),  alpha = .8) +
#     labs(title="Histogram for number of cited reference") +
#     labs(x="log(# reference)", y="Count")+
#     theme(plot.title = element_text(hjust = 0.5))
# ggsave(filename = 'count_reference_data.png', path = FigPath)




################################
#### Lagged author citations within the dataset
TotalLagCit = numeric(npaper)
AveLagCit = numeric(npaper)

for(i in 1:npaper){
    tempyear = paperyear[i]         #publish year
    if(paperyear[i] > 1976){        # if published in 1976 no lagged citation 
        tempid = authorlistnew[[i]]$newID
        id = tempid[tempid>0]
        if(length(id) > 1 & paperyear[i] > 1977){
            temp = rowSums(authorcited_year[id,1:(tempyear-zero-1)])
            TotalLagCit[i] = sum(temp)
            AveLagCit[i] = mean(temp)
        }else if(length(id)==1 & paperyear[i] > 1977 ){
            temp = sum(authorcited_year[id,1:(tempyear-zero-1)])
            TotalLagCit[i] = temp
            AveLagCit[i] = temp
        }else if(length(id) >1 & paperyear[i] == 1977 ){
            temp = authorcited_year[id,1]
            TotalLagCit[i] = sum(temp)
            AveLagCit[i] = mean(temp)
        }
    }
}



################################
#### Lagged author publication within the dataset
TotalLagPub = numeric(npaper)
AveLagPub = numeric(npaper)
for(i in 1:npaper){
    tempyear = paperyear[i]         #publish year
    if(paperyear[i] > 1976){        # if published in 1976 no lagged citation 
        tempid = authorlistnew[[i]]$newID
        id = tempid[tempid>0]
        if(length(id) > 1 & paperyear[i] > 1977){
            temp = rowSums(authorpub_year[id,1:(tempyear-zero-1)])
            TotalLagPub[i] = sum(temp)
            AveLagPub[i] = mean(temp)
        }else if(length(id)==1 & paperyear[i] > 1977 ){
            temp = sum(authorpub_year[id,1:(tempyear-zero-1)])
            TotalLagPub[i] = temp
            AveLagPub[i] = temp
        }else if(length(id) >1 & paperyear[i] == 1977 ){
            temp = authorpub_year[id,1]
            TotalLagPub[i] = sum(temp)
            AveLagPub[i] = mean(temp)
        }
    }
}




################################
#### author pagerank





################################
#### Number of authors
AuthorN = numeric(npaper)
for(i in 1:npaper){
    temp = nrow(authorlistnew[[i]])
    if(length(temp) > 0){
        AuthorN[i] = temp
    }
}


################################
#### Journal impact factor
# Impact Factor at publication year IFatPubyear

# first calculate IF, the impact factor for all journals at each year
journalID = journal_info$id
Nback = 5   # how many years to trace back
IF = matrix(0,nrow = njournal,ncol = nyear)     # Impact factor for each journal at each year
Npub = matrix(0, nrow = njournal, ncol = nyear) # How many papers in each journal at each year
for(i in 1:njournal){
    # temp_pubN = numeric(nyear)
    publist = list()
    temp_ind = which(journalID == i)
    for(j in 1:nyear){
        publist[[j]] = which(paperyear[temp_ind]==zero+j)
        Npub[i,j] = length(publist[[j]]) # the journal publish how many papers in this year
        if(j > Nback){
            # Number of items published in 2011 and 2012
            pub_lastNback = sum(Npub[i,(j-Nback):(j-1)])
            # Cites in 2013 to items published in:  2011 and 2012
            ind = numeric(0)
            for(h in 1:Nback){
                ind = c(ind,publist[[j-h]])
            }
            if(length(ind)>0){
                cite_this = sum(paper_year[ind,j])
                if(length(pub_lastNback) > 0){
                    IF[i,j] = cite_this/pub_lastNback
                }
            }
        }
    }
}

# Find the IFatPubyear for each paper
IFatPubyear = numeric(npaper)
for(i in 1:npaper){
    if(journalID[i] > 0){
        IFatPubyear[i] = IF[journalID[i],paperyear[i]-zero]
    }
}


# max(IF)
# max(Npub)

# Npubnorm = Npub/1000
# 
# 
# for(i in 1:njournal){
#     df = data.frame(year = (zero+Nback+1):2014, impact = IF[i,(1+Nback):39],npub = Npubnorm[i,(1+Nback):39])
#     ggplot(df, aes(year)) + 
#         geom_line(aes(y = impact, group = 1, color = "IF"),alpha = 0.5) +
#         geom_point(aes(y = impact),size=1) +
#         geom_bar(aes(y = npub,fill = 'Count'), stat="identity",col = '#ffffff',alpha = 0.2) +
#         scale_colour_manual(" ", values=c("Count" = "blue","IF" = "black"))+
#         scale_fill_manual("",values="blue")+
#         xlab("Year") +
#         ylab("IF") +
#         ylim(c(0,0.45)) +
#         ggtitle(journal_info$shortname[i]) +
#         theme(legend.title=element_blank(),
#               legend.position=c(0.2,0.8),
#               legend.box="horizontal",
#               plot.title = element_text(hjust = 0.5))
#     
#     filename = paste(journal_info$shortname[i],'.png',sep = '')
#     filepath = paste(FigPath,'/impact',sep = '')
#     
#     ggsave(filename = filename, path = filepath)
# }

###########################################################################
# SELECT THE GOOD OBSERVATIONS
###########################################################################
# yearcut = 1980
# citcut = 1
# 
# 
# 
# # Consider papers with reference number greater than citcut/or citecutrate
# lesscit = which(as.numeric(as.character(papernew$timesCited)) < citcut)
# # consider pappers published after 1980 
# beforeyear = which(paperyear < yearcut)
# # consider papers within the 36 journal
# nojournal = which(journal_info$id == 0)  # 499
# # consider papers with author info ??
# noauthor = which(lapply(authorlistnew,length)==0)#422
# 
# 
# totalind = 1:npaper
# ind = totalind[-unique(c(lesscit,beforeyear,nojournal,noauthor))]

df_full = data.frame(CitationWOS = CitationWOS,CitationDATA =CitationDATA, AgeofPub = AgeofPub,
                     RefCount = RefCount, RefCountin = RefCountin,TotalLagCit = TotalLagCit,AveLagPub = AveLagPub,
                     TotalLagPub = TotalLagPub,AveLagCit = AveLagCit, IFatPubyear = IFatPubyear, AuthorN = AuthorN)
save(df_full, file = 'Desktop/ADA/Data2016/model/df_full.RData')

# df2014 = df_full[ind,]
# save(df2014, file = 'Desktop/ADA/Data2016/model/df2014.RData')


###########################################################################
# LINEAR REGRESSION
###########################################################################
DataPath = "~/Desktop/ADA/Data2016/model/"
load(paste(DataPath,"df2014.RData",sep = ''))
ind1 = which(df2014$CitationDATA>=0)
df = df2014[ind1,]
attach(df)
fit = lm(log(CitationDATA+1)~AgeofPub*(log(RefCountin+1)+log(TotalLagCit+1)+log(TotalLagPub+1)+IFatPubyear+AuthorN))
summary(fit)

# s = step(lm(log(CitationDATA+1)~AgeofPub),scope = list(lower = lm(log(CitationDATA+1)~AgeofPub),
#      upper = lm(log(CitationDATA+1)~(AgeofPub+AgeofPub^2)*(log(RefCountin+1)+log(TotalLagCit+1)+log(TotalLagPub+1)+IFatPubyear+AuthorN))),
#      direction = 'forward')

fit2 = lm(log(CitationWOS+1)~ poly(AgeofPub,2)*(log(RefCount+1)+log(TotalLagCit+1)+log(TotalLagPub+1)+IFatPubyear+AuthorN))
summary(fit2)

pairs(~AgeofPub+log(RefCountin+1)+log(TotalLagCit+1)+log(TotalLagPub+1)+IFatPubyear+AuthorN,data=df,pch=20,col=rgb(0,0,0,alpha=0.3) )

##################################################################################################
# CONSIDER THE TIME SERIES
###################################################################################################
library(splines)

### CONSTRUCT THE DATA FRAME AS TIME SERIES

## PATH
DataPath = "~/Desktop/ADA/Data2016/model/"
FigPath = '/Users/PengMinshi/Desktop/ADA/NOTES/notes3/png'


## load datasets
load(paste(DataPath,"df_full.RData",sep = ''))
load(paste(DataPath,"papernew.RData",sep = ''))
load(paste(DataPath,"citation_year_info.RData",sep = ''))
load(paste(DataPath,'journal_info.RData',sep = ''))
load(paste(DataPath,"author_year_info.RData",sep = ''))



# PARAMETERS
paperyear = papernew$year
npaper = nrow(papernew)
startyear = 1986
endyear = 2014
yearcut = 1985
citcut = 0



df_PredYear <- function(predyear){
    zero = 1975
    # npaper = nrow(papernew)
    # paperyear = papernew$year
    # yearcut = 1985
    # citcut = 0
    
    # DEPENDENT VARIABLE
    CitationDATA = rowSums(paper_year[,1:(predyear-zero)])
    
    # INDEPENDENT VARIABLE
    AgeofPub = predyear - paperyear
    
    df = data.frame(CitationDATA =CitationDATA, AgeofPub = AgeofPub,
                    RefCountin = df_full$RefCountin,TotalLagCit = df_full$TotalLagCit,
                    TotalLagPub = df_full$TotalLagPub, IFatPubyear = df_full$IFatPubyear, 
                    AuthorN = df_full$AuthorN, YearofPub = paperyear)
    
    
    # Consider papers with reference number greater than citcut/or citecutrate
    lesscit = which(CitationDATA < citcut)
    # consider pappers published after 1980, papers published after the prediction year
    falseyear = which(paperyear < yearcut | paperyear >= predyear)
    # consider papers within the 36 journal
    nojournal = which(journal_info$id == 0) 
    # consider papers with author info ??
    noauthor = which(lapply(authorlistnew,length)==0)
    
    totalind = 1:npaper
    ind = totalind[-unique(c(lesscit,falseyear,nojournal,noauthor))]
    
    return(df[ind,])
}

# Construct dataframe
df = data.frame()
for(predyear in startyear:endyear){
    df = rbind(df, df_PredYear(predyear))
}

# Linear regression
# fit = glm(data = df, CitationDATA~ poly(AgeofPub,2)*(RefCountin+TotalLagCit+TotalLagPub+
#                                                                IFatPubyear+AuthorN+I(TotalLagCit*AuthorN)+
#                                                        I(TotalLagPub*AuthorN)),family = poisson())
# summary(fit)


# spline with possion regression

# Computes the logistic regression model using natural splines (note famhist is included as a factor): 
# form = "CitationDATA ~ ns(AgeofPub,df=3)*(ns(log(RefCountin+1),df=2) + ns(log(TotalLagCit+1),df=2) + 
# ns(log(TotalLagPub+1),df = 2) + ns(IFatPubyear,df=1)+ns(YearofPub,df = 2) + ns(AuthorN,df=2))"
form = "CitationDATA ~ (ns(AgeofPub,df=4)+ns(RefCountin,df=2) + ns(TotalLagCit,df=2) + 
ns(TotalLagPub,df = 2) + ns(IFatPubyear,df=1)+ns(YearofPub,df = 2) + ns(AuthorN,df=2))"
form = formula(form)

fit = glm( form, data=df, family= poisson())
print( summary(fit), digits=3 )
(predrate = (summary(fit)$null.deviance-summary(fit)$deviance)/summary(fit)$null.deviance)
# Duplicates the numbers from Table 5.1:
# drop1( fit2, scope=form, test="Chisq",trace = T)



##############################################################################################
# Plot the prediction
plotpred <- function(fit, paperid){
    zero = 1975
    newdata = data.frame()
    for( i in (paperyear[paperid]+1):2014){
        temp = df_full[paperid,c(3,5,6,8,10,11)]
        temp[1] = i-paperyear[paperid]
        newdata = rbind(newdata,temp)
    }
    newdata = cbind(newdata,paperyear[paperid])
    names(newdata) = c("AgeofPub","RefCountin", "TotalLagCit","TotalLagPub", "IFatPubyear","AuthorN","YearofPub")
    y = predict(fit,newdata,type="response")
    realy = cumsum(paper_year[paperid,(paperyear[paperid]+1):2014-zero])
    xaxes  = (paperyear[paperid]+1):2014
    plot(xaxes,y,type = 'o',pch = 20,ylim=c(0,max(c(y,realy))))
    lines(xaxes,realy, type = "o", pch = 20,col = "blue")
}


l = which(rowSums(paper_year) > 10)
par(mfrow = c(3,3))
k =20; for(i in (k*9+1):((k+1)*9)){ plotpred(fit,l[i])}



