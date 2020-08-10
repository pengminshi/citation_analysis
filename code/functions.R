# library(tm)
# library(SnowballC)
# library(RWeka)
# trim_text_bigram <- function(docs,n){
#     corpus <- VectorSource(docs)
#     corpus <- VCorpus(corpus)
#     corpus <- tm_map(corpus, removePunctuation)
#     # corpus <- tm_map(corpus, tolower)
#     # corpus <- tm_map(corpus, stemDocument)
#     corpus <- tm_map(corpus, stripWhitespace)
#     corpus <- tm_map(corpus, removeNumbers)
#     corpus <- tm_map(corpus, removeWords, c(stopwords("english"),myStopwords))
#     cat('before matrix\n')
#     matrix <- TermDocumentMatrix(corpus, control=list( #global = c(10000,Inf),
#                                                        tokenize=BigramTokenizer,
#                                                        stopwords=stopwords("english")))
# 
#     # matrix <- TermDocumentMatrix(
#     #     corpus,
#     #     control = list(
#     #         tokenize=BigramTokenizer,
#     #         # global = c(20000,Inf),
#     #         weight = weightTfIdf,
#     #         tolower=TRUE,
#     #         removeNumbers = TRUE,
#     #         minWordLength = 8,
#     #         removePunctuation = TRUE,
#     #         stemming = TRUE,
#     #         stopwords=stopwords("english")
#     #     ))
#     cat('after matrix\n')
#     m = as.matrix(matrix)
#     cat('dim(m)',dim(m))
#     wordfreq = sort(rowSums(m),decreasing = T,index.return = T)
#     m1 = t(m[wordfreq$ix[1:min(n,nrow(m))],])
#     cat('dim(m1)',dim(m1))
#     return(m1)
# }
# 
# BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
# 
# 
# trim_text <- function(docs,n){
#     docs = tolower(docs)
#     corpus <- VectorSource(docs)
#     corpus <- VCorpus(corpus)
#     corpus <- tm_map(corpus, removePunctuation)
#     # corpus <- tm_map(corpus, tolower)
#     corpus <- tm_map(corpus, removeWords, c(stopwords("english")))
#     corpus <- tm_map(corpus, stemDocument)
#     corpus <- tm_map(corpus, stripWhitespace) 
#     corpus <- tm_map(corpus, removeNumbers)
#     matrix <- TermDocumentMatrix(corpus, 
#                                  control = list(global = c(100,Inf),
#                                                 weighting =
#                                                     function(x)
#                                                         weightTfIdf(x, normalize =
#                                                                         FALSE)))
#     wordfreq = sort(rowSums(as.matrix(matrix)),decreasing = T,index.return = T)
#     m1 = t(as.matrix(matrix)[wordfreq$ix[1:n],])
#     #s = sapply(corpus,"[[",1)
#     return(m1)
# }

# keywordYearfreq<- function(keywords.matrix, pub.years){
#     min.year = min(pub.years)
#     max.year = max(pub.years)
#     
#     years = min.year:max.year
#     n.years = length(years)
#     freq.per.year = matrix(0, nrow = n.years, 
#                             ncol = ncol(keywords.matrix))
#     for(i in 1:n.years){
#         freq.per.year[i,] = colSums(keywords.matrix[pub.years==years[i],])/sum(pub.years==years[i])
#     }
#     
#     return(list(years = years, freq.matrix = freq.per.year))
# }

# patternSmoothing <- function(data, k.smoothing, column = T){
#     
#     if (column.smooth){
#         data.length = nrow(data)
#         n.data = ncol(data)
#         
#         data.smooth = data
#         for(i in (k.smoothing + 1):data.length){
#             smoothing.ind = max(1, i-k.smoothing) : min(data.length, i + k.smoothing)
#             for (j in 1:n.data){
#                 data.smooth[i,j] = mean(data[smoothing.ind,j])
#             }
#         }
#     }
# 
#     return(data.smooth)
# }


# myStopwords <-c('paper','elsevier','this','we','show','shown','all','consider','it','less','greater','result',
#                 'also','can','use','the','articl','in','particular','propos','studi','study','consider','consid',
#                 'problem','give','et','al','bv',"we")

plot_y_yhat <- function(y, yhat, log = F,
                        ylim = NULL,cut = NULL, main = NULL,xlab = NULL,ylab = NULL, legend = F){
    
    if(log){
        y = log(y + 1)
        yhat = log(yhat + 1)
    }
    
    if(is.null(cut)){
        cut = max(y)
    }
    
    yhat = yhat[y<=cut]
    y = y[y<=cut]
    
    
    if(is.null(xlab)){
        xlab = 'true y'
        ylab = 'predicted y'
    }
    if(is.null(ylim)){
        ylim = cut
    }
    plot(y, yhat,xlim = c(min(y), cut),ylim = c(min(y),ylim),
         pch = 16, col = scales::alpha('black',0.5), cex = 0.7,
         main = main, xlab =xlab ,ylab = ylab)
    fit = lm(yhat[y <= cut] ~ y[y <= cut])
    abline(a = fit$coefficients[1], b = fit$coefficients[2], 
           lty = 'dashed',lwd = 1.5, col = 'red')
    cor.pearson = cor(y, yhat, method = 'pearson')
    cor.spearman = cor(y, yhat, method = 'spearman')
    if(legend){
        legend('topright', legend = c(paste('cor=',round(cor.pearson,3),sep = '')))
    }
    
    return(list(cor.pearson = cor.pearson,
                cor.spearman = cor.spearman,
                fit = fit))
}

make.nls.data <- function(x.data, papernew, paper_year, paper.ind){
    df = data.frame(matrix(vector(), 0, ncol(x.data)+2),
                    stringsAsFactors=F)
    
    x.data.matrix = as.matrix(x.data)
    for(i in 1:length(paper.ind)){
        ind = paper.ind[i]
        yt = cumsum( paper_year[ind, papernew[ind,'year']:2014 - 1975])
        n.years = length(yt)
        t = 365 * (1:n.years)
        df.paper = cbind(matrix(rep(x.data.matrix[i,], n.years), 
                                nrow = n.years, byrow = T), 
                         t, yt)
        df = rbind(df, df.paper)
    }
    colnames(df) = c(colnames(x.data), 't', 'yt')
    return(df)
}

predictCit <- function(fit, xdata, var.ind, m, t.years){
    n.papers = nrow(xdata)
    beta = coef(fit)[-c(1:2)]
    
    mu = coef(fit)['mu']
    sigma = coef(fit)['sigma']
    
    lambda = sapply(1:n.papers, function(i) sum(xdata[i,var.ind] * beta))
    citations = sapply(1:n.papers, function(i) m*(exp(lambda[i] * plnorm(t.years * 365, mu, sigma))-1))
    
    return(list(lambda = lambda,
                citations = citations))
}

trueCit <- function(pubyear, paper_year, t.years){
    n.years = length(pubyear)
    citations = sapply(1:n.years, function(i){
        if (pubyear[i] + t.years > 2014){
            cat('Not enough citation history!\n')
        }
        cumsum( paper_year[i, pubyear[i]:2014 - 1975])[t.years]
    })
    
    return(list(citations = citations))
}

topcitAccuracy <- function(truecit, predcit, percent){
    n.papers = length(truecit)
    n.top = ceiling(n.papers * percent)
    top.ind.true = order(truecit, decreasing = T)[1:n.top]
    top.ind.pred = order(predcit, decreasing = T)[1:n.top]
    acc = length(intersect(top.ind.pred, top.ind.true)) / n.top
    return(acc)
}

plotAccResult <- function(result, percent.list, year.list){
    n.p = length(percent.list)
    n.y = length(year.list)
    par(mfrow = c(1,3), mai = c(0.6,0.6,0.2,0.1))
    for( i in 1:n.p){
        percent = percent.list[i]
        plot(year.list, result[i,], 
             ylim = c(0, max(result[i,])*4.5/3),
             xlab = 'year after publish', ylab = paste("prediction accuracy"),
             main = paste("predict top ", percent*100,"% papers", sep = ''),
             type = 'b', pch = 3, col = 'blue', lty =1)
        points(year.list, rep(percent, length(year.list)),
               type = 'b', pch = 1, col = 'black', lty = 2)
        legend('top', legend=c('prediction','ramdon guess'),
               pch = c(3,1), lty = c(1,2), col = c('blue','black'))
    }
    par(mfrow = c(1,1),mar = rep(4,4))
}


myROC <- function(true.label, pred.prob1, type = 'classification', n.breaks = 100, plot = T){
    if (type == 'classification'){
        if(! is.factor(true.label)){
            true.label = as.factor(true.label)
        }
        n = length(true.label)
        threshold = seq(from = 0, to = 1, length.out = n.breaks)
        spec = numeric(length = n.breaks)
        sens = numeric(length = n.breaks)
        for(i in 1:n.breaks){
            pred.label = as.factor(as.numeric(pred.prob1 >= threshold[i]))
            out = caret::confusionMatrix(pred.label, true.label)
            sens[i] = out$byClass[1]
            spec[i] = out$byClass[2]
        }
        
        if(plot){
            plot(1-spec, sens, type  ='l', pch = '.', lty = 1, lwd = 2, main = 'ROC curve',
                 xlab = 'TPR (sensitivity)', ylab = 'FPR (1- specificity)')
            abline(a = 0,b=1, lty = 'dashed')
        }

    }
    return(list(spec = spec, sens = sens))
}


targetFeaturePlot <- function(data, feature.ind, target.ind, y.lab = NULL){
    # plot the features
    par(mfrow = c(3,5), mai = c(0.6,0.3,0.1,0.1))
    if(is.null(y.lab)){
        y.lab = 'target'
    }
    
    for(i in feature.ind){
        plot(data[,i], data[,target.ind], xlab =names(data)[i], ylab =y.lab,
             col = scales::alpha('black',0.5), cex = 0.5, pch = 16)
        fit = lm(data[,target.ind] ~ data[,i])
        abline(fit, col='red', lwd=1)
    }
    par(mfrow = c(1,1), mar = rep(4,4))
} 


color.bar <- function(lut, min, max, nticks=11, ticks=seq(min, max, len=nticks), title='') {
    scale = (length(lut)-1)/(max-min)
    
    dev.new(width=1.75, height=5)
    plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
    axis(2, ticks, las=1)
    for (i in 1:(length(lut)-1)) {
        y = (i-1)/scale + min
        rect(0,y,10,y+1/scale, col=lut[i], border=NA)
    }
}


fitNLS <- function(paper, m, k.smoothing = NULL){
    
    n.years = length(paper$years)
    
    if(!is.null(k.smoothing)){
        citperyear = citationSmoothing(paper$citperyear, k.smoothing)
    } else{
        citperyear = paper$citperyear
    }
    
    df = data.frame(t = 1:n.years*365, ct = cumsum(citperyear))
    fit = minpack.lm::nlsLM(ct  ~ m*(exp(lambda * plnorm(t, meanlog = mu, sdlog = sigma))-1), 
                            start=list(lambda = 3, mu = 8, sigma = 1),
                            data = df,control = list(maxiter = 100))
    
    return(list(lambda = coef(fit)['lambda'],
                mu = coef(fit)['mu'],
                sigma = coef(fit)['sigma'],
                converge = fit$convInfo$isConv))
}

# smoothing the citation with the k nearest neigher years
citationSmoothing <- function(citperyear, k.smoothing){
    n.year = length(citperyear)
    
    if (n.year < k.smoothing + 1){
        cat('WARNING! Not enough years to smooth.')
        k.smoothing = n.year - 1
    }
    
    citperyear.new = citperyear
    for ( i in (k.smoothing+1): n.year){
        smoothing.ind = max(1, i-k.smoothing) : min(n.year, i + k.smoothing)
        citperyear.new[i] = mean(citperyear[smoothing.ind])
    }
    citperyear.new = ceiling(citperyear.new)
    
    return(citperyear.new)
}
