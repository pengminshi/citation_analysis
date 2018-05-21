load('/Users/PengMinshi/Dropbox/research/citation/data/2018-04.RData')
code.path = '/Users/PengMinshi/Dropbox/research/citation/code/'
source(paste(code.path,'functions.R',sep = ''))


paperyear = as.numeric(as.character(paper$year))
# put papers in the order of publish year

paper.yearorder = paper[order(paperyear,decreasing = F),]
out = trim_text_bigram(paper.yearorder$abstract[13892:13895], 100) #_bigram
keywords.matrix = out[which(rowSums(out)>0),]
year.select = paperyear[order(paperyear,decreasing = F)][which(rowSums(out)>0)]

words = colnames(keywords.matrix)

# 2
word.ind = 1:5
words[word.ind]
smooth.out = keywordYearfreq(keywords.matrix.binary[,word.ind],year.select)
matplot(smooth.out$years,smooth.out$freq.matrix,type = 'l',
        ylim = c(0,max(c(smooth.out$freq.matrix)+0.1)), 
        col = 1:length(word.ind), lty = 1:length(word.ind))
legend('top', legend = words[word.ind], col = 1:length(word.ind), 
       lty =1:length(word.ind),horiz = T )

