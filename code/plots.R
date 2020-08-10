

DataPath = "~/Desktop/ADA/Data2016/model/"
setwd(DataPath)
load(paste(DataPath,"papernew.RData",sep = ''))
load(paste(DataPath,"citation_year_info.RData",sep = ''))
load(paste(DataPath,'paperRefnewTrim.RData',sep = ''))

#################################################################################
# section2
sum(rowSums(paper_year))/nrow(papernew)
# [1] 5.433267
sum(rowSums(paper_year) == 0)
# [1] 25652
sum(rowSums(paper_year) == 0)/nrow(papernew)
# [1] 0.3571508
sum(sapply(paperRefnewTrim, length) == 0)
# 10699
sum(sapply(paperRefnewTrim, length) == 0)/nrow(papernew)
# [1] 0.1489613
sum(sapply(paperRefnewTrim, length) == 0 & rowSums(paper_year) == 0)
# [1] 5186
sum(sapply(paperRefnewTrim, length) == 0 & rowSums(paper_year) == 0)/nrow(papernew)
# [1] 0.07220428
#################################################################################
# plot the citation distribution for paper published in each five year period
library(ineq)
par(mfrow = c(1,1))
# paperyear = papernew$year
# for( y in c(1976,1981,1986,1991,1996,2001, 2006)){
#     ind  = which(paperyear>=y & paperyear < y+5)
#     cites = rowSums(paper_year[ind,])
#     hist(log(cites+1), 
#          main = paste(as.character(y),'-',as.character(y+5),sep = ''), 
#          xlab = "log(Number of citations)", 
#          border = F, 
#          col = "grey",
#          xlim = c(0,8),
#          las=1, 
#          breaks = 16)
# }

# gini index and Lorenz curve
# clear the plots
source('/Users/PengMinshi/Desktop/ADA/code/functions.R')
years = c(1976,1981,1986,1991,1996,2001,2006)
gini = rep(0,length(years))

graphics.off()
dev.new()
par(mfrow = c(1,1))
for( i in 1:length(years) ){
    y = years[i]
    ind  = which(paperyear>=y & paperyear < y+5)

    cites = rowSums(paper_year[ind,])
    # gini index
    gini[i] = ineq(cites,type="Gini")
    # Lorenze curve
    lorenz(cites, T, 0.1,paste('Lorenz curve  ',as.character(y),'-',as.character(y+5),sep=''))
}
par(mfrow = c(1,1))
plot(years+2, gini, main='Gini Index', 'b',lty = 2, ylim = c(0,1),xlab = 'Years')

