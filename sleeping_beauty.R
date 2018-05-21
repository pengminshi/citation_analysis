
DataPath = "~/Dropbox/projects/ADA/Data2016/model/"
save_data_path = "/Users/PengMinshi/Dropbox/research/citation/data/"
FigPath = '/Users/PengMinshi/Dropbox/research/citation/notes/notes0410/fig/'

load(paste(DataPath,"citation_year_info.RData",sep = ''))
load(paste(DataPath,'papernew.RData', sep = ''))

source.path = '/Users/PengMinshi/Dropbox/research/citation/code/'
source(paste(source.path, 'SBfunctions.R', sep = ''))





####################################################################
# find the top sleeping beauties for papers having citations greater than 50
year.zero = 1975
papers.ind = which(rowSums(paper_year) >= 50)
(npaper = length(papers.ind))
papers.list  = lapply(1:length(papers.ind), 
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
B.list = sapply(1:npaper, function(i){
    paper = papers.list[[i]]
    # sb.out = SBCoefficient3(paper$citperyear, paper$years,
    #                         k.smoothing = 1, eta = 1/2)
    # 345 387 400 500 606 874
    sb.out = SBCoefficient2(paper$citperyear, paper$years,
                            k.smoothing = 1)
    return(sb.out$sleeping.coef)
})

# plot the results
n.sleep = 9
par(mfrow = c(3,3))
ordered.ind = order(B.list,decreasing = T)[1:n.sleep]
out.list = lapply(ordered.ind, function(ind){
    cat(ind,'\n')
    paper = papers.list[[ind]]
    # SBCoefficient3(paper$citperyear, paper$years,main = toupper(paper$paper.title),
    #                k.smoothing = 1, eta = 1/2, plot = 1) 
    SBCoefficient2(paper$citperyear, paper$years,main = '',
                   k.smoothing = 1, plot = T) 
})



#################################################################################
# sleeping beauty figure for Benjamini and Hochberg paper
bh.ind = 47386
cit.years = papernew$year[bh.ind]:2014
citperyear = paper_year[bh.ind,cit.years-1975]
tm = cit.years[which.max(citperyear)]

par(mar = rep(4,4))
plot(cit.years, citperyear, col = 'blue', lwd = 2,type ='b', ylim = c(0, max(citperyear)+10),
     xlab = '', ylab = '',col.lab="red", cex.lab=1.5)
lines(c(tm,tm),c(0, max(citperyear)), lty = 'dashed', col = 'black',lwd = 1.5)
lines(c(1995,2003),c(citperyear[1], citperyear[9]), lty = 'dashed', col = 'black',lwd = 3)
lines(c(1995,tm),c(citperyear[1], citperyear[tm-1995+1]), lty = 'dashed', col = 'red',lwd = 3)
temp <- locator(1)
text(temp,expression('(t'[0]*',C(t'[0]*'))'))
temp <- locator(1)
text(temp,expression('(t'[0]*',C(t'[0]*'))'))
temp <- locator(1)
text(temp,expression('(t,C(t))'))



SBCoefficient2(citperyear, cit.years,main = '',k.smoothing = 1) 

# $sleeping.coef
# [1] 60.30767
# 
# $awaken.time
# [1] 2002

