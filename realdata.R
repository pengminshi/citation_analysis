DataPath = "~/Dropbox/projects/ADA/Data2016/model/"
save_data_path = "/Users/PengMinshi/Dropbox/research/citation/data/"
FigPath = '/Users/PengMinshi/Dropbox/research/citation/notes/notes0410/fig/'

load(paste(DataPath,"citation_year_info.RData",sep = ''))
load(paste(DataPath,'papernew.RData', sep = ''))

source.path = '/Users/PengMinshi/Dropbox/research/citation/code/'
source(paste(source.path, 'rppFunctions.R', sep = ''))
source(paste(source.path, 'SBfunctions.R', sep = ''))



####################################################################
# 50 most cited papers
n.papers = 50
year.zero = 1975
ind = 1:nrow(papernew)
#ind = which(2014 -papernew$year >=30 &2014 -papernew$year < 41)
length(ind)
papers.ind = ind[order(rowSums(paper_year[ind,]), decreasing = T)[1:n.papers]]
# papers.ind = papers.ind[-6]

mean.ref = 15
refperyear = sapply(1976:2015, function(y) sum(papernew$year == y) ) * mean.ref

papers.list  = lapply(1:length(papers.ind), 
                      function(i){
                          papers.selected = paper_year[papers.ind,]
                          papers.year = papernew[papers.ind,'year']
                          paper.title = papernew[papers.ind,'title']
                          
                          years = min(papers.year[i],2014):2014
                          citperyear = papers.selected[i,years-year.zero]
                          inflation = refperyear[years - year.zero]
                          return(list(years = years, 
                                      citperyear = citperyear,
                                      inflation = inflation,
                                      paper.title = as.character(paper.title[i])))
                      } )
#################################################################
pred.out30.m100.lm.inf = predictCitation(paper.list = papers.list[[1]], fit.nyears.list = c(10,15,20,25),
                            pred.nyears = 5, m = 100, inflat = T, decay.type = 'lognormal',
                            k.smoothing = 0, mu.init = 5, sigma.init = 1,eps = 10^(-7),
                            n.day = 365, verbose = F, max.iter = 1000, plot = T)
# pred.out20.m30.nm = predictCitation(paper.list = papers.list, fit.nyears.list = c(10,15), 
#                                          pred.nyears = 5, m = 30, inflat = F, decay.type = 'normal',
#                                          k.smoothing = 0, mu.init = 2000, sigma.init = 2000,eps = 10^(-6), 
#                                          n.day = 365, verbose = F, max.iter = 4000, plot = T)



pred.out = pred.out20.m30.nm.inf

pdf(paste(FigPath,'error20p5m30nminflat.pdf',sep = ""),width=4.5,height=3)
plotPredError(pred.out = pred.out, converge.only = F,log.scale = T,ylim = 1, main = 'normalized, m=30, normal')
dev.off()

pdf(paste(FigPath,'scatter20p5m30lminflat.pdf',sep = ""),width=4,height=3)
plotPredScatter(pred.out = pred.out, converge.only = F)
dev.off()



##################################################################
m.list = c(10,30,100)
ind = 39
(paper = papers.list[[ind]])
paper = paper.test
# example of sb: 39, 3(lasso) 
# example of nonsb: 1, 4, 8
# paper.fit.normal = rppAnalysis(papers.list[[ind]], m.list, eps = 10^(-6),
#                                fit.nyear = 30,verbose = F, max.iter = 5000,
#                                mu.init = 3000, sigma.init = 2000,nyear.extend = 20,
#                                real.only = T, k.smoothing = 2,
#                                decay.type = 'normal', inflat = F,
#                                main = 'Normal decay function')
paper.fit.lognormal = rppAnalysis(paper, m.list, eps = 10^(-6),
                                  fit.nyear = 40,verbose = F, max.iter = 1000, 
                                  mu.init = 8, sigma.init = 1,nyear.extend = 20,
                                  real.only = F, k.smoothing = 3, 
                                  decay.type = 'lognormal', inflat = F,
                                  main = 'Fit all citation counts', 
                                  awake.time = 1995)



paper.test = list(years = 1976:2015,
                  citperyear = c(c(1,0,0,0,0,1,0,0,2,0,0,0,1,1,1),
                                 c(2,6,10,14,30,27,15,23,33,23,
                                   34,48,31,41,31,50,42,38,46,39,
                                   40,35,30,41,38)),
                  paper.title = 'anonymous')
                                 
                                 




############################################################################################
# analysis using rpp_prior
# estimate the prior of alpha and beta
############################################################################################
DataPath = "~/Dropbox/projects/ADA/Data2016/model/"
save_data_path = "/Users/PengMinshi/Dropbox/research/citation/data/"
FigPath = '/Users/PengMinshi/Dropbox/research/citation/notes/notes0410/fig/'

load(paste(DataPath,"citation_year_info.RData",sep = ''))
load(paste(DataPath,'papernew.RData', sep = ''))

source.path = '/Users/PengMinshi/Dropbox/research/citation/code/'
source(paste(source.path, 'rpp_prior.R', sep = ''))

papers.ind = which(rowSums(paper_year) >= 20 & papernew$year < 2015-15 & papernew$year > 1977)
year.zero = 1975
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


out = rppAnalysis(paper = papers.list[[120]], m.list = 5.5, fit.nyear=20, 
                  alpha = 3, beta = 1,
                  mu.init = 8, sigma.init = 1,eps = 10^(-8),
                  nyear.extend = 10, verbose = F, max.iter = 1000, 
                  real.only = F, k.smoothing = 0, decay.type = 'lognormal',
                  main = NULL, awake.time = NULL)

(pred.out = predictCitofPaper(paper = papers.list[[122]], fit.nyears = 15, pred.nyears = 10, m = 5.5,
                             verbose = F, max.iter = 1000, plot = T,
                             alpha = 5, beta = 2, pred.type = 0))

# papers.list  = lapply(papers.ind, function(i){
#         pub.year = papernew$year[i]
#         n.years = 2014 -pub.year+1
#         citation.times = 365*rep(1:n.years, paper_year[i, (pub.year:2014)-year.zero])
#         return(citation.times)
#     })
# 
# 
# out = fitRppPrior(citation.list = papers.list[1:200] , m= 5.5, 
#                   mu.init = 8, sigma.init = 1, alpha.init = 4, beta.init = 4, 
#                   max.iter = 30, eps = 10^(-6), verbose = T, decay.type = 'lognormal') 
