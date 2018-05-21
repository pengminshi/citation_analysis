load_features <- function(paper.ind){
    DataPath = "~/Dropbox/projects/ADA/Data2016/model/"
    
    load(paste(DataPath,"citation_year_info.RData",sep = ''))
    load(paste(DataPath,'paperRefnewTrim.RData',sep = ''))
    load(paste(DataPath,'papernew.RData', sep = ''))
    load(paste(DataPath,'selfcit_info.RData',sep = ''))
    load(paste(DataPath,"author_year_info.RData",sep = ''))
    load(paste(DataPath,'authorinfo.RData',sep=''))
    load(paste(DataPath,'second_order_statistics.RData',sep = ''))
    load(paste(DataPath,'selfcit_info.RData',sep = ''))
    load(paste(DataPath,'journal_info.RData',sep = ''))
    
    zero.year = 1975
    n.papers = length(paper.ind)
    # publication year
    pub.year = papernew$year[paper.ind]
    
    # reference count
    ref.count = sapply(paperRefnewTrim[paper.ind], length) #>
    ref.count.fix.log = rep(0,n.papers)
    for(i in 1:length(ref.count)){
        ref.count.fix.log[i] = log(ref.count[i]/ (pub.year[i]-zero.year) * 40+1)
    }
    # plot(sapply(as.numeric(names(table(pub.year))), function(y) mean(ref.count.fix.log[pub.year==y])))
    
    # self citation proportion 
    selfcit.prop = selfcitprop[paper.ind]
    
    # total number of citations of the authors of the paper
    author.total.cit = sapply(paper.ind, function(i){
        tempid = authorlistnew[[i]]$newID
        id = tempid[tempid>0]
        if(length(id) > 1){
            if( papernew$year[i]-zero.year == 1)
                return(sum(authorcited_year[id,1]))
            else
                return(sum(rowSums(authorcited_year[id,1:(papernew$year[i]-zero.year)])))
        } else if(length(id) == 1){
            return(sum(authorcited_year[id,1:(papernew$year[i]-zero.year)]))
        } else {
            return(0)
        }
    })
    # plot(sapply(as.numeric(names(table(pub.year))), function(y) mean(author.total.cit[pub.year==y])))
    
    
    # total number of publications of the author
    author.total.pub = sapply(paper.ind, function(i){
        tempid = authorlistnew[[i]]$newID
        id = tempid[tempid>0]
        if(length(id) > 1){
            if( papernew$year[i]-zero.year == 1)
                return(sum(authorpub_year[id,1]))
            else
                return(sum(rowSums(authorpub_year[id,1:(papernew$year[i]-zero.year)])))
        } else if(length(id) == 1){
            return(sum(authorpub_year[id,1:(papernew$year[i]-zero.year)]))
        } else {
            return(0)
        }
    })
    # plot(sapply(as.numeric(names(table(pub.year))), function(y) mean(author.total.pub[pub.year==y])))
    
    
    #  average citation/publication ratio
    ave.citpub.ratio = sapply(paper.ind, function(i){
        tempid = authorlistnew[[i]]$newID
        id = tempid[tempid>0]
        if(length(id) > 1){
            if( papernew$year[i]-zero.year == 1)
                return(sum(authorcited_year[id,1]/max(authorpub_year[id,1],1))/length(id))
            else
                return(sum(rowSums(authorcited_year[id,1:(papernew$year[i]-zero.year)])/
                               max(rowSums(authorpub_year[id,1:(papernew$year[i]-zero.year)]),1))/length(id))
        } else if(length(id) == 1){
            return(sum(authorcited_year[id,1:(papernew$year[i]-zero.year)]/
                           max(authorpub_year[id,1:(papernew$year[i]-zero.year)],1)))
        } else {
            return(0)
        }
    })
    # plot(sapply(as.numeric(names(table(pub.year))), function(y) mean(ave.citpub.ratio[pub.year==y])))
    
    # number of authors
    n.authors = sapply(paper.ind, function(i){
        if(is.null(nrow(authorlistnew[[i]]))){
            return(0)
        } else {
            return(nrow(authorlistnew[[i]]))
        }
    } )
    
    # journal (top 4 or not)
    journal4 = (sapply(paper.ind, function(i){
        jr = journal_info$id[i]
        if (jr == 4 | jr == 10 | jr == 20 | jr == 21) return(1)
        else return(0)
    }))
    # journal impact factor
    journal.if = sapply(paper.ind, function(i){
        journal.id = journal_info$id[i]
        if(journal.id == 0){
            return(0)
        } else {
            return(journal_info$jif[journal.id, papernew$year[i]-zero.year] )
        }
    } )
    
    # average cites of the reference papers
    ref.ave.cit = refaveciteperyear[paper.ind]
    
    # average age of the reference papers
    ref.ave.pubyear = refavepubyear[paper.ind]
    
    # citation of collaborators
    collab.cit = numeric(n.papers) #total citations of all the collaborators
    collab.pub = numeric(n.papers) # total publications of all the collaborators
    n.collab = numeric(n.papers)
    for(i in 1:n.papers){
        ind = paper.ind[i]
        tempid = authorlistnew[[ind]]$newID
        id = tempid[tempid>0]
        if(length(id) > 0){
            collab.cit[i] = sum(totalcitofcollaborators[id,(papernew$year[ind]-zero.year)])
            collab.pub[i] = sum(totalpubofcollaborators[id,(papernew$year[ind]-zero.year)])
            n.collab[i] = sum(Ncollaborators[id,(papernew$year[ind]-zero.year)])
        }
    }
    
    
    # length of title
    library(stringr)
    space_title = str_replace_all(papernew$title[paper.ind],"-"," ")
    title.length = unlist(lapply(1:length(space_title),function(i) length(strsplit(space_title[i], " ")[[1]])))
    
    ####################################################################
    # contruct the data frame for features
    x.data = data.frame(pubyear = pub.year,
                        refsize.log = log(ref.count+1),
                        selfcit.prop = selfcit.prop,
                        authorcit.log = log(author.total.cit+1),
                        authorprod.log = log(author.total.pub+1),
                        authorratio.loglog = log(log(ave.citpub.ratio+1)+1),
                        teamsize = n.authors,
                        topjournal = journal4, 
                        jif = journal.if,
                        refcit.log = log(ref.ave.cit+1),
                        refage.log = log(ref.ave.pubyear+1),
                        cocit.log = log(collab.cit+1),
                        coprod.log = log(collab.pub+1),
                        npco.log = log(n.collab+1),
                        titlelen = title.length)
    
    return(list(df = x.data,
                pub.year =  pub.year,
                ref.count = ref.count,
                selfcit.prop = selfcit.prop,
                author.total.cit = author.total.cit,
                author.total.pub = author.total.pub,
                ave.citpub.ratio = ave.citpub.ratio,
                n.authors = n.authors,
                journal4 = journal4,
                journal.if = journal.if,
                ref.ave.cit = ref.ave.cit,
                ref.ave.pubyear = ref.ave.pubyear,
                collab.cit = collab.cit,
                collab.pub = collab.pub,
                n.collab = n.collab))

}


