##############################################################################################
###########                          PART 1: CLEAN DATA                       ################
##############################################################################################

load("minshi.Rdata") # given
load("1976.RData") # given


# conbine two data sets 
data <- rbind(paper,paper1976)
dataref <- append(paperRef,paperRef1976)

# change year variable to numeric
data[,5] <- as.numeric(as.character(data[,5])) 



######################################################################################
# data -> newdata, dataref->newref

# removing the missing item ('year' missing, since it seems if year missing most of other variables are also missing)
# record the missing index in `missing list`

# removing the duplicated items (duplicated by `mr` and `wos`)
# combine the reference list of the duplicate items
# record the index of duplicate items in `duplist`

######################################################################################

################# Initialize ####################
newdata <- data
newref <- list()
new_old_index <- list() # record the old index and according new index in data

# missing data
numbermissing <- 0 # count the number of records discarded
missinglist = numeric() # record the index of missing items

# duplication in the data
dupcount <- 0 # number of duplication
duplist <- list()

################# Iteration ####################

dupindicator <- rep(0,nrow(data)) 
pb <- txtProgressBar(min = 0, max = nrow(data), style = 3) # check the progress
for(i in 1:nrow(data)){
    setTxtProgressBar(pb,i)
    
    # check existence before
    if(dupindicator[i] == 1) # exist before
        next
    
    # whether is missing data 
    if( is.na(data[i,5])){
        numbermissing <- numbermissing + 1
        missinglist <- c(missinglist,i) # add to the missing list
        next # this is a missing data, discard
    }
    
    newi <- length(new_old_index) + 1 # the newi-th paper in the newdata
    newdata[newi,] <- data[i,]
    
    
    # find all the item which duplicate this in terms of `mr` and `wos`
    tempdup <- which(data$mr == data$mr[i])  # duplicated as  mr
    tempdup1 <- which(data$wos == data$wos[i])   # wos duplicate
    
    if( length(tempdup) > 1 | length(tempdup1) > 1){
        tempdup <- union(tempdup,tempdup1)
        dupcount <- dupcount + 1
        duplist[[dupcount]] <- tempdup # record this group of duplicated items
        dupindicator[tempdup] <- 1 # mark them as existed
        
        # take the union of the reference
        tempref <- character(0)
        for(j in 1:length(tempdup)){
            tempref <- union(tempref,dataref[[tempdup[j]]])
        }
        newref[[newi]] <- tempref
    }else{
        newref[[newi]] <- dataref[[i]]
    }
    
    new_old_index[[newi]] <- tempdup
}
close(pb)



#################### save data ########################
papernew = newdata[1:length(newref),]
paperRefnew = newref

save(missinglist, file = 'missinglist.RData')
save(duplist, file = 'duplist.RData')
save(new_old_index, file = 'new_old_index.RData')
save(papernew, file = 'papernew.RData')
save(paperRefnew, file = 'paperRefnew.RData')

######################################################################################
# newref -> newreftrim
# trim the reference list, leave the reference to papers in the newdata
######################################################################################

npaper = length(newref)
newreftrim = list() # initialize
dup = 0  # just for checking no duplication, expected to be zero

pb <- txtProgressBar(min = 0, max = npaper, style = 3)
for(i in 1:npaper){
    setTxtProgressBar(pb,i)
    
    tempcitelist = newref[[i]]
    
    l = length(tempcitelist)
    if(l == 0)  
        next # zero reference
    
    indicator <- rep(F,l) # initialize the indicator wether the reference is in dataset
    for(j in 1:l){
        temp <- which(tempcitelist[j]==as.character(newdata$wos))
        if(length(temp) > 0) 
            indicator[j] <- T
        if(length(temp) > 1)    
            dup <- dup + 1 # check dupliation
    }
    
    newreftrim[[i]] <- tempcitelist[indicator]
}



############### save the data ##############
paperRefnewTrim = newreftrim
save(paperRefnewTrim, file = 'paperRefnewTrim.RData')



##############################################################################################
###########  PART 2: construct authorlist according to the papernew order     ################
##############################################################################################
# load data
load('paperAuthor.RData') # given
load('papernew.RData')
load('paperRefnew.RData')

npaper <- nrow(papernew)

# initialize
authorlistnew = list() # new author list
maxauthorid = 0     # save the maximum author id
noauthor = numeric()
error_authorID = numeric() # the authorid that contains error
names = character() # author names

pb <- txtProgressBar(min = 0, max = npaper, style = 3)
for(i in 1:npaper){
    setTxtProgressBar(pb,i)
    
    ch <- as.character(papernew[i,1])
    
    # add 0 to `mr` number to ensure the length of 9
    if(nchar(as.character(papernew[i,1])) == 8){
        ch <- paste(substr(ch,1,2),'0',substr(ch,3,8),sep = '')
    }
    
    # find the index in paperAuthor dataset with this mr 
    authorind <- which( ch == attributes(paperAuthor)[[1]])
    if(length(authorind) > 1){ # more than one matching records
        authorind <- authorind[1] # select the first one
    }else if(length(authorind) == 0){ # no record
        noauthor <- c(noauthor, i) # record the noauthor paper index
        authorlistnew[[i]] <- NULL
        next
    }
    
    authorlistnew[[i]] <- paperAuthor[[authorind]]
    names = c(names,ch)
    
    #find the maximum author id
    tempmax = max(as.numeric(as.character(paperAuthor[[authorind]]$authorID)))
    if (is.na(tempmax)){
        error_authorID <- c(error_authorID, authorind) # record the non numerical author id
        next
    }else if (tempmax > maxauthorid){
        maxauthorid <- tempmax
    }
}
close(pb)
attr(authorlistnew,'names') <- names

# save data
save(authorlistnew,file = 'authorlistnew.RData')


###########################################################################
# construct a author name list with existing author in the authorlistnew
###########################################################################
# initialize
authorName <- character()
authorId <- numeric()
count <- numeric() # the number of times each author exist (number of papers from each author)
nullauthor <- numeric() # record the papers with no authors
error1 <- numeric() # error indicator

for(i in 1: length(authorlistnew)){
    
    if(is.null(authorlistnew[[i]])){ # no author
        nullauthor <- c(nullauthor,i)
        next
    }
    
    tempauthor <- as.numeric(as.character(authorlistnew[[i]]$authorID))
    l <- length(tempauthor)
    
    for(j in 1:l){

        tempind <- which(authorId == tempauthor[j])
        if(length(tempind) == 0){ # doesn't exist before
            # add a name into the author name list
            authorName <- c(authorName,as.character(authorlistnew[[i]]$authorName[j]))
            authorId <- c(authorId, tempauthor[j])
            count <- c(count,1)
        }else if(length(tempind) > 1){ # error in code
            error1 <- c(error1,tempauthor[l])
        }else{ # exist before
            count[tempind] <- count[tempind] + 1
        }
    }
}

# Sort author name according to the last name
temp <- sort(toupper(authorName),index.return = T)
authorNamesort <- temp$x
authorIdsort <- authorId[temp$ix]

# save the data
AuthorName <- authorNamesort
AuthorID <- authorIdsort
save(AuthorName,AuthorID, file = 'Authorinfo.RData')



##############################################################################################
###########                        PART 3: Journal info                       ################
##############################################################################################

####################################################################
# construct a journal info datafram, including:
#   fullname, shortname, ISSN(paper and electronic), establish year of journal, paper count of each journal
#   id: each journal belongs to which paper (1-36)
####################################################################


# load data 
load('papernew.RData')
load('journal.RData')

# Bayesian analysis issn is 1931-6690 not 1936-0975
# journal[7,'issn'] = '1931-6690'
# journal[11,'issn'] = '1465-4644'

# the paper and electronic issn of each journal
ISSN <- matrix(c('0246-0203',' ',
                '1932-6157', '1941-7330',
                '0091-1798', '2168-894X',
                '0090-5364', '2168-8966',
                '0020-3157','1572-9052',
                '1369-1473', '1467-842X',
                '1931-6690', '1936-0975',
                '1350-7265', '1573-9759',
                '0006-341X', '1541-0420',
                '0006-3444', '1464-3510',
                '1465-4644', '1468-4357',
                '0319-5724', '1708-945X',
                '0361-0926', '1532-415X',
                '0167-9473', '1872-7352',
                '1935-7524',' ',
                '1386-1999', '1572-915X',
                '0306-7734', '1751-5823',
                '1061-8600', '1537-2715',
                '1532-4435', '1533-7928',
                '0162-1459', '1537-274X',
                '1369-7412', '0035-9246',
                '0266-4763', '1360-0532',
                '0176-4268', '1432-1343',
                '0047-259X',' ',
                '0964-1998', '1467-985X',
                '0035-9254', '1467-9876',
                '0378-3758', '1873-1171',
                '0143-9782', '1467-9892',
                '1048-5252', '1026-7654',
                '0178-8051','1432-2064',
                '0883-4237', '2168-8745',
                '0303-6898', ' ',
                '1017-0405','1996-8507',
                '0960-3174', '1573-1375',
                '0167-7152',' ',
                '0277-6715', '1097-0258'),ncol =2, byrow = T)


JournalID <- numeric(nrow(papernew))

# combine JRSS-B together
# find which journal the issn belongs to
PubperJournal <- numeric(nrow(journal)-1)
for(i in 1:(nrow(journal)-1)){
    temp <- which(papernew$issn == ISSN[i,1] | papernew$issn == ISSN[i,2] )
    JournalID[temp] <- i
    PubperJournal[i] <- length(temp)
    
}

# the establishment year of the journal
establish_year <- c(1964,2007,1973,1973,1949,1998,2006,1995,1945,1901,2000,1973,1976,1983,2007,
                   1998,1972,1992,2001,1922,1948,1984,1984,1971,1988,1964,1977,1980,1991,1986,
                   1986,1974,1991,1991,1982,1982)

# construct the journal info data frame
journal_info <- list(fullname = journal$fullName[-22], shortname = journal$shortName[-22],
                    issn = ISSN, estabYear = establish_year, count = PubperJournal, 
                    id = JournalID)

# save data
save(journal_info, file = 'journal_info.RData')







##############################################################################################
###########            PART 4: Construct citation edgelist by year            ################
##############################################################################################

load('papernew.RData')
load('paperRefnewTrim.RData')


count_paper_per_year = table(sort(papernew$year,decreasing = F))
npaper = length(paperRefnewTrim)
years = 1976:2015
nyear = 40
zero = 1975
paperyear = papernew$year


citation_year_list = list()
for(i in years){
    citation_year_list[[i-zero]] = matrix(0,nrow = 0,ncol = 2)
}
#tempindicator = rep(0,npaper)       # avoid the duplicated record
paper_year = matrix(0,npaper,nyear)
total_publishyear = rep(0,nyear)   # number of citations for papers published in a year
dupagain = numeric(0)
error = matrix(0,nrow = 0,ncol =2)

pb <- txtProgressBar(min = 0, max = npaper, style = 3)
for(i in 1:npaper){
    setTxtProgressBar(pb,i)
    tempcitelist = paperRefnewTrim[[i]]
    l = length(tempcitelist)
    if(l == 0)
        next
    for(j in 1:l){
        temp = which(tempcitelist[j]==as.character(papernew$wos))
        # if(length(temp)==0)
        #     error = rbind(error,c(i,j))
        # if(length(temp) > 1){
        #     dupagain = c(dupagain,temp)
        # }
        paper_year[temp[1],paperyear[i]-zero] = paper_year[temp[1],paperyear[i]-zero] + 1
        citation_year_list[[paperyear[i]-zero]] = rbind(citation_year_list[[paperyear[i]-zero]],
                                                        c(i,temp[1]))
        total_publishyear[paperyear[temp[1]]-zero] = 1 + total_publishyear[paperyear[temp[1]]-zero] 
    }
}
close(pb)

save(paper_year,citation_year_list,total_publishyear,file = 'citation_year_info.RData')


################################################################################
################################################################################
######## Construct authorcit_year_list and author_paper incidence matrix
load('Authorinfo.RData')
load('authorlistnew.RData')
load('papernew.RData')
# First the author_paper_incidence
# nauthor = length(AuthorName

npaper = length(authorlistnew)
# paper_author_incidence = matrix(0, nrow = npaper, ncol = nauthor)
for(i in 1:npaper){
    # the authors of this paper
    tempauthor = as.numeric(as.character(authorlistnew[[i]]$authorID)) # use the trimed version of reference list
    # number of author
    l = length(tempauthor)
    # if l==0 we dont have the author information
    if(l > 0){
        newID = numeric(l) # initialize new vector save the new id of author
        for(j in 1:l){
            tempid = which(AuthorID == tempauthor[j])
            if(length(tempid) > 0){ #find the author in the authorname list
                newID[j] = tempid #new author id in the authornamesort
                # add edge in the paper-author incidence matrix 
                # paper_author_incidence[i, tempid] = paper_author_incidence[i, tempid] + 1
            }else{
                newID[j] = -1 # ??  no matching author in the given author list
            }
        }
        # add new author id in the authorlistnew
        authorlistnew[[i]] = cbind(authorlistnew[[i]],newID = newID)
    }
}



## construct the authorcite_year_list
load('citation_year_info.RData')
load('papernew.RData')
nyear = 40
nauthor = length(AuthorName)

authorcite_year_list = list() #initialize
authorcited_year = matrix(0, nauthor, nyear) #lagged number of citation of the author

for(y in 1:nyear){
    tempedgelist = citation_year_list[[y]]
    authoredgelist = matrix(0,nrow = 0,ncol = 2)
    for(i in 1:nrow(tempedgelist)){
        send = authorlistnew[[tempedgelist[i,1]]]$newID
        receive = authorlistnew[[tempedgelist[i,2]]]$newID
        if(length(send)>0 & length(receive)>0){
            for(n in 1:length(receive)){
                for(m in 1:length(send)){
                    authoredgelist = rbind(authoredgelist, c(send[m], receive[n]))
                }
            }
        }
        authorcited_year[receive,y] = authorcited_year[receive,y] + 1
        # authorpub_year[send,y] = authorpub_year[send,y] + 1
    }
    authorcite_year_list[[y]] = authoredgelist
}

paperyear = papernew$year
zero = 1975
authorpub_year = matrix(0, nauthor,nyear) #lagged number of papers of the author
for( i in 1:npaper){
    tempid = authorlistnew[[i]]$newID
    if(length(tempid) > 0){
        authorpub_year[tempid,paperyear[i]-zero] =  authorpub_year[tempid,paperyear[i]-zero] + 1
    }
}


save(authorlistnew,authorpub_year,authorcited_year,file = 'author_year_info.RData')


#####################################################################################################
# PART 5: SECOND ORDER INFO
#####################################################################################################
# load data
load('papernew.RData')
# load('authorlistnew.RData')
# load('Authorinfo.RData')
load('author_year_info.RData')
load('citation_year_info.RData')

npaper = nrow(papernew)
nauthor = nrow(authorcited_year)
year = papernew$year
nyear = 40
zero = 1975

# PaperofAuthor_list for authorlistnew in author_year_info
PaperofAuthor_list = vector("list", nauthor) 
for(i in 1:npaper){
    tempauthor = authorlistnew[[i]]$newID
    tempauthor = tempauthor[tempauthor>0]
    l = length(tempauthor)
    if( l > 0 ){
        for(j in 1:l){
            tempind = tempauthor[j]
            if(is.null(PaperofAuthor_list[[tempind]])){
                PaperofAuthor_list[[tempind]] = i
            }else{
                PaperofAuthor_list[[tempind]] = c(PaperofAuthor_list[[tempind]],i)
            }
        }
    }
}

# papermaxcitebefore_author_year and papermaxcitebefore_author_year_pubyear
papermaxcitebefore_author_year = matrix(0,nauthor,nyear)
papermaxcitebefore_author_year_pubyear = matrix(0, nauthor,nyear)
# totalcitofcollaborators and Ncollaborators
totalcitofcollaborators = matrix(0,nauthor, nyear)
totalpubofcollaborators = matrix(0,nauthor, nyear)
Ncollaborators = matrix(0,nauthor,nyear)

for( i in 1:nauthor){
    if(is.null(PaperofAuthor_list[[i]])){
        next
    }
    temp = PaperofAuthor_list[[i]]
    for(y in 2:nyear){
        ind_y = temp[year[temp]< y + zero] # papers published before y+zero
        tempcollab = numeric()
        if(length(ind_y) > 1){
            tempcit = rowSums(paper_year[ind_y,1:y]) # citations got by the papers from 1976 to y+zero
            maxind = which.max(tempcit)
            papermaxcitebefore_author_year[i,y] = tempcit[maxind]
            papermaxcitebefore_author_year_pubyear[i,y] = y + zero -year[ind_y[maxind]]
            for(j in 1:length(ind_y)){
                tmp = authorlistnew[[ind_y[j]]]$newID
                tmp = tmp[tmp > 0]
                tempcollab = c(tempcollab,tmp)
            }
            tempcollab =  unique(tempcollab)
        }else if(length(ind_y) == 1){
            papermaxcitebefore_author_year[i,y] = sum(paper_year[ind_y,1:y])
            papermaxcitebefore_author_year_pubyear[i,y] = y + zero -year[ind_y]
            tmp = authorlistnew[[ind_y]]$newID
            tempcollab = tmp[tmp > 0]
        }
        # delect the index of the author himselp
        tempind = which(tempcollab == i)
        if(length(tempind)==1){
            tempcollab = tempcollab[-tempind]
        }
        if(length(tempcollab) > 0)
        totalcitofcollaborators[i,y] = sum(authorcited_year[tempcollab,1:y])
        totalpubofcollaborators[i,y] = sum(authorpub_year[tempcollab,1:y])
        Ncollaborators[i,y] =  length(tempcollab)
    }
}




# refaveciteperyear and refavepubyear
refaveciteperyear = numeric(npaper)
refavepubyear = numeric(npaper)
for(i in 1:npaper){
    tempyear = year[i]
    if(tempyear == 1976)
        next
    tempref = citation_year_list[[tempyear-zero]]
    ref = tempref[tempref[,1] == i,2]
    if(length(ref) > 1){
        temprefpubyear = tempyear-year[ref]
        temprefpubyear[temprefpubyear < 1] = 1
        refaveciteperyear[i] = mean(rowSums(paper_year[ref,1:(tempyear-zero)])/temprefpubyear)
        refavepubyear[i] = mean(temprefpubyear)
    }else if(length(ref) == 1){
        refaveciteperyear[i] = sum(paper_year[ref,1:(tempyear-zero)])/max(tempyear-year[ref],1)
        refavepubyear[i] = max(tempyear-year[ref],1)
    }
}




save(papermaxcitebefore_author_year,
     papermaxcitebefore_author_year_pubyear,
     totalcitofcollaborators, 
     totalpubofcollaborators, 
     Ncollaborators, 
     refaveciteperyear,
     refavepubyear, file = 'second_order_statistics.RData')

#####################################################################################################
# proportion of self citation in the reference
#####################################################################################################
load("authorlistnew.RData")
load("paperRefnewTrim.RData")
load("papernew.RData")

npaper = nrow(papernew)
selfcitlist = matrix(0,nrow = 0,ncol = 2)
selfcitprop = rep(0,npaper)
for(i in 1:npaper){
    authorind = as.numeric(as.character(authorlistnew[[i]]$authorID))
    if(length(authorind) == 0 )
        next
    
    tempref = paperRefnewTrim[[i]]
    l = length(tempref)
    count = 0
    if(l > 0){
        for(j in 1:l){
            tempind = which(tempref[j] == as.character(papernew$wos))
            tempauthorind = as.numeric(as.character(authorlistnew[[tempind]]$authorID))
            if(length(tempauthorind)>0){
                if( !all(is.na(match(authorind, tempauthorind)))){
                    selfcitlist = rbind(selfcitlist,c(i,j))
                    count = count+1
                }
            }
        }
        selfcitprop[i] = count/l
    }
}
    

save(selfcitlist,selfcitprop, file = '~/Dropbox/projects/ADA/Data2016/model/selfcit_info.RData')



#####################################################################################################
# Add journal impact factor to journal_info
#####################################################################################################

load("~/Dropbox/projects/ADA/Data2016/model/papernew.RData")
load("~/Dropbox/projects/ADA/Data2016/model/journal_info.RData")
load("~/Dropbox/projects/ADA/Data2016/model/citation_year_info.RData")

n.journal = 36
year.zero = 1975

jif = matrix(0, nrow = n.journal, ncol = 40) #
for (journal_id in 1:n.journal){
    paper.ind = which(journal_info$id == journal_id)
    for (y in 1978:2015){ # 1976 and 1977 don't have info of previous two years
        # paper that published in y-1 and y-2 years
        paper.ind.pub2 = paper.ind[papernew$year[paper.ind]==y-1 |papernew$year[paper.ind]==y-2]
        n.pub2 = length(paper.ind.pub2)
        if( n.pub2 == 0){
            jif[journal_id, y- year.zero] = 0
        } else{
            jif[journal_id, y- year.zero] = sum(paper_year[paper.ind.pub2, y-year.zero])/n.pub2
        }
    }
}

journal_info[[length(journal_info)+1]] = jif
names(journal_info)[length(journal_info)] = 'jif'

save(journal_info,file = "~/Dropbox/projects/ADA/Data2016/model/journal_info.RData")




