#LOAD PACKAGES####
library(Hmisc)
library(excel.link)
library(fastLink)
library(tidyverse)
library(glue)
library(uuid)

source('fastLink_tools.R')

#exception cases
#1. different datasets must have different number of columns

#note
#several entries of the same patient from one dataset can be joined by 
#the same element from another dataset multiple times
#(e.g. John Doe (idx. 1, 2, 3) from dataset #1 can be all matched by 
#John Doe (idx. 123) from dataset #2 multiple times)

#switches
EXPORT_ORIGINAL = TRUE #exports original datasets with the updated UUID

#runtime start
ptm = proc.time()

#define folder to be read from
datafolder = './data'

#read list of files in data
listfile = list.files(datafolder, full.names=TRUE, include.dirs=FALSE, recursive=FALSE)

#load data from the files
df = list()              #data frame storage
df_sep = list()          #data frame storage (separate search columns)
dfNUM = length(listfile) #number of files
colist = list()          #list of existing search columns
cols = list()            #df-wise columns

#search columns
colSEARCH = c("MRN", "FNAME", "LNAME", "ADDR", "CITY", "ZIP", "DOB", "PHONE")
pSEARCH = c("FNAME", "LNAME", "ADDR") #columns for partial search

#column name cleaning
for (i in 1:dfNUM){
  temp = read.csv(listfile[i])
  temp = data.frame(lapply(temp, function(x){toupper(x)}))
  
  #initialize UUID if no UUID is found
  if (!('UUID' %in% colnames(temp))){
    temp = cbind(data.frame('UUID'=uuid_generator(length(temp[[1]]))), temp)
  } 
  #assign missing UUID if its column exists
  #p1. pre-existing UUID
  else{
    emptyuuid = temp[['UUID']] == ''
    if (sum(emptyuuid) > 0){
      temp[['UUID']][emptyuuid] = uuid_generator(sum(emptyuuid))
    }
  }
  
  names(temp) = toupper(names(temp))
  
  #clean phone number(not implemented yet)
  if (length(temp[['PHONE']]) != 0){
    temp[['PHONE']] = gsub('[^0-9.]', "", temp[['PHONE']])
    temp[['PHONE']] = gsub("(^\\d{3})(\\d{3})(\\d{4}$)", "\\1-\\2-\\3", temp[['PHONE']])
  }
  
  temp_sep = names(temp)
  colist[[i]] = intersect(colSEARCH, names(temp)) #searches columns
  df[[i]] = temp
  df_sep[[i]] = temp
  names(df_sep[[i]]) = paste0(names(df_sep[[i]]), glue('__{i}')) #individualize column names
  cols[[i]] = names(df_sep[[i]])
}

#matching index (e.g. [1 2, 1 3, 1 4])
matchidx = list() 
c = 0

for (i in 1:(dfNUM-1)){
  for (j in (i+1):dfNUM)
  {
    c = c + 1
    matchidx[[c]] = c(i,j)
  }
}

#calls probLink over all possible permutation without replacement
temp = fullSurvey(df, df_sep, colist, pSEARCH)

#export original datasets with the UUID update
if (EXPORT_ORIGINAL){
  export = temp[[4]]
  listfile2 = gsub('\\b./data/\\b','', listfile) #only the file names
  for (i in 1:dfNUM){
    write.csv(export[[i]], glue('./export/{listfile2[[i]]}'), row.names = FALSE)
  }
}

#extract matching list from temp df
match_cat = temp[[3]]

c = 0

#extracting and reformatting only the non-matching list
temp2 = list()

for (i in 1:2){
  for (j in 1:length(temp[[i]])){
    c = c + 1
    temp2[[c]] = temp[[i]][[j]]
  }
}

temp = temp2

idx = c()
nonmatch_cat = list()
cnt = 0

#get column dimension for joining 
for (i in 1:length(temp)){
  idx[i] = dim(temp[[i]])[2]
  names(temp[[i]]) = toupper(names(temp[[i]]))
}

cnt = 0

#extract and join non-matching list
for (i in unique(idx[duplicated(idx)])){
  cnt = cnt + 1
  loc = which(idx == i)
  a = loc[1]
  b = loc[2]
  #changed from unique/rbind to intersect as non-matched element
  #could be matched from other permutation pairs (3/13/2021)
  nonmatch_cat[[cnt]] = intersect(temp[[a]],temp[[b]])
  
  if (length(loc) > 2){
    for (i in 3:length(loc)){
      a = loc[i]
      nonmatch_cat[[cnt]] = intersect(nonmatch_cat[[cnt]],temp[[a]])
    }
  }
}

#joining the first and second non-matching lists
tempall = unique(fastmerge(nonmatch_cat[[1]],nonmatch_cat[[2]]))

#joining the rest of the non-matching lists
if (length(nonmatch_cat) != 2){
  for (i in 3:length(nonmatch_cat)){
    tempall = unique(fastmerge(tempall, nonmatch_cat[[i]]))
  }
}

#link datasets sequentially
for (i in 1:length(match_cat)){
  
  a = matchidx[[i]][1]
  b = matchidx[[i]][2]
  
  col1 = cols[[a]]
  col2 = cols[[b]]

  if (i == 1){
    tempall2 = tempall[0,]
    tempall2[1,] = 0
    templist = findElement(match_cat[[i]], tempall2, col1, col2);
  } else{
    templist = findElement(match_cat[[i]], templist[[1]], col1, col2);
  }
}

#final results
fdf = templist[[1]]
fdf = fdf[2:dim(fdf)[1],]
results = rbind(tempall, fdf) #joining nonmatch and match

#save the finalized linked data
write.csv(results, glue('./result/{Sys.Date()}_linked_data.csv'), row.names = FALSE)

#save section matches
for (i in 1:length(match_cat)){
  a = matchidx[[i]][1]
  b = matchidx[[i]][2]
  
  write.csv(match_cat[[i]], glue('./result/{Sys.Date()}_df{a}{b}.csv'), row.names = FALSE)
}

#report runtime
now = proc.time() - ptm
time = round(now[[3]])
print(glue('It took {round(time/60)} minutes {time%%60} seconds'))
