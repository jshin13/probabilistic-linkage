#LOAD PACKAGES####
library(Hmisc)
library(excel.link)
library(fastLink)
library(tidyverse)
library(glue)

source('fastLink_tools.R')

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
pSEARCH = c("FNAME", "LNAME", "ADDR")



#column name cleaning
for (i in 1:dfNUM){
  temp = read.csv(listfile[i])
  temp = data.frame(lapply(temp, function(x){toupper(x)}))
  names(temp) = toupper(names(temp))
  
  #clean phone number(not implemented yet)
  if (length(temp[['PHONE']]) != 0){
    # temp[['PHONE']] = parse_number(temp[['PHONE']])
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

idx = c()
nonmatch_cat = list()
match_cat = list()
cnt = 0

#get column dimension for joining 
for (i in 1:length(temp)){
  idx[i] = dim(temp[[i]])[2]
  names(temp[[i]]) = toupper(names(temp[[i]]))
}

#extract matching list from temp df
newidx = length(temp) - length(temp) / 3 + 1

for (i in newidx:length(temp)){
  cnt = cnt + 1
  match_cat[[cnt]] = temp[[i]]
}

cnt = 0

#extract non-matching list
for (i in unique(idx[duplicated(idx)])){
  cnt = cnt + 1
  loc = which(idx == i)
  a = loc[1]
  b = loc[2]
  nonmatch_cat[[cnt]] = unique(rbind(temp[[a]],temp[[b]]))
  
  if (length(loc) > 2){
    for (i in 3:length(loc)){
      a = loc[i]
      nonmatch_cat[[cnt]] = unique(rbind(nonmatch_cat[[cnt]],temp[[a]]))
    }
  }
}

#joining the first and second non-matching lists
tempall = fastmerge(nonmatch_cat[[1]],nonmatch_cat[[2]])

#joining the rest of the non-matching lists
if (length(nonmatch_cat) != 2){
  for (i in 3:length(nonmatch_cat)){
    tempall = fastmerge(tempall, nonmatch_cat[[i]])
  }
}

#link datasets sequentially
for (i in 1:dfNUM){
  
  a = matchidx[[i]][1]
  b = matchidx[[i]][2]
  
  col1 = cols[[a]]
  col2 = cols[[b]]

  if (i == 1){
    templist = findElement(match_cat[[i]], tempall, col1, col2);
  } else{
    templist = findElement(match_cat[[i]], templist[[1]], col1, col2);
  }
}

#final results
results = templist[[1]]

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
