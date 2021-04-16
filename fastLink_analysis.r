#LOAD PACKAGES####
library(Hmisc)
library(excel.link)
# library(fastLink)
library(tidyverse)
library(glue)
library(uuid)

# source('fastLink_tools.R')

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

df_summary = list()

for (i in 1:5){
  temp = data.frame(listfile[[i]])
  
  for (col in colSEARCH){
    if ( is.na(match(col, colnames(df[[i]]))) ){
      var_i = paste('NA /', as.character(nrow(df[[i]])))
    }
    else{
      if (i == 1){
        temp_df = df[[i]][!duplicated(df[[i]]['ID']),]
        var_i = paste( as.character((sum(temp_df[col] == '') + sum(is.na(temp_df[col])))), '/', as.character(nrow(temp_df[col])) )
      }
      else {
        var_i = paste( as.character((sum(df[[i]][col] == '') + sum(is.na(df[[i]][col])))), '/', as.character(nrow(df[[i]][col])) )
      }
    }
    temp = cbind(temp, data.frame(col=var_i))
  }
  df_summary = rbind(df_summary, temp)
}

colnames(df_summary) = append('FILENAME', colSEARCH)

write.csv(df_summary, "result/dataset_summary.csv")
