#LOAD PACKAGES####
library(Hmisc)
library(excel.link)
library(fastLink)
library(tidyverse)

#LOAD DATA####
data_testing_sacredheart=read.csv('Covid19ResponseForTh_DATA_2021-02-13_0055.csv')
data_hotline_esperanza=read.csv('EsperanzaCallCenterD_DATA_2021-02-13_0036.csv')
data_encounters_esperanza3months<-read.csv('Esperanza_patients_from_last_3_months.csv')

# rename datasets
df1 <- data_encounters_esperanza3months #Encounter
df2 <- data_hotline_esperanza #Hotline
df3 <- data_testing_sacredheart #Sacredheart


# make the dataframe all uppercase
df1 = data.frame(lapply(df1, function(x) {toupper(x)}))
df2 = data.frame(lapply(df2, function(x) {toupper(x)}))
df3 = data.frame(lapply(df3, function(x) {toupper(x)}))

# initialize variables
df1_col = c()
df2_col = c()
df3_col = c()
c = 0
dat = list(df1,df2,df3)

# make columns lowercase
for (j in dat){
  c = c + 1
  temp = c()
  for (i in names(j)){
    temp = c(temp, tolower(i))
  }
  if (c == 1)
    df1_col = temp
  else if (c == 2)
    df2_col = temp
  else if (c == 3)
    df3_col = temp
}

# manual search for common variables
search = 'sex'
print(names(df1[grep(search,df1_col)])) 
print(names(df2[grep(search,df2_col)]))
print(names(df3[grep(search,df3_col)]))

# unify common variables
colnames(df2)[which(names(df2) == 'call_fname')] = "PAT_FIRST_NAME"
colnames(df2)[which(names(df2) == 'call_lname')] = "PAT_LAST_NAME"
colnames(df2)[which(names(df2) == 'call_dob')] = "DOB"
colnames(df2)[which(names(df2) == 'call_phone')] = "HOME_PHONE"
colnames(df2)[which(names(df2) == 'call_zip1')] = "ZIP"
colnames(df2)[which(names(df2) == 'call_age_excel')] = "AGE"

colnames(df3)[which(names(df3) == 'mrn')] = "EMRN"
colnames(df3)[which(names(df3) == 'nm_first')] = "PAT_FIRST_NAME"
colnames(df3)[which(names(df3) == 'nm_last')] = "PAT_LAST_NAME"
colnames(df3)[which(names(df3) == 'address')] = "ADDR_1"
colnames(df3)[which(names(df3) == 'city')] = "CITY"
colnames(df3)[which(names(df3) == 'dob')] = "DOB"
colnames(df3)[which(names(df3) == 'zip')] = "ZIP"
colnames(df3)[which(names(df3) == 'age')] = "AGE"
colnames(df3)[which(names(df3) == 'phone')] = "HOME_PHONE"

# correct column types (does not work)
# df1 = transform(df1, ZIP = as.numeric(ZIP))
# df1 = transform(df1, AGE = as.numeric(AGE))
# df3 = transform(df3, ZIP = as.numeric(ZIP))
# df3 = transform(df3, AGE = as.numeric(AGE))

# common column, search criteria
cols12 = c("HOME_PHONE", "ZIP", "DOB", "PAT_FIRST_NAME", "PAT_LAST_NAME")
cols13 = c("EMRN", "PAT_FIRST_NAME", "PAT_LAST_NAME", "ADDR_1", "CITY", "ZIP", "DOB", "HOME_PHONE")
cols23 = c("ZIP", "HOME_PHONE", "DOB", "PAT_FIRST_NAME", "PAT_LAST_NAME") 

# there has to be three unique matches followed by proper joining
# 1<->2
# 1<->3
# 2<->3

# matching between df1 and df2
results12 <- fastLink(df1, df2, cols12, stringdist.match = cols12, cut.a = 1, cut.p = 0.7,
                      partial.match = c("PAT_FIRST_NAME", "PAT_LAST_NAME"),
                    threshold.match = 0.85, return.df=TRUE)

A12 = results12$dfA.match
B12 = results12$dfB.match

# matching between df1 and df3
results13 <- fastLink(df1, df3, cols13, stringdist.match = cols13, cut.a = 1, cut.p = 0.7,
                      partial.match = c("PAT_FIRST_NAME", "PAT_LAST_NAME", "ADDR_1"),
                      threshold.match = 0.85, return.df=TRUE)#, stringdist.match = cols, cut.a = 1, cut.p = 0.7, threshold.match = 0.85)

A13 = results13$dfA.match
B13 = results13$dfB.match

# matching between df2 and df3
results23 <- fastLink(df2, df3, cols23, stringdist.match = cols23, cut.a = 1, cut.p = 0.7,
                      partial.match = c("PAT_FIRST_NAME", "PAT_LAST_NAME"),
                    threshold.match = 0.85, return.df=TRUE)

A23 = results23$dfA.match
B23 = results23$dfB.match


# fastmerge algorithm for joining two dataframe with different number of columns
fastmerge <- function(d1, d2) {
  d1.names <- names(d1)
  d2.names <- names(d2)
  
  # columns in d1 but not in d2
  d2.add <- setdiff(d1.names, d2.names)
  
  # columns in d2 but not in d1
  d1.add <- setdiff(d2.names, d1.names)
  
  # add blank columns to d2
  if(length(d2.add) > 0) {
    for(i in 1:length(d2.add)) {
      d2[d2.add[i]] <- NA
    }
  }
  
  # add blank columns to d1
  if(length(d1.add) > 0) {
    for(i in 1:length(d1.add)) {
      d1[d1.add[i]] <- NA
    }
  }
  
  return(rbind(d1, d2))
}

##########DATA MERGING STEP#######################
# match_12: unique match list between df1 and df2#
# match_13: unique match list between df1 and df3#
# match_23: unique match list between df2 and df3#
# new_df12: non-match list between df1 and df2   #
# new_df13: non-match list between df1 and df3   # 
# new_df23: non-match list between df2 and df3   #
# df12: complete match list between df1 and df2  #
# df13: complete match list between df1 and df3  # 
# df23: complete match list between df2 and df3  #
##################################################

# reset datasets for merging
df1 <- data_encounters_esperanza3months
df2 <- data_hotline_esperanza
df3 <- data_testing_sacredheart

# differentiating overlapping column names
colnames(df2) = toupper(names(df2))

###### merge df1 and df2 #######
# get lists of matching indices
idx12a <- results12$matches$inds.a
idx12b <- results12$matches$inds.b

# merge non-matching df1 and df2 
new_df1a = df1[c(1:dim(df1)[1])[-idx12a],]
new_df2a = df2[c(1:dim(df2)[1])[-idx12b],]
new_df12 = fastmerge(new_df1a, new_df2a)

# merge matching df1 and df2
match_a = df1[idx12a,]
match_b = df2[idx12b,]
match_12 = cbind(match_a, match_b)

# merge matching and non-matching df12
df12 = rbind(new_df12, match_12)
################################


###### merge df1 and df3 #######
# get lists of matching indices
idx13a <- results13$matches$inds.a
idx13b <- results13$matches$inds.b

# merge non-matching df1 and df3 
new_df1b = df1[c(1:dim(df1)[1])[-idx13a],]
new_df2b = df3[c(1:dim(df3)[1])[-idx13b],]
new_df13 = fastmerge(new_df1b, new_df2b)

# merge matching df1 and df2
match_a = df1[idx13a,]
match_b = df3[idx13b,]
match_13 = cbind(match_a, match_b)

# merge matching and non-matching df13
df13 = rbind(new_df13, match_13)
################################


###### merge df2 and df3 #######
# get lists of matching indices
idx23a <- results23$matches$inds.a
idx23b <- results23$matches$inds.b

# merge non-matching df2 and df3 
new_df1c = df2[c(1:dim(df2)[1])[-idx23a],]
new_df2c = df3[c(1:dim(df3)[1])[-idx23b],]
new_df23 = fastmerge(new_df1c, new_df2c)

# merge matching df2 and df3
match_a = df2[idx23a,]
match_b = df3[idx23b,]
match_23 = cbind(match_a, match_b)

# merge matching and non-matching df23
df23 = rbind(new_df23, match_23)
################################

## joining non-matching datasets
tempa = unique(rbind(new_df1a, new_df1b))
tempb = unique(rbind(new_df2a, new_df1c))
tempc = unique(rbind(new_df2b, new_df2c))

tempall = fastmerge(tempa, tempb)
tempall = fastmerge(tempall, tempc) # joined non-matching dataset

## column names to be used as search criteria
col1 = names(df1);
col2 = names(df2);
col3 = names(df3);

#function that finds matching elements based on df1
#and adjoins df2 to df1 by c1 as search criteria
#then overwrites df1 with df2 according to c2
findElement = function(df1, df2, c1, c2){
  found = c()
  flag = FALSE
  N = dim(df1)[1]
  
  idx = 1:N
  
  n1 = match(c1, names(df1));
  n2 = match(c1, names(df2));
  n3 = match(c2, names(df1));
  n4 = match(c2, names(df2));
  
  for (i in 1:N){
    if (flag == FALSE){
      for (j in 1:dim(df2)[1]){
        check = df1[i,n1[1:15]] == df2[j,n2[1:15]];
          if (all(check, na.rm=TRUE) == TRUE & sum(check, na.rm=TRUE) != 0){#
            cat(sprintf('%i/%i: index #%i\n', i, N, j))
            found[length(found)+1] = i;
            df2[j,n4] = df1[i,n3];
            flag = TRUE
            break
        }
      }
    }
    if (flag == FALSE){
      cat(sprintf('%i/%i: match not found\n', i, N))
    }
    flag=FALSE
  }
  ridx = which(!(idx %in% found))

  if (length(ridx) != 0){
    df2 = fastmerge(df2, df1[ridx,]);
  }
  
  return (list(df2, ridx, df2[j,n4], df1[i,n3]))
}

#link datasets sequentially
templist = findElement(match_12, tempall, col1, col2);
templist = findElement(match_13, templist[[1]], col1, col3);
templist = findElement(match_23, templist[[1]], col2, col3);

#final results
results = templist[[1]]

#save section matches
write.csv(match_12, '20210217_df12.csv', row.names = FALSE)
write.csv(match_13, '20210217_df13.csv', row.names = FALSE)
write.csv(match_23, '20210217_df23.csv', row.names = FALSE)

#save the finalized linked data
write.csv(results, '20210217_linked_data.csv', row.names = FALSE)
