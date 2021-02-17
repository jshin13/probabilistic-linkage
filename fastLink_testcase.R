#install.packages('fastLink')
#install.packages('readxl')

# import library
library('fastLink')
library('readxl')
library('magrittr')

# read database
db1A <- read_excel('20200724_matching_metCV_by_model.xlsx', sheet = 'model_1A')
db3 <- read_excel('20200724_matching_metCV_by_model.xlsx', sheet = 'model_3')

# read column (remove first 3 that contains unique identifier (e.g. sample id #))
col1 <- names(db1A) %>%
  .[4:length(.)]
col2 <- names(db3) %>%
  .[4:length(.)]

# change one numeric column as string column (for testing purpose)
db1A = transform(db1A, wt = as.character(wt))
db3 = transform(db3, wt = as.character(wt))

# find matching columns
# [1] "age"     "sex"     "ht"      "wt"      "waist"   "smokehx"
col_both <- col2[col1 == col2]

# select columns to be used for linkage
cols = col_both[which(c(1,1,1,1,1,1) == 1)]

# id columns to be permuted
plist = which(c(1,0,0,0,1,1) == 1)

# permutate selected column
for (i in plist){
  db1A[[col_both[i]]] = sample(db1A[[col_both[i]]])
  db3[[col_both[i]]] = sample(db3[[col_both[i]]])  
}

c = 0 # counter to select specific database
ndel = 100 #number of elements deleted

###### randomly remove values ######
if (ndel > 0)
  for (i in list(db1A, db3)){
    
    c = c + 1
    nrow = dim(i)[1]
    ncol = dim(i)[2]
    
    rowList = sample(c(1:nrow), ndel, replace=TRUE)
    colList = sample(c(2:ncol), ndel, replace=TRUE)
    
    for (j in 1:length(rowList))
    {
      col = names(i)[colList[j]]
      if (c == 1)
        db1A[[col]][rowList[j]] = NA
      else
        db3[[col]][rowList[j]] = NA
    }
  }
####################################

# run fastLink
results <- fastLink(db1A, db3, cols, stringdist.match = 'wt', cut.a = 1, cut.p = 0.7,
                    numeric.match = cols[cols!='wt'], cut.p.num = 2.5, cut.a.num = 1, 
                    threshold.match = 0.85)

# acquire true label wrt db3
# y_1 = match(db3$idno, db1A$idno)
# y_2 = match(db1A$idno, db3$idno)

# acquire estimated label wrt dfA
y_a = results$matches$inds.a
y_b = results$matches$inds.b

# calculate accuracy
s = db3[y_b,'idno'] == db1A[y_a,'idno']
acc = sum(s) / dim(db3)[1]
