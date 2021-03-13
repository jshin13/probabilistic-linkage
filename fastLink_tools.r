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

#function that finds matching elements based on df1
#and adjoins df2 to df1 by using c1 as search criteria
#then overwrites the row with df2 according to c2
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
        check = df1[i,n1[1:5]] == df2[j,n2[1:5]];
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

#probabilistic linkage by fastLink
probLink = function(df1, df2, df1s, df2s, col1, col2, pmatch){
  
  #pmatch: column names for partial match
  
  #find matching columns between df1 and df2
  mcol = intersect(col1, col2)
  pmatch = intersect(mcol, pmatch)
  
  #matching between df1 and df2
  results12 <- fastLink(df1, df2, mcol, stringdist.match = mcol, cut.a = 1, cut.p = 0.7,
                        partial.match = pmatch, threshold.match = 0.85, return.df=TRUE)
  
  # #matching indices
  # A12 = results12$dfA.match
  # B12 = results12$dfB.match
  
  # df1 = df1s
  # df2 = df2s
  
  # # differentiating overlapping column names (important)
  # colnames(df1) = tolower(names(df1))
  # colnames(df2) = toupper(names(df2))
  
  # if (keep_scolumns == TRUE){
  #   names(df1)[!is.na(match(names(df1), col1))] = paste0(col1, 1) #replace names
  #   names(df2)[!is.na(match(names(df2), col1))] = paste0(col2, 1) #replace names
  # }
  
  ##########DATA MERGING STEP#######################
  # match_12: unique match list between df1 and df2#
  # new_df12: non-match list between df1 and df2   #
  ##################################################
  
  # get lists of matching indices
  idx12a <- results12$matches$inds.a
  idx12b <- results12$matches$inds.b

  # overwriting UUID (1st onto 2nd)
  # assumes UUID to be the first column
  # grep('UUID', colnames(df_sep[[1]]))
  df2[idx12b,][1] = df1[idx12a,][1]
  df2s[idx12b,][1] = df1s[idx12a,][1]
    
  # merge non-matching df1 and df2 
  new_df1a = df1s[c(1:dim(df1s)[1])[-idx12a],]
  new_df2a = df2s[c(1:dim(df2s)[1])[-idx12b],]
  
  # merge matching df1 and df2
  match_a = df1s[idx12a,]
  match_b = df2s[idx12b,]
  match_12 = cbind(match_a, match_b)
  
  return (list(new_df1a, new_df2a, match_12, df2, df2s)) #return non-match and match
}

#runs probabilistic linkage over all possible permutations
fullSurvey = function(df, df_sep, colist, pmatch){
  
  dfNUM = length(df)
  match = list()
  nonmatch1 = list()
  nonmatch2 = list()
  c = 0
  
  for (df_i in 1:(dfNUM-1)){
    for (df_j in (df_i+1):dfNUM)
    {
      c = c + 1
      
      #prob linkage
      templink = probLink(df[[df_i]],df[[df_j]],df_sep[[df_i]],df_sep[[df_j]], 
                          col1=colist[[df_i]],col2=colist[[df_j]], 
                          pmatch=pSEARCH)
      
      # store match and non-match
      nonmatch1[[c]] = templink[[1]]
      nonmatch2[[c]] = templink[[2]]
      match[[c]] = templink[[3]]
      df[[df_j]] = templink[[4]] #reassign UUID
      df_sep[[df_j]] = templink[[5]] #reassign UUID
    }
  }
  return (list(nonmatch1, nonmatch2, match, df, df_sep))
}

#uuid generator
uuid_generator = function(N=1){
  temp = gsub("([-])", "", UUIDgenerate(FALSE, n=N))
  temp = gsub("(\\w{4})(\\w{4})(\\w{4})(\\w{4})", "\\1-\\2-\\3-\\4", substr(temp,1,16))
  return (temp)
}

