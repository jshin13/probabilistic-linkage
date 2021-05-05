#LOAD PACKAGES####
library(Hmisc)
library(excel.link)
library(tidyverse)
library(ggplot2)

#define folder to be read from
setwd("S:/Page_IRB00252774/intervention_evaluation_study") #setting a default path
datafolder = './raw_data'
resultfolder = './R_scripts'
filename = 'Covid19ResponseForTh_DATA_2021-05-03_1128.csv'

#read linked datasets and remove missing rows by first dataset MRN column
df = read.csv(paste0(datafolder, '/', filename))

new_df = df %>% drop_na(mrn)

for (i in c('mrn', 'zip')){
  cond = new_df[i] != ''
  new_df = new_df[cond,]
}

#census tract conversion (input)
#in order to convert physical address into census tract number on census website,
#dataframe is clean to meet the format required by the API
#https://geocoding.geo.census.gov/geocoder/geographies/addressbatch?form
#index | address | city | state | zip code
col = c('address', 'city', 'zip') #note that state information is missing
CT_df = new_df[,col] %>% add_column(state = '', .after='city')
write.table(CT_df, paste0(resultfolder, '/', 'census_input.csv'), sep=',', col.names=FALSE)

#clean census tract import
#reading converted csv file
fname = paste0(resultfolder, '/', 'GeocodeResults.csv')
ct_import = read.csv(fname, header=FALSE)# %>% order('V1')
ct_sorted = ct_import[order(ct_import$V1),]

#get tie (there are several matching states [match, tie, no_match])
#while enough address information is provided for entries marked as tie
#the conversion has not happened, thus the dataframe with only the
#tied results are submitted again for re-evaluation (work in progress)
tie_idx = ct_sorted[ct_sorted$V3 == 'Tie',]$V1
ct_tie = CT_df[match(tie_idx, rownames(CT_df)),]
write.table(ct_tie, paste0(resultfolder, '/', 'census_tiebreaker.csv'), sep=',', col.names=FALSE)

#select match (V11: census tract number, V12: census block number)
#removes unsuccessful match by V12 column
ct_select = ct_sorted[ct_sorted$V12 != '',] %>% drop_na()
df_ct_select = new_df[match(ct_select$V1, rownames(new_df)),] #rownames(new_df)

#evaluate positivity rate by date
cols = c('lab_date', 'lab_result','age')
df_result = df_ct_select[,cols]
df_result$census = ct_select$V11

result_table = df_result %>% group_by(lab_date) %>% 
                            summarise(age = mean(age), 
                                      pos_rate = sum(lab_result) / length(lab_result),
                                      total_pos = sum(lab_result),
                                      total_num = length(lab_result))

result_table = result_table[order(as.Date(result_table$lab_date, format='%m/%d/%Y')),]
# result_table$lab_date = factor(result_table$lab_date, levels=result_table$lab_date)

write.csv(result_table, paste0(resultfolder, '/', 'positivity_bytime.csv'))

#generate figure for positivity rate over time
ggplot(data=result_table, aes(x=lab_date, y=pos_rate, group=1)) + 
  geom_line() + geom_point() + labs(y='Positivity Rate (positive cases over total number)',title='Positivity Rate over time') +
  theme(axis.text.x = element_text(angle = 45,size=8, hjust = 1), 
        axis.title.x = element_blank(), axis.title.y=element_text(size=10),
        plot.title = element_text(hjust=0.5))

ggsave(paste0(resultfolder, '/', 'pos_time.png'), device=png(), width=10, height=4)


#evaluate positivity rate by census tract number
result_table = df_result %>% group_by(census) %>% 
  summarise(age = mean(age), 
            pos_rate = sum(lab_result) / length(lab_result),
            total_pos = sum(lab_result),
            total_num = length(lab_result))

result_table = result_table[order(as.character(result_table$census)),]

write.csv(result_table, paste0(resultfolder, '/', 'positivity_bycensus.csv'))

#generate figure for positivity rate by census number
ggplot(data=result_table, aes(x=census, y=pos_rate, group=1)) +
  geom_point() + labs(x='Census Block Number', y='Positivity Rate (positive cases over total number)', title='Positivity Rate by Census Block Number') +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_text(size=9), axis.title.y=element_text(size=9),
        plot.title = element_text(hjust=0.5))

ggsave(paste0(resultfolder, '/', 'pos_census.png'), device=png(), width=10, height=4)
