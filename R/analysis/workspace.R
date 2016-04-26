# Episode Stats
# - length distribution
# - agreement by length
# - number per sleep episode (normalized for 8h episodes)


# Lengths of NREM/REM without wake interruption (so REM bordered by NREM and NREM bordered by 2 REM)
chp <- episodes[method=="changepoint"]
col_chp <- chp[,collapsed_changepoint(.SD),by='subject_code,activity_or_bedrest_episode']
ws <- col_chp[subject_code=='3353GX' & activity_or_bedrest_episode==3]
col_chp[,`:=`(next_label=c(label[-1L], NA), prev_label=c(NA,label[-.N]),next_length=c(length[-1L], NA),prev_length=c(NA,length[-.N])),by='subject_code,activity_or_bedrest_episode']
col_chp[,neighbor_length:=prev_length+next_length]

nrem_lengths_around_wake <- col_chp[label=='WAKE' & prev_label=='NREM' & next_label=='NREM', (next_length+prev_length)]

qplot(log(length), log(neighbor_length), data=nrem_neighbors) + geom_smooth()
qplot(length, neighbor_length, data=nrem_neighbors[length<50]) + geom_smooth()
qplot(length, neighbor_length, data=col_chp[label=='WAKE' & prev_label=='REM' & next_label=='REM'])

nrem_neighbors <- col_chp[label=='WAKE' & prev_label=='NREM' & next_label=='NREM']
rem_neighbors <- col_chp[label=='WAKE' & prev_label=='REM' & next_label=='REM']
rem_to_nrem <- col_chp[label=='WAKE' & prev_label=='REM' & next_label=='NREM']
nrem_to_rem <- col_chp[label=='WAKE' & prev_label=='NREM' & next_label=='REM']

# 1450
nrem_neighbors # 763
rem_neighbors  # 10
rem_to_nrem    # 134
nrem_to_rem    # 35


nrem_full_episodes <- col_chp[label=="NREM" & prev_label=='REM' & next_label=="REM"]
nrem_broken_episodes <- col_chp[label=="NREM" & ((prev_label=='REM' & next_label == 'WAKE') | (prev_label=="WAKE" & next_label =="REM"))]


summary(nrem_neighbors$neighbor_length)
summary(nrem_full_episodes$length)
summary(nrem_broken_episodes$length)
summary(nrem_neighbors$prev_length)
summary(nrem_neighbors$next_length)
summary(nrem_neighbors$length + nrem_neighbors$neighbor_length)

?qplot
