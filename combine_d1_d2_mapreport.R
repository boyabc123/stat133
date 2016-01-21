d1_raw = read.csv("D1 End User Mapping_30Jun15.csv")
d2_raw = read.csv("D2 End User Mapping_08Jul15.csv")
training_raw = read.csv("D1_and_D2_Outstanding_Training_as_of_2015-1215.csv")
training = training_raw[(training_raw[,13]!=""),c(2,3,5)]
training_new = training
training_new[which(is.na(training_new[,1])),]
x = which(is.na(training_new[,1]))
training_new[which(is.na(training_new[,1])),1] = c(24737,10952,12760)
training_new[is.na(training_new[,1]),]

dailyupdate_raw = read.csv("MAP Report.csv")
d1 = d1_raw[,c(1,2,6)]
d2 = d2_raw[,c(1,2,6)]
dd = dailyupdate_raw[,c(2,3,8)]
colnames(d1) = colnames(d2)
colnames(dd) = colnames(d2)

result = rbind(d1[1:801,],d2[1:12111,],dd)

for (i in 1:nrow(result)){
  temp = result
  temp_id = temp[i,1]
  temp2 = as.character(training_new[training_new[,1]==temp_id,3])
  if(as.character(temp[i,3]) %in% temp2){
    result[i,4] = 0
  }
  else{
    result[i,4] = 1
  }
}

result = result[result[,4]==1,-4]


write.csv(result, "d_list.csv", row.names = FALSE)
