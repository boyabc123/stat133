training_raw = read.csv("D1_and_D2_Outstanding_Training_as_of_2015-1215.csv")
mapping_raw = read.csv("Mariposa User Role Map_4JAN2016.csv")
d1_raw = read.csv("D1 End User Mapping_30Jun15.csv")
d2_raw = read.csv("D2 End User Mapping_08Jul15.csv")
map_raw = read.csv("MAP.csv")
dailyupdate_raw = read.csv("MAP Report.csv")
BA_list_raw = read.csv("D2 BA Data Level.csv")
non_BA_list_raw = read.csv("other_displays.csv",header = F)
#################################################
#EXTRACT
training = training_raw[(training_raw[,13]!=""),c(2,3,5)]
mapping = cbind( mapping_raw[,1:3], ActivityGroup = 0, Role = mapping_raw[,7], Unmap = 0, Keep = 0 )
map = map_raw[,c(2,3,4)]
map[,2] = as.character(map[,2])
map[,3] = as.character(map[,3])


#need to change the format of the names in "daily_update"
Last_name_first = function(First_name_first){
  temp = as.character(First_name_first)
  output = rep(0,length(temp))
  for (i in (1:length(temp))){
    a = unlist( strsplit(temp[i], split="") )
    index = max(grep(" ", a))
    
    if(gregexpr("Jr|Jr.", temp[i])[[1]][1] > 0) {
      n = length(grep(" ", a))
      index = sort(grep(" ", a), partial=n-1)[n-1]
    }
    
    first = substr(temp[i],1,(index-1))
    last = substr(temp[i],(index+1),nchar(temp[i]))
    output[i] = paste0(last,",",first)
  }
  return(output)
}
##########################################################
#create training_new
training_new = training
training_new[which(is.na(training_new[,1])),]
training_new[which(is.na(training_new[,1])),1] = c(24737,10952,12760)
training_new[is.na(training_new[,1]),]
#85, 32735, 2840, 29207, 2640 are already removed from mapping
training_new = training_new[(training_new[,1] %in% mapping[,3]),]


############################
##cleaning AG names#########
############################
std_ag_list = read.csv("AG010_Activity Group Master.csv")
std_AG = std_ag_list[,1]
training_AG = as.character(training_new[,3])

#remove start with D2
index = unlist(grep("^(D2)",training_AG))
training_AG[(index)]
for(i in index){
  n = nchar(training_AG[i])
  training_AG[i] = substr(training_AG[i], start=4, n)
}
training_AG[(index)]

#remove ends with (D1)
index = unlist(grep("\\(D1\\)$",training_AG))
training_AG[(index)]
for(i in index){
  n = nchar(training_AG[i])
  training_AG[i] = substr(training_AG[i], start=1, n-5)
}
training_AG[(index)]

#LEX_WM_WM Outbound Material Handler
#LEX_WM_WM Warehouse Supervisor
index = unlist(grep("LEX_WM_WM Warehouse Supervisor", training_AG))
training_AG[(index)] = "LEX_WM_WM Warehouse Supervisor"
training_AG[(index)]
index = unlist(grep("LEX_WM_WM Outbound Material Handler", training_AG))
training_AG[(index)] = "LEX_WM_WM Outbound Material Handler"
training_AG[(index)]

training_new[,3] = (training_AG)

#training_AG thats not in map
training_AG[!(training_AG %in% as.character(map_raw[,2]))]
d_newest[!(d_newest[,3] %in% map_raw[,2]),3]

index = unlist(grep("\\)$", training_AG))
training_AG[(index)]



##########################################################
#create d1d2_new
daily_update = dailyupdate_raw[,c(2,3,8)]
daily_update[,2] = Last_name_first(daily_update[,2])
d1 = d1_raw[,c(1,2,6)]
d2 = d2_raw[,c(1,2,6)]
colnames(d2) = colnames(d1)
colnames(daily_update) = colnames(d2)
d_new = rbind(d1,d2,daily_update)



##########################################################
#create mapping_new subset
mapping_new = mapping[mapping[,3] %in% training_new[,1],]
sum(is.na(mapping_new))
length(unique(mapping_new[,3])) == length(unique(training_new[,1]))

##########################################################

#given id, table, map return a list of unmapping roles for the id
#table = userid + name + ag
#map = ag + derived.role.name
roles_for_id = function(id, table, map){
  ag_list = as.character( unique( table[ table[ , 1] == id, 3] ) )
  temp = map[(as.character(map[,1]) %in% ag_list),]
  return ( temp )
}
#input a list of strings, and return a list of strings
remove_last_part = function(str_list){
  temp = as.character(str_list)
  #at least 5 parts
  a = unlist(lapply(temp, function(x) length( gregexpr("_", x )[[1]])>3 ))
  #ends with _XXX
  b = unlist(lapply(temp, function(x) gregexpr("_([a-zA-Z0-9]){3}$",x)[[1]][1] > 0))

  c = temp[a&&b]
  end = unlist(lapply(c, function(x) gregexpr("_([a-zA-Z0-9]){3}$",x)[[1]][1]))
  temp[a&&b] = substr(temp[a&&b],1,(end-1))
  
  return(temp)
}


#check if user_role in the ag
#users is the user_mapping
#table = userid + name + ag
#map = ag + derived.role.name
unmap = function(users, table, map){
  output = rep(0, nrow(users))
  for (i in 1:nrow(users)){
    user_id = users[i,3]
    user_role = as.character(users[i,5])
    user_role_list = as.character(roles_for_id(user_id, table, map)[,2])
    if (length(user_role_list)>0){
      user_role_list_truncated = as.character(remove_last_part(user_role_list))
      output[i] = (user_role %in% user_role_list)|
                  (user_role %in% user_role_list_truncated)
    }
    else {
      output[i] = (user_role %in% user_role_list)
    }
  }
  return( output )
}
  

ptm = proc.time()

mapping_new[,6] = unmap(mapping_new, training_new, map)

proc.time()-ptm

###############################################################
###############################################################
###############################################################

d_newer = d_new[d_new[,1] %in% mapping_new[,3],]
#31913 32122 2594 not in d list
d_AG = as.character(d_newer[,3])
index1 = unlist(grep("LEX_WM_WM Warehouse Supervisor", d_AG))
index2 = unlist(grep("LEX_WM_WM Outbound Material Handler", d_AG))
index3 = unlist(grep("LEX_WM_WM Distribution Manager", d_AG))
d_AG[index1] = "LEX_WM_WM Warehouse Supervisor"
d_AG[index2] = "LEX_WM_WM Outbound Material Handler"
d_AG[index3] = "LEX_WM_WM Distribution Manager"
d_newer[,3] = d_AG



x_but_not_y = function(x,y){
  
  output = apply(x, 1, function(z){
    id = as.numeric(z[1])
    ag_id = as.character(z[3])
    ag_list = as.character( y[y[,1]==id,3] )
    return( !(ag_id %in% ag_list) )
  })
  return(output)
}
##################Jan20

d_newer_but_not_training_new = d_newer[as.logical(x_but_not_y(d_newer, training_new)),]
nrow(d_newer_but_not_training_new)



ptm = proc.time()
test = unmap(mapping_new, d_newer_but_not_training_new, map)
test[ which(mapping_new[,3] %in% c(31913,32122,2594)) ] = 0
proc.time()-ptm

sum(test)
nrow(mapping_highlihgted)
mapping_highlihgted = mapping_new[mapping_new[,6]==1,]


mapping_new[,7] = test 

table( mapping_new[,6], mapping_new[,7])

###############################################################
###############################################################
#BA
ba_list = as.character(BA_list_raw[,4])
mapping_ZB_DISPLAY = mapping_new[grep("^ZB.*DISPLAY$", mapping_new[,5]),]
mapping_ZB_DISPLAY[mapping_ZB_DISPLAY[,3]==24629,]
nrow(mapping_ZB_DISPLAY)

a = unique(mapping_ZB_DISPLAY[(mapping_ZB_DISPLAY[,6]==1 & mapping_ZB_DISPLAY[,7]==0),3])
b = unique(mapping_ZB_DISPLAY[(mapping_ZB_DISPLAY[,6]==0 & mapping_ZB_DISPLAY[,7]==1),3])

id_remove_ba = a[!(a %in% b)]
#id_remove_ba = c(282,2601,2616,2626,2641,2727,3043,3088,3414,10098,10292,10878,11000,13532,13669,14225,14266,16242,16257,16999,17204,21669,22158,23409,24753,24777,25590,25831,25979,26159,26869,27080,27911,29489,31714,32110,32383)

add_to_mapping_new = function(users, additional_list, add_id, col6){
  output = users
  if (col6){
    for (id in add_id){
      index1 = (output[,3] == id)
      index2 = (output[,5] %in% additional_list)
      output[(index1 & index2),6] = 1
  }}
  else{
    #id that has roles left
    for (id in add_id){
      index1 = (output[,3] == id)
      index2 = (output[,5] %in% additional_list)
      output[(index1 & index2),7] = 1
    }
  }
  return(output)
}



x = add_to_mapping_new(mapping_new, ba_list, id_remove_ba, col6 = T)


#non_ba
non_ba_list = as.character(unlist(non_BA_list_raw))
non_ba_list
id_keep = unique(mapping_new[(mapping_new[,6]==0
  | mapping_new[,7]==1) ,3])
y = add_to_mapping_new(x, non_ba_list, id_keep, col6 = F)

table(x[,6],x[,7])
table(y[,6],y[,7])
table(mapping_new[,6],mapping_new[,7])


################################
#cleanup names
name_id = unique(mapping[,c(2,3)])
output = y
for (id in unique(output[,3])){
  output[output[,3]==id,2] = name_id[name_id[,2]==id,1]
}




write.csv(unique(output), "unmapping_ROLES.csv", row.names = FALSE)


###############################################################
###############################################################
###############################################################
######################END######################################
###############################################################
###############################################################
###############################################################

#fill Activity Group
'''
head(mapping_final)
head(training_new)
head(training_new)
#Check if a role is in the group
in_the_group = function(role,ag){
  table = map
  return(c(role %in% as.character(table[table[,1]==ag,2]), ag))
}


id = 25590
ag = unique(as.character(training_new[training_new[,1]==id,3]))
  role = unique(as.character(mapping_final[mapping_final[,3]==id,5]))
temp = matrix(0, nrow = length(role), ncol = length(ag), 
dimnames = list(role,ag))
for (i in 1:nrow(temp)){
for (j in 1:ncol(temp)){
 if (in_the_group(role[i],ag[j])[1]) temp[i,j] = in_the_group(role[i],ag[j])[2]
}
}
temp

'''

'''
report = list()
counter = 1

#Create a list of matrix indicating which role is in which group
for (id in unique(mapping_final[,3])){
  ag = unique(as.character(training_new[training_new[,1]==id,3]))
  role = unique(as.character(mapping_final[mapping_final[,3]==id,5]))
  temp = matrix(0, nrow = length(role), ncol = length(ag), 
                dimnames = list(role,ag))
  for (i in 1:nrow(temp)){
    for (j in 1:ncol(temp)){
      #temp[i,j] = in_the_group(role[i],ag[j])[1]
      if (in_the_group(role[i],ag[j])[1]) {
        temp[i,j] = in_the_group(role[i],ag[j])[2]}
      else {
        temp[i,j] = ""
      }
    }
  }
  report[[counter]] = temp
  counter = counter + 1
}

sapply(report, function(x) apply(x, 1, sum))

report[[67]]
unique(mapping_final[,3])[67]

#id25590
'''


'''
kathy = read.csv("Kathy.csv", header = T)

orange_rows = kathy[(1:16),c(2,3,4,7,10)]
pink_rows = kathy[(17:44),c(2,3,4,7,10)]
head(pink_rows)
View(pink_rows)

dic_username_emplid = unique(mapping_raw[,c(1,3)])
first = unlist(lapply(pink_rows[,3], function(x) substring(x,2, nchar(as.character(x)) )))
Preferred.Name = paste0(pink_rows[,2],",",first)
User.Name = rep(0,nrow(pink_rows))
#2594, 12760, 695, 1853, 31660, 24392, 3343
for (i in 1:nrow(pink_rows)){
User.Name[i] = as.character(dic_username_emplid[dic_username_emplid[,2]==pink_rows[i,1],1])
}
#User.Name, Preferred.Name, EmplID, Role
Role = unlist(lapply(pink_rows[,5], function(x) substr(x,5,nchar(as.character(x)))))
pink_rows_new = cbind(User.Name, 
Preferred.Name,
EmplID=pink_rows[,1],
ActivityGroup = 0,
Role,
Unmap = 1,
NotKeep = 0)

mapping_highlihgted = rbind(mapping_highlihgted, pink_rows_new)
'''


