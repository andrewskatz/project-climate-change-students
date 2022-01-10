getwd()
setwd('/Volumes/GoogleDrive/My Drive/shared/students and climate change')
students = read.csv(file='climate_data.csv')
colnames(students)
students$Q29 == 4
civil_engineers <- subset(students, Q29 == 4)
#technical and social issue
barplot(table(civil_engineers$Q28a), main = 'Environmental')
barplot(table(civil_engineers$Q28b), main = 'Moral')
barplot(table(civil_engineers$Q28c), main = 'Religious')
barplot(table(civil_engineers$Q28d), main = 'Social Justice')
barplot(table(civil_engineers$Q28e))
barplot(table(civil_engineers$Q28f))
barplot(table(civil_engineers$Q28g))
barplot(table(civil_engineers$Q28h))
barplot(table(civil_engineers$Q28i))
barplot(table(civil_engineers$Q28j))
barplot(table(civil_engineers$Q28k))
barplot(table(civil_engineers$Q28l))

#Q27
barplot(table(civil_engineers$Q27a))
barplot(table(civil_engineers$Q27b))
barplot(table(civil_engineers$Q27c))
barplot(table(civil_engineers$Q27d))
barplot(table(civil_engineers$Q27e))
barplot(table(civil_engineers$Q27f))
barplot(table(civil_engineers$Q27g))
barplot(table(civil_engineers$Q27h))
barplot(table(civil_engineers$Q27i))

#subseting civil engineers who answered Now or 10 years 
civil_engineers_Q27a = subset(civil_engineers, Q27a == 1 | Q27a == 2)
civil_engineers_Q27b = subset(civil_engineers27a, Q27b == 1 | Q27b == 2)
civil_engineers_Q27abc = subset(civil_engineers_Q27b, Q27c == 1 | Q27c == 2)
#subsetting those that also said social justice, 
civil_engineers_Q27_and_28 = subset(civil_engineers_Q27abc, Q28b > 2 & Q28j >2 & Q28e > 2 & Q28l > 2)

#not
#had to create an opertor that is opposite for in
'%not in%' = function(x,y)!('%in%'(x,y))
#comparing to dataframes and keeping the group of students not in the subset
civil_engineers_no_Q27_and_28 = subset(civil_engineers, Litho %not in% civil_engineers_Q27_and_28$Litho)
#civil_engineers_no_Q27_and_28$Q5d[civil_engineers_no_Q27_and_28$Q5d == ' '] == 0
civil_engineers_Q5d_yes_climate = data.frame(civil_engineers_Q27_and_28$Litho, civil_engineers_Q27_and_28$Q5d)
colnames(civil_engineers_Q5d_yes_climate)[colnames(civil_engineers_Q5d_yes_climate)=="civil_engineers_Q27_and_28.Litho"] <- "Litho"
colnames(civil_engineers_Q5d_yes_climate)[colnames(civil_engineers_Q5d_yes_climate)=="civil_engineers_Q27_and_28.Q5d"] <- "Q5d"
civil_engineers_Q5d_yes_climate['Immediate_Global_Warming'] = 'Yes'
civil_engineers_Q5d_no_climate = data.frame(civil_engineers_no_Q27_and_28$Litho, civil_engineers_no_Q27_and_28$Q5d)
colnames(civil_engineers_Q5d_no_climate)[colnames(civil_engineers_Q5d_no_climate)=="civil_engineers_no_Q27_and_28.Litho"] <- "Litho"
colnames(civil_engineers_Q5d_no_climate)[colnames(civil_engineers_Q5d_no_climate)=="civil_engineers_no_Q27_and_28.Q5d"] <- "Q5d"
civil_engineers_Q5d_no_climate['Immediate_Global_Warming'] = 'No'
civil_engineers_Q5d = dplyr::bind_rows(civil_engineers_Q5d_yes_climate, civil_engineers_Q5d_no_climate)
#changing NA to 0
civil_engineers_Q5d[is.na(civil_engineers_Q5d)] <- 0
tbl = table(civil_engineers_Q5d$Q5d, civil_engineers_Q5d$Immediate_Global_Warming)
tbl
chisq.test(tbl)
55/164
35/382
