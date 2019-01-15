### setting the working directory
setwd("C:/Users/htw606/Documents/2018 09 08 Universitet/Disinfo/Logistics/Own course/Courses 2018 info/SNA/scripts")

### loading a dataframe with student names
df <- read.csv("data/student_names.txt", stringsAsFactors = F)

### assigning a random group number ranging from 1 to 10 for each student
df$group_number <- sample(1:10,replace=T, nrow(df))

### sorting names by group number
library(dplyr)
df <- df %>% arrange(group_number)

### exporting the dataset
write.csv(df, "data/studentgroups.txt")