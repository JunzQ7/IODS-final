# Juhana Rautavirta 
# juhana.rautavirta@helsinki.fi
# 3.3.2017
# This data is collected from Italian secondary school students who study Portuguese and mathematics.
# There are several variables, which are concerning for example alcohol consumption, family relationships and usage of free time.
# 
# https://archive.ics.uci.edu/ml/datasets/STUDENT+ALCOHOL+CONSUMPTION

# Reading the two datasets
math <- read.csv("/home/juhana/Desktop/IODS-project/data/student-mat.csv", sep = ";", header = T)
por <- read.csv("/home/juhana/Desktop/IODS-project/data/student-por.csv", sep = ";", header = T)

# Exploring the structure and dimension
str(math)
dim(math)
str(por)
dim(por)

library(ggplot2)
library(GGally)
library(tidyr)
library(dplyr)

# Because we are combining two datasets and there are students who have answered both of these surveys, we need to identify these students.
# Let's identify them with these variables:

join_by <- c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery","internet")

# Joining the two datasets
math_por <- inner_join(math, por, by = join_by, suffix = c(".math", ".por"))

alc <- select(math_por, one_of(join_by))

# the columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]
# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# Creating a new column for alcohol usage on weeks and weekends and another column
# for high usage of alcohol

alc <- mutate(alc, alc_use = (Dalc + Walc)/2)
alc <- mutate(alc, high_use = alc_use > 2)

# Creating a logical variable that is TRUE if both of the student's parents are at home and not in a job
alc <- mutate(alc, bothAtHome = (Mjob == "at_home" & Fjob == "at_home"))

# Creating a logical variable that is TRUE if both of the student's parents have at least a secondary level education
alc <- mutate(alc, parentEdu = (Medu > 2 & Fedu > 2))




setwd("/home/juhana/Desktop/IODS-final-master/")
write.csv(x = alc, file = "alc.csv")
