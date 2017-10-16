
library(tidyr)
library(dplyr)
library(stringr)
titanic_data <- read.table("titanic_original.csv", header=TRUE, sep=",")
fill_miss_strcolumn<-function(x,fill_str){
  y=c()
  for (value in x){
    new_value<-gsub(" ","",value)
    if (str_length(new_value)==0){
      y<-c(y,fill_str)
    }else{
      y<-c(y,value)
    }
  }
  return(y)
}
fill_with_mean<-function(x){
  y=c()
  mean_value<-mean(x,na.rm=TRUE)
  for (value in x){
    if (is.na(value)){
      y<-c(y,mean_value)
    }else{
      y<-c(y,value)
    }
  }
  return(y)
}
fill_embrk="S"
fill_boat="NA"
titanic_data$embarked<-fill_miss_strcolumn(titanic_data$embarked,fill_embrk)

titanic_data$age<-fill_with_mean(titanic_data$age)

titanic_data$boat<-fill_miss_strcolumn(titanic_data$boat,fill_boat)

titanic_data<-titanic_data%>%mutate(has_cabin_number = ifelse(str_length(cabin)==0, 0, 1))

write.csv(titanic_data, file = "titanic_clean.csv")
