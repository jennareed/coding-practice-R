## This script is to run practice examples
## for importing, exporting, and cleaning data from csv files

# clear your global environment
remove(list=ls())

# ENTER path for files to be saved
path = ""

# we will be using a dataset from R
# splitting the data, adding NAN values,
# exporting it to csv files for practice
# and then importing the files back and merging the data

### CREATE DATASETS AND EXPORT THEM AS .CSV FILES
# open the ChickWeight dataset
df <-  ChickWeight

# find a list of the diet
unique(df$Diet)

# find the number of samples in each diet
for(i in 1:length(unique(df$Diet))){
  print(paste('Diet',i, 'has', 
        sum(df$Diet==i),'rows of data',sep=" "))
}

## ADD ERRORS INTO THE DATA TO CLEAN LATER

# generate 10 random NA Time values in the dataset
index_t <- round(runif(10, min=1, max=nrow(df)))
df[index_t,"Time"] <- NA
# verify the NAs
sum(which(is.na(df$Time)) == sort(index_t))

# find a random weight and multiply by -1
index_w <- round(runif(1, min=1, max=nrow(df)))
df[index_w,"weight"] <- df[index_w,"weight"]*-1

## WRITE THE DATAFRAMES AS SEPARATE CSV FILES

# split the dataframes based by diet
mylist <- split(df, df$Diet)

# export data to csv files
for(i in 1:4){
  write.csv(mylist[[i]],file = paste(path,"diet_",i,".csv",sep=""),row.names=F)
}

## IMPORT THE CSV FILES TO MERGE 

# list the files in the folder
filenames <-  list.files(path,full.names = T)

# open each file and save to a list vector
files <- lapply(filenames, read.csv)

# make each csv file into a unique dataframe
# we will do this to practice merging them after
j <- 1
for(i in files){
  df_name <- paste("df_",j,sep="")
  assign(df_name, i)
  j <- j+1
}

# merge the dataframes into one large dataframe
# using rbind (binding by rows)
df_full <-  rbind(df_1,df_2,df_3,df_4)

## CLEAN THE DATA

# check the data types of the columns
str(df_full)

#find the number of NA values
sum(is.na(df_full))

# check which columns contain NA
apply(df_full, 2, function(x) any(is.na(x)))

# find the indeces of the NA values
index_na <- which(is.na(df_full$Time))

# plot these rows according to the other factors
# to see if there are patterns in the NA values
plot(df[index_na,"Chick"],xlab="Chick",ylab="# NA Values")
plot(df[index_na,"Diet"],xlab="Diet",ylab="# NA Values")
plot(df[index_na,"weight"],xlab="NA Index",ylab="Weight")

# remove the NA values
df_clean <- na.omit(df_full)

# check to see that the weights are within constraints
# between 0 and 500 (no negative values)
index_w <- which(df_clean$weight<0 | df_clean$weight>500)

# remove weight data that is outside of the constraints
if(index_w>0){
  df_clean <- df_clean[-index_w,]
}

## PLAY WITH THE DATA 
## select specific chicks, diets, ranges, etc.

# choose chick 50
chick_50 <- df_clean[which(df_clean$Chick==50),]

# choose diets 2 and 3
diets_2_3 <- df_clean[which(df_clean$Diet==2 | df_clean$Diet==3),]

# choose weights between 30 and 60 
weights <- df_clean[which(df_clean$weight>30 & df_clean$weight<60),]


## CREATE IDs IN THE DATA TO PRACTICE MERGING BY A KEY
# here the key will be the row number 
# this is a meaningless merge, just for practice
df_1$ID <- 1:nrow(df_1)
df_2$ID <- 1:nrow(df_2)

# merge by only including data with the same key
df_by_same <- merge(df_1, df_2, by="ID")

# merge and include all of the data, even if it doesn't match
# this will generate NAs where it doesn't match
df_by_all <- merge(df_1, df_2, by="ID", all=TRUE)

# you can also say "all.x = all" or "all.y = all"

# merge by two columns
df_1$ID_back <- 200 - 1:nrow(df_1)
df_2$ID_back <- 200 - 1:nrow(df_2)
df_by_two <- merge(df_1, df_2, by=c("ID","ID_back"))

## EXAMPLE FOR READING EXCEL FILES OTHER THAN CSV

# open the readxl package (part of tidyverse)
# and the Write XLS package
#library(readxl)
#library(WriteXLS)
#df_excel <- read_excel(path=paste(path,"diet_1.xlsx",sep=""))

# write to xls file
#WriteXLS(df_2, ExcelFileName = paste(path,"diet_2.xlsx",sep=""))
