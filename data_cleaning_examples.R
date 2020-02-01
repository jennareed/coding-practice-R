## This script is to run practice examples
## for importing, exporting, and cleaning data from excel files

# clear your global environment
remove(list=ls())

# we will be using a dataset from R
# splitting the data, adding NAN values,
# exporting it to an excel file for practice
# and then importing the files back and merging the data

# open the ChickWeight dataset
df <-  ChickWeight

# find a list of the diet
unique(df$Diet)

# find the number of samples in each diet
for(i in 1:length(unique(df$Diet))){
  print(paste('Diet',i, 'has', 
        sum(df$Diet==i),'rows of data',sep=" "))
}

# generate 10 random NA Time values in the dataset
nums <- round(runif(10, min=1, max=nrow(df)))

for(n in 1:length(nums)){
  for(i in 1:nrow(df)){
    if(i==nums[n]){
      df[i,"Time"] <- NA
    }
  }
}

# verify the NAs
sum(which(is.na(df$Time)) == sort(nums))

# split the dataframes based by diet
mylist <- split(df, df$Diet)

# export data to excel files
for(i in 1:4){
  write.csv(mylist[[i]],file = paste("data/diet_",i,".csv",sep=""))
}