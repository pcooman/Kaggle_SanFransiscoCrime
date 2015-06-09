# It's K Nearest Criminals! The show where we look at the crimes in the previous and subsequent week,
# and those committed in the immediate neighborhood to get a normalized histogram.
# This code loops through each line of the code, grabs the crimes from the training set that happened
# one week before, one week after, and within the immediate area.

# Warning: it's pretty time-intensive! For all 800.000 rows of the test data, it takes a little less than 2 hours.

library(dplyr)
library(reshape2)
library(readr)
library(ggplot2)
library(lubridate)

# Read in the training data
train <- read.csv("input/train.csv")
test <- read.csv("input/test.csv")

test$Dates <- as.Date(test$Dates)
train$Dates <- as.Date(train$Dates)

# Calculate the mean longitude coordinate for each district
train2 <- train[train$Y != 90,]
district_centers_Y <- lapply(split(train2$Y,train2$PdDistrict),mean)
district_centers_Y <- cbind(read.table(text = names(district_centers_Y), sep = "*", colClasses = "character"),unlist(district_centers_Y))
names(district_centers_Y) <- c("PdDistrict", "CenterY")

# Loop through the missing longitudes Y and fill in the mean value of the corresponding PdDistrict
index_centers_Y_missing <- which(test$Y == 90)
for (i in index_centers_Y_missing) {
  test$Y[i] <- district_centers_Y$CenterY[match(test$PdDistrict[i], district_centers_Y$PdDistrict)]
}

index_centers_Y_missing <- which(train$Y == 90)
for (i in index_centers_Y_missing) {
  train$Y[i] <- district_centers_Y$CenterY[match(train$PdDistrict[i], district_centers_Y$PdDistrict)]
}

# initialize the submission data frame
submission <- NULL

train$Descript <- NULL
train$DayOfWeek <- NULL
train$PdDistrict <- NULL
train$Resolution <- NULL
train$Address <- NULL

# Add weeks since start date
t_start <- train$Dates[nrow(train)]
train$Week <- floor(as.numeric(difftime(train$Dates, t_start, unit = "week")))+2
test$Week <- floor(as.numeric(difftime(test$Dates, t_start, unit = "week")))+2

train$Dates <- NULL
test$Dates <- NULL

train <- arrange(train,Week)
test <- arrange(test,Week)

# splitting the data up here and then indexing into it saves a lot of time compared to subsetting within the loop
train_split <- split(train,train$Week)
# test_split <- split(test,test$Week)

submission <- NULL

pt <- proc.time()

for (i in 1) {
  
  if (!(i %% 1000)) {
  print(i)
  }
  
  crimes_previous_week <- NULL
  crimes_next_week <- NULL
  
  j <- test$Week[i]
  
  if (j < 645) {
  crimes_next_week <- train_split[[ceiling(j/2)]]
  }
  
  if (j > 1) {
    crimes_previous_week <- train_split[[floor(j/2)]] 
  }
  
  crimes_weeks <- rbind(crimes_previous_week,crimes_next_week)
  crimes_subset <- subset(crimes_weeks, subset = (between(crimes_weeks$X,test$X[i] - 0.1,test$X[i] + 0.1) &
                                                      between(crimes_weeks$Y,test$Y[i] - 0.05, test$Y[i] + 0.05)), 
                                            select = c("Category"))
  crimes_table <- table(crimes_subset$Category)
  crimes_table_norm <- scale(crimes_table, center = FALSE, scale = nrow(crimes_subset))
  crimes_table_line <- as.data.frame.vector(crimes_table_norm)
  names(crimes_table_line) <- c("Prob")
  submission <- rbind(submission, t(crimes_table_line$Prob))
}

proc.time() - pt

# for each row of the test data
#for (i in c(1:100)) {
#  if (!(i %% 100)) {
#    print(i)
#  }
  # create a time interval
  # time_near <- interval(test$Dates[i] - days(7),test$Dates[i] + days(7))
  # subset only those observations from the training set that are near the test case (in both time and space)
  
#  crimes_subset <- subset(train, subset = (train$Dates %within% interval(test$Dates[i] -wday(test$Dates[i]) - days(7),test$Dates[i] - wday(test$Dates[i]) + days(14)) &
#                                             between(train$X,test$X[i] - 0.1,test$X[i] + 0.1) &
#                                             between(train$Y,test$Y[i] - 0.05, test$Y[i] + 0.05)), select = c("Category"))
  # make a contingency table of these crimes  
#  crimes_table <- table(crimes_subset$Category)
  # normalize with respect to the total number of crimes in this subset --> takes no time
#  crimes_table_norm <- scale(crimes_table, center = FALSE, scale = nrow(crimes_subset))
  # append this to the submission data frame --> takes no time
#  crimes_table_line <- as.data.frame.vector(crimes_table_norm)
#  names(crimes_table_line) <- c("Prob")
#  submission <- rbind(submission, t(crimes_table_line$Prob))
#}
#proc.time() - pt

# Remove variables from the workspace to create enough space to create the submission file
#rm(train)
#rm(train2)
#rm(test)
#rm(test2)

# add the Id column from test to submission
submission <- data.frame(Id = test$Id[800001:nrow(test)],submission)
# Create the submission file
# write.csv(submission, file = "Based_on_PdDistrict.csv", row.names=FALSE,quote=FALSE)
write_csv(submission, "kNearestCriminals_Part7.csv")
file.info("kNearestCriminals_Part7.csv")$size
dir()

part1<- read.csv("kNearestCriminals_Part1.csv")
part2<- read.csv("kNearestCriminals_Part2.csv")
part3<- read.csv("kNearestCriminals_Part3.csv")
part4<- read.csv("kNearestCriminals_Part4.csv")
part5<- read.csv("kNearestCriminals_Part5.csv")
part6<- read.csv("kNearestCriminals_Part6.csv")
part7<- read.csv("kNearestCriminals_Part7.csv")

submission <- NULL

submission <- rbind(submission, part1)
submission <- rbind(submission, part2)
submission <- rbind(submission, part3)
submission <- rbind(submission, part4)
submission <- rbind(submission, part5)
submission <- rbind(submission, part6)
submission <- rbind(submission, part7)

names(submission) <- c("Id",names(crimes_table))

# write_csv(submission,"kNearestCriminals_Submission.csv")

# data <- read_csv("kNearestCriminals.csv")
na_l <- which(is.na(submission$ARSON))
for (i in na_l) {
  submission[i,] <- c(test$Id[i],t(rep(0,times=39)))
}

submission$Id <- test$Id

write_csv(submission,"kNearestCriminals_Submission.csv")
