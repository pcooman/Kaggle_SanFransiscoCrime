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

#test$Dates <- as.Date(test$Dates)
#train$Dates <- as.Date(train$Dates)

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
train$Hour <- hour(ymd_hms(train$Dates))
test$Hour <- hour(ymd_hms(test$Dates))

train$Month <- month(ymd_hms(train$Dates))
test$Month <- month(ymd_hms(test$Dates))

train$Year <- year(ymd_hms(train$Dates))
test$Year <- year(ymd_hms(test$Dates))


train$Night <- 0
test$Night <- 0

train$Night[train$Hour > 20 | train$Hour < 8] <- 1
test$Night[test$Hour > 20 | test$Hour < 8] <- 1



crimes_split <- split(train$Category, list(train$Year,train$Month,train$PdDistrict,train$Night), drop = TRUE)
#crimes <- lapply(crimes_split, myfunction)

crimes_matrix <- NULL

for (i in 1:length(crimes_split)) {
  if (!(i %% 100)) {
    print(i)
  }
x <- crimes_split[i]
crimes_table <- table(x)
crimes_table_line <- as.data.frame.vector(crimes_table,row.names = NULL)
crimes_table_line_norm <- t(crimes_table_line/sum(crimes_table))
crimes_matrix <- rbind(crimes_matrix,crimes_table_line_norm[2,])
}

# 878286

crimes_probs <- cbind(read.table(text = names(crimes), sep = ".", colClasses = "character"),crimes_matrix)
colnames(crimes_probs)[1] <- "Year"
colnames(crimes_probs)[2] <- "Month"
colnames(crimes_probs)[3] <- "PdDistrict"
colnames(crimes_probs)[4] <- "Night"


submission <- subset(test,select = c("Id","Year","Month","PdDistrict","Night"))
submission <- merge(submission,crimes_probs, by = c("Year","Month","PdDistrict","Night"))

submission$Year <- NULL
submission$Month <- NULL
submission$PdDistrict <- NULL
submission$Night <- NULL

write_csv(submission, "SF_Crime_Darkest_Knight.csv")
