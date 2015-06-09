# Based on some exploratory plotting, it appears like the day of the week has little to no influence
# on the category of the crime. Here I estimate the odds of each crime category based on the PdDistrict,
# simply by normalizing the counts for each district.

# For some reason I can't generate an output file here. Can anybody explain why? Thanks!


library(dplyr)
library(reshape2)
library(readr)
library(ggplot2)
library(lubridate)
library(arrayhelpers)

# Read in the training data
data <- read.csv("input/train.csv")
data$Year <- year(ymd_hms(data$Dates))
data$Month <- month(ymd_hms(data$Dates))
data$Day <- day(ymd_hms(data$Dates))
data$Hour <- hour(ymd_hms(data$Dates))
data$Minute <- minute(ymd_hms(data$Dates))
data$Second <- second(ymd_hms(data$Dates))

data$Year <- factor(data$Year)

# Build a contingecy table: count of each crime category for each district
crimes_by_district <- table(data$PdDistrict,data$Year,data$Category)

crimes_total <- matrix(data = 0, nrow = dim(crimes_by_district)[1], ncol = dim(crimes_by_district)[2])
for (i in 1:dim(crimes_by_district)[1]) {
  for (j in 1:dim(crimes_by_district)[2]) {
    i
    j
    crimes_total[i,j] <- sum(crimes_by_district[i,j,])
  }
}

for (k in 1:dim(crimes_by_district)[3]) {
  crimes_by_district[,,k] <- crimes_by_district[,,k]/crimes_total
}

crimes_by_district_melt <- melt(crimes_by_district)
names(crimes_by_district_melt) <- c("PdDistrict","Year","Category","Prob")
crimes_by_district_norm <- data.frame(crimes_by_district_melt)
crimes_by_district_norm2 <- acast(crimes_by_district_norm, PdDistrict + Year ~ Category, crimes_by_district_norm = "Prob")

x <- cbind(read.table(text = rownames(crimes_by_district_norm2), sep = "_"))
crimes_by_district_norm2 <- cbind(x,crimes_by_district_norm2)
colnames(crimes_by_district_norm2)[1] <- "PdDistrict"
colnames(crimes_by_district_norm2)[2] <- "Year"

# Read in the test data
test <- read.csv("input/test.csv")

test$Year <- year(ymd_hms(test$Dates))

# Only retain the Id and the PdDistrict
submission <- subset(test, select = c("Id","PdDistrict","Year"))
# Merge the test data with the normalized crimes_by_district
submission <- merge(submission,crimes_by_district_norm2, by = c("PdDistrict","Year"))
# Drop the PdDistrict column
submission$PdDistrict <- NULL
submission$Year <- NULL


# Remove variables from the workspace to create enough space to create the submission file
rm(data)
rm(crimes_by_district)
rm(crimes_by_district_norm)
rm(test)

head(submission)

# Create the submission file
# write.csv(submission, file = "Based_on_PdDistrict.csv", row.names=FALSE,quote=FALSE)
write_csv(submission, "Based_on_PdDistrict_and_Year.csv")
file.info("Based_on_PdDistrict_and_Year.csv")$size
dir()