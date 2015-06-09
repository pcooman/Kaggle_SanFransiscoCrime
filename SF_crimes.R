# Based on some exploratory plotting, it appears like the day of the week has little to no influence
# on the category of the crime. Here I estimate the odds of each crime category based on the PdDistrict,
# simply by normalizing the counts for each district.

# For some reason I can't generate an output file here. Can anybody explain why? Thanks!


library(dplyr)
library(reshape2)
library(readr)
library(ggplot2)

# Read in the training data
data <- read_csv("input/train.csv.zip")

# Plot crimes by district
crimes_by_district_plot <- table(data$Category,data$PdDistrict)
crimes_by_district_plot <- melt(crimes_by_district_plot)
names(crimes_by_district_plot) <- c("Category","PdDistrict","Count")

g <- ggplot(crimes_by_district_plot,aes(x=Category, y=Count,color = Category)) + 
  geom_bar(stat = "Identity") + 
  coord_flip() +
  facet_grid(.~PdDistrict) +
  theme(legend.position = "none")
ggsave(g, file="Crimes_by_district.png", width=20, height=8)

# Build a contingecy table: count of each crime category for each district
crimes_by_district <- table(data$PdDistrict,data$Category)
# normalize the counts by the sum of the rows
crimes_by_district_norm <- t(scale(t(crimes_by_district), center = FALSE, scale = colSums(t(crimes_by_district))))
# save as a data frame
crimes_by_district_norm <- as.data.frame.matrix(crimes_by_district_norm) 
# Round to four digits to reduce submission file size?
crimes_by_district_norm <- round(crimes_by_district_norm, digits = 4)

# add a a column with the PdDistrict (for merging later)
crimes_by_district_norm$PdDistrict <- rownames(crimes_by_district_norm)

# Read in the test data
test <- read_csv("input/test.csv.zip")

# Only retain the Id and the PdDistrict
submission <- subset(test, select = c("Id","PdDistrict"))
# Merge the test data with the normalized crimes_by_district
submission <- merge(submission,crimes_by_district_norm, by = "PdDistrict")
# Drop the PdDistrict column
submission$PdDistrict <- NULL

# Remove variables from the workspace to create enough space to create the submission file
rm(data)
rm(crimes_by_district)
rm(crimes_by_district_norm)
rm(test)

head(submission)

# Create the submission file
# write.csv(submission, file = "Based_on_PdDistrict.csv", row.names=FALSE,quote=FALSE)
write_csv(submission, "Based_on_PdDistrict.csv")
file.info("Based_on_PdDistrict.csv")$size
dir()