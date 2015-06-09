library(dplyr)
library(reshape2)
library(ggplot2)
library(readr)

data <- read.csv("input/train.csv")

crimes_by_day <- table(data$Category,data$DayOfWeek)

crimes_by_day <- melt(crimes_by_day)
names(crimes_by_day) <- c("Category","DayOfWeek","Count")

windows()
g <- ggplot(crimes_by_day,aes(x=Category, y=Count,color = Category)) + 
  geom_bar(stat = "Identity") + 
  coord_flip() +
  facet_grid(.~DayOfWeek) +
  theme(legend.position = "none")
print(g)

ggsave(g, file="Crimes_by_day.png", width=20, height=8)

###################### Same but by district

crimes_by_district <- table(data$Category,data$PdDistrict)

crimes_by_district <- melt(crimes_by_district)
names(crimes_by_district) <- c("Category","PdDistrict","Count")

windows()
g <- ggplot(crimes_by_district,aes(x=Category, y=Count,color = Category)) + 
  geom_bar(stat = "Identity") + 
  coord_flip() +
  facet_grid(.~PdDistrict) +
  theme(legend.position = "none")
print(g)

ggsave(g, file="Crimes_by_district.png", width=20, height=8)

###################### Same but by Month
data$Month <- month(ymd_hms(data$Dates))

crimes_by_month <- table(data$Category,data$Month)

crimes_by_month <- melt(crimes_by_month)
names(crimes_by_month) <- c("Category","Month","Count")

windows()
g <- ggplot(crimes_by_month,aes(x=Category, y=Count,color = Category)) + 
  geom_bar(stat = "Identity") + 
  coord_flip() +
  facet_grid(.~Month) +
  theme(legend.position = "none")
print(g)

ggsave(g, file="Crimes_by_month.png", width=20, height=8)

###################### Same but by Year
data$Year <- year(ymd_hms(data$Dates))
crimes_by_year <- table(data$Category,data$Year)

crimes_by_year <- melt(crimes_by_year)
names(crimes_by_year) <- c("Category","Year","Count")

windows()
g <- ggplot(crimes_by_year,aes(x=Category, y=Count,color = Category)) + 
  geom_bar(stat = "Identity") + 
  coord_flip() +
  facet_grid(.~Year) +
  theme(legend.position = "none")
print(g)

ggsave(g, file="Crimes_by_year.png", width=20, height=8)
