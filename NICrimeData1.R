# Importing the necessary libraries
library(dplyr)
library(data.table)

# SECTION I

# Loading the data from "NIPostcodes.csv" into dataframe "postcode"
NIPostcodes <- read.csv("NIPostcodes.csv", header = FALSE)

# To view the NIPostcodes dataframe
NIPostcodes

# To check the total number of rows in NIPostcodes dataframe
nrow(NIPostcodes)

# To view the structure of the data
str(NIPostcodes)

# To view the first 10 rows of the data
head(NIPostcodes,10)

# Adding title for each attribute
title_column <- c("Organisation Name", "Sub-building Name", 
                  "Building Name", "Number", "Primary Thorfare", "Alt Thorfare", 
                  "Secondary Thorfare", "Locality", "Townland", "Town", "County", 
                  "Postcode", "x-coordinates", "y-coordinates", "Primary Key (identifier)")

# Inserting the the title row to the data frame
colnames(NIPostcodes) <- title_column
head(NIPostcodes,10)

# Replacing the missing values with "NA" 
NIPostcodes [NIPostcodes == ""] <- NA
NIPostcodes

# To find out the number of missing values
sum(is.na(NIPostcodes))
sapply(NIPostcodes,function(x) sum(is.na(x)))

# To find the mean of the missing value
colMeans(is.na(NIPostcodes))

# Modifying the "County" attribute to the categorizing factor
NIPostcodes$County <- as.factor(NIPostcodes$County)
str(NIPostcodes)

# Moving the last column "primary key identifier" to starting
NIPostcodes <- NIPostcodes[c(15,1:14)]
NIPostcodes
names(NIPostcodes)

# Extracting the data which contains the information "Limavady" 
Limavady_data <- subset(NIPostcodes, grepl("LIMAVADY",Locality) & grepl("LIMAVADY",Townland) &
                          grepl("LIMAVADY",Town))
head(Limavady_data,5)

# Writing the Limavady data into a csv file
write.csv(Limavady_data, file = "Limavady.csv", row.names = FALSE)



# Writing the NIPostcodes data into another csv file
write.csv(NIPostcodes, file = "CleanNIPostcodeData.csv", row.names = FALSE)

# Checking the working directory
getwd()

#SECTION - II

# listing all the files in 'NI Crime Data' folder into a dataframe
list_files <- list.files("NI Crime Data", pattern='*.csv$', 
                         full.names = TRUE, recursive=TRUE)
# Checking files list
list_files

# Now, binding all the listed files into one data frame
AllNICrimeData <- Reduce(rbind, lapply(list_files, read.csv))
AllNICrimeData

# Checking the number of rows in the dataframe
nrow(AllNICrimeData)

# Removing the following attributes - CrimeID, Reported by, Falls within, LSOA code, LSOA name, last outcome and context.
AllNICrimeData <- AllNICrimeData[, -c(1,3,4,8,9,11,12)]

# Checking the column names in updated dataframe
names(AllNICrimeData)

#Checking the structure of the updated dataframe
str(AllNICrimeData)

# Factorizing the "Crime type" attribute in the dataframe
AllNICrimeData$Crime.type <- as.factor(AllNICrimeData$Crime.type)
#AllNICrimeData$Crime.type <- levels(AllNICrimeData$Crime.type)

# Checking and verifying the structure
str(AllNICrimeData)

# Removing "On or near " to obtain only street name
AllNICrimeData$Location <- as.factor(gsub(".*On or near ","",AllNICrimeData$Location))
# Removing "No Location" to obtain only street name
AllNICrimeData$Location <- as.factor(gsub(".*No Location","",AllNICrimeData$Location))
# Viewing the dataframe
AllNICrimeData

# Passing "NA" to the blank values in the dataframe
AllNICrimeData$Location [AllNICrimeData$Location == ""] <- NA
AllNICrimeData

# creating a sample dataframe to to loading the data without NA
sample <- AllNICrimeData[!is.na(AllNICrimeData$Location), ]
sample

# Creating another dataframe random_crime_sample, 
# where 1000 samples from "sample" dataframe is loaded into the new dataframe
random_crime_sample <- sample[sample(1:nrow(sample), 1000, replace = FALSE),]
random_crime_sample


# The data from "CleanNIPostcodeData.csv" is read and saved to "CleanNIPostcodeData" data frame
CleanNIPostcodeData <- read.csv("CleanNIPostcodeData.csv", stringsAsFactors = FALSE)

# a new data frame - "loc_data" is created to take only "Primary Thorfare" and "Postcode" columns
loc_data <- tbl_df(CleanNIPostcodeData[c(6,13)])

#In the loc_data dataframe, a new column key is created and the conncatenation of 
# Primary Thorfare and Postcode columns are pasted in the key column
loc_data$key <- paste(loc_data$Primary.Thorfare, loc_data$Postcode)
# A temporary table is created and the Key column values are stored in it to view the occurences of each location.
tmp_table <- as.data.frame(table(loc_data$key))
# The NA present in the column is removed
tmp_table <- na.omit(tmp_table)

#The name of the "var1"column is given as "key"
names(tmp_table)[1] <- "key"
# The loc_data dataframe and tmp_table is inner joined using the "key" attribute
loc_data_final <- join(loc_data, tmp_table,by = "key", type = "inner")
# The frequency of the location occurrences is ordered in descending order
loc_data_final <- loc_data_final[order(-loc_data_final$Freq),]
# The Duplicated rows in the Primary Thorfare is removed
loc_data_final <- loc_data_final[!duplicated(loc_data_final$Primary.Thorfare), ]
# The NA in the dataframe is removed
loc_data_final <- na.omit(loc_data_final)

# The values in the Location column is uppercased
random_crime_sample$Location <- toupper(random_crime_sample$Location)
# The NA values in the dataframe is removed
random_crime_sample <- na.omit(random_crime_sample)


# A function is created to merge the random_sample dataframe and loc_data_final dataframe 
# by matching the Location attribute in the random_crime_sample dataframe and 
# Primary Thorfare attribute in the loc_data_final dataframe. 
# From the merged dataframe, the key column and Frquency column were removed to give only 
# Month, Longitude, Latitude, Location, Crime.type, Postcode column
find_a_postcode <- function(Location, Primary.Thorfare){
  df1 <- merge(random_crime_sample, loc_data_final, by.x = c('Location'), by.y = ('Primary.Thorfare'), all.x = T)
  df2 <- df1[c(1:6)]
  return(df2)
}

# The function is called and the output is stored in random_crime_sample dataframe
random_crime_sample <- Reduce(find_a_postcode,random_crime_sample$Location)
random_crime_sample
nrow(random_crime_sample)
str(random_crime_sample)

# The order is changed to obtain the following order - 
# Month, Longitude, Latitude, Location, Crime.type, Postcode
random_crime_sample <- random_crime_sample[c(2:4,1,5,6)]
random_crime_sample <- na.omit(random_crime_sample)

# The modified and updated random_crime_sample dataframe is written into a random_crime_sample.csv file
write.csv(random_crime_sample, file = "random_crime_sample.csv", row.names = FALSE)

# updated random_sample is created as a copy of the random_crime_sample dataframe
updated_random_sample <- copy(random_crime_sample)

# The data in the updated_random_sample data frame is filtered to obtain only the postcodes having BT1
chart_data <- filter(updated_random_sample, grepl("BT1", Postcode))
# The filtered data is then sorted by Postcode and crime.type and then stored in a new dataframe called chart_data
chart_data <- chart_data[with(chart_data, order(Postcode, Crime.type)), ]

#Summary of the crime type
summary(chart_data$Crime.type)

# the Crime type in the chart_data dataframe is loaded into a new tale called plot_table 
plot_table <- table(chart_data$Crime.type)

View(plot_table)

# A bar plot is created from the table showing the occurences of each crime type.
barplot(plot_table, las = 2, ylab="Crime Occurences", main="Crime Type vs Occurences")
