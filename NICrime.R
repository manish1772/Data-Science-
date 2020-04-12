#final_crime_data <- data.frame()
#for(year in 2015:2017){ 
#  for(month in 1:12){
#    if (month < 10){
#      crime_data <- read.csv(file=paste0("/Users/Manish/Downloads/NI Crime Data/", year,"-0",month,"/",year,"-0", month, "-northern-ireland-street.csv"))

#      final_crime_data<-rbind(final_crime_data,crime_data)

#    }
#    else{
#      crime_data <- read.csv(file=paste0("/Users/Manish/Downloads/NI Crime Data/", year,"-", month, "/", year,"-", month, "-northern-ireland-street.csv"))

#      final_crime_data <- rbind(final_crime_data, crime_data)

#    }
#      }
#}


#gets current working directory
getwd()
#creating an empty dataframe to store the data after looping
final_crime_data <- data.frame()
#files_list contains all the files from 
files_list <- list.files(recursive = TRUE)
for(file_list in files_list){
  crime_data <- read.csv(file_list)
  final_crime_data <- rbind(final_crime_data, crime_data)
}



write.csv(final_crime_data, file = "AllNiCrimeData.csv")


nrow(final_crime_data)
str(final_crime_data)


AllNiCrimeData <- read.csv("AllNiCrimeData.csv")
AllNiCrimeData <- subset(AllNiCrimeData, select = -c(X))


#chaging the structure of crime.type to character
crime_type <- as.character(AllNiCrimeData$Crime.type)
AllNiCrimeData$Crime.type <- crime_type


#chaging the structure of location to character
location <- as.character(AllNiCrimeData$Location)
AllNiCrimeData$Location <- location

#Removing the attributes
AllNiCrimeData <- subset(AllNiCrimeData, select = -c(Crime.ID, Reported.by, Falls.within, LSOA.code, LSOA.name,Last.outcome.category,Context))
names(AllNiCrimeData)
str(AllNiCrimeData)

AllNiCrimeData$Crime.type[AllNiCrimeData$Crime.type == "Anti-social behaviour"] <- "ASBO"
AllNiCrimeData$Crime.type[AllNiCrimeData$Crime.type == "Bicycle theft"] <- "BITH"
AllNiCrimeData$Crime.type[AllNiCrimeData$Crime.type == "Burglary"] <- "BURG"
AllNiCrimeData$Crime.type[AllNiCrimeData$Crime.type == "Criminal damage and arson"] <- "CDAR"
AllNiCrimeData$Crime.type[AllNiCrimeData$Crime.type == "Other theft"] <- "OTTH"
AllNiCrimeData$Crime.type[AllNiCrimeData$Crime.type == "Public order"] <- "PUBO"
AllNiCrimeData$Crime.type[AllNiCrimeData$Crime.type == "Robbery"] <- "ROBY"
AllNiCrimeData$Crime.type[AllNiCrimeData$Crime.type == "Shoplifting"] <- "SHOP"
AllNiCrimeData$Crime.type[AllNiCrimeData$Crime.type == "Theft from the person"] <- "THPR"
AllNiCrimeData$Crime.type[AllNiCrimeData$Crime.type == "Vehicle crime"] <- "VECR"
AllNiCrimeData$Crime.type[AllNiCrimeData$Crime.type == "Violence and sexual offences"] <- "VISO"
AllNiCrimeData$Crime.type[AllNiCrimeData$Crime.type == "Other crime"] <- "OTCR"
AllNiCrimeData$Crime.type[AllNiCrimeData$Crime.type == "Drugs"] <- "DRUG"
AllNiCrimeData$Crime.type[AllNiCrimeData$Crime.type == "Possession of weapons"] <- "POW"


install.packages("ggplot2")
library(ggplot2)
ggplot(AllNiCrimeData, aes(x=Crime.type)) + geom_bar(fill = "blue", color ="black")


#install.packages("stringr")
library(stringr)

#storing all the new renamed names 
modified_location <- str_remove_all(AllNiCrimeData$Location, "On or near ")

#assigning the new names to location
AllNiCrimeData$Location <- modified_location

#Assigning NA to empty spaces
AllNiCrimeData$Location[AllNiCrimeData$Location == ""] <- NA
AllNiCrimeData$Location[AllNiCrimeData$Location == "No Location"] <- NA

#couting the sum of NA values
sum(is.na(AllNiCrimeData$Location))


#selecting non null location values
rdn <- subset(AllNiCrimeData[, ], !is.na(AllNiCrimeData$Location))

#random sampling 
set.seed(100)
random_crime_sample <- rdn[sample(1:nrow(rdn), 5000, replace = FALSE), ]

#changing the values to lower case
my_data$`Primary Thorfare` <- str_to_lower(my_data$`Primary Thorfare`)
random_crime_sample$Location <- str_to_lower(random_crime_sample$Location)

#the function which finds info for location in NICrime data
find_a_town <- function(x,y){
  
  return(my_data$Town[match(x,y)])
}

random_crime_sample$Town <- find_a_town(random_crime_sample$Location, my_data$`Primary Thorfare`)


#Reading village list
village_data  <- read.csv("VillageList.csv")


village_data$ï..CITY.TOWN.VILLAGE  <- str_to_upper(village_data$ï..CITY.TOWN.VILLAGE)

village_data$ï..CITY.TOWN.VILLAGE<- str_replace_all(village_data$ï..CITY.TOWN.VILLAGE, "DERRY", "LONDONDERRY")
#function which adds poppulation
add_town_data <- function(x,y){
  
  return(village_data$POPULATION[match(x,y)])
}

random_crime_sample$Population <- add_town_data(random_crime_sample$Town, village_data$ï..CITY.TOWN.VILLAGE)

#Updating the column name
colnames(random_crime_sample)[6] <- c("City-Town-Village") 

#saving the dataframe in csv file
write.csv(random_crime_sample, "random_crime_sample.csv")





random_crime_belfast <- subset(random_crime_sample, random_crime_sample$`City-Town-Village` == "BELFAST")
random_crime_derry <- subset(random_crime_sample, random_crime_sample$`City-Town-Village` == "LONDONDERRY")


library(dplyr)
library(ggplot2)

#This piece code was referenced from https://datacarpentry.org/R-genomics/04-dplyr.html
# Step 1
plot1 <- random_crime_belfast %>% 
  #Step 2
  group_by(Crime.type) %>% 
  #Step 3
  summarise(count_fre = n()) %>% 
  #Step 4
  ggplot(aes(x = Crime.type, y = sort(count_fre, decreasing = TRUE), fill = Crime.type)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "Crime_Types",
    y = "Frequency",
    title = paste(
      "BELFAST"
    )
  )

plot2 <- random_crime_derry %>% 
  #Step 2
  group_by(Crime.type) %>% 
  #Step 3
  summarise(count_fre = n()) %>% 
  #Step 4
  ggplot(aes(x = Crime.type, y = sort(count_fre, decreasing = TRUE), fill = Crime.type)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "Crime_Types",
    y = "Frequency",
    title = paste(
      "LONDONDDERRY"
    )
  )

install.packages("gridExtra")
require(gridExtra)
#To arrange side by side
grid.arrange(plot1,plot2,ncol=2)




