

#Reading the dataset and loading it into dataFrame
my_data <- read.csv("NIPostcodes.csv", header = FALSE)
#I made header = FALSE because read.csv was not reading the first row
#Any how the column names must be changed so there would be any problem 



#This prints the total number of rows
nrow(my_data)  #943033

#gives the structure of the dataframe
str(my_data)

#Printing top 10 records
head(my_data, 10)

#storing new names in the vector 
new_col_names <- c("organisation Name",
                   "Sub-building Name",
                   "Building Name",
                   "Number",
                   "Primary Thorfare",
                   "Alt Thorfare",
                   "Secondary Thorfare",
                   "Locality",
                   "Townland",
                   "Town",
                   "County",
                   "Postcode",
                   "x-coordinates",
                   "y-coordinates",
                   "Primary Key")

#Assigning new column names to the dataframe
colnames(my_data) <- new_col_names

#printing first 10 rows 
head(my_data,10)


#checking for sum of missing values in each column 
data.frame(lapply(my_data, function(x)sum(x =="")))


#Filling the empty spaces with NA 
my_data[my_data == ""] <- NA



View(my_data)

#Checking for the sum of NA values in each column
data.frame(sapply(my_data, function(x)sum(length(which(is.na(x))))))


library(mice)
md.pattern(my_data)


library(VIM)

missing_values <- aggr(my_data, prop= FALSE, numbers = TRUE)
summary(missing_values)


#displaying first 5 rows 
head(my_data,5)

#changiing the position of primary key
my_data <-my_data[, c(15,1,2,3,4,5,6,7,8,9,10,11,12,13,14)]

#displaying first 5 rows
head(my_data,5)


attach(my_data)

#The attributes which contain word limavady
limavady_data <- subset(my_data, Locality== "LIMAVADY"|Townland == "LIMAVADY"|Town=="LIMAVADY")
View(limavady_data)

#counting the number of rows 
nrow(limavady_data)

#writing the data to csv file
write.csv(limavady_data, "Limavady.csv")


write.csv(my_data,"CleanNIPostCode.csv")
















