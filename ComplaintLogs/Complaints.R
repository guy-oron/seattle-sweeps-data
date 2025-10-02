#Compilation and analysis of complaints from Seattle residents about homeless people's encampments and vehicles
#Data obtained from the City of Seattle's Customer Service Bureau
#Time period requested was January 1, 2019 to December 31, 2024
#Code by Guy Oron
#Last updated September 14, 2025

#Import libraries
library(data.table) #Easier to work with
library(dplyr) #Important functions
library(readxl) #For importing excel sheets
library(ggplot2) #For Visualizations
library(tsibble) #For special date types (e.g. yearmonth)

#Set path to correct path
setwd("C:/Users/Guy/Documents/R/Repository/")

#Import themes from visualizations script
source("Visualizations/SweepVisualizations.R")

##### 
#Comment this code which deals with unredacted complaint logs
#mass import original excel files
#originalFolder <- "EncampmentComplaints/2019-2024/Original/"
#CSVFolder <- "EncampmentComplaints/2019-2024/CSV/"

#originalFiles <- list.files(originalFolder, pattern = ".xlsx")

#Convert original logs to .csv extension
#for (i in originalFiles){
#  readxl::read_excel(paste0(originalFolder, i)) %>%
#    write.csv(., paste0(CSVFolder ,gsub(".xlsx", ".csv", i)))
#}
# 
# #Import all complaint logs from correct .csv folder
# q1_2019 <- fread("EncampmentComplaints/2019-2024/CSV/2019 Q1.csv")
# q2_2019 <- fread("EncampmentComplaints/2019-2024/CSV/2019 Q2.csv")
# q3_2019 <- fread("EncampmentComplaints/2019-2024/CSV/2019 Q3.csv")
# q4_2019 <- fread("EncampmentComplaints/2019-2024/CSV/2019 Q4.csv")
# q1_2020 <- fread("EncampmentComplaints/2019-2024/CSV/2020 Q1.csv")
# q2_2020 <- fread("EncampmentComplaints/2019-2024/CSV/2020 Q2.csv")
# q3_2020 <- fread("EncampmentComplaints/2019-2024/CSV/2020 Q3.csv")
# q4_2020 <- fread("EncampmentComplaints/2019-2024/CSV/2020 Q4.csv")
# q1_2021 <- fread("EncampmentComplaints/2019-2024/CSV/2021 Q1.csv")
# q2_2021 <- fread("EncampmentComplaints/2019-2024/CSV/2021 Q2.csv")
# q3_2021 <- fread("EncampmentComplaints/2019-2024/CSV/2021 Q3.csv")
# q4_2021 <- fread("EncampmentComplaints/2019-2024/CSV/2021 Q4.csv")
# q1_2022 <- fread("EncampmentComplaints/2019-2024/CSV/2022 Q1.csv")
# q2_2022 <- fread("EncampmentComplaints/2019-2024/CSV/2022 Q2.csv")
# q3_2022 <- fread("EncampmentComplaints/2019-2024/CSV/2022 Q3.csv")
# q4_2022 <- fread("EncampmentComplaints/2019-2024/CSV/2022 Q4.csv")
# q1_2023 <- fread("EncampmentComplaints/2019-2024/CSV/2023 Q1.csv")
# q2_2023 <- fread("EncampmentComplaints/2019-2024/CSV/2023 Q2.csv")
# q3_2023 <- fread("EncampmentComplaints/2019-2024/CSV/2023 Q3.csv")
# q4_2023 <- fread("EncampmentComplaints/2019-2024/CSV/2023 Q4.csv")
# q1_2024 <- fread("EncampmentComplaints/2019-2024/CSV/2024 Q1.csv")
# q2_2024 <- fread("EncampmentComplaints/2019-2024/CSV/2024 Q2.csv")
# q3_2024 <- fread("EncampmentComplaints/2019-2024/CSV/2024 Q3.csv")
# q4_2024 <- fread("EncampmentComplaints/2019-2024/CSV/2024 Q4.csv")
# 
# #combine all logs together
# masterLogO <- rbind(q1_2019, q1_2020, q1_2021, q1_2022, q1_2023, q1_2024,
#                   q2_2019, q2_2020, q2_2021, q2_2022, q2_2023, q2_2024,
#                   q3_2019, q3_2020, q3_2021, q3_2022, q3_2023, q3_2024,
#                   q4_2019, q4_2020, q4_2021, q4_2022, q4_2023, q4_2024, fill = TRUE)
# 
# #remove duplicates
# masterLogO <- distinct(masterLogO)
# 
# #Add time columns (year, month, week, yearweek)
# masterLogO$Date <- as.Date(masterLogO$`Created Date`)
# masterLogO$Year <- year(masterLogO$Date)
# masterLogO$Month <- month(masterLogO$Date)
# masterLogO$Week <- week(masterLogO$Date)
# masterLogO$YearMonth <- yearmonth(masterLogO$Date)
# 
# masterLogO$`Customer name` <- tolower(masterLogO$`Customer name`)
# masterLogO$`Customer email address` <- tolower(masterLogO$`Customer email address`)
# masterLogO$`Customer address` <- tolower(masterLogO$`Customer address`)
# masterLogO$`Customer phone number` <- tolower(masterLogO$`Customer phone number`)
# 
# #Calculate and export unique number of complainants 
# #This confirms there has been a genuine increase in the number of people complaining about homeless encampments
# #Since the number of unique emails, names, addresses, and phone numbers have all increased in correlation with total complaint counts
# names <- as.data.table(table((masterLogO %>% count(`Customer name`, Year))$Year))
# setnames(names, old = c("V1", "N"), new = c("Year","Count"))
# fwrite(names, "EncampmentComplaints/UniqueComplainantNames.csv")
# 
# emails <- as.data.table(table((masterLogO %>% count(`Customer email address`, Year))$Year))
# setnames(emails, old = c("V1", "N"), new = c("Year","Count"))
# fwrite(emails, "EncampmentComplaints/UniqueComplainantEmails.csv")
# 
# phone <- as.data.table(table((masterLogO %>% count(`Customer phone number`, Year))$Year))
# setnames(phone, old = c("V1", "N"), new = c("Year","Count"))
# fwrite(phone, "EncampmentComplaints/UniqueComplainantPhones.csv")
# 
# address <- as.data.table(table((masterLogO %>% count(`Customer address`, Year))$Year))
# setnames(address, old = c("V1", "N"), new = c("Year","Count"))
# fwrite(address, "EncampmentComplaints/UniqueComplainantAddresses.csv")
# 
# #remove columns containing personal information of complainers to mitigate potential for doxxing
# masterLog <- copy(masterLogO)
# masterLog[, c("Customer name","Customer phone number", "Customer email address", "Customer address"):=NULL]
# 
# #export master .csv list
# fwrite(masterLog, "EncampmentComplaints/2019-2024/CSV/MasterLog2019-2024.csv")

#####
#Start here for public code (using log redacted for privacy)

#import master log
masterLog <- fread("EncampmentComplaints/2019-2024/CSV/MasterLog2019-2024.csv")
                  

#Create histogram to count complaints by year and month
ComplaintsYM <- as.data.table(table(masterLog$YearMonth))
setnames(ComplaintsYM, "V1", "YearMonth") #Rename columns
setnames(ComplaintsYM, "N", "ComplaintsCount")
ComplaintsYM$YearMonth <- yearmonth(ComplaintsYM$YearMonth)

#Create histogram to count complaints by just year
ComplaintsByYear <- as.data.table(table(masterLog$Year))
setnames(ComplaintsByYear, "V1", "Year") #Rename columns
setnames(ComplaintsByYear, "N", "ComplaintsCount")
ComplaintsByYear$Year <- as.numeric(ComplaintsByYear$Year)

#Export complaints by year
fwrite(ComplaintsByYear, "EncampmentComplaints/ComplaintsByYear.csv")

### Some basic visualizations to demonstrate scale of complaints and increasing trend over time
#Yearmonth chart
complaintsYMChart <- ggplot(ComplaintsYM, mapping=aes(x=YearMonth,y=ComplaintsCount)) + 
  geom_col(fill=rgb(116,211,255,max=255)) + 
  scale_x_yearmonth() +
  scale_y_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000)) +
  labs(x="", y="Number of Complaints") + 
  theme_gossip()

#Yearly chart
complaintsYearlyChart <- ggplot(ComplaintsByYear, mapping=aes(x=Year,y=ComplaintsCount)) + 
  geom_col(fill=rgb(116,211,255,max=255)) + 
  scale_x_continuous(breaks = c(2019, 2020, 2021, 2022, 2023, 2024)) +
  scale_y_continuous(breaks = c(0, 10000, 20000, 30000, 40000, 50000)) +
  labs(x="", y="Number of Complaints") + 
  theme_gossip()
