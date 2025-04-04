#This script creates a master list of removals of unhoused people's tents, encampments and vehicles in Seattle, conducted by the city government.
#This displacement, commonly known as "sweeps," were recorded in different logs that have obtain by public disclosure requests.
#The code below outlines the methodology of combining the various logs and removing errors and duplicates.
#These numbers should be taken as estimates and not absolutely accurate
#Code by Guy Oron
#Last updated April 3, 2025

#Import libraries
library(data.table) #Easier to work with
library(dplyr) #Important functions
#library(measurements) #Used to convert Latitude and Longitude coordinates

#Set path to correct path
setwd("C:/Users/Guy/Documents/R/Repository/")

#Import all the sweeps logs
Seris_1 <- fread("SweepLogs/CSV/Sweeps2008-2017_SERIS_RC.csv")
Seris_2 <- fread("SweepLogs/CSV/Sweeps2016-2018_SERIS.csv")
Nav <- fread("SweepLogs/CSV/Sweeps2017-2020_NAV.csv")
Covid_1 <- fread("SweepLogs/CSV/Sweeps2020_COVID.csv", header = TRUE)
Covid_2 <- fread("SweepLogs/CSV/Sweeps2021_COVID.csv")
Uct_1 <- fread("SweepLogs/CSV/Sweeps2022_UCT.csv")
Uct_2 <- fread("SweepLogs/CSV/Sweeps2023_UCT.csv")
Uct_3 <- fread("SweepLogs/CSV/Sweeps2024_UCT.csv")

###Clean data, assign new columns###
#Clean first SERIS log. Spans from 2008 to 2017, obtained by former Real Change reporter and editor Aaron Burkhalter
#Creating columns consistent across different logs
Seris_1 <- distinct(Seris_1) #Remove blanks/duplicates
Seris_1[,SweepDate:=as.Date(Cleanup_1, "%m/%d/%Y")] #Convert to date format
Seris_1 <- Seris_1[order(SweepDate),] #Sort by sweep date
Seris_1[,SweepID:= paste("A", FID, sep = "")] #Create ID
Seris_1[,SweepLocation:=Site_Name] #Assign Location
Seris_1[,SweepType:=Type] #SERIS sweeps are divided between "camping" and "encampment" types
Seris_1 <- Seris_1[!(SweepType %in% ""),] #remove a blank row

#Create placeholder columns with counts of how many tents, structures or RVs were cleared
Seris_1[,TentsRemoved:=NA] #No tent, vehicle or structure removals were recorded in this spreadsheet
Seris_1[,StructuresRemoved:=NA] 
Seris_1[,VehiclesRemoved:=NA] 

#Create Latitude and Longitude columns
#Real Change researchers had already geocoded this log
Seris_1[,SweepLongitude:=Longitude]
Seris_1[,SweepLatitude:=Latitude]

#Create a Boolean column to track whether prior notice was given to residents before a sweep
#Sweeps in this data set were did not record whether a notice was given before a sweep
Seris_1[,PriorNotice <- NA]

#Clean Second SERIS log, with encampments between 2016 and 2018 obtained by records request
#Remove non-sweeps (C == complete)
Seris_2 <- distinct(Seris_2) #Remove blanks/duplicates
Seris_2 <- Seris_2[(`CleanupStatus` %in% "C")]
Seris_2[,SweepDate:=as.Date(CleanupScheduledOrCompleteDate, "%m/%d/%Y")] #Convert to date format
Seris_2 <- Seris_2[order(SweepDate),] #Sort by sweep date
Seris_2[,SweepID:= paste("B", CleanupID, sep = "")] #Create ID
Seris_2[,SweepLocation:=sitename] #Assign Location
Seris_2[,SweepType:=EncampmentStatus] #SERIS sweeps are divided between "camping" and "encampment" types, most unspecified

#Create placeholder columns with counts of how many tents, structures or RVs were cleared
Seris_2[,TentsRemoved:=NA] #No tent, vehicle or structure removals were recorded in this spreadsheet
Seris_2[,StructuresRemoved:=NA] 
Seris_2[,VehiclesRemoved:=NA] 

#Create Latitude and Longitude columns
#This log provided by the city already contained geocoded locations
Seris_2[,SweepLongitude:=MapLongitude]
Seris_2[,SweepLatitude:=MapLatitude]

#Column to track whether prior notice was given to residents before a sweep
#Sweeps in this data set were did not record whether a notice was given before a sweep
Seris_2[,PriorNotice <- NA]

#Clean Navigation Team log, which documents sweeps between 2017 and the onset of the COVID-19 pandemic in March 2020
#Data comes from several different records requests. However, the data collection method remains consistent throughout the period
Nav <- distinct(Nav) #Remove blanks/duplicates

#Remove non-sweeps, such as inspections and cancellations
Nav <- Nav[!(Removal_Start %in% c("","n/a","N/A","Audit")),]
Nav <- Nav[!(Type %in% c("","CANCELLED", "Clean - No Campers", "Inspection","Support",
                         "Litter Pick", "Litter Pick *No Inspection", "No Encampment Presence" ))]

#Correct misspellings and alternate naming conventions in the data 
Nav[Type=="72-Hr Encampment Removal", Type := "72-Hour Clean"]
Nav[Type=="Emphasis zone", Type := "Emphasis Zone"]

Nav[,SweepDate:=as.Date(Removal_Start, "%m/%d/%Y")] #Convert to date format
Nav <- Nav[order(SweepDate),] #Sort by sweep date
Nav[,SweepID:=paste("C", 1:nrow(Nav), sep = "")] #Create an ID (navigation team sweeps don't have an ID)
Nav[,SweepLocation:=Location] #Assign location
Nav[,SweepType:=Type] #Copy over type. Includes 72 hour, obstruction and hazard sweeps
Nav[SweepType=="72-Hour Clean", SweepType := "72-Hour Notice Sweep"] 

#Create columns with counts of how many tents, structures or RVs were cleared
Nav[,TentsRemoved:=as.numeric(`Tents`)] #Copy over tent count 
Nav[,StructuresRemoved:=as.numeric(`Structures`)] #Copy over structure count
Nav[,VehiclesRemoved:=as.numeric(`Vehicles`)] #Copy over vehicle count
#The navigation team logs also count number of bedrolls/sleeping bags swept, while others don't

#Create Latitude and Longitude columns
#A small number of the Navigation Team sweeps featured latitude and longitude locations, but most didn't
Nav[,SweepLongitude:=Longitude]
Nav[,SweepLatitude:=Latitude]
Nav[SweepLongitude=="", SweepLongitude := NA] #convert blank cells to NAs
Nav[SweepLatitude=="", SweepLatitude := NA]
Nav[SweepLongitude=="N/A", SweepLongitude := NA] #convert "N/A" string to NA
Nav[SweepLatitude=="N/A", SweepLatitude := NA]
#Will convert long/lat coords that are stored as minutes and second formats in future update

#Column to track whether prior notice was given to residents before a sweep
#Sweeps that had prior notice were recorded as "72 hour cleans"
Nav[SweepType == "Hazard" | SweepType == "Emphasis Zone" | SweepType == "Obstruction", PriorNotice := FALSE]
Nav[SweepType == "72-Hour Notice Sweep", PriorNotice := TRUE]

#Clean Logs from March 2020 to the end of 2021, when Seattle operated under the COVID-19 emergency.
#In August 2020, the city council disbanded the navigation team, which resulted in several changes to sweep record keeping
#This log is just April-December 2020, a period that saw very few sweeps
Covid_1 <- distinct(Covid_1) #Remove blanks/duplicates
Covid_1[,SweepDate:=as.Date(Date, "%m/%d/%Y")] #Convert to date format
Covid_1 <- Covid_1[order(SweepDate),] #Sort by sweep date
Covid_1[,SweepID:=paste("D", 1:nrow(Covid_1), sep = "")] #Create an ID (no ID number supplied)
Covid_1[,SweepLocation:=`Encampment Site`] #Assign location
Covid_1[,SweepType:=Type] #Copy over type of sweep

#Create placeholder columns with counts of how many tents, structures or RVs were cleared
Covid_1[,TentsRemoved:=NA] #No tent, vehicle or structure removals were recorded in this spreadsheet
Covid_1[,StructuresRemoved:=NA] 
Covid_1[,VehiclesRemoved:=NA] 

#Create Latitude and Longitude columns
#By default, this log doesn't have latitude and longitude coordinates
Covid_1[,SweepLongitude:=NA]
Covid_1[,SweepLatitude:=NA]

#Column tracks if prior notice was given to residents before a sweep
#Sweeps labeled as obstructions or hazards did not have confirmed prior notice
Covid_1[SweepType == "Hazard" | SweepType == "Obstruction", PriorNotice := FALSE]

#This log covers all of 2021, when the city started sweeping again at a higher frequency, focusing on large visible encampments
Covid_2 <- distinct(Covid_2) #Remove blanks/duplicates
Covid_2 <- Covid_2[!(`If postponed?` %in% c("Yes","Yes (Posting pulled)", "Self resoloved"))] #remove canceled sweeps

Covid_2[,SweepDate:=as.Date(`Removal Date`, "%m/%d/%Y")] #Convert to date format
Covid_2 <- Covid_2[order(SweepDate),] #Sort by sweep date
Covid_2[,SweepID:=paste("E", 1:nrow(Covid_2), sep = "")] #Create an ID (no ID number supplied)
Covid_2[,SweepLocation:=`Location`] #Assign location
Covid_2[,SweepType:="Unspecified"] #This log did not specify the type of sweep carried out

#Create columns with counts of how many tents, structures or RVs were cleared
Covid_2[,TentsRemoved:=as.numeric(`Tent Count`)] #Copy over tent count 
Covid_2[,StructuresRemoved:=NA] #No structure removals were recorded in 2021
Covid_2[,VehiclesRemoved:=NA] #No RV removals were recorded in 2021
#No vehicle or structure removals were recorded in this spreadsheet

#Create Latitude and Longitude columns
#By default, this log doesn't have latitude and longitude coordinates
Covid_2[,SweepLongitude:=NA]
Covid_2[,SweepLatitude:=NA]

#Column tracks if prior notice was given to residents before a sweep
#Sweeps in this log did not record the type of sweep (i.e. obstructions, hazards, 72 hour notice sweeps, etc.)
Covid_2[,PriorNotice <- NA]

#In January 2022 upon becoming mayor, Bruce Harrell convened the Unified Care Team (UCT) to carry out sweeps.
#This team was similar to the old Navigation Team, just bigger. In 2022 the team used similiar data keeping practices to 2021.
Uct_1 <- distinct(Uct_1) #Remove blanks/duplicates
Uct_1 <- Uct_1[!(`If postponed?` %in% c("Yes","Yes (Posting pulled)", "Self resolved",
                                        "Self resolved-Horan", "Clear", "Yes (Posting pulled)",
                                        "Clear-No obstruction", "Yes (P.P) Due to Protestors", "Verbal Compliance"))] #remove canceled sweeps, including those canceled due to protests

Uct_1[,SweepDate:=as.Date(`Removal Date`, "%m/%d/%Y")]
Uct_1 <- Uct_1[order(SweepDate),] #Sort by sweep date
Uct_1[,SweepID:=paste("F", 1:nrow(Uct_1), sep = "")] #Create an ID (no ID number supplied)
Uct_1[,SweepLocation:=`Location`] #Assign location

#The log records whether an encampment received posted notice or was deemed an obstruction.
#As Such, we can divide the type of sweep between obstruction and non-obstruction sweeps, 
#in which presumably more outreach was done (or at least folks had more time to prepare to move)
Uct_1[,SweepType:="Notice Given"] #Assign default type
Uct_1[`Posting Date`=="?", SweepType := "Unspecified"] #Some rows record that UCT did not determine whether prior notice was given
Uct_1[`Posting Date`=="-", SweepType := "Unspecified"]
Uct_1[`Posting Date`=="", SweepType := "Unspecified"]
Uct_1[`Posting Date`=="Obstruction", SweepType := "Obstruction"] 
Uct_1[`Posting Date`=="Obstrcution", SweepType := "Obstruction"] #misspelling

#Create columns with counts of how many tents, structures or RVs were cleared
Uct_1[,TentsRemoved:=as.numeric(`Tent Count`)] #Copy over tent count 
Uct_1[,StructuresRemoved:=NA] #No structure removals were recorded by UCT in 2022
Uct_1[,VehiclesRemoved:=NA] #No RV removals were recorded by UCT in 2022 
#(that doesn't mean vehicle sweeps didn't happen, they did, but just weren't recorded here)

#Create Latitude and Longitude columns
#By default, this log doesn't have latitude and longitude coordinates
Uct_1[,SweepLongitude:=NA]
Uct_1[,SweepLatitude:=NA]

#Column tracks if prior notice was given to residents before a sweep
#Sweeps deemed obstructions did not have prior notice. Some were also unspecified
Uct_1[SweepType == "Obstruction", PriorNotice := FALSE]
Uct_1[SweepType == "Notice Given", PriorNotice := TRUE]
Uct_1[SweepType == "Unspecified", PriorNotice := NA]

#The 2023 sweeps log --- by far the largest log --- uses a new record keeping method.
#This is the updated log which appears to be more accurate than the other logs provided which was published in the June 2024 report in Real Change 
#Presumably 2024 data will look similar, though it is has not been yet obtained. This is the second year of the UCT
Uct_2 <- distinct(Uct_2) #Remove blanks/duplicates

#Remove non sweeps (i.e.: inspections, three instances where outreach led to encampment resolutions)
Uct_2 <- Uct_2[!(`Action` %in% c("Active Location Inspection","Resolved - By Outreach",
"RV Site Inspected - Active Location Inspection"))]

Uct_2[,SweepDate:=as.Date(ActionDate, "%m/%d/%Y")] #Convert to date format
Uct_2[,SweepID:= paste("G", `Unique ID`, sep = "")] #Copy over ID. Unlike many of the other logs, the 2023 records unique IDs for every sweep
Uct_2[,SweepLocation:=LocationName] #Assign Location
Uct_2[,SweepType:=Action] #Assign type of sweep

#Create columns with counts of how many tents, structures or RVs were cleared
Uct_2[,TentsRemoved:=as.numeric(`TentCount`)] #Copy over tent count 
Uct_2[,StructuresRemoved:=as.numeric(`LivingStructureCount`)] #Copy over structure count
Uct_2[,VehiclesRemoved:=as.numeric(`RVCount`)] #Copy over RV count

#Align sweep types with those used in previous logs
Uct_2[SweepType=="Resolved - Obstruction", SweepType := "Obstruction"] 
Uct_2[SweepType=="Resolved - Advanced Notice Obstruction", SweepType := "Notice Given"] #This appears to be the most appropriate label, given that prior notice was given at these obstruction sweeps
Uct_2[SweepType=="Resolved - 72-Hour Notice", SweepType := "72-Hour Notice Sweep"] 

#Create Latitude and Longitude columns
#By default, this log doesn't have latitude and longitude coordinates
Uct_2[,SweepLongitude:=NA]
Uct_2[,SweepLatitude:=NA]

#Column tracks if prior notice was given to residents before a sweep
#Sweeps deemed obstructions or did not have prior notice, while some were deemed 72 hour sweeps or labeled "advance notice obstructions"
Uct_2[SweepType == "Obstruction" | SweepType == "RV Remediation", PriorNotice := FALSE]
Uct_2[SweepType == "Notice Given" | SweepType == "72-Hour Notice Sweep", PriorNotice := TRUE]

###2024 Update###
#This is the new log obtained for 2024. Very similiar in record keeping method to the 2023 log
Uct_3 <- distinct(Uct_3) #Remove blanks/duplicates

#Remove non sweeps (i.e.: where outreach led to encampment resolutions). No inspections were included in this list
Uct_3 <- Uct_3[!(`Action` %in% "Resolved - By Outreach")]

Uct_3[,SweepDate:=as.Date(`Date of Action`, "%m/%d/%Y")] #Convert to date format
Uct_3[,SweepID:= paste("H", `Unique ID`, sep = "")] #Copy over unique IDs for every sweep
Uct_3[,SweepLocation:=`Location Name`] #Assign Location
Uct_3[,SweepType:=Action] #Assign type of sweep

#Create columns with counts of how many tents, structures or RVs were cleared
Uct_3[,TentsRemoved:=as.numeric(`Tents Cleared`)] #Copy over tent count 
Uct_3[,StructuresRemoved:=as.numeric(`Living Structure Count`)] #Copy over structure count
Uct_3[,VehiclesRemoved:=as.numeric(`RV Count`)] #Copy over RV count

#Align sweep types with those used in previous logs
Uct_3[SweepType=="Resolved - Obstruction", SweepType := "Obstruction"] 
Uct_3[SweepType=="Resolved - Advanced Notice Obstruction", SweepType := "Notice Given"] #This appears to be the most appropriate label, given that prior notice was given at these obstruction sweeps
Uct_3[SweepType=="Resolved - Scheduled", SweepType := "72-Hour Notice Sweep"] 

#Create Latitude and Longitude columns
#By default, this log doesn't have latitude and longitude coordinates
Uct_3[,SweepLongitude:=NA]
Uct_3[,SweepLatitude:=NA]

#Column tracks if prior notice was given to residents before a sweep
#Sweeps deemed obstructions or did not have prior notice, while some were deemed 72 hour sweeps or labeled "advance notice obstructions"
Uct_3[SweepType == "Obstruction" | SweepType == "RV Remediation", PriorNotice := FALSE]
Uct_3[SweepType == "Notice Given" | SweepType == "72-Hour Notice Sweep", PriorNotice := TRUE]


#####Combine all logs#####
MasterLog <- rbind(Seris_1, Seris_2, Nav, Covid_1, Covid_2, Uct_1, Uct_2, Uct_3, fill = TRUE)

#Calculations for how many sweeps were carried out in various mayoral administrations
#Layout dates for when certain mayoral terms occurred. Harrell's term is projected based on the assumption of no relection.
#Technically, mayoral terms end midway through Jan. 1, but to make it simple and ensure no overlap, I assumed an end of term date on Dec. 31
#Harrell, as city council president, was in office for 5 days. Then Burgess was appointed to fill out the rest of the term. So his name represents the interregnum term
mayoralTerms <- data.table(mayors = c("Nickels", "McGinn", "Murray", "Burgess", "Durkan", "Harrell"),
                           start = c(as.Date("01/01/2002", "%m/%d/%Y"), as.Date("01/01/2010", "%m/%d/%Y"), as.Date("01/01/2014", "%m/%d/%Y"), as.Date("09/13/2017", "%m/%d/%Y"), as.Date("11/28/2017", "%m/%d/%Y"), as.Date("01/01/2022", "%m/%d/%Y")),
                           end = c(as.Date("12/31/2009", "%m/%d/%Y"), as.Date("12/31/2013", "%m/%d/%Y"), as.Date("09/12/2017", "%m/%d/%Y"), as.Date("11/27/2017", "%m/%d/%Y"), as.Date("12/31/2021", "%m/%d/%Y"), as.Date("12/31/2025", "%m/%d/%Y")))

mayoralTerms <- dcast(melt(mayoralTerms, id.vars = "mayors"), variable ~ mayors) #transpose data table

MasterLog[,CurrentMayor := ""] #Create placeholder value

#Assign values current mayor values based on mayoral terms data table
MasterLog[SweepDate >= mayoralTerms$Nickels[1] & SweepDate <= mayoralTerms$Nickels[2], CurrentMayor := "Nickels"]
MasterLog[SweepDate >= mayoralTerms$McGinn[1] & SweepDate <= mayoralTerms$McGinn[2], CurrentMayor := "McGinn"]
MasterLog[SweepDate >= mayoralTerms$Murray[1] & SweepDate <= mayoralTerms$Murray[2], CurrentMayor := "Murray"]
MasterLog[SweepDate >= mayoralTerms$Burgess[1] & SweepDate <= mayoralTerms$Burgess[2], CurrentMayor := "Burgess"] 
MasterLog[SweepDate >= mayoralTerms$Durkan[1] & SweepDate <= mayoralTerms$Durkan[2], CurrentMayor := "Durkan"]
MasterLog[SweepDate >= mayoralTerms$Harrell[1] & SweepDate <= mayoralTerms$Harrell[2], CurrentMayor := "Harrell"]

#Keep only common columns
MasterLog <- as.data.table(MasterLog[,c("SweepID", "SweepDate", "SweepLocation", "SweepType",
                                        "TentsRemoved", "StructuresRemoved", "VehiclesRemoved",
                                        "SweepLongitude", "SweepLatitude", "PriorNotice", "CurrentMayor")])
MasterLog <- distinct(MasterLog)

#Approximately 20.9% of sweeps in MasterLog are currently geocoded
#To calculate that percentage: table(is.na(MasterLog$SweepLatitude))/9459 

#Create variables by year, week and month
MasterLog[,SweepYear:=year(SweepDate)]
MasterLog[,SweepMonth:=month(SweepDate)]
MasterLog[,SweepWeek:=week(SweepDate)]

#Create a simple yearly count spreadsheet
SweepsByYear <- as.data.table(table(MasterLog$SweepYear))
setnames(SweepsByYear, "V1", "Year") #Rename columns
setnames(SweepsByYear, "N", "SweepCount")

#Create a simple spreadsheet to count the number of sweeps by mayoral administration
SweepsByMayor <- as.data.table(table(MasterLog$CurrentMayor))
setnames(SweepsByMayor, "V1", "Mayor") #Rename columns
setnames(SweepsByMayor, "N", "SweepCount")

#Export data
fwrite(MasterLog, "Sweeps2008-2024_Combined.csv")
fwrite(SweepsByYear, "SweepsByYear.csv")
fwrite(SweepsByMayor, "SweepsByMayor.csv")