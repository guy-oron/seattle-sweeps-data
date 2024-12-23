#Description, documentation


#Import libraries
library(data.table) #Easier to work with
library(dplyr) #Important functions
library(tsibble) #yearweek function

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

###Clean data, assign new columns###
#Clean first SERIS log. Spans from 2008 to 2017, obtained by Real Change reporters Aaron Burkhalter and Ashley Archibald
#Creating columns consistent across different logs
Seris_1 <- distinct(Seris_1) #Remove blanks/duplicates
Seris_1[,SweepDate:=as.Date(Cleanup_1, "%m/%d/%Y")] #Convert to date format
Seris_1 <- Seris_1[order(SweepDate),] #Sort by sweep date
Seris_1[,SweepID:= paste("A", FID, sep = "")] #Create ID
Seris_1[,SweepLocation:=Site_Name] #Assign Location
Seris_1[,SweepType:=Type] #SERIS sweeps are divided between "camping" and "encampment" types
Seris_1 <- Seris_1[!(SweepType %in% ""),] #remove a blank row

#Clean Second SERIS log, with encampments between 2016 and 2018 obtained by records request
#Remove non-sweeps (C == complete)
Seris_2 <- distinct(Seris_2) #Remove blanks/duplicates
Seris_2 <- Seris_2[(`CleanupStatus` %in% "C")]
Seris_2[,SweepDate:=as.Date(CleanupScheduledOrCompleteDate, "%m/%d/%Y")] #Convert to date format
Seris_2 <- Seris_2[order(SweepDate),] #Sort by sweep date
Seris_2[,SweepID:= paste("B", CleanupID, sep = "")] #Create ID
Seris_2[,SweepLocation:=sitename] #Assign Location
Seris_2[,SweepType:=EncampmentStatus] #SERIS sweeps are divided between "camping" and "encampment" types, most unspecified

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


#Clean Logs from March 2020 to the end of 2021, when Seattle operated under the COVID-19 emergency.
#In August 2020, the city council disbanded the navigation team, which resulted in several changes to sweep record keeping
#This log is just April-December 2020, a period that saw very few sweeps
Covid_1 <- distinct(Covid_1) #Remove blanks/duplicates
Covid_1[,SweepDate:=as.Date(Date, "%m/%d/%Y")] #Convert to date format
Covid_1 <- Covid_1[order(SweepDate),] #Sort by sweep date
Covid_1[,SweepID:=paste("D", 1:nrow(Covid_1), sep = "")] #Create an ID (no ID number supplied)
Covid_1[,SweepLocation:=`Encampment Site`] #Assign location
Covid_1[,SweepType:=Type] #Copy over type of sweep

#This log covers all of 2021, when the city started sweeping again at a higher frequency, focusing on large visible encampments
Covid_2 <- distinct(Covid_2) #Remove blanks/duplicates
Covid_2 <- Covid_2[!(`If postponed?` %in% c("Yes","Yes (Posting pulled)", "Self resoloved"))] #remove canceled sweeps

Covid_2[,SweepDate:=as.Date(`Removal Date`, "%m/%d/%Y")] #Convert to date format
Covid_2 <- Covid_2[order(SweepDate),] #Sort by sweep date
Covid_2[,SweepID:=paste("E", 1:nrow(Covid_2), sep = "")] #Create an ID (no ID number supplied)
Covid_2[,SweepLocation:=`Location`] #Assign location
Covid_2[,SweepType:="Unspecified"] #This log did not specify the type of sweep carried out

#In January 2022 upon becoming mayor, Bruce Harrell convened the Unified Care Team (UCT) to carry out sweeps.
#This team was similiar to the old Navigation Team, just bigger. In 2022 the team used similiar data keeping practices to 2021.
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

#Align sweep types with those used in previous logs
Uct_2[SweepType=="Resolved - Obstruction", SweepType := "Obstruction"] 
Uct_2[SweepType=="Resolved - Advanced Notice Obstruction", SweepType := "Notice Given"] #This appears to be the most appropriate label, given that prior notice was given at these obstruction sweeps
Uct_2[SweepType=="Resolved - 72-Hour Notice", SweepType := "72-Hour Notice Sweep"] 

###Combine all logs###
MasterLog <- rbind(Seris_1, Seris_2, Nav, Covid_1, Covid_2, Uct_1, Uct_2, fill = TRUE)

#Keep only common columns
MasterLog <- as.data.table(MasterLog[,c("SweepID", "SweepDate", "SweepLocation", "SweepType")])
MasterLog <- distinct(MasterLog)


#Create variablews by year, week and month
MasterLog[,SweepYear:=year(SweepDate)]
MasterLog[,SweepMonth:=month(SweepDate)]
MasterLog[,SweepWeek:=yearweek(SweepDate)]

SweepYearHist <- as.data.table(table(MasterLog$SweepYear))

#Export
#fwrite(MasterLog, "Sweeps2008-2023_Combined.csv")
