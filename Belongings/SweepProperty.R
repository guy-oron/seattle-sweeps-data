#Analysis of records related to belongings taken and stored by Seattle's Unified Care Team (UCT)
#Time period requested was January 1, 2022 to December 31, 2024
#Code by Guy Oron
#Last updated August 18, 2025


#Import libraries
library(data.table) #Easier to work with
library(dplyr) #Important functions
library(waffle) #for pictograms/unit charts/ icon charts
library(ggplot2) #For visualizations
library(extrafont)
library(Cairo)


#Set path to correct path
setwd("C:/Users/Guy/Documents/R/Repository/")

#import themes from visualizations script
source("Visualizations/SweepVisualizations.R")

#Import records
Storage <- fread("Belongings/UCTStorage.csv")
Deliveries <- fread("Belongings/UCTDeliveries.csv")

######Clean up data

#remove skipped numbers and blanks
Storage <- Storage[!(`Camp Location` %in% "SKIPPED NUMBER"),]
Storage <- Storage[!(`Camp Location` %in% ""),]

#Identify outcome of stored items
Storage$`Disposed (Yes/No = Delivered)`<-tolower(Storage$`Disposed (Yes/No = Delivered)`) #ensure same case

Storage$Outcome <- "" #Create variable to track outcomes
Storage[`Disposed (Yes/No = Delivered)` == "yes", Outcome := "Disposed"]
Storage[`Disposed (Yes/No = Delivered)` == "no", Outcome := "Delivered"]

#Identify outcomes that were either pending or unknown at time of the records request
Storage[`Disposed (Yes/No = Delivered)` == "hold", Outcome := "Pending"]
Storage[`Disposed (Yes/No = Delivered)` == "hold arrested", Outcome := "Pending"]
Storage[`Disposed (Yes/No = Delivered)` == "hold rc bradly smith is working with reach", Outcome := "Pending"]
Storage[`Disposed (Yes/No = Delivered)` == "", Outcome := "Pending"]
Storage[Description == "Self-Stored Black backpack w/clothes, Red black heater, Black heater, Tent in bag, Tarps, sleeping bags, Blue chair, Blue cot, Personal paperworkd, mail in white bag, books, pillow, weights, Green cart, hanger, Green duffel bag, Green black duffel bag and boots.",
        Outcome := "Unknown"]

Storage[,Date := as.Date(`Date of Collection`, format = "%m/%d/%Y")]
Storage[,Year := year(Date)]

#Summarize the storage outcomes in a table
StorageStats <- as.data.table(table(Storage$Outcome))
setnames(StorageStats, "V1", "OutcomeType") #Rename columns
setnames(StorageStats, "N", "Count")

#Identify outcomes for deliveries spreadsheet
Deliveries$Outcome <- ""
Deliveries[`Delivery Date` == "", Outcome := "No Action"]
Deliveries[`Delivery Date` != "", Outcome := "Delivered"] #If there is a delivery date, assume delivery unless notes or other info indicates otherwise

#Using delivery notes, identify cases when deliveries did not occur
Deliveries[`Delivery Notes`=="FC was unable to deliver due to short staff from snow weather. Rescheduled for 12/12/2022",
           Outcome := "Failed Delivery"]
Deliveries[`Delivery Notes`=="Mr. Motley did not call to finalize property delivery",
           Outcome := "Failed Delivery"]
Deliveries[`Delivery Notes`=="Driver arrived at location and stayed for 1 hour.  Customer did not show.  Customer called me at 2:28 pm to inform he apologizes, couldn't make it and will call me next week to reschedule.",
           Outcome := "Failed Delivery"]

#Summarize the delivery outcomes in a table
DeliveryStats <- as.data.table(table(Deliveries$Outcome))
setnames(DeliveryStats, "V1", "OutcomeType") #Rename columns
setnames(DeliveryStats, "N", "Count")

unitChart <- waffle(
  c(`Delivered` = 27, `Disposed` = 351, `Unknown/Pending` = 9), 
  rows = 10, colors = c("#93FB98", "#FD6F6F", "lightblue"),
  glyph_size = 32, 
  title = "UCT property stats",
  legend_pos="bottom"
) + theme_gossip()

#ggsave("Visualizations/PropertyChart.png",plot=unitChart, units="px", type = "cairo", width=5760/2, height=3240/2)

