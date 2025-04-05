#This script uses data from the master log of sweeps to create charts with ggplot2, a visualization package
#This is just a small sample of the ways the data could be used. Feel free to make your own charts/visualizations!
#By Guy Oron
#Last updated April 3, 2025

#Import libraries
library(data.table) #Easier to work with
library(dplyr) #Important functions
library(tsibble) #yearmonth function
library(ggplot2) #library for visualizations
library(Cairo) #better anti-aliasing
library(showtext) #custom fonts
library(ggrepel) #helps with labels for graphs
library(png) #for importing logo
library(jpeg) #for importing potraits for mayoral chart
library(grid) #for adding logo

#Set path to correct path
setwd("C:/Users/Guy/Documents/R/Repository/")

#Import custom fonts
#Make sure your path is correct for your fonts
showtext::showtext_auto() #Initialize font library
font_add(family = "Poppins", regular = "C:/Users/Guy/Documents/R/Scripts/Poppins-SemiBold.ttf",
         bold = "C:/Users/Guy/Documents/R/Scripts/Poppins-Black.ttf",
         italic = "C:/Users/Guy/Documents/R/Scripts/Poppins-SemiBoldItalic.ttf")

#customize colors
l_g <- "#e2e2e2"
m_g <- "#4c4c4c"

#Create custom theme for Gossip Guy style visualizations
theme_gossip <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.y = element_line(color = l_g), #light gray lines
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(color="white", fill = "white"),
                   plot.background = element_rect(color="white", fill = "white"),
                   title = element_text(size = 24 * 2, family = "Poppins", face = "bold"),
                   plot.subtitle = element_text(size = 18 * 2, family = "Poppins", face = "italic"),
                   axis.title = element_text(size = 18 * 2, family = "Poppins", face = "bold"),
                   axis.text = element_text(size = 18 * 1.5, family = "Poppins", face = "plain", color = m_g),
                   legend.title = element_text(size = 24 * 2, family= "Poppins", face = "bold"),
                   legend.text = element_text(size = 18 * 2, family = "Poppins", face = "plain", color=m_g),
                   plot.caption = element_text(size = 18 * 1.5, family = "Poppins", face = "plain", color = m_g),
                   axis.line.x = element_line(color=l_g),
                   axis.ticks.x = element_line(color=l_g),
                   axis.ticks.length.x = unit(12, unit = "pt"),
                   plot.margin = unit(c(12,12,12,12),unit="pt"),
                   plot.caption.position =  "plot",
                   legend.key.size =unit(24 * 1.5,'pt'),
                   legend.margin = margin(c(4,4,4,4), unit = "pt"),
                   legend.spacing.x = unit(6, "pt"),
                   legend.spacing.y = unit(6, "pt"))
  
}

#Import logo for graphs. Probably way easier to add logos in another program like Canva or photoshop
logo <- rasterGrob(readPNG("Visualizations/GossipGuyLogo.png", native = TRUE))

#Import Mayor's portraits
nickelsPic <- rasterGrob(readJPEG("Visualizations/MayorPortraits/nickelspic.jpg", native = TRUE))
mcGinnPic <- rasterGrob(readJPEG("Visualizations/MayorPortraits/mcginnpic.jpg", native = TRUE))
murrayPic <- rasterGrob(readJPEG("Visualizations/MayorPortraits/murraypic.jpg", native = TRUE))
burgessPic <- rasterGrob(readJPEG("Visualizations/MayorPortraits/burgesspic.jpg", native = TRUE))
durkanPic <- rasterGrob(readJPEG("Visualizations/MayorPortraits/durkanpic.jpg", native = TRUE))
harrellPic<- rasterGrob(readJPEG("Visualizations/MayorPortraits/harrellpic.jpg", native = TRUE))

#Import the logs we created in the other script
MasterLog <- fread("Sweeps2008-2024_Combined.csv")
SweepsByYear <- fread("SweepsByYear.csv")
SweepsByMayor <- fread("SweepsByMayor.csv")

#Set correct order of mayoral admins
SweepsByMayor <- SweepsByMayor[c(6,4,5,1,2,3),]
SweepsByMayor[,ColOrder := c(1, 2, 3, 4, 5, 6)]
SweepsByMayor$Mayor <- factor(SweepsByMayor$Mayor, levels = c("Nickels", "McGinn", "Murray", "Burgess", "Durkan", "Harrell"))
  
#Visualize by year and month
MasterLog[,SweepYearMonth:=yearmonth(SweepDate)] #create yearmonth column with tsibble function

SweepsByYM <- as.data.table(table(MasterLog$SweepYearMonth))
setnames(SweepsByYM, "V1", "YearMonth") #Rename columns
setnames(SweepsByYM, "N", "SweepCount")
SweepsByYM$YearMonth <- yearmonth(SweepsByYM$YearMonth) #reconvert to yearmonth
#SweepsByYear$Year <- year(SweepsByYear$Year) #reconvert year back into date

#Year by Year line graph
#This chart shows sweeps
lineChartYearly <- ggplot(SweepsByYear, mapping = aes(x = Year, y = SweepCount)) +
  theme_gossip() + #add our theme
  geom_line(linewidth = 1.5, color = "#682877") + #purple line chart
  #Add labels for the yearly sweep counts
  #ggrepel package has useful function for text labels so they don't overlap with rest of graph
  geom_text_repel(aes(label = SweepCount), color = m_g, family = "Poppins", fontface = "plain", size = 10,
                  force = 10, force_pull = 10, direction = "y", point.padding = unit(1, "lines"), box.padding = unit(.5, "lines")) +
  geom_point(size = 2.5, color = "#682877") + #add dots
  scale_y_continuous(limits = c(0, 3000), breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000)) + #y axis
  scale_x_continuous(limits = c(2008,2024), breaks = c(2008:2024)) + # x axis
  #add labels to chart
  labs(x = "", y = "Number of sweeps", title = "Yearly sweeps of homeless people in Seattle",
       subtitle = "UPDATED : Public records show the city displaces unhoused people thousands of times a year",
       caption = "Chart by Guy Oron. Source: Seattle Sweeps Open Data Repository (https://github.com/guy-oron/seattle-sweeps-data/)") +
  coord_cartesian(clip = "off") + #need to turn off clipping to get logo to work
  #add logo in top right corner. Don't ask me how this works, honestly it would probably be smarter to insert the logo in photoshop
  annotation_custom(logo, xmin =  2022, xmax = 2026, ymin = 3120, ymax = 3840) 
  

#Week by Week histogram
#This chart gives a sense of time
histYMSweeps <- ggplot(SweepsByYM, mapping = aes(x = YearMonth, y = SweepCount)) +
  theme_gossip() + #add our theme
  geom_col(fill=rgb(116,211,255,max=255)) + #light blue colored columns for the histogram 
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") + #add x axis labels
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300)) + #Set y axis
  #add labels
  labs(x="", y="Number of sweeps", title = "Sweeps of unhoused people in Seattle, 2008-2024",
                                      subtitle = "UPDATED : After a pause during COVID, encampment removals have surged to record levels",
       caption = "Totals by month. Chart by Guy Oron. Source: Seattle Sweeps Open Data Repository (https://github.com/guy-oron/seattle-sweeps-data/)") +
  coord_cartesian(clip = "off") + #need to turn off clipping to get logo to work
  #add logo in top right corner
  annotation_custom(logo, xmin = as.Date(yearmonth("11-2023", format = "%m-%Y")), xmax = as.Date(yearmonth("2-2026", format = "%m-%Y")),
                    ymin = 312, ymax = 384) 
#Mayoral bar chart
#This chart shows how many sweeps were carried out by each mayoral administration
mayoralChart <- ggplot(data = SweepsByMayor, aes(x = Mayor, y = SweepCount, label = SweepCount)) +
  geom_col(fill = "#b2baf7", width = 0.8) + #add columns for column chart
  theme_gossip() + #add custom style
  scale_y_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000), limits = c(0, 7000)) + #add y axis labels
  #add labels
  labs(x="", y="Number of sweeps", title = "Which mayor displaced the most homeless people?",
       subtitle = "Seattle Mayor Bruce Harrell has overseen an sharp increase in sweeps",
       caption = "Data from 2008 to 2024. Chart by Guy Oron. Source: Seattle Sweeps Open Data Repository. Photo credits: Joe Mabel, Ryan Georgi, City of Seattle") +
  #Add label with precise number of sweeps per mayor
  geom_text(position = position_stack(vjust = 0.5), size = 12, color = "black", family = "Poppins", fontface = "plain") +
  coord_cartesian(clip = "off") + #need to turn off clipping to get logo to work
  #Harrell portrait placed here so that it is layered under logo
  annotation_custom(harrellPic, xmin = 5.5, xmax = 6.5, ymin = 6000, ymax = 8000) +
  #add logo in top right corner
  annotation_custom(logo, xmin = 5.7625, xmax = 6.8725, ymin = 7280, ymax = 8980) +
  #add mayor's portraits to chart
  annotation_custom(nickelsPic, xmin = 0.5, xmax = 1.5, ymin = 500, ymax = 2500 ) +
  annotation_custom(mcGinnPic, xmin = 1.5, xmax = 2.5, ymin = 1000, ymax = 3000) +
  annotation_custom(murrayPic, xmin = 2.5, xmax = 3.5, ymin = 2000, ymax = 4000) +
  annotation_custom(burgessPic, xmin = 3.5, xmax = 4.5, ymin = 500, ymax = 2500) +
  annotation_custom(durkanPic, xmin = 4.5, xmax = 5.5, ymin = 2000, ymax = 4000) +
  #make the mayors' names bolder
  theme(axis.text.x.bottom = element_text(size = 18 * 2, family = "Poppins", face = "bold", color = "black"))

#Simple Recent years chart that shows sweeps over the past three years
recentYearsChart <- ggplot(SweepsByYear[Year >= 2022,], mapping = aes(x = Year, y = SweepCount, label = SweepCount)) +
  theme_gossip() + #add our theme
  geom_col(width = 0.8, fill = "#ff6e30" ) + #orange column chart
  #Add labels for the yearly sweep counts
  geom_text(position = position_stack(vjust = 0.5), size = 12, color = "black", family = "Poppins", fontface = "plain") +
  scale_y_continuous(limits = c(0, 2600), breaks = c(0, 500, 1000, 1500, 2000, 2500)) + #y axis
  scale_x_continuous(breaks = c(2022:2024)) + # x axis
  #add labels to chart
  labs(x = "", y = "Number of sweeps",
      title = "Record levels of displacement in Seattle over last 2 years",
      subtitle = "Data shows an average of 6.8 sweeps of homeless people a day in 2024",
      caption = "Chart by Guy Oron. Source: Seattle Sweeps Open Data Repository (https://github.com/guy-oron/seattle-sweeps-data/)") +
  coord_cartesian(clip = "off") + #need to turn off clipping to get logo to work
  #add logo in top right corner. Don't ask me how this works, honestly it would probably be smarter to insert the logo in photoshop
  annotation_custom(logo, xmin =  2023.675, xmax = 2025.125, ymin = 2704, ymax = 3328) 

#export charts
ggsave("Visualizations/LineChartv2.png",plot=lineChartYearly, units="px", type = "cairo", width=5760/2, height=3240/2)
ggsave("Visualizations/HistogramChartv2.png",plot=histYMSweeps, units="px", type = "cairo", width=5760/2, height=3240/2)
ggsave("Visualizations/MayoralChartv1.png",plot=mayoralChart, units="px", type = "cairo", width=5760/2, height=3240/2)
ggsave("Visualizations/SimpleChartv1.png",plot=recentYearsChart, units="px", type = "cairo", width=5760/2, height=3240/2)
