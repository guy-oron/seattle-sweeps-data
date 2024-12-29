#This script uses data from the master log of sweeps to create charts with ggplot2, a visualization package
#This is just a small sample of the ways the data could be used. Feel free to make your own charts/visualizations!
#By Guy Oron

#Import libraries
library(data.table) #Easier to work with
library(dplyr) #Important functions
library(tsibble) #yearmonth function
library(ggplot2) #library for visualizations
library(Cairo) #better anti-aliasing
library(showtext) #custom fonts
library(ggrepel) #helps with labels for graphs
library(png) #for importing logo
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
logo <- rasterGrob(readPNG("GossipGuyLogo.png", native = TRUE))

#Import the logs we created in the other script
MasterLog <- fread("Sweeps2008-2023_Combined.csv")
SweepsByYear <- fread("SweepsByYear.csv")

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
                  force = 110, force_pull = 20, direction = "y") +
  geom_point(size = 2.5, color = "#682877") + #add dots
  scale_y_continuous(limits = c(0, 2500), breaks = c(0, 500, 1000, 1500, 2000, 2500)) + #y axis
  scale_x_continuous(limits = c(2008,2023), breaks = c(2008:2023)) + # x axis
  #add labels to chart
  labs(x = "", y = "Number of sweeps", title = "Yearly sweeps of homeless people in Seattle",
       subtitle = "Public records show the city displaces unhoused people hundreds of times a year",
       caption = "Chart by Guy Oron. Source: Seattle Sweeps Open Data Repository (https://github.com/guy-oron/seattle-sweeps-data/)") +
  coord_cartesian(clip = "off") + #need to turn off clipping to get logo to work
  #add logo in top right corner. Don't ask me how this works, honestly it would probably be smarter to insert the logo in photoshop
  annotation_custom(logo, xmin =  2021, xmax = 2025, ymin = 2600, ymax = 3200) 
  

#Week by Week histogram
#This chart gives a sense of time
histYMSweeps <- ggplot(SweepsByYM, mapping = aes(x = YearMonth, y = SweepCount)) +
  theme_gossip() + #add our theme
  geom_col(fill=rgb(116,211,255,max=255)) + #light blue colored columns for the histogram 
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") + #add x axis labels
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300)) + #Set y axis
  #add labels
  labs(x="", y="Number of sweeps", title = "Sweeps of unhoused people in Seattle, 2008-2023",
                                      subtitle = "After a pause during COVID, encampment removals have surged to record levels",
       caption = "Totals by month. Chart by Guy Oron. Source: Seattle Sweeps Open Data Repository (https://github.com/guy-oron/seattle-sweeps-data/)") +
  coord_cartesian(clip = "off") + #need to turn off clipping to get logo to work
  #add logo in top right corner
  annotation_custom(logo, xmin = as.Date(yearmonth("11-2022", format = "%m-%Y")), xmax = as.Date(yearmonth("2-2025", format = "%m-%Y")),
                    ymin = 312, ymax = 384) 


#export charts
ggsave("LineChart.png",plot=lineChartYearly, units="px", type = "cairo", width=5760/2, height=3240/2)
ggsave("HistogramChart.png",plot=histYMSweeps, units="px", type = "cairo", width=5760/2, height=3240/2)
