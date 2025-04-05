# Seattle Sweeps Open Data Repository
This is a public, open data repository containing data on Seattle's policies of displacement against unhoused people, commonly known as sweeps. All documents and files were originally created by city officials and obtained by public disclosure requests to the city of Seattle under Washington State's Public Records Act (RCW 42.56). 
<br>
<br>
**Why create this repository?**
<br>
The goal of this repository is create an easily accessible public portal for accessing quantitative data about Seattle's sweeps policies. Over the years, I have recieved inquiries from researchers, advocates and fellow journalists about this data. This feels like the best way to make minimize gatekeeping while also making the data easy to access and understand.
<br>
<br>
Because these documents were created with no intent for publication, they likely contain errors. I have done my best to compile a master list of sweeps, removing duplicates and other events like inspections. However, it is unlikely we will never know the true number of encampment removals that have been conducted, so these counts should be taken as an estimate with a range of error.
<br>
<br>
While politicians choose to hide data sets like these from the public, they publish dashboards to track how few unhoused people's encampments remain standing. I reject the premise that we should let those in power decide what information you get to see. Look through the data and come to your own conclusions on how Seattle treats its residents who are forced to live on the street. 
<br>
<br>

**What's in the repository?**
<br>
This repository is broadly divided several subfolders. Within each subfolder is a short read me document, explaining the documents inside. Perhaps the first place to go in the repository is the file "Sweeps2008-2023_Combined.csv," which contains a master list of all encampment sweeps. There is also a table of sweeps by year, as well as corresponding excel versions of the spreadsheets. In "SweepLogsCode.R," I outline the methodology I use to combine the lists, as they were recorded by different teams with varying record keeping styles. In "SweepVisualizations.R," I create visualizations used for Gossip Guy posts. The two images created by this script can also be found in this main folder.
<br>
<br>
The raw data for the master list of sweeps is found in the RemovalLogs folder, along with a .csv version of the files. The ComplaintLogs folder contains data on complaints submitted to the city of Seattle about encampments and homeless people. The SweepCosts folder contains information about the budget for the agencies/cross-departmental formations tasked with carrying out sweeps, including the Navigation Team and the Unified Care Team.
<br>
<br>

**Who created this repository?**
<br>
I (Guy Oron) created this repository. I am a Seattle-based writer and journalist, currently working for Real Change as its staff reporter. You can find me and my work on my various social media channels as well as [my newsletter Gossip Guy]([url](https://gossipguy.net/)). Brandon Morande, of the University of Washington Sociology Department, has generously contributed to the repository, adding logs he obtained relating to impounds of vehicles by the city of Seattle.
<br>
<br>
I want to give special thanks to Ashley Archibald and Aaron Burkhalter, two former Real Change editors and reporters, for providing me the spreadsheet of data for sweeps between 2008 and 2017. If he had not retrieved that list, it would have probably been deleted permanently.
<br>
<br>

**What are the next planned updates?**
<br>
I am requesting records for sweeps carried out in 2024. I also hope to expand the SweepCosts and ComplaintLogs folders with other record requests as well. The largest update I plan on doing is to add longitude and latitude columns to the combined sweeps log. If you delve into the raw data, you will see that many of the logs did not include that information. This means coordinates will have to be entered manually for many of the roughly 7,000 sweeps.
<br>
<br>
If you are interested in collaborating or contributing to this repository, please let me know! You can email me at [gossip@guyoron.net](gossip@guyoron.net).
<br>
<br>
<br>
Version history
<br>
<br>
v1.2 - 4 April 2025
<br>-Added new removal log for 2024, which was recently obtained.
<br>-Updated .R code.
<br>-Added new visualizations and reorganized several folders.
<br>-Added new columns to master log of sweeps.
<br>-Fixed several bugs.
<br><br>v1.1 - 22 Jan. 2025
<br>-Added new logs of vehicle impounds in new FAS and SPD folders. These logs range from 2014 to 2024 and can help with research of impoundments of homeless people's vehicles. Because they includes all vehicles impounded, the files are quite large.
<br>-Fixed the 2023 raw excel log, restoring to the original file I recieved via a public records requests. The previous version only had an exported version of the log.
<br>-Added a data dictionary document from Seattle's customer service bureau explaining the logs of complaints about homeless people.
<br><br>v1.0 - 30 Dec. 2024
<br>-Created repository, including all base data.
<br>
<br>

_All data and records were obtained from public and governmental sources and are thus intended for use as public domain under Creative Commons Zero Universal license. A credit or link back to this repository and its creator is appreciated but not required or expected._

