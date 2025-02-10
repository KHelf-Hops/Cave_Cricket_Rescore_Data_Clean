#Most often the mistakes made during image scoring data entry are transcription or mis-typing EventID or Date.  These
#mistakes can prevent importation of the image scoring .csv file into the cave cricket database.  A quick and easy way to
#look for these mistakes is to use the data.table package

#This short script can detect transcription errors by yielding a table with counts of correctly and *incorrectly* typed
#attributes.

# Clear the console
cat("\014")

# Clear the environment
rm(list = ls())

#load data.table to check for consistent data
library(data.table)
library(tidyverse)
library(readr)
library(janitor)

#set working directory to where Merged .csv files from the Bout in question are contained
#For example, setwd("D:/My Book File Backup/CUPN_MACA_Cave_Cricket_Images/Summer2019/Summer2019_Merged") 
setwd("Z:\\CUPN\\CUPN-MACA IM Projects\\Vital_Signs\\Vital_Signs_Prj\\Cave_Crickets\\Images\\Winter2022\\FrNiEn\\Merged")

#read in data from CC_EventID_ScorerInitials_Merged .csv file 
CC_139_Merged<-read.csv("CC_139_merged.csv")

View(CC_139_Merged)
 
#look for mis-typed Dates or EventIDs
#I'd like to be able to check Cluster #s; we know the # of clusters per event after fieldwork so I'd like to be able to check that
#all N are there
setDT(CC_139_Merged)[,.N,by=Date]
setDT(CC_139_Merged)[,.N,by=EventID]
setDT(CC_139_Merged)[,.N,by=ClusterN]
setDT(CC_139_Merged)[,.N,by=Sex]
setDT(CC_139_Merged)[,.N,by=Strip_Pos]
dplyr::n_distinct(CC_139_Merged$ClusterN)


#setDT(CC_44_Merged)[,.N,by=EventID]
#EventID    N
#1:   CC_44 1781  CORRECT 
#2:   CC-44    1  INCORRECT

#setDT(CC_44_Merged)[,.N,by=Date]
#Date    N
#1: 6/19/2018 1728  CORRECT
#2: 6/29/2018   54  INCORRECT

#Full join of scorers' merged .csv files
Winter2020_Merged_Deason<-read.csv("Winter2020_Merged/Winter2020_Merged_Deason.csv")
Winter2020_Merged_Huck<-read.csv("Winter2020_Merged/Winter2020_Merged_Huck.csv")

#Combine .csv files into one
Winter2020_Merged<-rbind(Winter2020_Merged_Deason,Winter2020_Merged_Huck)

View(Winter2020_Merged)

#save to Merged folder and Rescore folder
write.csv(Winter2020_Merged, "Winter2020_Merged/Winter2020_Merged.csv")
write.csv(Winter2020_Merged, "Winter2020_QC_Rescore/Winter2020_Merged.csv")







###Ellen's script for renaming Strip Pos from "-1" to "StripPos"### 
template_StripPos <- unique(dat[c("ClusterId", "StripPos")]) %>%
  dplyr::filter(StripPos != -1) %>%
  dplyr::rename(StripPosX = StripPos)
dat %<>%
  left_join(template_StripPos, by = c("ClusterId"))
dat$InStrip <- dat$StripPos!=-1

write.csv(Summer2019_Merged_Deason, "D:/My Book File Backup/CUPN_MACA_Cave_Cricket_Images/Summer2019
/Summer2019_Merged/Summer2019_Merged_Deason.csv") 
#save changes to specific image scorer's merged file

