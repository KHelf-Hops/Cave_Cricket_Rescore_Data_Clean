# Reading, merging, writing, and summarizing cave cricket monitoring bout data
# Author: Kurt L. Helf
# Last modified on 2024-03-14

# This script looks for all merged files from specific cave cricket monitoring bouts, merges them into a single file, writes as a .csv file, summarizes the data for the purpose of detecting data entry errors (e.g., transcription errors) by yielding a table with counts of correctly and (possibly) *incorrectly* typed attributes. These mistakes can prevent importation of the image scoring .csv file into the cave cricket database so it is important to detect them ahead of time.

# # DEFINITIONS -------------------------------------------------------

# Bout = All surveys (i.e., Events) of all 15 cave entrances within a survey season (e.g., Winter2023) 
# Event = A survey at a specific cave entrance on a specific date within a specific Bout; synonymous with EventID (e.g., CC_176)

# This script also yields basic statistical summaries of bout data for inclusion in trip reports or resource briefs.

# Clear the console

cat("\014")

# Clear the environment

rm(list = ls())

#load data.table to check for consistent data

library(data.table)
library(tidyverse)
library(janitor)

# # Check File List -------------------------------------------------------

# Checks to see if all events have EventID_merged.csv (e.g., CC_176_merged) files so BoutID_merged file (e.g., Winter2023_merged) can be created; this depends on where the data are stored but, typically, storing the scored image data prior to importation to the database and the Z drive (i.e., CUPN drive on MACA server) is MUCH faster on an external HDD  

cc_csvfiles <- 
  list.files('D:/Summer2023', 
             pattern = "^CC_.*\\.csv$",
             recursive = TRUE,
             full.names = TRUE)

cc_csvfiles

# Grab and merge EventID_merged.csv files into BoutID_merged.csv ----------

Winter2024_merged <- 
  read_csv(cc_csvfiles) %>%
  bind_rows()

view(Winter2024_merged)

str(Winter2024_merged)

summary(Winter2024_merged)

# Write file to Bout Merged folder

#Assumes two folders have been created in the BoutID folder: "BoutID_Merged" and "BoutID_Rescore".

bout_path <- "D:/Summer2023/Summer2023_Merged"

write_csv(Winter2024_merged, 
          file.path(bout_path, 
          "Winter2024_merged.csv"), 
          col_names=TRUE)

# Check data are consistent/formatted correctly ---------------------------

# Summarize by Date, EventID, Sex

Winter2024_merged_QAQC <-
  read_csv(
    file.path(bout_path,
              "Winter2024_merged.csv")) %>% # Steps must be separated here(?)
  setDT(Winter2024_merged_QAQC) %>% 
  .[, .N, by = .(Date, EventID, Sex)] %>% 
  dplyr::as_tibble() %>% 
  print(n = 50)

write_csv(Winter2024_merged_QAQC, 
          file.path(bout_path, 
          "Winter2024_merged_QAQC.csv"), 
          col_names=TRUE)

# Basic Stats from Bout ---------------------------------------------------

# Count number clusters (via max ClusterN) but not number crickets

Winter2024_totclustevent <- 
  read_csv(file.path(bout_path,
  "Winter2024_merged.csv")) %>% 
  group_by(EventID, 
           ClusterN) %>% 
  summarise(TotClusters = n()) %>% 
  top_n(1, ClusterN) 

Winter2024_totclustevent <-
  Winter2024_totclustevent[,-3]# max value for ClusterN

write_csv(Winter2024_totclustevent,
          file.path(bout_path,
          "Winter2024_totclustevent.csv"), 
          col_names=TRUE)

# Total number of crickets per event 
  
Winter2024_totcrickevent <- 
  read_csv(
    file.path(bout_path,
              "Winter2024_merged.csv")) %>% 
  group_by(EventID) %>% 
  summarise(TotCrickets = n()) 

write_csv(Winter2024_totcrickevent, 
          file.path(bout_path, 
                    "Winter2024_totcrickevent.csv"), 
          col_names=TRUE)

# Join two dfs from above
# The result gives only the total number of crickets so why the redundant
# code?  Why remove ClusterN if it's supposed to summarize the bout?

Winter2024_summary <-
              left_join(
                Winter2024_totclustevent,
                Winter2024_totcrickevent,
    by = "EventID") # %>% 
 # select(-c(ClusterN)) 

write_csv(Winter2024_summary, 
          file.path(bout_path, 
                    "Winter2024_summary.csv"), 
          col_names=TRUE)

#   count("EventID") %>% 
#   sum(ClusterN)
# 
#   count(ClusterN)
#   #group_by(ClusterN) %>% 
#   #top_n(1, ClusterN)
#   rename(nCrickets = n) 
# 
# winter2023_merged_TotCrick_ByEvent <- winter2023_merged_TotCrick_ByEvent %>%
#   group_by(ClusterN) %>% 
#   top_n(1, ClusterN)
# 
# [winter2023_merged_TotCrick_ByEvent[,
#     .I[ClusterN ==max(ClusterN)], by = EventID]$V1]
 
# 5-10% Rescore From Bout Summary --------------------------------------------

rescore_path <- "D:/Summer2023/Summer2023_Rescore"

# Collapse all cluster data to number of crickets per cluster per Event for 
# 5% rescore

Summer2023_totclust <- 
  read_csv(
    file.path(bout_path,
              "Summer2023_merged.csv")) %>% 
  group_by(EventID,
           ClusterN) %>% 
  summarise(count = n()) %>%   
  rename(TotCrickets = count) # same as 

#write.table(winter2023_TotClust, "winter2023_TotClust.csv", row.names=FALSE, sep=",")

write_csv(Summer2023_totclust,
          file.path(rescore_path,
                    "Summer2023_totclust.csv"),
          col_names=TRUE)

# Random 5-10% Sample for Rescore --------------------------------------------

#Random 5-10% sample; use collapsed df (i.e., "EventID_TotClust") 

Summer2023_rescore_draw <- 
  Summer2023_totclust[sample(
    nrow(Summer2023_totclust),
    nrow(Summer2023_totclust) * 0.1),] %>% # Can there be a choice between 5% and 10% given? 
  select(-c(TotCrickets)) %>% 
  arrange(-desc(EventID)) # sorts EventID in ascending order

write_csv(Summer2023_rescore_draw,
          file.path(rescore_path,
          "Summer2023_rescore_draw.csv"), 
          col_names=TRUE)

# Check Merged Rescore File and Save --------------------------------------

# Summarize merged rescore file by Date, EventID, Sex
# This would be useful *after* the rescore is done

Winter2024_rescoreQAQC <- 
  read_csv(
    file.path(rescore_path,
              "Winter2024_rescore_draw.csv")) %>%
  setDT("Winter2024_rescore_draw.csv") %>% 
  .[,.N,by=.(EventID, 
             ClusterN, 
             Date, 
             Sex, 
             Life_stage, 
             Strip_Pos)] %>% 
  dplyr::as_tibble() %>% 
  print(n = 303)

write_csv(winter2023_rescoreQAQC , 
          file.path(bout_path, 
                    "winter2023_rescoreQAQC.csv"), 
          col_names=TRUE)

# count unique number of clusters per event 

# my code
uniquemergemssgclust <- 
  read_csv(file.path(bout_path,
                     "Missing_CC_Mon_Data_20250130.csv")) %>%
    setDT("Missing_CC_Mon_Data_20250130.csv") %>% 
  .[,.(Missg_Clust = length(unique(ClusterN))), 
    by = EventID] %>% 
  dplyr::as_tibble() %>% 
  print(n = 15) 

# savannah's code; correct but doesn't give ordered results
S_uniquemergemssgclust <- 
  read_csv(file.path(bout_path,
                     "Missing_CC_Mon_Data_20250130.csv")) %>%
  group_by(EventID) %>% 
  dplyr::summarise(Missing_clust = length(unique(ClusterN))) %>%
                     ungroup()
                                         
