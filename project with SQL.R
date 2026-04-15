#SQL
install.packages(c("DBI", "RSQLite"))
library(DBI)
library(RSQLite)
library(tidyverse)
library(haven)



## CONVERT XPT TO R
#blood pressure taken BEFORE COVID
bbfile_path <- "C:/Users/oyaya/R_Projects/biostats_project/NHANES_data/bp_2017.xpt"
before_bp <- read_xpt(bbfile_path)
head(before_bp)
View(before_bp)


#bp of people in study AFTER COVID
abfile_path <- "C:/Users/oyaya/R_Projects/biostats_project/NHANES_data/bp_2021.xpt"
after_bp <- read_xpt(abfile_path)
summary(after_bp)
View(after_bp)


bdfile_path <- "C:/Users/oyaya/Downloads/NHANES_data/demo_2017.xpt"
before_demo <- read_xpt(bdfile_path)
head(before_demo)
View(before_demo)

#demographics of people in the study AFTER COVID
adfile_path <- "C:/Users/oyaya/Downloads/NHANES_data/demo_2021.xpt"
after_demo <- read_xpt(adfile_path)
View(after_demo)



#R TO SQL DATABASE

library(DBI)
library(RSQLite)

con <- dbConnect(
  RSQLite::SQLite(),
  "practice_db.sqlite"
)

dbWriteTable(con, "bp_before", before_bp, overwrite = TRUE)
dbWriteTable(con, "bp_after", after_bp, overwrite = TRUE)
dbWriteTable(con, "demo_before", before_demo, overwrite = TRUE)
dbWriteTable(con, "demo_after", after_demo, overwrite = TRUE)

head(before_bp)

result <- dbGetQuery(con, 
  "SELECT SEQN, BPXOSY1, BPXODI1, BPXOSY2, BPXODI2, BPXOSY3, BPXODI3
  FROM bp_before"
)
result <- dbGetQuery(con,
  "SELECT SEQN, BPXOSY1, BPXODI1, BPXOSY2, BPXODI2, BPXOSY3, BPXODI3
  FROM bp_after"
)

result <- dbGetQuery(con,
  "")





