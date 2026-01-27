#PLAN
#1. get data
#2. summarize
#3. histograms
#4. model of demos vs bp

#install stuff
library(tidyverse)
install.packages("haven")
library(haven)
library(ggplot2)

##1
#blood pressure taken BEFORE COVID
file_path <- "C:/Users/oyaya/Downloads/NHANES_data/bp_2017.xpt"
before_bp <- read_xpt(file_path)
head(before_bp)
View(before_bp)


#bp of people in study AFTER COVID
file_path1 <- "C:/Users/oyaya/Downloads/NHANES_data/bp_2021.xpt"
after_bp <- read_xpt(file_path1)
summary(after_bp)
View(after_bp)




##3 (setup)

#only show these columns


bbp <- before_bp |>
  select(SEQN, BPXOSY1:BPXODI3)

abp <- after_bp |>
  select(SEQN, BPXOSY1:BPXODI3)


after_bp <- after_bp |>
  mutate(BPXODI = (BPXODI1+BPXODI2+BPXODI3)/3, .before = BPXOSY1) |>
  mutate(BPXOSY = (BPXOSY1+BPXOSY2+BPXOSY3)/3, .before = BPXOSY1)

before_bp <- before_bp |>
  mutate(BPXODI = (BPXODI1+BPXODI2+BPXODI3)/3, .before = BPXOSY1) |>
  mutate(BPXOSY = (BPXOSY1+BPXOSY2+BPXOSY3)/3, .before = BPXOSY1)

#are the di bp different or same?
t.test(before_bp$BPXODI, after_bp$BPXODI, var.equal = FALSE)
t.test(before_bp$BPXODI, after_bp$BPXODI, alternative = "less", var.equal = FALSE)
#ok, so the x is less than y significantly woohoo!

t.test(before_bp$BPXOSY, after_bp$BPXOSY, var.equal = FALSE)
t.test(before_bp$BPXOSY, after_bp$BPXOSY, alternative = "greater", var.equal = FALSE)
#ok, so x is greater than y significantly woohoo!




#demographics of the people in the study BEFORE COVID
dfile_path <- "C:/Users/oyaya/Downloads/NHANES_data/demo_2017.xpt"
before_demo <- read_xpt(dfile_path)
head(before_demo)
View(before_demo)

#demographics of people in the study AFTER COVID
dfile_path1 <- "C:/Users/oyaya/Downloads/NHANES_data/demo_2021.xpt"
after_demo <- read_xpt(dfile_path1)
View(after_demo)

bd <- before_demo |>
  select(SEQN,RIDAGEYR,RIDRETH3,DMDEDUC2)

#combine pre-COVID demographics and blood pressure into one
nhanes1 <- inner_join(bd, bbp, by = 'SEQN')
View(nhanes1)


#take the average of the di and sy blood pressures
nhanes1 <- nhanes1 |>
  mutate(BPXODI = (BPXODI1+BPXODI2+BPXODI3)/3, .before = BPXOSY1) |>
  mutate(BPXOSY = (BPXOSY1+BPXOSY2+BPXOSY3)/3, .before = BPXOSY1)


##3
#race vs dbp
plot(nhanes1$RIDRETH3, nhanes1$BPXODI)
#Mexican-American has the lowest dbp

#age vs dbp
plot(nhanes1$RIDAGEYR, nhanes1$BPXODI)
#looks quadratic; peaks at 40-60yrs

#race vs sbp
plot(nhanes1$RIDRETH3, nhanes1$BPXOSY)

#age vs sbp
plot(nhanes1$RIDAGEYR, nhanes1$BPXOSY)


##4
bp.model <- lm(BPXODI ~ RIDAGEYR, data = nhanes1)
bp.model
summary(bp.model)

bp.model1 <- lm(I(BPXODI^2) ~ RIDAGEYR, data = nhanes1)
summary(bp.model1)

bp.model2 <- lm(BPXODI ~ RIDAGEYR + I(RIDAGEYR^2), data = nhanes1)
summary(bp.model2)

x2 = as.numeric(nhanes1$RIDAGEYR>50)
x2_star = (nhanes1$RIDAGEYR-50)*x2
pw.bp.model <- lm(BPXODI ~ RIDAGEYR + x2_star, data = nhanes1)
summary(pw.bp.model)
summary(bp.model2)
