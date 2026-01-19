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
#blood pressure taken before COVID
file_path <- "C:/Users/oyaya/Downloads/NHANES_data/bp_2017.xpt"
before_bp <- read_xpt(file_path)
head(before_bp)
View(before_bp)


#demographics of the people in the study before COVID
file_path <- "C:/Users/oyaya/Downloads/NHANES_data/demo_2017.xpt"
before_demo <- read_xpt(file_path)
head(before_demo)
View(before_demo)

##2
summary(before_bp)
summary(before_demo)

##3 (setup)

#only show these columns
bd <- before_demo |>
  select(SEQN,RIDAGEYR,RIDRETH3,DMDEDUC2)

bbp <- before_bp |>
  select(SEQN, BPXOSY1:BPXODI3)


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


