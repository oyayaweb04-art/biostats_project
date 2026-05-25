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
#xb = mean before
#xa = mea after
#s = systolic
#d = diastolic

#H0: xbd = xad
#HA: xbd != xad  /  xbd < xad
t.test(before_bp$BPXODI, after_bp$BPXODI, var.equal = FALSE)
t.test(before_bp$BPXODI, after_bp$BPXODI, alternative = "less", var.equal = FALSE)
#ok, so the before is less than after significantly woohoo! p = 0.0002751


#H0: xbs = xas
#HA: xbs != xas  /  xbs > xas
t.test(before_bp$BPXOSY, after_bp$BPXOSY, var.equal = FALSE)
t.test(before_bp$BPXOSY, after_bp$BPXOSY, alternative = "greater", var.equal = FALSE)
#ok, so before is greater than after significantly woohoo! p = 0.006172


t.test(before_bp$BPXOSY)
#95% confidence: [119.4326, 120.1818]
#this means that if we were to take many more samples with sample size n, then 95% of the calculated CIs will cover or capture the true mean  of this population.


t.test(after_bp$BPXOSY)
#95% confidence: [118.6871, 119.5082]
#this means that if we were to take many more samples with sample size n, then 95% of the calculated CIs will cover or capture the true mean  of this population.


t.test(before_bp$BPXODI)
#95% confidence: [71.37614, 71.84070]
#this means that if we were to take many more samples with sample size n, then 95% of the calculated CIs will cover or capture the true mean  of this population.


t.test(after_bp$BPXODI)
#95% confidence: [71.96296, 72.48243]
#this means that if we were to take many more samples with sample size n, then 95% of the calculated CIs will cover or capture the true mean  of this population.




#look at before data
hist(before_bp$BPXODI, nclass=500, main = "diastolic blood pressure before", xlab="diastolic bp")
#pretty normal, but slightly skewed to the right (a lot of outliers)

hist(before_bp$BPXOSY, nclass=500, main = "systolic blood pressure before", xlab="systolic bp")
#more skewed to the right



#look at after data

hist(after_bp$BPXODI, nclass=500, main = "diastolic blood pressure after", xlab="diastolic bp")
#more normally distributed

hist(after_bp$BPXOSY, nclass=500, main = "systolic blood pressure after", xlab="systolic bp")
#skewed to right

#seems that both systolics are skewed to the right

summary(after_bp$BPXODI)
summary(after_bp$BPXOSY)



#demographics of people in the study AFTER COVID
dfile_path1 <- "C:/Users/oyaya/Downloads/NHANES_data/demo_2021.xpt"
after_demo <- read_xpt(dfile_path1)
View(after_demo)

ad <- after_demo |>
  select(SEQN,RIDAGEYR,RIDRETH3,DMDEDUC2)

#combine pre-COVID demographics and blood pressure into one
nhanes1 <- inner_join(ad, abp, by = 'SEQN')
View(nhanes1)



#take the average of the di and sy blood pressures
nhanes1 <- nhanes1 |>
  #rename("Age in Years" = RIDAGEYR, Race = RIDRETH3, "Education Level" = DMDEDUC2) |>
  mutate(BPXODI = (BPXODI1+BPXODI2+BPXODI3)/3, .before = BPXOSY1) |>
  mutate(BPXOSY = (BPXOSY1+BPXOSY2+BPXOSY3)/3, .before = BPXOSY1)



##3
#race vs dbp
plot(nhanes1$RIDRETH3, nhanes1$BPXODI)
#Mexican-American has the lowest dbp

summary(nhanes1$RIDRETH3)

#age vs dbp
plot(nhanes1$RIDAGEYR, nhanes1$BPXODI)
#looks quadratic; peaks at 40-60yrs

#race vs sbp
plot(nhanes1$RIDRETH3, nhanes1$BPXOSY)


#age vs sbp
plot(nhanes1$RIDAGEYR, nhanes1$BPXOSY)
#linear??


##4
bp.model <- lm(BPXODI ~ RIDAGEYR, data = nhanes1)
summary(bp.model)
#multiple R-sq = 0.09029

bp.model1 <- lm(I(BPXODI^2) ~ RIDAGEYR, data = nhanes1)
summary(bp.model1)
#multiple R-sq = 0.08201


bp.model2 <- lm(BPXODI ~ RIDAGEYR + I(RIDAGEYR^2), data = nhanes1)
summary(bp.model2)
#multiple R-sq = 0.2291

bp.model3 <- lm(BPXODI ~ RIDAGEYR + I(RIDAGEYR^2) + I(RIDAGEYR^3), data = nhanes1)
summary(bp.model3)
#multiple R-sq = 0.2291



bp.model5 <- lm(BPXODI ~ RIDAGEYR + I(RIDAGEYR^2) + I(RIDAGEYR^3) + I(RIDAGEYR^4) + I(RIDAGEYR^5), data = nhanes1)
summary(bp.model5)
#multiple R-sq = 0.2298

bp.model9 <- lm(BPXODI ~ RIDAGEYR + I(RIDAGEYR^2) + I(RIDAGEYR^3) + I(RIDAGEYR^4) + I(RIDAGEYR^5) + I(RIDAGEYR^6) + I(RIDAGEYR^7) + I(RIDAGEYR^8) + I(RIDAGEYR^9), data = nhanes1)
summary(bp.model9)
#multiple R-sq = 0.2301

x2 = as.numeric(nhanes1$RIDAGEYR>48)
x2_star = (nhanes1$RIDAGEYR-48)*x2
pw.bp.model <- lm(BPXODI ~ RIDAGEYR + x2_star, data = nhanes1)
summary(pw.bp.model)
#multiple R-sq = 0.2258

anova(bp.model5, bp.model9)

nl_model1 = lm((BPXODI) ~ RIDAGEYR + I(RIDAGEYR^2), data = nhanes1)
summary(nl_model1) #R-sq = 0.09588

nl_model2 = lm(log(BPXODI) ~ RIDAGEYR + I(RIDAGEYR^2), data = nhanes1)
summary(nl_model2)
#R-sq (multiple) 0.2443
#we'll use this one!

#residual analysis

plot(nhanes1$RIDAGEYR, nhanes1$BPXODI)

ggplot(nhanes1, aes(x = RIDAGEYR, y = log(BPXODI))) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              color = "blue",
              se = TRUE) +
  labs(title = "Quadratic Regression Fit",
       x = "Age",
       y = "log(BPXODI)")

length(nl_model1$residuals) #7478
length(nl_model2$residuals) #7478
#nhanes1 has 7801 observations. why are they different? remove NAs?

nhanes1 <- nhanes1 |>
  filter(!is.na(BPXODI))

res <- nl_model2$residuals #got it!!


plot(fitted(nl_model2), res)

qqnorm(res)
qqline(res)



