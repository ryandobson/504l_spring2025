
#> clear environment 
rm(list = ls()) 

# load packages 
library(tidyverse)
library(psych)
library(DescTools)

source("504_plot_theme.R")


df <- read.csv("data/friendship_feb12_2025.csv")


#> Contact Frequency: contact_freq 
#> 3 = daily 
#> 2 = several times a week 
#> 1 = less than once a week 
summary(as.factor(df$contact_freq))

df$contact_freq <- factor(df$contact_freq, levels = c(3, 2, 1), labels = c("daily", "several times a week", "less than once a week"))


m1 <- aov(rom.jealous ~ as.factor(contact_freq), data = df)
summary(m1)

DescTools::PostHocTest(m1, method = "hsd") #tukey hsd output 
DescTools::PostHocTest(m1, method = "scheffe")
DescTools::PostHocTest(m1, method = "bonferroni")



#> Testing if daily differs from the other two (no correction for multiple tests) 
contrasts(df$contact_freq) 
contrasts(df$contact_freq) <- contr.treatment(3, base = 1) #reference level = "daily" 

m2 <- lm(rom.jealous ~ contact_freq, data = df)
summary(m2)


summary(df$contact_freq)
#> Comparing daily with less than daily 
c1 <- c(2, -1, -1)

m3 <- aov(rom.jealous ~ C(contact_freq, c1), data = df)
summary(m3)
summary(m3, split = list("C(contact_freq, c1)" = list(
  "Daily vs Less than Daily" = 1
)))

#> Comparing several times a week with less than several times a week 
c2 <- c(0, -1, 1)

comps <- cbind(c1, c2)

contrasts(df$contact_freq) <- comps #combining the contrasts to get both sets
#done in a single analysis. 
contrasts(df$contact_freq)

m4 <- aov(rom.jealous ~ C(contact_freq, comps), data = df)
# Use `summary()` with contrast splits
summary(m4, split = list("C(contact_freq, comps)" = list(
  "Daily vs Less than Daily" = 1, 
  "Several Times a Week vs Less than Several Times a Week" = 2
)))




