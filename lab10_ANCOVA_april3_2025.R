

#> clear environment 
rm(list = ls()) 

# load packages 
library(tidyverse)
library(psych)
library(DescTools) #contrasts 

source("504_plot_theme.R")

df <- read.csv("data/friendship_feb12_2025.csv")

df$X <- NULL

df$contact_freq <- as.factor(df$contact_freq)

df |> group_by(contact_freq) |> 
  summarize(across(c(rom.happy, rom.jealous), \(x) mean(x, na.rm = TRUE)))


#> What makes for a good covariate for an ANCOVA design? 
#> Increasing power: 
#> (1) The covariate accounts for variance in the DV 
#> (2) The covariate is minimally related to the IV 


#> Linearity Assumption 
df |>ggplot(aes(x = rom.happy, y = log(rom.jealous))) +
  geom_point () +
  geom_smooth(method = "loess", span = .9) + #can adjust span as needed for data
  theme504()
#> Relationship appears to be roughly linear. Might be some small quadratic trend. 


#> Independence of Covariate and IV 
summary(lm(rom.happy ~ contact_freq, df))
#> There is a relationship between contact frequency and the covariate, suggesting
#> that it might not be an ideal covariate. 

#> Homogeneity of Slope test (i.e., is there an interaction effect?)
mslope <- aov(rom.jealous ~ rom.happy * contact_freq, df)
summary(mslope) #In this case, there is an effect...so we should interpret this
#differently. 
#> See the chapter I posted on Canvas! They recommend never doing a true ANOVA
#> and always using a lm() to run things.


#> Other assumptions...
#> Independence
#> Homogeneity of variance
#> Normality 
#> Fixed-effect
#> Covariate measured without error 


#> Testing the effect of the factor without the control. 
m1 <- aov(rom.jealous ~ contact_freq, df)
summary(m1)


#> Control for the covariate (rom.happy) and examine the effect of the IV (contact_freq) 
m2 <- aov(rom.jealous ~ rom.happy + contact_freq, df) #don't model the interaction! (i.e. use a "+" instead of "*")
summary(m2)
#> Interpretation: 
#> There is an effect for "rom.happy" which is the covariate. In any ANCOVA, we 
#> don't care about interpreting this effect. 
#> Honestly, it is probably rare that you would never care about the covariate at 
#> all. In addition, if your covariate does not account for any variance in the 
#> DV its probably not a very useful covariate. 
#> There should be some effect of your covariate for it to be useful. 
#> Now we can interpret the effect of contact frequency while the covariate is 
#> controlled. 


#> Follow-up Tests

library(multcomp)

mph <- glht(m1, linfct = mcp(contact_freq = "Tukey"))
summary(mph)
confint(mph) #95% Confidence Interval 

#> For each comparison...when holding rom.happy at the same level, the groups 
#> differ in the following ways: 
#> Group 2 compared to 1 there is no difference 
#> Group 3 compared to 1 there is a difference of 2.0 
#> Group 3 compared to 2 there is a difference of 2.0 

#> The interpretations are weird here because I edited the contact frequency
#> variable and group 2 has the same responses as group 1.












