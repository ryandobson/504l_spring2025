


#> clear environment 
rm(list = ls()) 

# load packages 
library(tidyverse)
library(psych)
library(DescTools) #contrasts 

source("504_plot_theme.R")


df <- read.csv("data/friendship_feb12_2025.csv") |> 
  rename(
    rel_status = use.relationship.status.participant
  )


#> Contact Frequency: contact_freq 
#> 3 = daily 
#> 2 = several times a week 
#> 1 = less than once a week 
summary(as.factor(df$contact_freq))

df$contact_freq <- factor(df$contact_freq, levels = c(3, 2, 1), 
                          labels = c("daily", "several times a week", "less than once a week"))

summary(df$contact_freq)


df$rel_status <- factor(df$rel_status, levels = c(1, 2), labels = c("single", "committed"))
df$gender <- factor(df$gender, levels = c(1, 2), labels = c("men", "women"))

table(df$rel_status, df$gender)


#> R defaults to Type 1 Sums of Squares 
m1 <- aov(rom.jealous ~ gender * rel_status, data = df)
summary(m1)

library(broom) #great package to easily get statistical information into a 
# data frame that is much easier to work with. 
# Get model estimates and standard errors
m1_sum <- broom::tidy(m1) 
m1_sum


# Load car package for type 3 sums of squares 
library(car)

#> This will change your global options to change the contrast to "contr.sum" 
#> which compares effects to the grand mean. 
#> The R default is "contr.treatment," which will compare one group to all of the
#> other groups. 
#> ChatGPT told me that "contr.poly" is specified for continuous predictors. We
#> only have categorical predictors, but I guess its best to specify both because
#> R wants to have information for both, even if you only have categorical predictors. 
options(contrasts = c("contr.sum", "contr.poly"))

# Fit a linear model
lm1 <- lm(rom.jealous ~ gender * rel_status, data = df)
summary(lm1)
# Perform Two-Way ANOVA with Type III SS
m2 <- car::Anova(lm1, type = 3)  # Type III SS
print(m2)


#> Graph the Effects

# Calculate means and standard errors for each group
df_p1 <- df %>%
  group_by(gender, rel_status) %>%
  summarise(
    Mean = mean(rom.jealous, na.rm = TRUE),
    SEM = sd(rom.jealous, na.rm = TRUE) / sqrt(n()), #Standard Error of the Mean -- this isn't
    #exactly the 95% CI that your model would have so sometimes the error bars will
    #not be perfectly accurate, but I find its much easier to calculate and will 
    #give you the general idea. Might want to pull out the exact 95% CI out of the
    #model for publication. 
    .groups = "drop"
  )

df_p1 #data frame for plot 1 


# Create the interaction plot
df_p1 |> ggplot(aes(x = rel_status, y = Mean, color = gender, group = gender)) +
  geom_line(size = 1) +  # Add lines connecting means
  geom_point(size = 3) +  # Add points for each mean
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.2) +  # Add error bars
  labs(
    title = "Interaction Plot",
    x = "Relationship Status",
    y = "Romatic Jealousy",
    color = "Gender"
  ) +
  theme_minimal() +  # Use a clean theme
  theme(legend.position = "right")



#> Post-hoc Tests to Probe Main Effects 

library(emmeans) #this is a new package that is really powerful and great for 
#follow up tests 

emm1 <- emmeans(lm1, ~ gender)
summary(emm1) # this gives you the means of gender averaged over relationship status 

contrast(emm1, method = "pairwise", adjust = "tukey") #this tests whether men and
#women are different from each other. 

emm2 <- emmeans(lm1, ~ rel_status)
summary(emm2)

contrast(emm2, method = "pairwise", adjust = "tukey") #this tests whether single
#versus committed people are different from each other. 



#> This will give you the main effect comparisons and all pairwise comparisons. 
#> The results are nearly identical to the above method, I'm not entirely sure
#> why they differ. I'm sure it has to do with how the sums of squares are calculated
#> with uneven groups though. The TukeyHSD function takes the aov model that uses
#> type 3 sums of squares. I'm not exactly sure how the lm model function calculates
#> sums of squares. 
TukeyHSD(m1)


# Compute Type III ANOVA manually
# lm2 <- drop1(lm1, . ~ ., test = "F")
# print(lm2)


#> Probing Interactions --------------


#> Most simply, you are just looking at effects within a single group. 
#> Practically, you can just split your data and perform new tests: 

#Method2:
#Testing the effect of alcohol on attractiveness at different gender groups 
#subset dataset based on the levels of one factor, taking gender as example
#Note that his method does not have the option to adjust p-value
df.w <- subset(df, gender=="women")
df.m <- subset(df, gender=="men")

m1w <- aov(rom.jealous ~ rel_status, data = df.w)
m1m <- aov(rom.jealous ~ rel_status, data = df.m)
summary(m1w)
summary(m1m)
#> This tells you if the effect of relationship status is significant for men 
#> and women. There are only two groups, so it is equivalent to the t-test here,
#> but if there were more than two groups, we would need to perform post-hoc
#> tests on the significant results. 
#> With this method you can control for type 1 error rates on the post-hoc tests
#> within groups. 
#> HOWEVER, importantly, you are not controlling for the p-values in the models
#> themselves. Ideally, you would control for the p-values in m1w and m1m, in 
#> a single test. 

#> If desired, you could explore the interaction in the other way. You only need
#> one version, but you have to decide which version is better for interpretation. 
df.s <- subset(df, rel_status=="single")
df.c <- subset(df, rel_status=="committed")

m1s <- aov(rom.jealous ~ gender, data = df.s)
m1c <- aov(rom.jealous ~ gender, data = df.c)
summary(m1s)
summary(m1c)


library(phia)

testInteractions(m1, fixed = "gender", across = "rel_status", adjustment = "none")
#> This output is --roughly-- equivalent to m1w and m1m output. 
#> However, the test is still done a bit differently so F-tests and p-values vary
#> a little bit, even when you specify no p-value adjustment as above. 
#> With the testInteractions function, the tests are done with a pooled error variance
#> term. In the separate models, there is a unique error variance term used. 
#> Using the testInteractions function is preferred. 

#> Same as above but now with an adjustment. 
testInteractions(m1, fixed = "gender", across = "rel_status", adjustment = "bonferroni")
#> Interpretation: 
#> "men" row -- There is no effect of relationship status for men. 
#> "women" row -- There is no effect of relationship status for women. 

#> The below test is conceptually equivalent to the m1s and m1c output. Again, 
#> it just differs in its p-value adjustment and calculation of the F-test by 
#> a pooled error variance term. 
testInteractions(m1, fixed = "rel_status", across = "gender", adjustment = "bonferroni")
#> "single" row -- There is no effect of gender for people who are single 
#> "committed" row -- There is no effect of gender for people who are committed  


#> Notably, the testInteractions function still only provides F-tests for your simple effect 
#> analyses. 
#> If there were more than 2 levels of relationship or gender, you would not know
#> which groups actually differed from each other. 


# If the interaction is significant, you should test simple effects separately to 
# understand how one IV behaves at each level of the other IV.
#> We can use the emmeans package to do this. 

#> Testing the effect of gender at each level of relationship status (if significant interaction)
emm3 <- emmeans(lm1, ~ gender | rel_status)
summary(emm3)

#?contrast #check out more information on the contrast function 
c3 <- contrast(emm3, method = "pairwise", adjust = "bonferroni")
c3


# Testing the effect of relationship status at each level of gender 
emm4 <- emmeans(lm1, ~ rel_status | gender)
summary(emm4)

c4 <- contrast(emm4, method = "pairwise", adjust = "bonferroni")
#pairs(emm4) #this returns the same output as the above contrasts function here. 
c4


#> This provides all of the pairwise comparisons in a simple way: 
PostHocTest(m1, method = "hsd") # hsd = TukeyHSD (i.e., tukey)

#> This provides the same output but organizes it a little bit differently. 
#> Probably easier to read the above method 
emm5 <- emmeans(lm1, ~ gender * rel_status)
emm5

pairs(emm5, adjust = "tukey") #this output provides all pairwise comparisons
#it also matches the output from PostHocTest(m1, method = "hsd")

#> Testing all pairwise comparisons is probably the simplest method. If you look
#> closely, it gives you all of the relevant simple effect comparisons that you
#> will want to explore. 
#> For example, the pairs(emm5, adust = "tukey") gives you information from all of
#these models: 
c3 # single men versus women AND committed men versus women 
c4 # men who are single versus committed and women who are single versus committed 

#> All pairwise comparisons also gives you other unique comparisons not tested
#> in the simple effects: 
# men single versus women committed 
# women single versus men committed 

#> If you had specific hypotheses about those unique comparisons, you could also
#> use the emmeans contrast function to test those unique comparisons. 
contrast(emm5, 
         list("Single Men vs. Committed Women" = c(1, 0, 0, -1),
              "Committed Men vs. Single Women" = c(0, 1, -1, 0)),
         adjust = "bonferroni") 





