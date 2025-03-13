

#> clear environment 
rm(list = ls()) 

# load packages 
library(tidyverse)
library(psych)
library(DescTools) #contrasts 

source("504_plot_theme.R")

df <- read.csv("friendship_within.csv")


#> Mixed-Subject Design: 
#> one within subjects factor and one between subjects factor.

#> Within = imagined hypothetical friend interloper
#> e.g., In this study, participants were asked to imagine their opposite-sex friend
#> starting a (1) same-sex friendship, (2) opposite-sex friendship, or (3) romantic
#> relationship. 

#> Between = gender 
#> men or women 

#transform data from wide format to long format
dfl <- df |> 
  pivot_longer(
    cols = colnames(df[, grepl("jealous", names(df))]),
    names_to = "interloper",
    values_to = "jealous"
  ) |> 
  mutate(
    gender = factor(gender) |> 
      recode_factor(
        "1" = "men",
        "2" = "women"
      ),
    interloper = factor(interloper),
    pid = factor(pid)
  )


#Levene's test for between subject factor
library(car)
leveneTest(dfl$jealous, dfl$gender, center = mean)
# jealous = your dependent variable scores 
# gender = your between subjects grouping variable 

library(rstatix)
#rstatix::levene_test()
#Can also perform Levenes test at each level of the within-subjects variable. 
#According to Yu-Yu, if the interaction is of interest, you should do this. 
dfl |> group_by(interloper) |> 
  levene_test(jealous ~ gender)


#Mix design ANOVA
library(ez)
m1 <-ezANOVA(data = dfl, dv = .(jealous), wid = .(pid), within= .(interloper), 
                between = .(gender), detailed = TRUE, type = 3)
m1

#Report descriptive statistics
ezStats(data = dfl, dv = .(jealous), wid = .(pid), within= .(interloper), 
        between = .(gender))

#plot marginal means
ezPlot(data = dfl, dv = .(jealous), wid = .(pid), within= .(interloper), 
       between = .(gender), x = .(interloper), split = .(gender))


#> Some other functions to output these results: 

options(scipen = 999)
library(rstatix) #the function in this package also provides the extra tests
m2 <- anova_test(data = dfl, dv = jealous, wid = pid, within = interloper, 
                 between = gender, type = 3)
get_anova_table(m2, correction = "auto")
m2
?anova_test
#> Need the error term to be a factor for this to return the same
#> result. 
m3 <- aov(jealous ~ interloper * gender + Error(pid/interloper), data = dfl)
summary(m3)


#> Split up the data into men and women datasets: 
dflM <- subset(dfl, gender == "men")
dflW <- subset(dfl, gender == "women")

#paired t-test for men 
pairwise.t.test(dflM$jealous, dflM$interloper, paired = TRUE, p.adjust.method = "bonferroni")
#> For demonstration purposes of this function, I look at just men here. 
#> Practically, you would want to use this function if there was an interaction 
#> effect (e.g., interloper interacts with gender) and you want to see the differences
#> for your within-subjects factors (e.g., interloper) by your between subjects factor
#> (e.g., gender). 
#> Consider another example: you have a treatment and control group that you follow 
#> over time (pre, post, follow up) that you track for anxiety. The control group
#> receives nothing and the treatment group receives some form of therapy. 
#> You determine that there is an interaction effect for group (treatment or control)
#> and time (pre, post, follow up). 
#> Suggesting that the effect of time differs for people in the treatment and control
#> group. 
#> We might already have an idea of what this interaction is (if the treatment
#> is effective): the control group does not change on whatever outcome variable
#> you observe (e.g., anxiety) and the treatment group gets better. 
#> So, you can run this pairwise.t.test() function twice: 
#> Once for the treatment group and once for the control group. 
#> The output will give you all of the pairwise comparisons:
#> pre versus post
#> pre versus follow up 
#> post versus follow up
#> So, you can see what differences there are in each group. 


#> Because I don't have an interaction effect, all of my pairwise differences
#> should be identical for men and women: 
pairwise.t.test(dflW$jealous, dflW$interloper, paired = TRUE, p.adjust.method = "bonferroni")
#> As expected, the opposite-sex interloper and romantic partner interloper are
#> not different from each other, but they are both different from the same-sex
#> interloper. 

#> Because I didn't have an interaction test, I also could have just used this
#> function on the full sample, not splitting men or women: 
#> This is back to considering it a one-way within-subjects design. 
pairwise.t.test(dfl$jealous, dfl$interloper, paired = TRUE, p.adjust.method = "bonferroni")


#> Per lab 7, here are some other ways to look at the effect: 

library(car)

#install.packages("afex")  # Run only if you don't have afex
library(afex)

options(contrasts = c("contr.sum", "contr.poly"))  # Ensure Type III contrasts

# Need to specify the original aov function and use this new function that provides
# a type 3 sums of squares object that you can do post-hoc tests on with emmeans()
m3 <- aov_car(jealous ~ gender * interloper + Error(pid/interloper), data = dfl)
summary(m3)

# Post-hoc for interloper (within-subjects)
emmeans(m3, pairwise ~ interloper, adjust = "bonferroni")

# Post-hoc for gender (between-subjects)
emmeans(m3, pairwise ~ gender, adjust = "bonferroni")

# All post-hoc tests
emmeans(m3, pairwise ~ gender * interloper, adjust = "bonferroni")






