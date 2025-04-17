
######Two-Factor Hierarchical ANOVA#####
########################################

ch16_nested <- read.csv("data/Ch16_nested.csv")
names(ch16_nested)

#define the interventionist variable as a factor
ch16_nested$Intvnst <- as.factor(ch16_nested$Intvnst)

#Two-factor Hierarchical ANOVA
ch16nest <- aov(Quality ~ Tx + Error(Intvnst), ch16_nested)
summary(ch16nest)

#install.packages("nlme")
library(nlme)

#Running the same analysis but treat the model as a Multilevel Model
model_nest <- lme(fixed = Quality ~ Tx, random = ~1 | Intvnst, data = ch16_nested)
summary(model_nest)

library(lme4) #a different multilevel model (aka mixed-effects) package
library(lmerTest)

lmer_model <- lmer(Quality ~ Tx + (1 | Intvnst), data = ch16_nested)
summary(lmer_model)

# Tukey-adjusted comparisons on Tx
library(emmeans)
emmeans(model_nest, pairwise ~ Tx, adjust = "tukey")


#####Two-Factor Randomized Block Model for n=1#####
###################################################
ch16_block <- read.csv("data/ch16_block.csv")
names(ch16_block)

ch16_block$ProgramF <- factor(ch16_block$Program, labels = c("1/week", "2/week", "3/week", "4/week"))
ch16_block$Age <- ordered(ch16_block$Age, labels = c("20","30","40","50"))
summary(ch16_block)

#Two-Factor Fixed-Effect Randomized Block ANOVA
Model_block <- aov(formula = WtLoss ~ ProgramF + Age, data = ch16_block)
summary(Model_block)



#> Thinking About Nested versus Fully Crossed Designs: 


#> Repeated Measure ANOVA Designs 
#> What happens if you have 3 time-points for a single participant but that 
#> participant is missing even 1 time-point? 
#> For a repeated-measures ANOVA, it uses list-wise deletion, meaning that the 
#> case is entirely removed from the model before running it. 
#> This is connected to maintaining the fully factorial crossed design!
#> When you have a person missing any data point, they make the design not
#> fully crossed.
#> The way the repeated measures ANOVA calculates the error terms requires that
#> the design be fully crossed . 
#> A mixed-effects (aka multilevel modeling, aka hierarchical modeling) approach
#> calculates error terms differently and allows for participants to be missing
#> time points and still be included in the model. This is because the mixed-effects
#> approach naturally allows for nesting.  



#> It is obvious that siblings are nested in families, but what would it mean to
#> have a sibling design that is fully crossed? 
#> If we had a sibling design that was fully crossed EVERY sibling would have to
#> be in EVERY family. 


#> What about in a clinical example with clients and therapists? 
#> In a typical design, therapists are higher-level units, and clients are nested
#> within therapists. 
#> If we were to make this design crossed, every client would have to see every
#> therapist. 
#> For example, you could have 3 therapists and 10 clients. Every client would 
#> have to see all three therapists. Per an anova design, if there was any missing
#> data that client would have to be dropped entirely. 











