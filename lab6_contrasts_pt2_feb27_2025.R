# Load necessary packages
library(tidyverse)
library(multcomp)
library(DescTools)

# Set seed for reproducibility
set.seed(42)

# Define group names for a clinical psychology study
therapy_group <- rep(c("CBT", "Psychoanalysis", "Mindfulness", "Control"), each = 30)  

# Generate test scores (symptom reduction scores) for 30 participants per group
test_scores <- c(
  rnorm(30, mean = 75, sd = 5),  # CBT
  rnorm(30, mean = 70, sd = 5),  # Psychoanalysis
  rnorm(30, mean = 80, sd = 5),  # Mindfulness
  rnorm(30, mean = 60, sd = 5)   # Control
)

# Create dataframe
df <- data.frame(Therapy_Type = factor(therapy_group, levels = c("Control", "CBT", "Psychoanalysis", "Mindfulness")),
                 Symptom_Reduction = test_scores)


# Save dataset as CSV
#write.csv(df, "clinical_psychology_contrasts.csv", row.names = FALSE)

# Fit ANOVA model
anova_model <- aov(Symptom_Reduction ~ Therapy_Type, data = df)


#> Using the method from last class: 

# Define planned contrasts
c1 <- c(3, -1, -1, -1)  # Therapy (CBT, PA, MBT) vs. Control
c2 <- c(0, 1, 1, -2)    # Mindfulness vs. (CBT & Psychoanalysis)

# Fit ANOVA model with first contrast
m1 <- aov(Symptom_Reduction ~ C(Therapy_Type, c1), data = df)
summary(m1)

# Use `summary()` with contrast splits
summary(m1, split = list("C(Therapy_Type, c1)" = list(
  "Therapy vs Control" = 1
)))

# Second contrast: Comparing Mindfulness vs. CBT & Psychoanalysis
comps <- cbind(c1, c2)  # Combine contrasts

# Assign contrasts to the Therapy_Type factor
contrasts(df$Therapy_Type) <- comps
contrasts(df$Therapy_Type)  # View assigned contrasts

# Fit ANOVA model with both contrasts
m2 <- aov(Symptom_Reduction ~ C(Therapy_Type, comps), data = df)

# Use `summary()` with contrast splits
summary(m2, split = list("C(Therapy_Type, comps)" = list(
  "Therapy vs Control" = 1, 
  "Mindfulness vs CBT & Psychoanalysis" = 2
)))


c3 <- c(0, 0, 1, -1)
comps <- cbind(comps, c3)
contrasts(df$Therapy_Type) <- comps 
contrasts(df$Therapy_Type)


# Fit ANOVA model with both contrasts
m3 <- aov(Symptom_Reduction ~ C(Therapy_Type, comps), data = df)

# Use `summary()` with contrast splits
summary(m3, split = list("C(Therapy_Type, comps)" = list(
  "Therapy vs Control" = 1, 
  "Mindfulness vs CBT & Psychoanalysis" = 2,
  "Psychoanalysis vs Mindfulness" = 3
)))


# Tukey's HSD test for all pairwise comparisons
tukey_results <- stats::TukeyHSD(anova_model) #this is a Base R TukeyHSD function
# Print Tukey results
print(tukey_results)

?PostHocTest
#The DescTools is a wrapper function. In other words, it uses the stats::TukeyHSD
#function, and others, to have one neat function to return different results. 

DescTools::PostHocTest(anova_model, method = "hsd") #tukey hsd output 
DescTools::PostHocTest(m1, method = "scheffe")
DescTools::PostHocTest(m1, method = "bonferroni")


#> If contrasts are pre-planned and independent → No adjustment needed.
#> If contrasts overlap or are exploratory → Adjust using Holm, Bonferroni, or FDR.
#> If conducting both planned contrasts and post-hoc tests → Adjust post-hoc but not planned contrasts.

# This is a different method for testing contrasts that I came across by asking
#> chatGPT some stuff. 
#> Notice that it doesn't give the same exact output. It uses the t-distribution 
#> instead of F distribution. You should be able to go from the t ratio to the 
#> F ratio by simply squaring the t value, but I found that it wasn't giving
#> me the same exact output. I'm not sure exactly why the results don't match 
#> exactly but it must be calculating the variance terms slightly differently. 
#> I imagine that with unequal group sizes this would be even more important. 

# Define planned contrasts
contrast_matrix <- rbind(
  "Therapy vs. Control" = c(-3, 1, 1, 1),   # Any therapy vs. Control
  "MBT vs. CBT & PA" = c(0, -1, -1, 2)     # Mindfulness vs. CBT & Psychoanalysis
)

# Run planned contrasts
contrast_results <- glht(anova_model, linfct = mcp(Therapy_Type = contrast_matrix))

# Print summary of contrasts
summary(contrast_results)


# Define a new contrast (CBT & MBT vs. PA & Control)
custom_contrast_matrix <- rbind(
  "CBT & MBT vs. PA & Control" = c(1, -1, 1, -1)  # Comparing two therapy styles
)

# Run custom planned contrast
custom_contrast_results <- glht(anova_model, linfct = mcp(Therapy_Type = custom_contrast_matrix))

# Print summary of custom contrast
summary(custom_contrast_results)

