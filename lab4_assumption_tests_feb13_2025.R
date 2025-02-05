

#> clear environment 
rm(list = ls())

# load packages 
library(tidyverse)
library(psych)
source("504_plot_theme.R")

df <- read.csv("data/friendship1.csv")

df$X <- NULL #removing the random extra column that gets added. 

df[] <- lapply(df, as.double) #turn all variables into double (i.e., numeric) variables.

# Data Frame Overview 
#> Variables 1-6 are all potential dependent variables. 
#> The experiment asked participants how they would feel if their best opposite-sex
#> friend formed either a new same-sex friendship or a new romantic relationship. 
#> The type of relationship the opposite-sex friend formed is a within-subjects
#> factor. 

#> There are two potential categorical variables: relationship status or gender 
#> Friendship duration can be considered a continuous control variable. 


# Coding: use.relationship.status.participant 
#> 1 = single 
#> 2 = committed relationship 

# Coding: gender 
# 1 = men
# 2 = women 

summary(as.factor(df$use.relationship.status.participant))
summary(as.factor(df$gender))


df$gender <- factor(df$gender, levels = c(1, 2), labels = c("men", "women"))


#> I will use gender and rom.happy to do the current analyses. 
#> Thus, I am testing whether men or women reported being happy by the thought of 
#> their best opposite-sex friend starting a new romantic relationship 


#> Running the Model: 
m1 <- aov(rom.happy ~ gender, data = df)
summary(m1) #absolutely no difference! 


#> Data Preparation -------

#> There are a few things from the model that we want to bring into our df so 
#> we can work with them. 
#> This is a lot of useful code for bringing in the residuals and fitted values
#> so we can easily work with them and use ggplot. 

#Grab the residuals from the model: 
m1res <- m1$residuals #how to view the residuals from the model 
resid(m1) #this also does the same thing as the above code 
residuals(m1) #and this is the same too 
all.equal(resid(m1), residuals(m1)) #you can check that they do the same thing with this 
identical(m1res, resid(m1)) #you can also check that they are the same with this 

#we first want to put the residuals into the data frame: 

# THE BELOW CODE DOES NOT WORK...
#df$m1res <- m1res 
#It doesn't work because there must be a single point of missing data on either
#the gender or rom.happy variable. (the length of m1res = 157 and the full df = 158)

#> Check for the missing data: 
df |> filter(is.na(rom.happy))
#> participant number 29 (pid) is the one with missing data on rom.happy. 
#> An easy solution is to just create a new df with this observation fitlered out,
#> then the above code would work to add the residuals. 
#> However, often times you will want to retain your full df and things get messy
#> when you have lots of different objects. 

#> This is a common problem when trying to put residuals back into the df and it
#> can be a huge pain to figure out. 
#> You need to put the residuals back into the data frame matching their specific
#> participant id. 
#> There are several ways you can accomplish this, I'll show you a few, which 
#> also serve as good review of the basics of the linear model. 

#> Remember, ANOVA is still part of the general linear model and an individuals
#> score on the dependent variable is made up of a few parts: 
#> the grand mean +
#> the group mean +
#> error 


#> the model.frame function is useful here. 
model.frame(m1)
#>  If you look closely, the row number goes from 1 to 158, but we know that there
#>  is only 157 responses. 
#>  We already know that pid 29 (which is also row number 29) is missing. If you
#>  check the output, the row number goes from 28 to 30. In other words, we can
#>  use the row number to merge the residuals in properly. 

df_mf <- m1$model #this is another way to access the model frame. 
df_mf$pid <- row.names(df_mf) # We need to put the row numbers into the df as a 
# variable so we can match on them. I've named them pid so I can match on them. 
str(df_mf$pid) #its a character vector but we want integers to match the df pid variable 
df_mf$pid <- as.numeric(df_mf$pid) 

#> We also need to bring the residuals into this model frame df. 
df_mf$m1res <- m1$residuals

#> You can bring in the fitted values too: 
#> Remember, the fitted values are the predicted values on the dependent variable. 
df_mf$m1fitted <- m1$fitted.values


#> If your data frame doesn't have a pid column, its very easy to create one: 
df$pid <- 1:nrow(df)  #I could also exchange "nrow(df)" for "158" here since I know the number of rows 

#> Now we can merge on pid 
# Merge the residuals back into `df` while keeping all original rows
df <- merge(df, df_mf[, c("pid", "m1res", "m1fitted")], by = "pid", all.x = TRUE)

#> The tidyverse version of merging is a bit easier and provides some additional
#> warnings that merge() won't tell you about: 
df |> left_join(df_mf[, c("pid", "m1res", "m1fitted")], by = join_by(pid))
#> You'll notice there is a "m1res.x" and a "m1res.y" 
#> This occurs because there are duplicate column names. 
#> .x = the original column from df
#> .y = the new column from df_mf that was merged in. 


#> You can accomplish all of the above by using the tidyverse (i.e., mutate) and an
#> if_else statement: 
df |> mutate(
  m1res1 = ifelse(row_number() %in% as.numeric(rownames(model.frame(m1))),
         residuals(m1), 
         NA),
  m1fitted1 = ifelse(row_number() %in% as.numeric(rownames(model.frame(m1))),
                    fitted(m1), 
                    NA)
) #end of mutate 

#> If you want to get a better idea of exactly what the above code does, just put it 
#> into ChatGPT and ask it to explain it! 



#> Also, remember that the residuals are calculated by taking observed score - fitted values:

df <- df |> mutate(
  resid_calc = rom.happy - m1fitted
) 
#check to see if the residuals calculated here equal the residuals from the model:
all.equal(df$m1res, df$resid_calc) #returns TRUE. 

#> Okay, I don't need that additional column though, so I'm going to remove it: 

df <- df |> select(-resid_calc) #selecting variables in the tidyverse is very convenient
str(df) #double check the df 

#> Independence Tests ---------

#> There is no practical reason to not meet the independence test for these
#> variables. Although there is a within-subject factor in this data, it is not 
#> used in this analysis (i.e., could compare the difference between ssf and rom).
#> The data was collected from random students on a college campus who were unlikely
#> to be any more alike because they were in the same class or be siblings. 

# making a residual versus fitted plot 
plot(m1, which = 1) #specify which = 1 to not get multiple plot outputs 
#> These residuals look a bit funky. 
#> On the x-axis, we can see that the mean of both groups is quite low (~1.7 on
#> a 1-5 point Likert scale) 
#> There are many larger residual values than smaller residual values. 
#> Nonetheless, there isn't particular trends that the residuals are different
#> across groups, the trend is the same across groups. 


#> It should be noted that you can recreate this same plot in the tidyverse,
#> which allows you to adjust things for output as needed. Many times you won't
#> need to use the tidyverse to create this plot because you don't need this plot
#> to look especially good for it to be useful. But maybe you want to show your
#> advisor something and the regular plot function does not display well in 
#> a RMarkdown (Quarto) document and using ggplot can solve that issue.

# Create residuals vs. fitted plot
df |> ggplot(aes(x = m1fitted, y = m1res)) +
  geom_point(alpha = 0.6) +  # Scatter plot of residuals
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Reference line at y = 0
  labs(
    title = "Residuals vs. Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme504()  

#> This is also very nice because you can now specify unique different values 
#> as desired: 

df$extreme_resid <- abs(df$m1res) > quantile(abs(df$m1res), 0.95, na.rm = TRUE)  # Top 5% residuals


df |> ggplot(aes(x = m1fitted, y = m1res, color = extreme_resid)) +
  geom_point(alpha = 0.6) +  # Scatter plot of residuals
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_color_manual(values = c("black", "red")) +  # Red for extreme residuals
  labs(
    title = "Residuals vs. Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme504()  
#> Admittedly, this isn't very useful here, but its a good example of how 
#> bringing things into ggplot allows for more flexibility with looking at your
#> data 


#> Normality Assumption (inferential) ----------

#> Can run shapiro-wilks on residuals or by-group

#Conduct Shapiro-Wilk test by group 
sw1 <- by(data = df$rom.happy, INDICES = df$gender, FUN = shapiro.test)
sw1
#The null hypothesis for the shapiro-wilk test is that the distribution of data
#provided is the same as the normal distribution. 
#> In both cases, there is evidence to reject the null hypothesis, neither the
#> men nor women have a normally distributed dependent variable. 


#Conduct Shapiro-Wilk test on model residuals 

#Remember, the residuals are the part of the scores that could not be predicted
#from the model. 
summary(m1$residuals) #the residuals are also standardized (i.e., mean of 0 and in standard
#deviation units)
#> Just based off of the summary, I can tell that the median is .15 standard 
#> deviations away from the mean, an initial indication of left (negative) skew. 
describe(m1res)$skew  #you can grab individual elements from the describe function
#like this. Handy in some cases. 
describe(m1res) #although, its a bit nicer to see everything most of the time. 

#> Run the test: 
shapiro.test(m1res)


#> The shapiro-wilks test is very over powered and you will almost always reject
#> the null hypothesis with large enough groups. As a very rough rule of thumb, 
#> when you have group sizes less than 50, its probably reasonable. 
#> Running an inferential test for normality is nice because it gives you a clear
#> decision: the data is normal or not. However, its also limited because (1) anova
#> can be pretty robust to normality violations and (2) visualizing the residuals
#> can give you a better sense of the severity of the non-normality. 


#> Normality Assumptions (visual)


#> Now that we have the residuals in the original df, we can do lots of things
#> with them. 

df |> ggplot(aes(x = m1res)) +
  geom_histogram(
    bins = 10 #sometimes you need to play around with the bins to make the histogram
    #look decent. 
  ) +
  theme504()
#> You can see that this warning pops up: 
#> Warning: Removed 1 row containing non-finite outside the scale range (`stat_bin()`).
#> This warning makes perfect sense considering we know that there is one participant
#> who does not have a residual value since they had missing data on rom.happy. 

#> This histogram looks reasonably normal with the exception that there are a lot
#> of values stacked up at less than 2 standard deviations below the residuals. 
#> These values cause that left skew. 

#> I happen to know why this is the case: there were more people that reported
#> higher numbers on the original rom.happy variable: 
df |> group_by(as.factor(rom.happy)) |> count() 
#This output now shows me the count for each response 
# 1 = not at all happy 
# 3 = somewhat happy 
# 5 = extremely happy 


#> Q-Q Plot 
plot(m1, which = 2)


#> Once again, you can put this plot into a ggplot as well to allow for some
#> helpful customizations. 

# Create a Q-Q plot for the residuals (m1res)

df |> 
  filter(!is.na(m1res)) |> 
  ggplot(aes(sample = m1res)) +
  stat_qq(size = 2, alpha = 0.7) +  # Q-Q plot points with transparency
  stat_qq_line(color = "red", linetype = "dashed") +  # Adds reference line
  labs(title = "Q-Q Plot of Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme504() 

#> You can play around with trying to identify outliers and changing colors of
#> points to better inform the graph. 
#> ChatGPT is a great resource for helping with that! 

#> Homogeneity of Variance Assumption (inferential) -------

library(car)


#> I hadn't specified my gender variable as a factor previously
leveneTest(rom.happy ~ gender, data = df, center = mean)
#> The null hypothesis is that all of the group variances are the same. We want
#> all of the variances of groups to be the same. 
#> So, we do not want to reject the null hypothesis. 
#> The group variances are the same per the Levene's test here. 

#> Again, similar to the shapiro test, this test is over powered and with larger
#> samples (and more groups), you will always reject the null, making the test
#> not that useful. 

#> Another rule of thumb is that the greatest group variance to the smallest 
#> group variance is not >4. 
#> You can calculate the variance by group with the following method: 
df |> group_by(gender) |> 
  summarize(
    var = var(rom.happy, na.rm = TRUE)
  )
#> I only have two groups, so its really easy to just calculate this proportion:
var_prop <- 1.58 / 1.33
var_prop #this is < 4, so that is a good indication that we meet the equal variance assumption

#> When you have a lot of groups this can be more tedious and its helpful to write
#> code that will give you the answer in a single step, no matter how many groups
#> you have: 

df |> group_by(gender) |> 
  summarize(
    group_var = var(rom.happy, na.rm = TRUE)
  ) |> 
  mutate(
    max_var = max(group_var),
    min_var = min(group_var),
    var_proportion = max_var / min_var
  )
# the var proportion column gives the output for var_prop as above. 
#> Notice how with the tidyverse you can accomplish a lot through the "piping" 
#> feature! 
#> It saves you from having to constantly save a new object and you can streamline 
#> your calculations. 



### Assumption Checking Function -------

#> All of that is a lot of coding! You can save yourself a lot of time by writing
#> a function to do all of those calculations at once...

#> Actually, I copy and pasted the above code into ChatGPT and had it write me
#> an initial function for this and then I made a few edits to the function to 
#> my liking. 
#> I recommend you start by writing the function yourself because there are a lot
#> of particular things about writing functions that you will need to know. Now
#> that I know them, I can easily edit functions that ChatGPT writes.

library(ggplot2)
library(dplyr)
library(car)  # For Levene's test

residual_diagnostics <- function(df, residual_col, fitted_col, extreme_col, group_col, dv_col, print = FALSE) {
  
  # Convert the group variable to a factor if it isn't already
  df[[group_col]] <- as.factor(df[[group_col]])
  
  # Create a column for extreme residuals (Top 5%)
  df[[extreme_col]] <- abs(df[[residual_col]]) > quantile(abs(df[[residual_col]]), 0.95, na.rm = TRUE)
  

  # ----- 2️⃣ Residuals vs. Fitted Plot (Highlighting Extreme Residuals) -----
  p1 <- ggplot(df, aes_string(x = fitted_col, y = residual_col, color = extreme_col)) +
    geom_point(alpha = 0.6) +  
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    scale_color_manual(values = c("black", "red")) +  
    labs(title = "Residuals vs. Fitted Values (Extreme Residuals Highlighted)",
         x = "Fitted Values",
         y = "Residuals") +
    theme_minimal()
  
  # ----- 3️⃣ Shapiro-Wilk Normality Tests -----
  shapiro_resid <- shapiro.test(df[[residual_col]])  # Test on residuals
  shapiro_by_group <- by(df[[dv_col]], df[[group_col]], shapiro.test)  # Test by group
  
  # ----- 4️⃣ Histogram of Residuals -----
  p2 <- ggplot(df, aes_string(x = residual_col)) +
    geom_histogram(bins = 10, fill = "blue", alpha = 0.5, color = "black") +
    labs(title = "Histogram of Residuals",
         x = "Residuals",
         y = "Count") +
    theme_minimal()
  
  # ----- 5️⃣ Q-Q Plot -----
  p3 <- df %>%
    filter(!is.na(.data[[residual_col]])) %>%
    ggplot(aes(sample = .data[[residual_col]])) +
    stat_qq(size = 2, alpha = 0.7) +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(title = "Q-Q Plot of Residuals",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()
  
  # ----- 6️⃣ Levene’s Test for Homogeneity of Variance -----
  levene_test <- leveneTest(as.formula(paste(dv_col, "~", group_col)), data = df, center = mean)
  
  # ----- 7️⃣ Proportion of Variance Calculation -----
  variance_by_group <- df %>%
    group_by(.data[[group_col]]) %>%
    summarize(group_var = var(.data[[dv_col]], na.rm = TRUE)) %>%
    mutate(
      max_var = max(group_var),
      min_var = min(group_var),
      var_proportion = max_var / min_var
    )
  
  # If print = TRUE, print all results
  if (print == TRUE) {
    cat("\n=== Shapiro-Wilk Test on Residuals ===\n")
    print(shapiro_resid)
    
    cat("\n=== Shapiro-Wilk Test by Group ===\n")
    print(shapiro_by_group)
    
    cat("\n=== Levene’s Test for Homogeneity of Variance ===\n")
    print(levene_test)
    
    cat("\n=== Variance Proportion by Group ===\n")
    print(variance_by_group)
    
    # Show plots
    print(p1)
    print(p2)
    print(p3)
  }
  
  # Return all objects as a list
  return(list(
    plots = list(residual_vs_fitted_highlight = p1,
                 residual_histogram = p2,
                 qq_plot = p3),
    shapiro_residuals = shapiro_resid,
    shapiro_by_group = shapiro_by_group,
    levene_test = levene_test,
    variance_by_group = variance_by_group
  ))
}

#> Testing the function: 
m1_diagonostics <- residual_diagnostics(df, 
                     "m1res", 
                     "m1fitted", 
                     "extreme_resids", 
                     "gender", 
                     "rom.happy",
                     print = FALSE #If you specify this as true it will automatically print everything
                     )
m1_diagonostics #you can print everything by calling the saved object if you
#specify print = FALSE

#> You can also grab individual plots or results if desired:

m1_diagonostics$plots[1] #grab the first plot 
m1_diagonostics$variance_by_group #the variance proportion test results 
















