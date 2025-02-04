

#> clear environment 
rm(list = ls())

# load packages 
library(tidyverse)
library(psych)
source("504_plot_theme.R")

# read in example data 
df <- read.csv("data/distraction.csv")

# get an idea of the data 
str(df) 
# 'data.frame':	12 obs. of  4 variables:
# $ id        : int  1 2 3 4 5 6 7 8 9 10 ...
# $ score     : int  8 9 11 12 12 11 12 13 15 15 ...
# $ group     : int  1 1 1 1 2 2 2 2 3 3 ...
# $ group_name: chr  "TV" "TV" "TV" "TV" ...

#> You can see that id, score, and group all load in as integers (i.e., numeric)

#> group_name loads in as a "chr" which means its a character string 
#> group is the integer version of that character string 

#> You could use either variable to turn it into a factor. 

#> If you use the "group" variable you'll have to specify levels and the
#> labels that correspond to those levels. 
df$groupF <- factor(df$group, levels = c(1, 2, 3), labels = c("TV", "Quiet", "Music"))

#> If you use the group_name variable its a bit easier. 
df$group_nameF <- as.factor(df$group_name)
summary(df$group_nameF)

#> You can sanity check to ensure these variables are identical: 

#> Since there are only 12 observations, its easy to just look at the data:
view(df[, c("group_nameF", "groupF")]) 

#> Its a bit easier to do this in tidyverse: 
df |> select(matches("F")) |> view() #take a look at how to use "matches" 
#using "matches" is very useful when you have a lot of variables you need to grab
#with a specific naming pattern. 

#> Remember, "|>" is the same as "%>%", and you can easily type this on Windows
#> by clicking "ctrl + shift + m". Maybe try "command + shift + m" on Mac?  
df %>% select(group_nameF, groupF)


#> Check the descriptive statistics: 

#across groups 
describe(df$score)

#by groups 
psych::describeBy(df$score, group = df$groupF)
psych::describeBy(df$score, group = df$group_nameF)

#> Although you'll have to specify more information, I think the output from
#> the tidyverse is much easier to look at: 
df |> group_by(groupF) |> 
  summarize(
    n = n(),
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    var = var(score, na.rm = TRUE),
    skew = psych::skew(score, na.rm = TRUE),
    kurtosis = psych::kurtosi(score, na.rm = TRUE)
   #if you want, keep adding in other statistics you want following the same format. e.g., min, max
    )
#> The downside of this is that you only get information for one variable. Of course, 
#> you could just copy and paste all of the elements in summarize and then change the
#> variable to something else. I don't recommend doing this, its tedious and will 
#> certainly cause errors. 

#> The describe function is actually really powerful for giving descriptives 
#> of a lot of variables. The output just isn't as nice when you want to compare 
#> a single variable split up by a group. 

#e.g., Below, I've specified that I want the descriptives of all of the numeric columns:  
describe(df[sapply(df, is.numeric)]) # "sapply" is a more complicated function, one which 
# I don't even use that much. However, I should use it more because it REALLY starts
# to make your code more compact. You can save a lot of copying and pasting with it. 


#> Generating more data so graphs are a bit more realistic -----

set.seed(123)  # For reproducibility

# Define groups
groups <- c("TV", "Quiet", "Music")
means <- c(10, 12, 14)
sds <- c(1.83, 0.816, 1.41)

# Create data for each group
df1 <- data.frame(
  group = as.factor(rep(groups, each = 200)),
  score = c(
    rnorm(200, mean = means[1], sd = sds[1]),
    rnorm(200, mean = means[2], sd = sds[2]),
    rnorm(200, mean = means[3], sd = sds[3])
  )
)

summary(df1$group)

###> DV Graphs -------

#> Graph your dependent variable: 
df1 |> ggplot(aes(x = score)) + 
  geom_histogram(fill = "lightblue")


#> Histograms by group? 
#> Is this graph useful for anything? 
#> What can you gather from it? 
df1 |> ggplot(aes(x = score, fill = group)) +
  geom_histogram()


###> Running Models --------

# m1 = model 1 
#> What happens if you use the numeric "group" variable? 
summary(df$group)
summary(aov(score ~ group, data = df))
#> The model runs...

#> But are the results the same if you use the factor variable? 
summary(aov(score ~ groupF, data = df)) # NO, the results change. 

#> What was the model with the numeric "group" variable doing? 
summary(lm(score ~ group, data = df))
# Its simply running a linear regression, treating your independent variable
#> as continuous, instead of categorical. 
#> One way you can notice this happened is by looking at the degrees of freedom of
#> the model. For this model, your degrees of freedom should be number of groups (k)  - 1
#> In this case, you don't want that! 


#> What happens if you use the chr "group_name" variable? 
summary(df$group_name)
summary(aov(score ~ group_name, data = df))
# It runs and gives the same exact output as the factor...
summary(aov(score ~ group_nameF, data = df))

#> So why do we Need to change things to factors? 

#> Well, most typically, your categorical variables will probably be loaded in
#> as numeric variables and you will have a codebook to turn it into the 
#> character strings/factors. In other words, you'll just have the "group" variable.

#> Also, although the aov() function does handle the string as you expected, not
#> all functions do...
#> For example, you won't be able to get descriptive information easily on your
#> character variable: 
summary(df$group_name) #the output here doesn't tell you anything 
summary(df$group_nameF) #with the factor you now get your group sizes 
#> All in all, if your variable is a character, rather than a factor, you will run
#> into some issues with various functions that can be a headache. 


#> What happens if you run the model as a linear model with the factor? 

summary(lm(score ~ groupF, data = df))
# We now have an intercept and two effects: 
#> What do they mean? 
#> Look back at your descriptives! 

#> What hypothesis is the linear model testing compared to the aov model? 

#> Plotting results: 

#> What are boxplots useful for? 
#> These are good for spotting outliers as there will be dots outside of the range
#> Here, we don't have any outliers. 
df |> ggplot(aes(x = group_name, y = score)) +
  geom_boxplot() +
  theme504() +
  coord_cartesian(ylim = c(6, 16)) +
  labs(
    x = "", # I don't need the extra title on the x-axis 
    y = "Score" # For actual graphs, you'd want something more informative 
    #title = "", #plot title  
    #caption = "" #sometimes useful to also have a caption 
    )

#> Bar plots can be good for mean difference comparisons: 

#> Single variable bar graph comparing 1 or 2 groups 
bar_mean_plot <- function(data, variable, group_var_1, group_var_2) {
  
  dodge <- position_dodge(1)
  
  #Getting descriptive statistics
  des <- data |> group_by({{group_var_1}}, {{group_var_2}}) |>
    summarize(
      N = n(),
      mean = mean({{variable}}, na.rm = TRUE),
      sd = sd({{variable}}, na.rm = TRUE),
      se = sd / sqrt(n()),
      se_2 = se * 2
    )
  
  marg_plot <- des |> 
    ggplot(aes(x = {{group_var_1}}, 
               y = mean,
               fill = {{group_var_2}})) +
    geom_bar(
      stat = "identity",
      width = .5,
      position = dodge
    ) +
    geom_errorbar(aes(ymin = mean - (se * 2),
                      ymax = mean + (se * 2),
                      width = 0.2),
                  position = dodge) +
    labs(
      x = "",
      y = "",
      title = "",
      caption = "Error bars represent +/- 2 SEM"
    ) #+ 
    #coord_cartesian(ylim = c(1, 7)) +
    #scale_y_continuous(breaks = seq(1, 7, by = 1)) 
  return(marg_plot)
  
}

pbar1 <- bar_mean_plot(df, score, group_nameF) 

#After you get that it saves as a ggplot object and you can still add in
#additional edits: 
pbar1 +
  aes(fill = group_nameF) + #this will give you colors 
  scale_fill_manual(values = c("green","orange", "pink")) + #this will change the colors from left to right
  labs(
    y = "Score",
    fill = "Group Name" 
  ) +
  theme504()

#> I've also written more complex functions that graph specific comparisons and
#> provides a very informative graph: 


#> Independent Samples T-test Plotting --------

#> This is for plotting simple two group mean differences and it displays 
#> relevant group sizes, statistical information, and effect size. 
#> The function is slightly specific in use cases but I will probably have enough
#> instances of wanting to make simple two group comparisons that it will still 
#> be a useful test. 
#> It also assumes independent groups and that the df is in a long format. 
#> e.g., sex in one column and a continuous variable in another column on which
#> I want to compare results across sex. 

plot_mean_dif <- function(df, variable, group_var) {
  
  #> Filtering out NA's because they don't matter anyway:
  df <- df |> filter(!is.na({{group_var}}))
  
  ## I want a nice graph with all of the relevant statistical information 
  #> Report: 
  #> group n's 
  #> Cohen's d and 95% CI 
  #> p-value from statistical test 
  #> r equivalent of difference between two means 
  
  # Capture the unquoted variable names
  variable <- ensym(variable)
  group_var <- ensym(group_var)
  
  # Create a formula dynamically (if needed elsewhere)
  lin_formula <- reformulate(rlang::as_string(group_var), response = rlang::as_string(variable))
  
  #> Fit a linear model (to be able to grab means with emmeans) 
  lin_mod <- lm(lin_formula, data = df)
  
  emm_info <- emmeans(lin_mod, specs = rlang::as_string(group_var))
  emm_df <- as.data.frame(emm_info)
  
  # Compute Cohen's d and 95% CI for pairwise contrasts
  contrast_info <- contrast(emm_info, method = "pairwise", adjust = "none")
  cohens_d_info <- psych::cohen.d(df[[variable]], df[[group_var]])
  
  
  # Create a text label for contrasts with p-values and test statistics
  contrast_df <- as.data.frame(contrast_info)
  contrast_label <- sprintf(
    "t = %.2f, p = %.3f",
    contrast_df$t.ratio[1],
    contrast_df$p.value[1]
  )
  
  # Get group size information  
  group_ns <- df  |> 
    filter(!is.na({{variable}})) |> 
    group_by({{group_var}})  |> 
    summarise(n = n(), .groups = "drop")
  # Set group_ns - y_label_position based on the y-axis limits
  y_limits <- range(df[[rlang::as_string(variable)]], na.rm = TRUE)
  y_label_position <- y_limits[1] + .5
  
  print(y_limits)
  print(y_label_position)
  print(group_ns)
  
  variable_label <- attr(df[[variable]], "label") #base R version will be more evergreen
  
  plot_title <- paste(variable_label)
  
  y_label <- paste(variable)
  
  #> Cohen's D caption 
  cohens_d_cap <- as.data.frame(cohens_d_info$cohen.d)
  cohens_d_cap <- cohens_d_cap |> mutate(across(where(is.numeric), ~round(., 2)))
  
  caption1 <- paste0(cohens_d_cap$effect[1], ",")
  caption1.1 <- paste0("[", cohens_d_cap$lower[1], ",")
  caption1.2 <- paste0(cohens_d_cap$upper[1], "]")
  caption1.3 <- paste("r equivalent =", round(cohens_d_info$r, 2))
  
  caption1.4 <- paste("Cohen's d = ", caption1, "95% CI", caption1.1, caption1.2, caption1.3)
  
  
  plot <- ggplot() +
    # geom_boxplot( # Boxplot
    #   data = df, aes(x = {{group_var}}, y = {{variable}}), 
    #   alpha = 0.3, 
    #   width = 0.3,
    #   color = "white", #boxplot outline color 
    #   fill = "white" #inside the box color
    #   ) + 
    geom_jitter( # Raw data points 
      data = df, aes(x = {{group_var}}, y = {{variable}}),
      width = 0.1, 
      alpha = 0.6,
      fill = "#67aeca",
      color = "#67aeca"
    ) + 
    geom_point( # Estimated means (i.e., group means) 
      data = emm_df, aes(x = {{group_var}}, y = emmean), 
      color = "black", 
      size = 3
    ) + 
    geom_errorbar( # Error bars by confidence intervals 
      data = emm_df, aes(x = {{group_var}}, 
                         ymin = lower.CL, 
                         ymax = upper.CL), 
      width = 0.2, color = "black"
    ) +
    coord_cartesian(
      ylim = c(y_limits[1], y_limits[2])
    ) +
    scale_y_continuous(breaks = seq(y_limits[1], y_limits[2], by = 1)) +
    annotate(
      "text",
      x = 1.5,
      y = 4,
      label = contrast_label,
      size = 7
    )  +
    geom_text(
      data = group_ns,
      aes(x = {{group_var}}, y = y_label_position,
          label = paste0("n = ", n)),
      #vjust = -.5,  #the y label position is set above. This also adjust it. 
      hjust = 2,
      size = 7
    ) +
    labs( #labels 
      title = stringr::str_wrap(plot_title, width = 50),
      x = "",
      y = y_label,
      caption = caption1.4
    ) +
    jermeys_theme() 
  
  
  return(plot)  
  
}
#> I want to look at a t-test graph, so I'm going to create a dichotomous grouping
#> variable. 
df$binarygroup <- rep(c(1, 2), 6) #repeats "1" "2" 6 times 
df$binarygroup #look at the variable 
df$binarygroup <- as.factor(df$binarygroup)

plot_mean_dif(df, score, binarygroup)

# This function is really really nice for looking at simple group
# differences.  

























