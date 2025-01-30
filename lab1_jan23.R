
rm(list = ls()) #this clears your environment
#I recommend putting this at the top of most of your R script files so you can
#easily run the whole document without running into errors.
#> You can run the full document with: 
#> ctrl + alt + r
#> It is also very handy to run everything up to your cursor in the document. 
#> Use the following to do that: 
#> ctrl + alt + b

library(psych)
library(tidyverse)
library(codechest) #my personal package
#> If you want to download my package you'll need to use:
#remotes::install_github("ryandobson/codechest")
library(codechest)

df <- read.csv("data/eclsk.revised.csv", stringsAsFactors = FALSE)


typeof(df$income)


source("504_plot_theme.R") #source in the custom theme for plots

str(df)
glimpse(df)


#> 1 --------
#> Create a histogram for the age variable

p1 <- df |> ggplot(aes(x = age)) +
  geom_histogram(
    color = "red", #this is for outline 
    fill = "lightblue"
  ) +
  labs(
    x = "Participant Age",
    y = "Count",
    title = "Histogram of Age",
    caption = ""
  ) +
  theme504() #apply custom theme


#> 2 -------
#> Create box plots for the math variable by public
#> Which type of school performs better?


#Create a new variable in the dataframe that is a 
#factor of the original variable. 
df$publicF <- factor(df$public, levels = c(0, 1), labels = c("Non-public", "Public"))
str(df$publicF)

#See how many people are in each group 
summary(df$publicF)
summary(df$public) #this gives you the 5 number summary because the original variable is numeric (0 and 1)

p2 <- df |> ggplot(aes(y = math, color = publicF)) +
  geom_boxplot() +
  labs(
    y = "Math",
    color = "" #changing color to blank removes the legend title -- comment this out to see what happens
  )  +
  theme(
    axis.text.x = element_blank(), # Removes the x-axis tick labels -- not 
    axis.ticks.x = element_blank() # Removes the tick marks
  ) +
  theme504() #apply custom theme






#> 3 -------
#> Calculate the...
#> mean
#> median
#> range
#> IQR
#> standard deviation
#> variance
#> skewness 
#> kurtosis 
#> for the income variable. 

describe(df$income) #gives most things
summary(df$income) #for IQR 

var(df$income) #for variance 

#Remember, variance is also the square root of the standard deviation 
sqrt(var(df$income)) 









#> 4 -------
#> Save graphs and tables from (1) and (2) 

#Save the first plot into a "plots" folder in your github
ggsave(filename = "plots/lab1_age_hist.png", plot = p1, height = 9, width = 12)

ggsave(filename = "plots/lab1_school_boxplot.png", plot = p2, height = 9, width = 12)


# Grab the files and format them to easily put them into markdown (.qmd) file: ------

plot_directory <- "plots"

#List all files with .html extension in the directory
plot_files <- list.files(plot_directory, pattern = "\\.png$", full.names = TRUE)

#Print the list of model file paths
print(plot_files)

for (file in plot_files) {
  file <- sub("output/results/", "../results/", file)
  cat("![](", file, ")\n\n", sep = "")
}
#> copy and paste the relevant output into the desired spot in the .qmd document






# Render quarto file as word doc -----
library(quarto)

quarto_render(input = "lab1_jan23.qmd", #qmd file name
              output_file = "Dobson_Ryan_Lab1.docx", #output file name
              output_format = "docx")




